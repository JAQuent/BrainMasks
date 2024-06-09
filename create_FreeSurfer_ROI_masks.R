# This script creates FreeSurfer ROI masks for HCP-style analyses with CIFTI files
# /* 
# ----------------------------- Libraries and parameter ---------------------------
# */
# Libs
library(stringr)
# Load the ciftiTools package and point to the Connectome Workbench --------
library(cifti)
# devtools::install_github("mandymejia/ciftiTools", ref="12.0", upgrade = "never")
library(ciftiTools)
# Use correct location based on the computer used
if(Sys.info()[4] == "DESKTOP-335I26I"){
  # Work laptop
  ciftiTools.setOption("wb_path", "C:/Program Files/workbench-windows64-v1.5.0/workbench/bin_windows64")
} else if(Sys.info()[4] == 'DESKTOP-91CQCSQ') {
  ciftiTools.setOption("wb_path", "D:/workbench/bin_windows64")
} else if(Sys.info()[4] == 'alex-Zenbook-UX3404VA-UX3404VA') {
  ciftiTools.setOption("wb_path", "/usr/bin/wb_command")
} else {
  ciftiTools.setOption("wb_path", "D:/Program Files/workbench/bin_windows64")
}

# /* 
# ----------------------------- Load a sample CIFTI file ---------------------------
# */
# CIFTI files
parcellationFile <- "sourceFiles/Q1-Q6_RelatedValidation210.CorticalAreas_dil_Final_Final_Areas_Group_Colors_with_Atlas_ROIs2.32k_fs_LR.dlabel.nii"
surfLeft         <- "sourceFiles/S1200.L.inflated_MSMAll.32k_fs_LR.surf.gii"
surfRight        <- "sourceFiles/S1200.R.inflated_MSMAll.32k_fs_LR.surf.gii"

# Load MMP parcellation
cifti_fname <- parcellationFile
xii         <- ciftiTools::read_cifti(cifti_fname, brainstructures = "all", 
                                      surfL_fname = surfLeft, 
                                      surfR_fname = surfRight)

# Create base file
new_xii <- xii

# Replace everything with a zero
new_xii$data$cortex_left  <- matrix(as.integer(rep(0, length(new_xii$data$cortex_left))), ncol = 1)
new_xii$data$cortex_right <- matrix(as.integer(rep(0, length(new_xii$data$cortex_right))), ncol = 1)
new_xii$data$subcort      <- matrix(as.integer(rep(0, length(new_xii$data$subcort))), ncol = 1)

# /* 
# ----------------------------- Libraries and parameter ---------------------------
# */
# Get all subcortical regions for which we create ROI masks
subcort_regions <- as.character(unique(new_xii$meta$subcort$labels))


# Loop through all regions
for(i in 1:length(subcort_regions)){
  # File names
  fileName <- paste0("masks/FreeSurfer_regions/dlabel/", 
                     str_replace_all(subcort_regions[i], pattern = " ", replacement = "_"), 
                     ".dlabel.nii")
  
  # Create new XIFTI based on boolean
  temp_masks <- new_xii
  
  # Change the key labels
  old_key_colours <- temp_masks$meta$cifti$labels$`vertex areas`
  new_key_colours <- old_key_colours[-(2:nrow(old_key_colours)),]
  additionalColours <- data.frame(Key = 1,
                                  Red = 1,
                                  Green = 0,
                                  Blue = 0,
                                  Alpha = 1)
  new_key_colours <- rbind(new_key_colours, additionalColours)
  row.names(new_key_colours)[1] <- "???"
  row.names(new_key_colours)[2] <- str_replace_all(subcort_regions[i], pattern = " ", replacement = "_")
  
  # Add to temp mask  
  temp_masks$meta$cifti$labels$`vertex areas` <- new_key_colours
  
  # Set values to 1 that are part of the ROI
  bool_index <- temp_masks$meta$subcort$labels == subcort_regions[i]
  temp_masks$data$subcort[bool_index] <- 1
  
  # Write to disk
  write_cifti(temp_masks, cifti_fname = fileName,
              surfL_fname = surfLeft,
              surfR_fname = surfRight,
              verbose = FALSE)
}