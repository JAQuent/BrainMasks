# This script creates separate CIFTIs for SN and VTA from the Adcock atlas

# Notes: More info in Use_MNI_nifti_2_create_ROI_cifiti.Rmd. Important: the 
# subcortical mask from the CIFTI cuts away from voxels from the mask. 
# /* 
# ----------------------------- Libraries and parameter ---------------------------
# */
# Libraries
library(oro.nifti)
library(ciftiTools)
library(stringr)

# Set path to workbench
ciftiTools.setOption("wb_path", "C:/Program Files/workbench/bin_windows64")

# List of NIFTI files
nifti_fileNames <- c("E:/Seafile/imaging_results/Midbrain_Atlases_all/mean_SN-L.nii.gz",
                     "E:/Seafile/imaging_results/Midbrain_Atlases_all/mean_SN-R.nii.gz",
                     "E:/Seafile/imaging_results/Midbrain_Atlases_all/mean_VTA-L.nii.gz",
                     "E:/Seafile/imaging_results/Midbrain_Atlases_all/mean_VTA-R.nii.gz")

# Create a template
## Load a random z-map
cifti_temp <- "E:/Seafile/imaging_results/SpaNov/OLMe_7T_SpaNov_gradient_6lvl_smo4_MSMAll/cope7.feat/stats/vwc/results_lvl2cope1_dat_ztstat_c1.dscalar.nii"
temp_xii   <- read_cifti(cifti_temp, brainstructures = "all")

## Create empty XIFTI based on this template
temp_xii <- newdata_xifti(temp_xii, 0)

# Use the subcortical mask of the CIFTI file to create a one-dimensional matrix/vector
subcort_mask   <- temp_xii$meta$subcort$mask

# /* 
# ----------------------------- Load NIFTIs and convert to CIFTIs ---------------------------
# */
# Loop through the nifti_fileNames
for(i in 1:length(nifti_fileNames)){
  # Load the NIFTI file
  ROI_nii <- readNIfTI(nifti_fileNames[i])
  ROI_nii <- cal_img(ROI_nii) # Not sure if this is necessary
  
  # Convert the NIFTI file to a one-dimensional matrix/vector
  subcort_matrix <- ROI_nii[subcort_mask]
  
  # Create new XIFTI
  new_xii <- temp_xii
  
  # Add this as new data to the XIFTI object
  new_xii$data$subcort <- matrix(subcort_matrix, nrow = length(subcort_matrix), ncol = 1)
  
  # Create new name
  new_name <- str_replace(nifti_fileNames[i], ".nii.gz", ".dscalar.nii")
  
  # Write CIFTI file
  write_xifti(new_xii, new_name)
}