# This script quickly loops through the files and converts dlabel to dscalar files to be used in ROI analysis. 
# This script was only tested/run on ubuntu. 

# Libraries
library(stringr)

# List all files
allFiles <- list.files("masks/dlabel", full.names = TRUE)

# Only use files we're interested in
allFiles <- allFiles[str_detect(allFiles, pattern = "Poppenk_AP") | str_detect(allFiles, pattern = "Poppenk_HBT")]

# Output folder
outputFolder <- "masks/dscalar/"

# Create a dscalar ROI file
base_cm <- "wb_command -cifti-label-to-roi INPUT OUTPUT -key 1 -map 1"

# Loop through all files
for(i in 1:length(allFiles)){
  # Create output name
  outputFile <- str_replace_all(allFiles[i], pattern = "dlabel", replacement = "dscalar")
  
  # Replace placeholders from base command
  temp_cmd <- str_replace_all(base_cm, pattern = "INPUT", replacement = allFiles[i])
  temp_cmd <- str_replace_all(temp_cmd, pattern = "OUTPUT", replacement = outputFile)
  
  # Run
  system(temp_cmd)
}