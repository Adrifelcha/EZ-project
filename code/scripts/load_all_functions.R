###############################################################################
#     A script that loads all custom functions developed for this project, 
#                             along with the required R packages
###############################################################################


# Part 1: Working directory (WD) manipulation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# First, we identify the WD active when calling the present script
original_WD <- getwd()
# Then, we identify the directory where the present script is located
get_current_File_path <- getSourceEditorContext()$path
get_current_file_name <- gsub('.*/ ?(\\w+)', '\\1', get_current_File_path)
get_current_file_location <- gsub(get_current_file_name,"",get_current_File_path)
# Use this known location to move to the /functions/ folder containing all custom functions
setwd(get_current_file_location)
setwd("../functions/")
# Load all custom functions
for(archive in dir()){
  source(archive)
}
# Return to the WD that was active when this script was called
setwd(original_WD)