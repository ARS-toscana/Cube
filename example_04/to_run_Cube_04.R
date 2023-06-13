#-------------------------------
# Example 1: based on aggregated data, aggregate further, along 3 dimensions: Geography (3 levels), CalendarTime (2 levels), and Gender (2 levels)

rm(list=ls(all.names=TRUE))

# set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

dirinput <- paste0(thisdir,"/input/")
diroutput <- paste0(thisdir,"/output/")

# load data.table
if (!require("data.table")) install.packages("data.table")
library(data.table)

#load function
source(paste0(thisdir,"/../Cube.R"))

#load input
data_example <- fread(paste0(thisdir,"/input/data_example4.csv"), sep = ",")

# USE THE FUNCTION 

# assign the mandatory argument assigned_levels, containing the levels of each dimension

assigned_levels <- vector(mode="list")
assigned_levels[["Ageband"]] <- c("Ageband")

# assign the argument assigned_statistics

assigned_statistics <- vector(mode="list")
for (col_name in c("prev_MS1", "prev_MS2", "prev_MS3", "prev_MS4", "prev_MS5", "in_population")) {
  assigned_statistics[[col_name]] <- "max"
}

# apply the function: note that the dimension Gender has its total computed, and that the statistics are not assigned, thus making Cube compute the default statistics (sum)

output <- Cube(input = data_example,
               dimensions = c("Ageband"),
               levels = assigned_levels,
               measures = c("prev_MS1", "prev_MS2", "prev_MS3", "prev_MS4", "prev_MS5", "in_population"),
               computetotal = c("Ageband"),
               statistics = assigned_statistics
)

View(output)
fwrite(output,file=paste0(diroutput,"/output.csv"))
