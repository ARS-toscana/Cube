#-------------------------------
# example 1:

rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

dirinput <- paste0(thisdir,"/input/")
diroutput <- paste0(thisdir,"/output/")

#load function
# source(paste0(thisdir,"/../Cube.R"))

# load data.table
if (!require("data.table")) install.packages("data.table")
library(data.table)
library(lubridate)


#load input
data_example <- fread(paste0(thisdir,"/input/data_example1.csv"), sep = ",")

#USE THE FUNCTION 

# assign the levels of each dimension
assigned_levels <- vector(mode="list")
assigned_levels[["Geography"]] <- c("Location","Country","Continent")
assigned_levels[["CalendarTime"]] <- c("Year","FiveYears")
assigned_levels[["Gender"]] <- c("Gender")

# assign the intervals needed to create FiveYears

assigned_rule <- vector(mode="list")
assigned_rule[["CalendarTime"]][["FiveYears"]] <- list("split_in_bands","Year",c(2020,2025,2030))

# apply the function

output <- Cube(input = data_example,
               dimensions = c("Geography","CalendarTime","Gender"),
               levels = assigned_levels,
               measures = c("N"),
               computetotal = c("Gender"),
               rule_from_numeric_to_categorical = assigned_rule
)
