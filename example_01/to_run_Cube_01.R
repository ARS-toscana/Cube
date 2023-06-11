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
data_example <- fread(paste0(thisdir,"/input/data_example1.csv"), sep = ",")

# generate the order of the values of the variable Location

data_example[, order_Location := fcase(
  Location == "Toronto", 1L,
  Location == "Paris", 3L,
  Location == "New York", 2L
)]


# USE THE FUNCTION 

# assign the mandatory argument assigned_levels, containing the levels of each dimension

assigned_levels <- vector(mode="list")
assigned_levels[["Geography"]] <- c("Location","Country","Continent")
assigned_levels[["CalendarTime"]] <- c("Year","FiveYears")
assigned_levels[["Gender"]] <- c("Gender")

# assign the optional argument assigned_rule to have Cube create the second level of CalendarTime (FiveYears) 

assigned_rule <- vector(mode="list")
assigned_rule[["CalendarTime"]][["FiveYears"]] <- list("split_in_bands","Year", c(2020,2025,2030))

# assign the optional argument assigned_order, containing the order of the values of the dimensions, only for the first level of the dimension Geography

assigned_order <- vector(mode="list")
assigned_order[["Geography"]][["Location"]] <- "order_Location"

# apply the function: note that the dimension Gender has its total computed, and that the statistics are not assigned, thus making Cube compute the default statistics (sum)

output <- Cube(input = data_example,
               dimensions = c("Geography","CalendarTime","Gender"),
               levels = assigned_levels,
               measures = c("N"),
               computetotal = c("Gender"),
               rule_from_numeric_to_categorical = assigned_rule,
               summary_threshold = 100,
               order = assigned_order
)

View(output)
fwrite(output,file=paste0(diroutput,"/output.csv"))
