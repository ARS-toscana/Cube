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

source("../Cube.R")

#load input
data_example <- fread(paste0(thisdir,"/input/data_example2.csv"), sep = ",")


#USE THE FUNCTION 

# create the count variable

data_example <- data_example[, N := 1]

# assign the levels of each dimension
assigned_levels <- vector(mode="list")
assigned_levels[["Ageband"]] <- c("ageband")
assigned_levels[["Gender"]] <- c("gender")


# assign the proportion rule
proportion_rule <- vector(mode="list")
proportion_rule[["Ageband"]][["N"]] <- 99
proportion_rule[["Gender"]][["N"]] <- 99

# assign the order

assigned_order <- vector(mode="list")
assigned_order[["Ageband"]][["ageband"]] <- "ageband_num"

# assign the statistics

assigned_statistics <- vector(mode="list")
assigned_statistics[["N"]] <- c("sum")
assigned_statistics[["age"]] <- c("mean","sd")
assigned_statistics[["followup"]] <- "median"
# apply the function

output <- Cube(input = data_example,
               dimensions = c("Ageband","Gender"),
               levels = assigned_levels,
               measures = c("N","age","followup"),
               computetotal = c("Ageband","Gender"),
               statistics = assigned_statistics,
               summary_threshold = 100,
               order = assigned_order,
               proportion = proportion_rule
)


View(output)
fwrite(output,file=paste0(diroutput,"output.csv"))
