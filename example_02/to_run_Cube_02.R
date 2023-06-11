#-------------------------------
# Example 2: based on individual-level data, generate a dataset containing descriptive statistics: counts of individuals (measure: N), mean and standard deviation of age (measure: age), median of followup time (measure: followup), and prevalence (statistics: 'mean') of a comorbidity stored in the binary variable 'diabetes' (measure: diabetes). The statistics are calculated across 2 dimensions: Ageband (2 levels) and Gender (2 levels). Also, counts of the lower level are computed as a proportion within the higher level  (order = 99), in both dimensions. 

rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
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
data_example <- fread(paste0(thisdir,"/input/data_example2.csv"), sep = ",")

# create a 'count' variable
data_example <- data_example[, N := 1]


#USE THE FUNCTION 

# assign the mandatory argument assigned_levels, containing the levels of each dimension

assigned_levels <- vector(mode="list")
assigned_levels[["Ageband"]] <- c("ageband")
assigned_levels[["Gender"]] <- c("gender")

# assign the optional argument proportion_rule, containing the level that acts as a denominator for proportions

proportion_rule <- vector(mode="list")
proportion_rule[["Ageband"]][["N"]] <- 99
proportion_rule[["Gender"]][["N"]] <- 99

# assign the optional argument assigned_order, containing the order of the values of the dimensions, only for the first level of the dimension Ageband

assigned_order <- vector(mode="list")
assigned_order[["Ageband"]][["ageband"]] <- "ageband_num"

# assign the argument assigned_statistics

assigned_statistics <- vector(mode="list")
assigned_statistics[["N"]] <- c("sum")
assigned_statistics[["age"]] <- c("mean","sd")
assigned_statistics[["diabetes"]] <- c("mean")
assigned_statistics[["followup"]] <- "median"

# apply the function: note that both dimensions have a total computed, and that the hierarachy datasets are saved in memory at the end of execution (savhhierarchy = T)

output <- Cube(input = data_example,
               dimensions = c("Ageband","Gender"),
               levels = assigned_levels,
               measures = c("N","age","diabetes","followup"),
               computetotal = c("Ageband","Gender"),
               statistics = assigned_statistics,
               summary_threshold = 100,
               order = assigned_order,
               proportion = proportion_rule,
               savhierarchy = T
)


View(output)
fwrite(output,file = paste0(diroutput,"output.csv"))

# save the hierarchy of all dimensions as a file on the hard drive

invisible(lapply(names(ouput_Hierarchy), function(x) fwrite(ouput_Hierarchy[[x]],
                                                  file = paste0(diroutput, "ouput-", x, "-Hierarchy.csv"))))
