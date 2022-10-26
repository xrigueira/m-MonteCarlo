# To get the accuracy: (1) the artificial outliers are marked with a 1
# (2) Get their week number and remove the duplicates (3) compare the
# resulting list with outliers

outlying_weeks <- unique(df_clean[df_clean$outlier == 1, c("week")])

# To compare I would have to do the detection again next
library(tidyverse)
library(glue)
library(reshape2)
library(mlmts)
library(fda.usc)

# Load the files
source("builder.R")
source("sha-outdec.R")
source("mag-outdec.R")
source("glob-outdec.R")
source("u-plotter.R")
source("m-plotter.R")
source("inter_u-plotter.R")
source("fda_u-plotter.R")
source("outlier-generator.R")

# Define the variables for the desired time units
time_frame <- "b" # "a" for months, "b" for weeks, "c" for days
span <- "a" # This variable is to select different combinations later
time_step <- "15 min"
variables <- c("conductivity", "nitrates", "oxygen", "pH", "temperature", "turbidity")
# variables <- c("conductivity", "nitrates", "oxygen")
montecarlo <- "Yes"

# Call the functions to get the results
mts <- builder(time_frame = time_frame, span = span, time_step = time_step, variables = variables, montecarlo = montecarlo)
print("[INFO] mts obtained")

# Shape depth
shape_depth <- shape_outdec(mts)
print("[INFO] shape depth obtained")

# Magnitude depth
magnitude_depth <- magnitude_outdec(mts)
print("[INFO] magnitude depth obtained")

# Global depth (combination of magnitude and shape)
global_depth <- global_outdec(mts, shape_depth, magnitude_depth)
print("[INFO] global depth obtained")

# Define the outliers monte carlo
outliers_mc <- global_depth[global_depth < quantile(global_depth, probs = c(0.10))]

# Plot the results
uni_grafic <- u_plotter(mts, outliers, variable_index = 1, variables) # univariate results
multi_grafic <- m_plotter(mts, time_unit = 1) # multivariate results
inter_uni_grafic <- inter_u_plotter(mts, outliers, variable_index = 1, variables) # interactive univariate
fda_grafic <- fda_u_plotter(mts, outliers, variable_index = 1, variables)

# Now check how many of outliers are in outlying weeks and get the accuracy
outliers_names <- as.numeric(names(outliers)) # or outliers mc

commons <- intersect(outliers_names, outlying_weeks)

accuracy <- (length(commons)) / length(outliers_names)