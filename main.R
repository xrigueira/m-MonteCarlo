# This file contains the implementation of the ms-outdec
# algorithm, which is a magnitude and shape outlier detector.
# The shape component is based on the CCR-periodogram proposed
# by Lopez-Oriona 2021, and the magnitude component is obtained
# with a weak multivariate version of the Fraiman-Muñiz depth.

# Get starting time
start_time <- Sys.time()

# Load the libraries
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

# Define the variables for the desired time units
time_frame <- "b" # "a" for months, "b" for weeks, "c" for days
span <- "a" # This variable is to select different combinations later
time_step <- "15 min"
variables <- c("conductivity", "nitrates", "oxygen", "pH", "temperature", "turbidity")
# variables <- c("conductivity", "nitrates", "oxygen")

# Call the functions to get the results
mts <- builder(time_frame = time_frame, span = span, time_step = time_step, variables = variables)
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

# Define the outliers
outliers <- global_depth[global_depth < quantile(global_depth, probs = c(0.10))]

# Plot the results
uni_grafic <- u_plotter(mts, outliers, variable_index = 1, variables) # univariate results
multi_grafic <- m_plotter(mts, time_unit = 1) # multivariate results
inter_uni_grafic <- inter_u_plotter(mts, outliers, variable_index = 1, variables) # interactive univariate
fda_grafic <- fda_u_plotter(mts, outliers, variable_index = 1, variables)

# Get ending time
end_time <- Sys.time()

# Output time elapsed
print(end_time - start_time)