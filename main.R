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
source("outlier-generator.R")

# Define the variables for the desired time units
time_frame <- "b" # "a" for months, "b" for weeks, "c" for days
span <- "a" # This variable is to select different combinations later
time_step <- "15 min"
variables <- c("conductivity", "nitrates", "oxygen", "pH", "temperature", "turbidity")
# variables <- c("conductivity", "nitrates", "oxygen")
montecarlo <- "No"

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

# Define the outliers
outliers <- global_depth[global_depth < quantile(global_depth, probs = c(0.10))]

# # Plot the results. Comment out when running many out of testing
# uni_grafic <- u_plotter(mts, outliers, variable_index = 1, variables) # univariate results
# multi_grafic <- m_plotter(mts, time_unit = 1) # multivariate results
# inter_uni_grafic <- inter_u_plotter(mts, outliers, variable_index = 1, variables) # interactive univariate
# fda_grafic <- fda_u_plotter(mts, outliers, variable_index = 1, variables)

# Monte Carlo starts here
# Start variables
accuracies <- c()
counter <- 0

# Start the while loop
while (counter < 2) {

    # Clean the database and input the artificial outliers
    df_clean <- outlier_generator(variables, outliers) # Ojo aquí porque en la primera iteración hay que pasar outliers pero luego outliers_mc. Arreglar esto de alguna forma

    # Get the week number of the artificial outliers
    outlying_weeks <- unique(df_clean[df_clean$outlier == 1, c("week")])

    # Apply outlier detection to get the outliers detected on df_clean
    montecarlo <- "Yes"

    mts <- builder(time_frame = time_frame, span = span, time_step = time_step, variables = variables, montecarlo = montecarlo)
    print("[INFO Monte Carlo] mts obtained")

    # Shape depth
    shape_depth <- shape_outdec(mts)
    print("[INFO Monte Carlo] shape depth obtained")

    # Magnitude depth
    magnitude_depth <- magnitude_outdec(mts)
    print("[INFO Monte Carlo] magnitude depth obtained")

    # Global depth (combination of magnitude and shape)
    global_depth <- global_outdec(mts, shape_depth, magnitude_depth)
    print("[INFO Monte Carlo] global depth obtained")

    # Define the outliers monte carlo
    outliers_mc <- global_depth[global_depth < quantile(global_depth, probs = c(0.10))]

    # Just testing. Comment out when not testing
    uni_grafic <- u_plotter(mts, outliers_mc, variable_index = 1, variables) # univariate results
    multi_grafic <- m_plotter(mts, time_unit = 1) # multivariate results
    inter_uni_grafic <- inter_u_plotter(mts, outliers_mc, variable_index = 1, variables) # interactive univariate
    fda_grafic <- fda_u_plotter(mts, outliers_mc, variable_index = 1, variables)

    # Check if the artificial outliers are within the ones detected
    outliers_mc_names <- as.numeric(names(outliers_mc)) # or outliers mc

    # Get accurary rate and append to accuracies
    commons <- intersect(outliers_mc_names, outlying_weeks)
    accuracy <- (length(commons)) / (length(outliers_mc_names))

    accuracies <- append(accuracies, accuracy)

    counter <- counter + 1

}

# Get the average of accuracies
final_accuracy <- mean(accuracies)
print(final_accuracy)
# Get ending time
end_time <- Sys.time()

# Output time elapsed
print(end_time - start_time)