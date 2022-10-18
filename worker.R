# Just a file to get resutls faster

# Get starting time
start_time <- Sys.time()

# Load the libraries
library(tidyverse)
library(glue)
library(reshape2)
library(mlmts)
library(fda.usc)
library(matrixStats)

# Load the files
source("builder.R")
source("sha-outdec.R")
source("mag-outdec.R")
source("glob-outdec.R")

indexer <- function(names, variable) {

    # Get the index of each name in each variable
    indexes_var <- c()
    counter <- 1
    for (i in names) { # TURN THIS BLOCKS OF CODE INTO A FUNCTION

        indexes_var[counter] <- match(variable[i], variable)

        counter <- counter + 1

    }

    return(indexes_var)

}

time_frame <- "a"
span <- "a"

# FIRST VARIABLE
variables <- c("Oxigeno")

mts <- builder(time_frame = time_frame, span = span, variables = variables)
shape_depth <- shape_outdec(mts)
magnitude_depth <- magnitude_outdec(mts)
global_depth <- global_outdec(mts, shape_depth, magnitude_depth)
outliers <- global_depth[global_depth < quantile(global_depth, probs = c(0.10))]

var1 <- global_depth
print("[INFO] var1 obtained")

# SECOND VARIABLE
variables <- c("Temperatura")

mts <- builder(time_frame = time_frame, span = span, variables = variables)
shape_depth <- shape_outdec(mts)
magnitude_depth <- magnitude_outdec(mts)
global_depth <- global_outdec(mts, shape_depth, magnitude_depth)
outliers <- global_depth[global_depth < quantile(global_depth, probs = c(0.10))]

var2 <- global_depth
print("[INFO] var2 obtained")

# N VARIABLE
variables <- c("Oxigeno", "Temperatura")

mts <- builder(time_frame = time_frame, span = span, variables = variables)
shape_depth <- shape_outdec(mts)
magnitude_depth <- magnitude_outdec(mts)
global_depth <- global_outdec(mts, shape_depth, magnitude_depth)
outliers <- global_depth[global_depth < quantile(global_depth, probs = c(0.10))]

varn <- global_depth
print("[INFO] varn obtained")


# COMPUTE THE OUTLIER CORRELATION
# Get the names
names <- sort(names(var1))

indexes_var1 <- indexer(names = names, variable = var1)
indexes_var2 <- indexer(names = names, variable = var2)
indexes_varn <- indexer(names = names, variable = varn)


# Create de df
df <- data.frame(dates = names, indexes_var1, indexes_var2, indexes_varn)
# rownames(df) <- names # Inser the row names

# Calculate the variance of the indexes
df$var <- rowVars(as.matrix(df[, c(-1)]))
# df$mad <- apply(df[, -1], 1, mad)

# Get the mean of the variance
outliers_corr <- mean(sort(df$var))
print(outliers_corr)

# Get ending time
end_time <- Sys.time()

# Output time elapsed
print(end_time - start_time)