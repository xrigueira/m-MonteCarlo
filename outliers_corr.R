# The "vars" have to be assigned with "global_depth"
# after every run of main.R for the desired variables

library(matrixStats)

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
# Get the names
names <- sort(names(var1))

indexes_var1 <- indexer(names = names, variable = var1)
indexes_var2 <- indexer(names = names, variable = var2)
indexes_varn <- indexer(names = names, variable = varn)


# Create de df
df <- data.frame(indexes_var1, indexes_var2, indexes_varn)
rownames(df) <- names # Inser the row names

# Calculate the variance of the indexes
df$mad <- rowMads(as.matrix(df), method = "media")
# df$mad <- apply(df[, -1], 1, mad)

# Get the mean of the variance
outliers_corr <- mean(sort(df$mad))
print(outliers_corr)