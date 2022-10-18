# This function plots a desired variable using
# fda.usc library for a functional representation
# of the data

y_labeler <- function(variable_index, variables) {

    if (variables[variable_index] == "amonium") {
        y_label <- "Value " ~ (m*g/L)
    } else if (variables[variable_index] == "conductivity") {
        y_label <- "Value " ~ (mu*S/cm)
    } else if (variables[variable_index] == "nitrates") {
        y_label <- "Value " ~ (m*g/L)
    } else if (variables[variable_index] == "oxygen") {
        y_label <- "Value " ~ (m*g/L)
    } else if (variables[variable_index] == "pH") {
        y_label <- "Value"
    } else if (variables[variable_index] == "temperature") {
        y_label <- "Value (\u00B0C)"
    } else if (variables[variable_index] == "turbidity") {
        y_label <- "Value (NTU)"
    }

    return(y_label)
}

fda_u_plotter <- function(mts, outliers, variable_index, variables) {

    # Matrix to create the univariate fdata object
    mat <- matrix(ncol = nrow(mts$data[[1]]), nrow = length(mts$time))

    counter <- 1
    for (i in mts$data) {

        mat[counter, ] <- i[, variable_index]
        counter <- counter + 1

    }

    # Insert the time stamps as column names
    rownames(mat) <- mts$time

    # Convert the matrix to fdata class (discrete data)
    mat_fdata <- fdata(mat)

    # Convert to functional data with a corr >= 0.95
    corr <- 0
    n_basis <- 2

    while (corr < 0.95) {

        mat_fd <- fdata2fd(mat_fdata, type.basis = "fourier", nbasis = n_basis)
        # Reverse fd object to fdata to be able to do the comparison in the test
        mat_fd2fdata <- fdata(mat_fd, argvals = mat_fdata$argvals)

        # Pearson correlation test
        result_pearson <- cor.test(mat_fdata$data, mat_fd2fdata$data, method = c("pearson"))

        corr <- result_pearson$estimate

        n_basis <- n_basis + 1

    }

    y_label <- y_labeler(variable_index, variables)

    plot_object <- plot(mat_fd, main = glue("Functional {variables[variable_index]} weekly data"),
            xlab = "Time (15 min intervals)",
            ylab = y_label)

    return(plot_object)
}
