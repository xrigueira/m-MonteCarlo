# This file does the magnitude outleir deteciton

magnitude_outdec <- function(mts) {

    # Matrix to create the univariate fdata object
    mat <- matrix(ncol = nrow(mts$data[[1]]), nrow = length(mts$time))

    # Matrix that will contain the depths of all curves of each variable
    mat_depths <- matrix(ncol = length(mts$time), nrow = ncol(mts$data[[1]]))
    colnames(mat_depths) <- mts$time

    number_variables <- seq_len(ncol(mts$data[[1]]))

    for (i in number_variables) {

        counter <- 1
        for (j in mts$data) {

            mat[counter, ] <- j[, i]
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

        # Get the depths of each curve
        depth_results <- depth.FM(mat_fd)
        depth <- depth_results$dep

        mat_depths[i, ] <- unname(depth)

        # Here I could save the functional plot for each variable

    }

    # Get multivariate magnitude depth
    mag_depth <- colMeans(mat_depths)

    # Sort it
    mag_depth <- sort(mag_depth)

    return(mag_depth)

}