# The second function plots all functions of a specific
# variable

plot_autofeeder <- function(i, data, outliers, series, plot_object) {

    if (colnames(data)[i] %in% names(outliers)) {

        plot_object <- plot_object +
            geom_line(aes(x = time, y = data[, i],  group = series[i]), color = "red")


    } else {

        plot_object <- plot_object +
            geom_line(aes(x = time, y = data[, i],  group = series[i]), color = "black")

    }

    return(plot_object)

}

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

u_plotter <- function(mts, outliers, variable_index, variables) {

    # Get the data of each desired variable and put it in a matrix (data)
    data <- matrix(ncol = length(mts$data), nrow = nrow(mts$data[[1]]))
    counter <- 1
    for (i in mts$data) {

        data[, counter] <- i[, variable_index] # This variable here is the number we have to change
        # depending on which variable we want, co2, no2 etc are in different columsn and
        # this number is the column we want

        counter <- counter + 1

    }

    colnames(data) <- mts$time
    data <- cbind(data, time = seq_len(nrow(mts$data[[1]])))
    data <- as.data.frame(data)
    series <- colnames(data)

    # Define the base plot
    y_label <- y_labeler(variable_index, variables)

    plot_object <- ggplot(data) +
        ggtitle(glue("Functional {variables[variable_index]} weekly data")) +
        xlab("Time (15 min intervals)") +
        ylab(y_label) + # plotly does not take this kind of "math mode" label
        scale_color_manual(name='Leyend', breaks=c('Outliers', 'No outliers'), values=c('Outliers'='red', 'No outliers'='black')) +
        theme_gray(base_size = 17)


    # Add all the remaining curves with a loop
    for (i in seq_len(ncol(data) - 1)) {

        plot_object <- plot_autofeeder(i, data, outliers, series, plot_object)

    }

    return(plot_object)

}
