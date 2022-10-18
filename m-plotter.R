# This file, which has to evolve into a function
# plots all functions/variables of a specific time frame

m_plotter <- function(mts, time_unit) {

    data <- mts$data[[time_unit]]

    data <- cbind(data, time = seq_len(nrow(mts$data[[1]])))

    data <- as.data.frame(data)

    data <- melt(data, id.vars = "time", variable.name = "series")

    plot_object <- ggplot(data, aes(time, value)) +
        geom_line(aes(colour = series), size = 0.5) +
        labs(title = glue("Multivariate plot of the time interval #", time_unit), x = "Time (15 min intervals)", y = "Value") + # y = "Value" ~ (mu*g/m^3)
        theme_gray(base_size = 17)

    return(plot_object)

}