# This set of functions get rid of those outlying timeframes
# in the database and input new artificial outliers for the
# Monte Carlo test

magnitude_outliers <- function(df_clean, variables, new_outliers) {

    # Get the number that has to multiply the data
    mag_factor <- runif(1, 1.5, 2.25)

    # Now multiply and save it back into df_clean
    df_clean[df_clean$week == new_outliers[1], variables] <- df_clean[df_clean$week == new_outliers[1], variables] * mag_factor
    df_clean[df_clean$week == new_outliers[1], c('outlier')] <-  1

    return(df_clean)

}

shape_outliers <- function(df_clean, variables, new_outliers) {

    # Multiply by a sine wave to generate the shape outliers
    # https://rstudio-pubs-static.s3.amazonaws.com/110183_06adc5f01fc940f98fdc0822ac408de0.html
    df_clean[df_clean$week ==  new_outliers[1], variables] <- abs((df_clean[df_clean$week ==  new_outliers[1], variables]) * sin(df_clean[df_clean$week ==  new_outliers[1], variables]))
    df_clean[df_clean$week ==  new_outliers[1], c('outlier')] <-  1

    return(df_clean)
}

mixed_outliers <- function(df_clean, variables, new_outliers) {

    # Get the number that has to multiply the data
    mag_factor <- runif(1, 1.5, 2.25)

    # Now apply both contamination models and it back into df_clean
    df_clean[df_clean$week ==  new_outliers[1], variables] <- abs((df_clean[df_clean$week ==  new_outliers[1], variables]) * sin(df_clean[df_clean$week ==  new_outliers[1], variables])) * mag_factor
    df_clean[df_clean$week ==  new_outliers[1], c('outlier')] <-  1

    return(df_clean)
}

outlier_generator <- function(variables, outliers) {

    # Delete the detected outliers
    outliers_names <- as.numeric(names(outliers))

    # Read the database, delete those weeks in outliers. Draw inspiration from builder
    df <- read.csv("Database/data_pro.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

    # Once the outliers are deleted, save the clean database
    df_clean <- df[!(df$week %in% outliers_names), ]

    # Mark all rows as not outliers with a zero (adding new column)
    df_clean$outlier <- integer((nrow((df_clean))))

    # Create the artificial outleirs
    # Extract the total length of weeks to get the number of outliers to generate (15%)
    weeks <- c(df_clean$week)[!duplicated(c(df_clean$week))]
    num_outliers <- as.integer(length(weeks) * 0.15)

    # Select num_outliers weeks from weeks randomly
    new_outliers <- sample(weeks, num_outliers)
    print(new_outliers)

    # Define how many weeks will be outliers in each model (33% of num_outliers)
    num_cont_outliers <- as.integer(length(new_outliers)) / 3 # cont -> contaminated

    # Input the magnitude outliers
    for (i in new_outliers[1:num_cont_outliers]) {

        df_clean <- magnitude_outliers(df_clean, variables, new_outliers)

        # Delete the week made into an outlier from the new_outliers
        new_outliers <- new_outliers[-c(1)]
        print(new_outliers)
    }

    # Input the shape outliers
    for (i in new_outliers[1:num_cont_outliers]) {

        df_clean <- shape_outliers(df_clean, variables, new_outliers)

        # Delete the week made into an outlier from the new_outliers
        new_outliers <- new_outliers[-c(1)]
        print(new_outliers)
    }

    # Input the mixed outliers
    for (i in new_outliers[1:num_cont_outliers]) {

        df_clean <- mixed_outliers(df_clean, variables, new_outliers)

        # Delete the week made into an outlier from the new_outliers
        new_outliers <- new_outliers[-c(1)]
        print(new_outliers)
    }

    return(df_clean)

}