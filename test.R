# Delete the detected outliers

outliers_names <- as.numeric(names(outliers))

# Read the database, delete those weeks in outliers. Draw inspiration from builder
df <- read.csv("Database/data_pro.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

# Once the outliers are deleted, save the clean database
df_clean <- df[!(df$week %in% outliers_names), ]

# Mark all rows as not outliers with a zero (adding new column)
df_clean$outlier <- integer((nrow((df_clean))))

# # Create the artificial outliers
variables <- c("conductivity", "nitrates", "oxygen", "pH",  "temperature", "turbidity")

# Now multiply and save it back into df_clean
df_clean[df_clean$week == 6, variables] <- df_clean[df_clean$week == 6, variables] * 100
df_clean[df_clean$week == 6, c('outlier')] <-  1

# # For the sine wave it could be something like this
df_clean[df_clean$week == 7, variables] <- abs((df_clean[df_clean$week == 7, variables]) * sin(df_clean[df_clean$week == 7, variables]))
df_clean[df_clean$week == 7, c('outlier')] <-  1
# https://rstudio-pubs-static.s3.amazonaws.com/110183_06adc5f01fc940f98fdc0822ac408de0.html

# And if i want to do both
df_clean[df_clean$week == 9, variables] <- abs((df_clean[df_clean$week == 9, variables]) * sin(df_clean[df_clean$week == 9, variables])) * 100
df_clean[df_clean$week == 9, c('outlier')] <-  1

# Now make te 100 a random int and the week number also a random int. No more than
# 10 outliers or 9 (3 of each) per run.
# Put the different options into a function