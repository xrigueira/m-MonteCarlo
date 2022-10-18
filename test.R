# Delete the detected outliers

for (i in names(outliers)) {
    print(as.numeric((i)))
}

# Read the database, delete those weeks in outliers. Draw inspiration from builder

df <- read.csv("Database/data_pro.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

# https://statisticsglobe.com/r-remove-row-from-data-frame-condition
# https://www.geeksforgeeks.org/how-to-conditionally-remove-rows-in-r-dataframe/

# Once deleted the outleirs, save the clean database