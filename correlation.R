# I think the name says it all
library(corrplot)

correlation <- function(data_frame, variables) {

    result <- cor(data_frame[, variables])

    return(result)

}

# Read the csv file
df <- read.csv("Database/data_pro.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
# df <- read.csv("PreprocessorDaily/Database/data_pro.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
# df <- read.csv("TestData/argentina_test.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

cor <- correlation(data_frame = df, variables = c("Amonio", "Conductividad", "Nitratos", "Oxigeno", "pH", "Temperatura", "Turbidez"))

png("corr_matrix.png", width = 600, height = 600)
cor_plot <- corrplot(cor, method = "number")
dev.off()
