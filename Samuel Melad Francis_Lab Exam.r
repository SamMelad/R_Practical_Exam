# Import libraries and set R work directory
library(mice)
library(ggplot2)
getwd()
setwd("E:\\Bio 4\\R Language")
# Read and View dataset
MyData <- read.csv("G2_anthropometry.csv")
View(MyData)

# Deal with NA's and empty cells
Check_NA <- any(is.na(MyData))
print(Check_NA)

# (1) Age Column Cleaning
NA_age <- any(is.na(MyData$age))
print(NA_age)
# Age in the form of float convert it to whole number
MyData$age <- round(MyData$age)


# (2) Gender Column Cleaning
NA_gender <- any(is.na(MyData$gender))
print(NA_gender)
MyData$gender <- ifelse(MyData$gender == "cm", "M", MyData$gender)


# (3) foot_length Column Cleaning
NA_foot <- MyData[!complete.cases(MyData$foot_length), ]
print(NA_foot)
# Replace NA's those gender female with the mean of the foot_length of all females
mean_F <- mean(MyData[MyData$gender == "F", "foot_length"], na.rm = TRUE)
MyData[is.na(MyData$foot_length) & MyData$gender == "F", "foot_length"] <- mean_F
# Use mice library to predict the values of foot_length for other NA's
predicted_values <- mice(MyData, m = 8, meth = c("", "", "pmm", ""), maxit = 20)
MyData <- complete(predicted_values, 8)
MyData$foot_length <- round(MyData$foot_length)


# (4) height Column Cleaning
NA_heigh <- MyData[is.na(MyData$height), ]
print(NA_heigh)
# Remove cm from the height column
MyData$height <- gsub("cm", "", MyData$height)


# (5) Converting variables to numeric and factors
MyData$gender <- as.factor(MyData$gender)
MyData$height <- as.numeric(MyData$height)


# (6) Recode the variables

# Recode age variable
MyData$age_categories[MyData$age <= 5] <- "Early childhood"
MyData$age_categories[MyData$age > 5 & MyData$age <= 10] <- "Middle childhood"
MyData$age_categories[MyData$age > 10] <- "Adolescence"

# Recode foot_length
foot_mean <- mean(MyData$foot_length)
MyData$foot_label[MyData$foot_length <= foot_mean] <- "Normal"
MyData$foot_label[MyData$foot_length > foot_mean] <- "Abnormal"

# Recode of code
MyData$foot_classification[MyData$foot_label == "Normal"] <- 0
MyData$foot_classification[MyData$foot_label == "Abnormal"] <- 1

# (7) Computing variable
MyData$foot_height_ratio <- MyData$foot_length / MyData$height


# (8) Filter the dataset
Subset1 <- MyData[MyData$foot_label == "Abnormal" &
    MyData$age > 8 & MyData$gender == "M", ]

Subset2 <- MyData[MyData$gender == "F" &
    MyData$height < 110 & MyData$foot_label == "Normal", c("age", "gender", "height", "foot_label")]

Subset3 <- MyData[MyData$foot_label == "Abnormal", -c(2, 5, 6, 8)]


# (9) Order the dataset
Sorted_dataset1 <- MyData[order(MyData$age), ]
Sorted_dataset2 <- MyData[order(MyData$foot_length, MyData$height), ]
Sorted_dataset3 <- MyData[order(-MyData$foot_height_ratio), ]


# Get the first and last 25 rows
print(head(MyData, 25))
print(tail(MyData, 25))

# Display some statics of data
print(summary(MyData))
View(MyData)

# (10) Data Visualization

# Scatter plot of relationship between height and foot_length
Scatter_foot_height <- ggplot(MyData, aes(x = height, y = foot_length)) +
    geom_point() +
    stat_smooth(se = TRUE) +
    labs(x = "Children Foot Lenght", y = "Children Height", title = "Foot Length and Height Scatter")

# Histogram of foot_length
Histogram_foot <- ggplot(MyData, aes(x = foot_length)) +
    geom_histogram(binwidth = 8, color = "black", fill = "blue", alpha = 0.5) +
    ggtitle("Foot Length Histogram") +
    labs(x = "Children Foot Length", y = "Number")

# Histogram of height
Histogram_height <- ggplot(MyData, aes(x = height)) +
    geom_histogram(binwidth = 10, color = "white", fill = "red", alpha = 0.4) +
    labs(x = "Childern Height", y = "Number")

# Bar chart of age fill with age_categories
Bar_age <- ggplot(MyData, aes(x = age, fill = age_categories)) +
    geom_bar() +
    labs(y = "Age Category Count", title = "Age Rate")


# Bar chart of age fill with age_categories with facet_wrap
Bar_age_facet <- ggplot(MyData, aes(x = age, fill = age_categories)) +
    geom_bar() +
    labs(y = "Age Category Count", title = "Age Rate") +
    facet_wrap(~foot_label)


# Heatmap of the correlation between variables
MyData$gender <- as.numeric(as.factor(MyData$gender))
# Subset and specify the numerical columns for correlation
subset_data <- MyData[, c("age", "gender", "foot_length", "height", "foot_classification", "foot_height_ratio")]
correlation_matrix <- cor(subset_data)
correlation_heatmap <- heatmap(correlation_matrix,
    main = "Correlation Heatmap",
    col = colorRampPalette(c("#F4A582", "#FFFFFF", "#92C5DE"))(100)
)
