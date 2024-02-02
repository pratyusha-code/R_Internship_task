#Problem Statement::

# In bike-sharing systems, the entire process from membership to rental and return
# has been automated. Using these systems, users can easily rent a bike from one
# location and return it to another. Hence, a bike rental company wants to
# understand and predict the number of bikes rented daily based on the
# environment and seasons.

#1. Exploratory data analysis:
  
  # • Load the dataset and the relevant libraries
  # • Perform data type conversion of the attributes
  # • Carry out the missing value analysis

# Data manipulation package
library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
# Create a multi-panel plot
library(gridExtra)
# Installing package 
install.packages("caTools")       # For sampling the dataset 
install.packages("randomForest")  # For implementing random forest algorithm 
# Loading package 
library(caTools) 
library(randomForest) # For random forest algorithm
library(caret) # For data splitting and model evaluation
set.seed(123) # For reproducibility


#loading the dataset 
Bike_rental_raw_data <- read_excel("1657875746_day.xlsx")
View(Bike_rental_raw_data)

#checking dimensions
dim(Bike_rental_raw_data)
glimpse(Bike_rental_raw_data)

#frst few observations
head(Bike_rental_raw_data,5)

#data types of each variables/attributes
str(Bike_rental_raw_data)

#statistical summary of each cols
summary(Bike_rental_raw_data)

#column names present
colnames(Bike_rental_raw_data)


# Assuming your timestamps are stored in a vector named 'timestamps' within 'mydata'
timestamps <- Bike_rental_raw_data$dteday

# Convert timestamps to POSIXct objects if they are in character format
timestamps <- as.POSIXct(timestamps, format = "%Y-%m-%d %H:%M:%S")

# Find the range of timestamps
timestamp_range <- range(timestamps)

# Print the range in a human-readable format
cat("Timestamp Range - Start:", format(timestamp_range[1], "%Y-%m-%d %H:%M:%S"), "\n")
cat("Timestamp Range - End:", format(timestamp_range[2], "%Y-%m-%d %H:%M:%S"), "\n")

#table on the columns and its range

# Summarize the content of 'season col'
summary_table_season<- table(Bike_rental_raw_data$season)

# Summarize the content of 'holiday'
summary_table_holiday <- table(Bike_rental_raw_data$holiday)

# Summarize the content of categorical
summary_table_workingday <- table(Bike_rental_raw_data$workingday)
summary_table_weathersit <- table(Bike_rental_raw_data$weathersit)


# Print the summary tables
print(summary_table_season)
print(summary_table_holiday)
print(summary_table_workingday)
print(summary_table_weathersit)
summary_table_yr<- table(Bike_rental_raw_data$yr)
print(summary_table_yr)


# Get the range for 'temp'
range_temp <- range(Bike_rental_raw_data$temp)
cat("Range for 'temp': Min =", range_temp[1], ", Max =", range_temp[2], "\n")

# Get the range for 'atemp'
range_atemp <- range(Bike_rental_raw_data$atemp)
cat("Range for 'atemp': Min =", range_atemp[1], ", Max =", range_atemp[2], "\n")

# Get the range for 'hum'
range_hum <- range(Bike_rental_raw_data$hum)
cat("Range for 'hum': Min =", range_hum[1], ", Max =", range_hum[2], "\n")

# Get the range for 'windspeed'
range_windspeed <- range(Bike_rental_raw_data$windspeed)
cat("Range for 'windspeed': Min =", range_windspeed[1], ", Max =", range_windspeed[2], "\n")

# Get the range for 'casual'
range_casual <- range(Bike_rental_raw_data$casual)
cat("Range for 'casual': Min =", range_casual[1], ", Max =", range_casual[2], "\n")

# Get the range for 'registered'
range_registered <- range(Bike_rental_raw_data$registered)
cat("Range for 'registered': Min =", range_registered[1], ", Max =", range_registered[2], "\n")

# Get the range for 'cnt'
range_cnt <- range(Bike_rental_raw_data$cnt)
cat("Range for 'cnt': Min =", range_cnt[1], ", Max =", range_cnt[2], "\n")

# Get the range for 'mnth'
range_mnth <- range(Bike_rental_raw_data$mnth)
cat("Range for 'cnt': Min =", range_mnth[1], ", Max =", range_mnth[2], "\n")



#----------
  
# Define a list of columns to convert to categorical
category_list <- c('season', 'holiday', 'workingday', 'weathersit')

# Loop through the columns and convert them to categorical
for (var in category_list) {
  Bike_rental_raw_data[[var]] <- as.factor(Bike_rental_raw_data[[var]])
}

# Define mappings for 'season' and 'weather'
season_dict <- c("Spring", "Summer", "Fall", "Winter")
weather_dict <- c("Clear", "Misty+Cloudy", "Light Snow/Rain", "Heavy Snow/Rain")

# Apply the mappings to 'season' and 'weather' columns in 'Bike_rental_raw_data'
Bike_rental_raw_data$season <- factor(Bike_rental_raw_data$season, levels = 1:4, labels = season_dict)
Bike_rental_raw_data$weathersit <- factor(Bike_rental_raw_data$weathersit, levels = 1:4, labels = weather_dict)

  

# View the first 3 rows of 'mydata'
head(Bike_rental_raw_data, n = 3)
view(Bike_rental_raw_data)


### Calculate the number of missing values in each column
missing_values <- sapply(Bike_rental_raw_data, function(x) sum(is.na(x)))

# Print the missing values summary
print(missing_values)
install.packages("visdat")

library(visdat)
vis_miss(Bike_rental_raw_data)


###visualization based on 'count' = number of bikes rented varies across the various categorical data (weather, season, working day)
# Calculate average values across the 'weather' column
group_weather <- aggregate(Bike_rental_raw_data$cnt, 
                           by = list(weather = Bike_rental_raw_data$weathersit), 
                           FUN = mean)
print(group_weather)
# Create the plot for average bike rentals across weather conditions
plot_weather <- ggplot(group_weather, aes(x = factor(weather), y = x, fill = factor(weather))) +
  geom_bar(stat = "identity") +
  labs(x = "Weather", y = "Average Count", title = "Average bike rentals across Weather") +
  theme_minimal() +
  geom_text(aes(label = ifelse(weather == 4, paste("Total:", round(x)), "")), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"))

# Display the first plot
print(plot_weather)


###
# Calculate average values across the 'weather' column
group_season <- aggregate(Bike_rental_raw_data$cnt, 
                           by = list(season = Bike_rental_raw_data$season), 
                           FUN = mean)
print(group_season)
# Create the plot for average bike rentals across weather conditions
plot_season <- ggplot(group_season, aes(x = factor(season), y = x, fill = factor(season))) +
  geom_bar(stat = "identity") +
  labs(x = "Season", y = "Average Count", title = "Average bike rentals across all seasons") +
  theme_minimal() +
  geom_text(aes(label = ifelse(season == 4, paste("Total:", round(x)), "")), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"))

# Display the first plot
print(plot_season)

##--------------------------


# Calculate monthly and yearly totals
#monthly_totals <- aggregate(cnt ~ mnth, data = Bike_rental_raw_data, FUN = sum)
#yearly_totals <- aggregate(cnt ~ yr, data = Bike_rental_raw_data, FUN = sum)

monthly_totals <- aggregate(Bike_rental_raw_data$cnt, 
                          by = list(monthly = Bike_rental_raw_data$mnth), 
                          FUN = sum)

print(monthly_totals)

# Create the plot for monthly distribution
plot_monthly <- ggplot(monthly_totals, aes(x = factor(monthly), y = x)) +
  geom_bar(stat = "identity", fill = "#1f77b4") +
  labs(x = "Month", y = "Total Count", title = "Monthly Distribution of Bike Rentals") +
  theme_minimal() +
  scale_x_discrete(labels = c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  ))
print(plot_monthly)

# Create the plot for yearly distribution

yearly_totals <- aggregate(Bike_rental_raw_data$cnt, 
                            by = list(yearly = Bike_rental_raw_data$yr), 
                            FUN = sum)

print(yearly_totals)


plot_yearly <- ggplot(yearly_totals, aes(x = factor(yearly), y = x)) +
  geom_bar(stat = "identity", fill = "#ff7f0e") +
  labs(x = "Year", y = "Total Count", title = "Yearly Distribution of Bike Rentals") +
  theme_minimal() +
  scale_x_discrete(labels = c("2011", "2012"))

print(plot_yearly)
# Display the plots
grid.arrange(plot_monthly, plot_yearly, ncol = 2)

# Calculate the total number of bikes rented in 2012
#total_bikes_2012 <- sum(Bike_rental_raw_data$cnt[Bike_rental_raw_data$yr == 1])

# Print the total number of bikes rented in 2012
#cat("Total number of bikes rented in 2012:", total_bikes_2012)



###visualization of bike rentals on variable working day
# First, summarize the data

Bike_rental_raw_data$count <- as.numeric(as.character(Bike_rental_raw_data$cnt))

# Now, check if there were any values that could not be converted to numeric
# This is optional but useful for diagnosing issues with data conversion
sum(is.na(Bike_rental_raw_data$count))


group_workingday <- Bike_rental_raw_data %>%
  group_by(workingday) %>%
  summarise(average_count = mean(count, na.rm = TRUE)) %>%
  ungroup() # ungroup is used to remove the grouping structure

# Now, plot the data using ggplot2
ggplot(group_workingday, aes(x = factor(workingday), y = average_count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Working Day", y = "Average Count", title = "Average bike rentals across Working Day") +
  theme_minimal()

###observation is that in weekdays more bike rentals are being taken wrt holidays

###Outlier analysis

# Load the required libraries
install.packages('patchwork')
library(patchwork)

# Assuming 'mydata' has 'count', 'weather', 'workingday', 'season', and 'holiday' columns

# Create boxplots
p1 <- ggplot(Bike_rental_raw_data, aes(x = weathersit, y = count)) +
  geom_boxplot() +
  scale_x_discrete(limits = c('Clear', 'Heavy Snow/Rain', 'Light Snow/Rain', 'Misty+Cloudy')) +
  labs(title = "Count by Weather")
#print(p1)
p2 <- ggplot(Bike_rental_raw_data, aes(x = factor(workingday), y = count)) +
  geom_boxplot() +
  labs(title = "Count by Working Day")
print(p2)
p3 <- ggplot(Bike_rental_raw_data, aes(x = season, y = count)) +
  geom_boxplot() +
  scale_x_discrete(limits = c('Fall', 'Spring', 'Summer', 'Winter')) +
  labs(title = "Count by Season")
print(p3)
p4 <- ggplot(Bike_rental_raw_data, aes(x = factor(holiday), y = count)) +
  geom_boxplot() +
  labs(title = "Count by Holiday")
print(p4)

#wrt season bike rentals are more in fall
##wrt clear weather more bike rentals took place
###in other days in working day variable more rentals are there


#######---------------------

# Create a boxplot for the 'cnt' column (total number of bikes rented)
boxplot(Bike_rental_raw_data$cnt, main = "Boxplot for Outlier Analysis",
        ylab = "Total Number of Bikes Rented")


###corelation analysis Regression Plots vs. Temperature, Humidity and Windspeed


# Create regression plot for Temperature vs. Count
p5 <- ggplot(Bike_rental_raw_data, aes(x = temp, y = count)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add linear regression line
  labs(title = "Reg Plot for Temperature vs. Count")
print(p5)
# Create regression plot for Humidity vs. Count
p6 <- ggplot(Bike_rental_raw_data, aes(x = hum, y = count)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add linear regression line
  labs(title = "Reg Plot for Humidity vs. Count")
print(p6)
# Create regression plot for Windspeed vs. Count
p7 <- ggplot(Bike_rental_raw_data, aes(x = windspeed, y = count)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE, color = "green") +  # Add linear regression line
  labs(title = "Reg Plot for Windspeed vs. Count")
print(p7)

###observations
##The above regplot indicates a negative correlation of count with humidity and windspeed 
##and a positive correlation with temperature



###heatmap
install.packages("corrplot")
library(corrplot)
# Select only numeric columns from the dataframe
numeric_data <- Bike_rental_raw_data[sapply(Bike_rental_raw_data, is.numeric)]

# Assuming 'mydata_without_outliers' is your dataframe
corr_matrix <- cor(numeric_data, use = "complete.obs")  # Calculate correlation matrix

# Create the heatmap
corrplot(corr_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, 
         # Add color spectrum configurations
         col = colorRampPalette(c("red", "yellow", "green"))(200),
         addCoef.col = "black")  # Add correlation coefficients to the plot

##observation:
#A positive correlation between count and temperature (as was seen in the regplot). This is probably only true for the range of temperatures provided
##A negative correlation between count and humidity. The more the humidity, the less people prefer to bike
##Count has a weak dependence on windspeed


###splitting the dataset in train and test
trainIndex <- createDataPartition(Bike_rental_raw_data$cnt, p = .8, list = FALSE, times = 1)
train_set <- Bike_rental_raw_data[trainIndex, ]
test_set <- Bike_rental_raw_data[-trainIndex, ]


# Assuming cnt is the target variable and all other relevant variables are predictors
model_rf <- randomForest(cnt ~ ., data = train_set, ntree = 500)
summary(model_rf)

#predictions on test_set
predictions <- predict(model_rf, newdata = test_set)
performance <- postResample(pred = predictions, obs = test_set$cnt)
print(performance)

#RMSE    Rsquared         MAE 
#115.2986429   0.9971232  72.4104384 

##Final comment on the performance using RandomForest

# - **RMSE (Root Mean Squared Error):** 115.2986429
# - **R-squared (Coefficient of Determination):** 0.9971232
# - **MAE (Mean Absolute Error):** 72.4104384
# 
### RMSE (Root Mean Squared Error)
#   - **What it measures:** The standard deviation of the residuals (prediction errors). It measures how far from the regression line data points are.
# - **Interpretation:** An RMSE of 115.2986429 means that, on average, the model's predictions are about 115.29 units away from the actual values. The lower the RMSE, the better the model's predictions are. Considering the scale of your target variable (which isn't provided here), this value could be relatively high or low.
# 
### R-squared (Coefficient of Determination)
# **What it measures:** The proportion of the variance in the dependent variable that is predictable from the independent variables.
# **Interpretation:** An R-squared of 0.9971232 is very close to 1, indicating that the model explains almost all the variability of the response data around its mean. This suggests a very good fit to the data, although in practical scenarios, a value this high might sometimes suggest overfitting, especially in complex models like Random Forest.
# 
### MAE (Mean Absolute Error)
# **What it measures:** The average of the absolute errors between the predictions and actual observations.
# **Interpretation:** An MAE of 72.4104384 means that, on average, the model's predictions are about 72.41 units away from the actual value. Unlike RMSE, MAE does not square the errors, providing a more straightforward interpretation of the average error magnitude.
    
###overall comments:
#These metrics suggest a model that performs exceptionally well in terms of fit, 
#caution is advised, especially with an R-squared value so close to 1. 
