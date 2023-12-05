#Loading the Data
rm(list=ls()) #Clean Memory
load("/Users/khushisampat/Desktop/Data Science/yelp_user_small.Rda")
load("/Users/khushisampat/Desktop/Data Science/yelp_review_small.Rda")
#Merging the two datasets
merged_data <- merge(user_data_small, review_data_small, by = "user_id")

library(jsonlite)
install.packages("tidyr")
library(tidyr)

#Setting Directory
 setwd("/Users/khushisampat/Desktop/Data Science")
#Loading Data
business_data <- stream_in(file("yelp_academic_dataset_business.json"))
#Merging the datset
Merged_data1 <- merge(merged_data, business_data, by = "business_id")
#The last colum is a df, hence unnesting the colums
Merged_data1 <- unnest(Merged_data1, cols = attributes)
Merged_data1 <- unnest(Merged_data1, cols=hours)
#Ommiting rows with missing values of stars.x
Merged_data1 <- Merged_data1[!is.na(Merged_data1$stars.x), ]

#Checking distribution and Balance of Stars.x (number of stars given by user i to business j)
install.packages("ggplot2")
library(ggplot2)
# Install the dplyr package
install.packages("dplyr")

# Load the dplyr package
library(dplyr)

# Useing ggplot2 to plot
ggplot(Merged_data1, aes(x = stars.x)) +
  geom_bar(fill = "blue") +
  labs(title = "Distribution of Stars", x = "Stars", y = "Frequency") +
  theme_minimal()

# Calculating the frequency and percentage of each category
Merged_data1_summary <- Merged_data1 %>%
  group_by(stars.x) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

# Arranging categories if they have a specific order
Merged_data1_summary$stars.x <- factor(Merged_data1_summary$stars.x, levels = c("1", "2", "3", "4", "5")) 

#Creating Bar Graph
ggplot(Merged_data1_summary, aes(x = stars.x, y = percentage)) +
  geom_bar(stat = "identity", fill = "blue") +
  ylab("Percentage") +
  xlab("Stars") +
  ggtitle("Distribution of Categories") +
  theme_minimal()

#We can see that the dataset is imbalanced

#Creating a Balanced Data Set 
install.packages("dplyr")
library(dplyr)

subdata <- Merged_data1[, 3:40] #After EDA getting rid of colums that do not significantly influence stars.x ~ reducing noise
subdata <- subdata %>% select(-is_open, -postal_code, -state, -city, -yelping_since, -elite, -friends, -fans, -compliment_cool, -compliment_cute, -compliment_hot, -compliment_more, -compliment_list, compliment_note, -compliment_plain, -compliment_profile, -compliment_writer, -compliment_photos, -review_id, -useful.y, -funny.y, -cool.y, -text, -date, -name.x, -name.y, -address)

classes <- sort(unique(subdata$stars.x))
balanced_data <- data.frame()

for(class in classes) {
  class_data <- subdata[subdata$stars.x == class, ]
  sampled_data <- class_data %>% 
    slice_sample(n = max(table(subdata$stars.x)), replace = TRUE)  # Oversampling
  balanced_data <- rbind(balanced_data, sampled_data)
}

# Shuffle the rows to mix the classes
set.seed(1)
balanced_data <- balanced_data[sample(nrow(balanced_data)), ]


# ~ Please upload data and run code till here before testing every model


#EDA
library(ggplot2)
ggplot(Merged_data1)+geom_point(mapping=aes(x=review_count.x, y=stars.x)) + xlim(0,5000)

max(user_data_small$useful)
table<- table(user_data_small$useful)
print(table) 
ggplot(Merged_data1)+geom_point(mapping=aes(x=useful.x, y=stars.x)) + xlim(0,1000)

ggplot(Merged_data1)+geom_point(mapping=aes(x=funny.x, y=stars.x)) + xlim(0,5000)

ggplot(Merged_data1)+geom_point(mapping=aes(x=funny.x, y=stars.x)) + xlim(0,1000)

max(user_data_small$cool)
ggplot(Merged_data1)+geom_point(mapping=aes(x=cool.x, y=stars.x)) + xlim(0,1000)

ggplot(Merged_data1)+geom_point(mapping=aes(x=compliment_more, y=stars.x)) + xlim(0, 500)

ggplot(Merged_data1)+geom_point(mapping=aes(x=compliment_profile, y=stars.x)) + xlim(0, 400)

ggplot(Merged_data1)+geom_point(mapping=aes(x=compliment_cute, y=stars.x)) + xlim(0, 400)

ggplot(Merged_data1)+geom_point(mapping=aes(x=compliment_list, y=stars.x)) + xlim(0, 400)
ggplot(Merged_data1)+geom_point(mapping=aes(x=compliment_list, y=stars.x)) + xlim(0, 200)

ggplot(Merged_data1)+geom_point(mapping=aes(x=compliment_note, y=stars.x)) + xlim(0, 3000) 

ggplot(Merged_data1)+geom_point(mapping=aes(x=useful.y, y=stars.x)) + xlim(0, 1000)

ggplot(Merged_data1)+geom_point(mapping=aes(x=funny.y, y=stars.x)) + xlim(0, 1000)

ggplot(Merged_data1)+geom_point(mapping=aes(x=review_count.y, y=stars.x)) + xlim(0, 5000)

#Spacial EDA - collecting data to conduct KNN

library(dplyr)

average_ratings_by_city <- Merged_data1 %>%
       group_by(city, state) %>%
      summarize(avg_rating = mean(stars.x, na.rm = TRUE))

average_ratings_by_city <- Merged_data1 %>%
       group_by(city) %>%
       summarize(avg_rating = mean(stars.x, na.rm = TRUE))
   
   # Viewing the result
print(average_ratings_by_city)

city_business_data <- Merged_data1 %>%
       filter(city == "Philadelphia") %>%
       group_by(business_id) %>%
       summarize(latitude = first(latitude),  # Useing the actual latitude
                                 longitude = first(longitude),  # Useing the actual longitude
                                 avg_stars = mean(stars.x, na.rm = TRUE))


ggplot(city_business_data, aes(x = longitude, y = latitude, color = as.factor(round(avg_stars)))) +
       geom_point() +
      scale_color_manual(values = c("1" = "red", "2" = "blue", "3" = "yellow", "4" = "green", "5" = "purple")) +
       labs(title = paste("Average Star Ratings per Business in", "Philidelphia"), 
                       x = "Longitude", y = "Latitude", color = "Average Star Rating") +
       theme_minimal()


city_business_data <- Merged_data1 %>%
      filter(city == "Saint Louis") %>%
       group_by(business_id) %>%
       summarize(latitude = first(latitude),  # Using the actual latitude
                                 longitude = first(longitude),  # Using the actual longitude
                                 avg_stars = mean(stars.x, na.rm = TRUE))

ggplot(city_business_data, aes(x = longitude, y = latitude, color = as.factor(round(avg_stars)))) +
       geom_point() +
       scale_color_manual(values = c("1" = "red", "2" = "blue", "3" = "yellow", "4" = "green", "5" = "purple")) +
       labs(title = paste("Average Star Ratings per Business in", "Saint Louis"), 
                       x = "Longitude", y = "Latitude", color = "Average Star Rating") +
       theme_minimal()

city_business_data <- Merged_data1 %>%
       filter(city == "Tampa") %>%
       group_by(business_id) %>%
       summarize(latitude = first(latitude),  # Using the actual latitude
                                 longitude = first(longitude),  # Using the actual longitude
                                 avg_stars = mean(stars.x, na.rm = TRUE))

  
   
ggplot(city_business_data, aes(x = longitude, y = latitude, color = as.factor(round(avg_stars)))) +
       geom_point() +
       scale_color_manual(values = c("1" = "red", "2" = "blue", "3" = "yellow", "4" = "green", "5" = "purple")) +
       labs(title = paste("Average Star Ratings per Business in", "Tampa"), 
                       x = "Longitude", y = "Latitude", color = "Average Star Rating") +
       theme_minimal()



 #Experimenting with different Models and Methods ~ Please Upload Data and run the first few lines before running every model
 
#Simple always 5 classifier
 # Creating a vector of predictions where every prediction is 5
 predictions <- rep(5, nrow(subdata))
 # Calculate accuracy
 actual <- subdata$stars.x  # Replace 'stars.x' with the actual column name
 accuracy <- sum(predictions == actual) / length(actual)
 print(paste("Accuracy:", accuracy))
 #0.461 accuracy


#KNN
subdataKnn <- subdata[, c("latitude", "longitude", "stars.x")]
# Install the class package
install.packages("class")

# Load the class package
library(class)


x <- subdataKnn[, -which(names(subdataKnn) == "stars.x")]  # Predictor variables
y <- subdataKnn$stars.x  # Target variable

# Splitting the data into training and test sets
set.seed(1)  # Setting seed for reproducibility
index <- sample(1:nrow(x), round(0.75*nrow(x)))
train_x <- x[index, ]
train_y <- y[index]
test_x <- x[-index, ]
test_y <- y[-index]

# Scaling the data
train_x_scaled <- scale(train_x)
test_x_scaled <- scale(test_x, center = attr(train_x_scaled, "scaled:center"), scale = attr(train_x_scaled, "scaled:scale"))

library(caret)
set.seed(1)
tune_grid <- expand.grid(k = 1:20)
control <- trainControl(method = "cv", number = 10)
knn_tune <- train(x = train_x_scaled, y = train_y, method = "knn", trControl = control, tuneGrid = tune_grid)
# Plotting tuning results
plot(knn_tune)
# Best model's k value
optimal_k <- knn_tune$bestTune$k

# Fitting KNN model
set.seed(1)
knn_pred <- knn(train = train_x_scaled, test = test_x_scaled, cl = train_y, k = 20)

# Confusion Matrix
table(Predicted = knn_pred, Actual = test_y)

# Accuracy
mean(knn_pred == test_y)


average_stars_by_state <- subdataKnn %>%
  group_by(state) %>%
  summarize(average_stars = mean(stars.x, na.rm = TRUE))
ggplot(average_stars_by_state, aes(x = state, y = average_stars)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  labs(x = "State", y = "Average Star Rating", title = "Average Star Rating by State") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# Or create a simple plot
ggplot(average_ratings, aes(x = region, y = average_rating)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Average Ratings by Region")

#Neural Networks


library(keras)
install.packages("dplyr")
install.packages("caret")

library(dplyr)
library(caret)
install.packages("nnet")
library(nnet)


#Imputing missing values with the median for numeric columns
subdata <- subdata %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), median(., na.rm = TRUE), .))


subdata$stars.x <- as.factor(subdata$stars.x)

numeric_columns <- sapply(subdata, is.numeric)
subdata[numeric_columns] <- scale(subdata[numeric_columns])

set.seed(1) # for reproducibility
train_index <- sample(1:nrow(balanced_data), 0.8 * nrow(balanced_data)) 
train_data <- subdata[train_index, ]
test_data <- subdata[-train_index, ]

model <- nnet(stars.x ~ ., data = train_data, size = 10, decay = 0.1, maxit = 200)

predictions <- predict(model, test_data, type = "class")
confusionMatrix <- table(predictions, test_data$stars.x)
print(confusionMatrix)

accuracy <- sum(predictions == test_data$stars.x) / nrow(test_data)
print(paste("Accuracy:", accuracy))
#.57 with subdata 

#RandomForest

install.packages("randomForest")
library(randomForest)

balanced_data <- lapply(balanced_data, function(x) if(is.character(x)) factor(x) else x) #changing variable storage type to factor
balanced_data <- na.omit(balanced_data) #omitting missing values
 # Convert the list back to a dataframe
balanced_data <- as.data.frame(balanced_data)

set.seed(1)  # Setting a seed for reproducibility
splitIndex <- createDataPartition(balanced_data$stars.x, p = 0.8, list = FALSE)
train_data <- balanced_data[splitIndex, ]
test_data <- balanced_data[-splitIndex, ]
train_data$stars.x <- as.factor(train_data$stars.x)

train_data <- lapply(train_data, function(x) ifelse(is.na(x), "false", x))
test_data <- lapply(test_data, function(x) ifelse(is.na(x), "false", x))

# Converting the lists back to dataframes
train_data <- as.data.frame(train_data)
test_data <- as.data.frame(test_data)

train_data$stars.x <- as.factor(train_data$stars.x)
test_data$stars.x <- as.factor(test_data$stars.x)

#Applying k-fold cross validation
control <- trainControl(method = "cv", number = 10)
set.seed(1)  # for reproducibility
#training
model_cv <- train(stars.x ~ ., data = train_data, method = "rf", trControl = control, ntree = 100)
#testing
predictions <- predict(model_cv, test_data)
#Accuracy
accuracy <- mean(predictions == test_data$stars.x)
print(paste("Accuracy:", accuracy))

rf_model <- randomForest(stars.x ~ ., data = train_data, ntree = 100, na.action = na.pass) 

install.packages("randomForest")
library(randomForest)

# Calculating feature importance
feature_importance <- importance(rf_model)

# Viewing the feature importance
print(feature_importance)

#visualizing the feature importance
install.packages("ggplot2")
library(ggplot2)

# Converting to a data frame for plotting
feature_importance_df <- as.data.frame(feature_importance)
feature_importance_df$Feature <- row.names(feature_importance_df)

# Plotting feature importance
ggplot(feature_importance_df, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Feature") +
  ylab("Importance") +
  ggtitle("Feature Importance in Random Forest Model")

#.4639 with RF and 0.5835 with NN 0.57 with subdata #0.933 for balanced_data where NN and RF were differnt combinations of variables picked for smaller subsets
install.packages("caret")
library(caret)

# Confusion matrix and associated statistics
confusionMatrix(predictions, as.factor(test_data$stars.x))
