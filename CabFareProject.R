rm(list=ls(all=T))

setwd("C:/Users/Pranav/Desktop/Cab Fare Project")

# Load Required packages
library(ggplot2)
library(scales)
library(DescTools)
library(DMwR)
library(caret)
library(randomForest)
library(corrgram)
library(rpart)

#Read the data from train and test csv file
train = read.csv("train_cab.csv", header = T)
test = read.csv("test.csv" , header = T)

# convert fare_amount from factor to numeric data type
train$fare_amount = as.numeric(paste(train$fare_amount))

# convert pickup_datetime from factor to date/time format
train$pickup_datetime = as.POSIXlt(train$pickup_datetime, format = "%Y-%m-%d %H:%M:%S")

#drop rows where pickup_datetime contain NA values
train = train[!is.na(train$pickup_datetime),]

#drop rows where passenger_count contain NA values
train = train[!is.na(train$passenger_count),]

#drop rows where fare_amount contain NA values
train = train[!is.na(train$fare_amount),]

# splitting the pickup_datetime into year,month,hour and day for training data
train$year = as.numeric(format(train$pickup_datetime,"%Y"))
train$year = as.factor(train$year)
train$year = as.numeric(train$year)

train$month = as.numeric(format(train$pickup_datetime,"%m"))
train$hour = as.numeric(format(train$pickup_datetime, "%H"))
train$day = train$pickup_datetime$wday

train$pickup_datetime = NULL

# doing same tranformations for test data
test$pickup_datetime = as.POSIXlt(test$pickup_datetime, format = "%Y-%m-%d %H:%M:%S")
test$year = as.numeric(format(test$pickup_datetime,"%Y"))
test$year = as.factor(test$year)
test$year = as.numeric(test$year)

test$month = as.numeric(format(test$pickup_datetime,"%m"))
test$hour = as.numeric(format(test$pickup_datetime, "%H"))
test$day = test$pickup_datetime$wday

test$pickup_datetime = NULL





###################### Outlier Analysis #######################

# Latitudes are bounded between (-90,90)
# Longitudes are bounded between (-180,180)
train = train[(!train$pickup_latitude < -90 & !train$pickup_latitude >90)  ,]
train = train[(!train$pickup_longitude < -180 & !train$pickup_longitude >180)  ,]

train = train[(!train$dropoff_latitude < -90 & !train$dropoff_latitude >90)  ,]
train = train[(!train$dropoff_longitude < -180 & !train$dropoff_longitude >180)  ,]


# function to calculate haversine distance
haversine <- function(lat1, long1, lat2, long2){
  
  R = 6371 # Radius of the earth
  
  phi1 = DegToRad(lat1)
  phi2 = DegToRad(lat2)
  
  delta_phi = DegToRad(lat2 - lat1)
  delta_lambda = DegToRad(long2 - long1)
  
  a = sin(delta_phi/2) ** 2 + cos(phi1) * cos(phi2) * sin(delta_lambda/2) **2
  
  c = 2 * atan2(sqrt(a), sqrt(1-a))
  
  d = R * c
  
  return (d)
}

# creating new column H_Distance for the train data
for(i in 1:nrow(train)){
  train$H_Distance[i] = haversine(train$pickup_latitude[i],
                                  train$pickup_longitude[i],
                                  train$dropoff_latitude[i],
                                  train$dropoff_longitude[i])
}

# also creating H_Distance column for test data
for(i in 1:nrow(test)){
  test$H_Distance[i] = haversine(test$pickup_latitude[i],
                                 test$pickup_longitude[i], 
                                 test$dropoff_latitude[i],
                                 test$dropoff_longitude[i])
}



## Now that we have calculated the distances , we continue with outlier analysis further

#####Case 1: check distances which are very large

train = train[order(train$H_Distance),]

# we will remove distances which are > 130
# some distances are wrongly noted in 8000's as coordinate information is missing

train = train[!train$H_Distance > 130 ,]



#####Case 2: Checking outliers for passenger counts

unique(train$passenger_count)

# we see that there are some decimal values. These values need to be removed

train = train[(((train$passenger_count * 10)%% 10)== 0) , ]

unique(train$passenger_count)

# the cab won't be able to occupy more than 6 passengers
train = train[! train$passenger_count > 6 ,]


#####Case 3: Outliers in fare aount

train = train[order(train$fare_amount),]

# in New York the minimum fare is $2.5
train = train[train$fare_amount >= 2.5 ,]

# we observe the fare amount value and decide to exclude values which are above 453
train = train[train$fare_amount <= 453 ,]


#####Case 4: Check where the distance covered is 0 but the fare may be positive
# This may be due to 2 reasons
# 1.pickup and dropoff coordinates may be same
# 2.pickup and dropoff information is not entered
# We will impute such distance values using formula -> distance = (fare_amount - 2.5)/1.56
# $2.5 is the base fare in New York and $1.56 is the charge for every extra km on average

train$H_Distance = ifelse( (train$H_Distance == 0) & (train$fare_amount !=0) , ((train$fare_amount -2.5)/1.56), train$H_Distance)






##################### Exploratory Data Analysis#########################

###### EDA on numerical variables

numerical_variables = c("H_Distance","fare_amount","pickup_latitude","pickup_longitude","dropoff_latitude","dropoff_longitude")

# plot correlation matrix
# correlation plots can only be done on continuous variables

corrgram(train[numerical_variables], order=FALSE,
         main="correlation plot",upper.panel=panel.pie,text.panel=panel.txt)

# drop pickup_latitude, pickup_longitude, dropoff_latitude, dropoff_longitude

train = train[,-c(2:5)]

# do smae for test data
test = test[, -c(1:4)]


# scatter plot of H_Distance vs fare_amount
s = ggplot(train, aes(x = H_Distance, y = fare_amount)) +
  geom_point()+
  scale_x_continuous(breaks = pretty_breaks(n = 10))

plot(s)



###### EDA on categorical variables

### Does passenger_count affect the fare_amount

p<-ggplot(train, aes(x=passenger_count)) + 
  geom_histogram(fill="blue" ) +
  scale_x_continuous(breaks = pretty_breaks(n = 6))
  
plot(p)


s = ggplot(train, aes(x = passenger_count, y = fare_amount)) +
  geom_point()+
  scale_x_continuous(breaks = pretty_breaks(n = 6))

plot(s)


### Does Hour affect fare amount

p<-ggplot(train, aes(x=hour)) + 
  geom_histogram(fill="blue" , bins = 50) +
  scale_x_continuous(breaks = pretty_breaks(n = 20))

plot(p)


s = ggplot(train, aes(x = hour, y = fare_amount)) +
  geom_point()+
  scale_x_continuous(breaks = pretty_breaks(n = 20))
plot(s)


### Does month affect fare_amount

p<-ggplot(train, aes(x=month)) + 
  geom_histogram(fill="blue" , bins = 60) +
  scale_x_continuous(breaks = pretty_breaks(n = 10))
plot(p)


s = ggplot(train, aes(x = month, y = fare_amount)) +
  geom_point()+
  scale_x_continuous(breaks = pretty_breaks(n = 12))

plot(s)


### Does day affect fare_amount
p<-ggplot(train, aes(x=day)) + 
  geom_histogram(fill="blue" , bins = 20) +
  scale_x_continuous(breaks = pretty_breaks(n = 6))
plot(p)

s = ggplot(train, aes(x = day, y = fare_amount)) +
  geom_point()+
  scale_x_continuous(breaks = pretty_breaks(n = 6))

plot(s)

### Does year affect fare_amount

p<-ggplot(train, aes(x=year)) + 
  geom_histogram(fill="blue" , bins = 20) +
  scale_x_continuous(breaks = pretty_breaks(n = 6))
plot(p)

s = ggplot(train, aes(x = year, y = fare_amount)) +
  geom_point()+
  scale_x_continuous(breaks = pretty_breaks(n = 6))

plot(s)

############## EDA ends########################




########### Handling skewness of data##################

##### Histogram plot of numerical variable H_Distance

p<-ggplot(train, aes(x=H_Distance)) + 
  geom_histogram(color="black", fill="green") +
  xlim(0,25)

plot(p)

# We see that the data is skewed . Let's perform square root transformation

train$H_Distance = sqrt(train$H_Distance)
test$H_Distance = sqrt(test$H_Distance)

p<-ggplot(train, aes(x=H_Distance)) + 
  geom_histogram(color="black", fill="green") +
  xlim(0,8)

plot(p)




############################### Model Development #####################

# diving the data into train and validation sets
set.seed(1234)
# Divide the data set into train and test
train_index = sample(1:nrow(train) , 0.80 * nrow(train))

trainSplit = train[train_index,]
validationSplit = train[-train_index,]




###### Linear Regression

linearModel = lm( fare_amount ~ . , data = trainSplit)

predictions_LR = predict(linearModel,validationSplit[,-1])

regr.eval(validationSplit[,1] ,predictions_LR , stats = c('mae','rmse','mape','mse') )


###### rpart (Decision Tree or CART)

set.seed(1234)

fit = rpart(fare_amount ~. , data = trainSplit , method = "anova")

# Predict for new test cases
predictions = predict(fit,validationSplit[,-1])


regr.eval(validationSplit[,1], predictions , stats = c('mae','rmse','mape','mse'))

rpart.plot::rpart.plot(fit)


# tuning the model using caret
library(caret)
rpart_grid = expand.grid(cp = c(0.1,0.2,0.01,0.02,0.001,0.002))

# Define the control
trControl <- trainControl(method = "cv", number = 3, search = "grid",  
                          verboseIter = TRUE)

set.seed(1234)

tuned_tree_model = caret::train(fare_amount ~ .,
                                data = trainSplit,
                                method = "rpart",
                                trControl = trControl,
                                tuneGrid = rpart_grid)

predictions = predict(tuned_tree_model,validationSplit[,-1])

regr.eval(validationSplit[,1], predictions , stats = c('mae','rmse','mape','mse'))



########## Random Forest regression

set.seed(1234)
rf_model = randomForest(fare_amount~.,data = trainSplit)

# Predict for new test cases
rf_predictions = predict(rf_model,validationSplit[,-1])


regr.eval(validationSplit[,1], rf_predictions , stats = c('mae','rmse','mape','mse'))



            ###### Hyperparameter tuning for Random Forest



set.seed(1234)

# default ntrees = 500
rf_tuned_model = caret::train(fare_amount~.,
                              data = trainSplit,
                              method = 'rf',
                              trControl = trainControl(method = "repeatedcv",
                                                       number = 3,
                                                       verboseIter = TRUE
                              ),
                              tuneGrid = expand.grid(.mtry = c(1:6)),
                              ntree = 500
)


rf_tuned_predictions = predict(rf_tuned_model,validationSplit[,-1])

regr.eval(validationSplit[,1], rf_tuned_predictions , stats = c('mae','rmse','mape','mse'))



######gradient boosting Regression ######################

gbmGrid = expand.grid(interaction.depth = c(3,4,5),
                      n.trees = c(130,140,150) ,
                      shrinkage = 0.1 ,
                      n.minobsinnode = c(2,4,6))

# Define the control
trControl <- trainControl(method = "cv", number = 3, search = "grid",  
                          verboseIter = TRUE)

set.seed(1234)
gbr_tuned_model = caret::train(fare_amount ~ .,data = trainSplit, method = "gbm", metric = "MAE",  
                               trControl = trControl, tuneGrid = gbmGrid , 
                               verbose = FALSE)

# use model$bestTune to see the optimising parameters

gbr_tuned_predictions = predict(gbr_tuned_model,validationSplit[,-1])

regr.eval(validationSplit[,1], gbr_tuned_predictions , stats = c('mae','rmse','mape','mse'))






############## making final predictions and storing to csv file

final_predictions = predict(gbr_tuned_model , test)

test$fare_amount = final_predictions

test$fare_amount = ifelse( test$fare_amount < 2.5 , 2.5, test$fare_amount)

write.csv(test,"Predictions in R.csv", row.names = FALSE)
