# Cab-Fare-Prediction
Smart System to predict the cab fare amount

## Problem Statement
You are a cab rental start-up company. You have successfully run the pilot project andnow want to launch your cab service across the country.You have collected the historical data from your pilot project and now have a requirement to apply analytics for fare prediction. You need to design a system that predicts the fare amount for a cab ride in the city.

## Dataset
* pickup_datetime - timestamp value indicating when the cab ride started.
* pickup_longitude - float for longitude coordinate of where the cab ride started.
* pickup_latitude - float for latitude coordinate of where the cab ride started.
* dropoff_longitude - float for longitude coordinate of where the cab ride ended.
* dropoff_latitude - float for latitude coordinate of where the cab ride ended.
* passenger_count - an integer indicating the number of passengers in the cab ride

## Steps Followed (refer project report)
1. missing value analysis
2. Outlier analysis 
3. Feature Engineering which involved creation of new column representing the distance. This was calculated by applying Havesine formula by using the pickup and drop coordinates
4. Exploratory Data Analysis
5. Tackled Multicollinearity using Pearson Correlation
6. Model Development
7. Hyperparameter tuning


## Regression Models Applied
* Linear Regression
* CART
* Random Forest
* Gradient Boosting Regression
