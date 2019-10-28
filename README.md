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

## Steps Followed 
* missing value analysis
* Outlier analysis 
* Feature Engineering which involved creation of new column representing Haversine distance
* Exploratory Data Analysis
* Modelling
<br>
All the steps are thoroughly explained in the project report


## Regression Models Applied
* Linear Regression
* CART
* Random Forest
* Gradient Boosting Regression
