# FA Women's Super League Match Outcome Predictor
This repository contains R code to predict the outcomes of football (soccer) matches in the FA Women's Super League using various statistical measures and machine learning models.

## Overview
The code provided is split into three parts:

1. Data preparation and feature engineering
2. The main function match_predictor() to train and test the model
3. A function multi_week_predict() to predict match outcomes over multiple weeks

## How to Use
### Prerequisites
You need to have R installed, along with the following packages in init.r

## Data
* fawsl_2021_matches: Match data for the FA Women's Super League, containing information about match dates, team names, and match scores.
* fawsl_2021_data_matches: Detailed event data for each match, including information about passes, shots, and goals.

## Step-by-Step Instructions
1. Run init.r which will install the necessary packages and load the required libraries. It will also load FAWSL data using Statsbomb.

2. Run match_predictor.r,  which defines the main function match_predictor(). The function will prepare the data and create new features based on the raw data. This includes calculating average goals, shots, passes, duels, and other relevant statistics for each team. It will also train a logistic regression model on the data for the specified number of weeks and predicts the match outcomes for the following week. The function returns a list containing predictions, team profiles, model summary, and the training data used.

3. Run multi_week_predict.r, which defines the function multi_week_predict(). This function predicts match outcomes over multiple weeks using the match_predictor() function. The function returns a data frame containing actual and predicted match results.

4. Call the multi_week_predict() function with the desired number of weeks to start predicting from. For example, prediction_df_16 <- multi_week_predict(16) will start predicting match outcomes from week 16 onwards.

## Output
The model's predictions will be stored in a data frame, which includes the actual and predicted match results. Additionally, the overall prediction accuracy will be printed as a percentage.

Contributing
Feel free to submit a pull request if you have any improvements or suggestions for this project.