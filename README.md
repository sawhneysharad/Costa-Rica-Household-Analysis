# Costa-Rica-Household-Analysis-Costa Rican Household Poverty Level Prediction
# Data source: Kaggle
Project type: Group

Data set contains 143 variables and 9558 rows of data  about socio economic conditions of costa Rican households.
And family’s, observable household attributes like the material of their walls and ceiling, or the assets found in the home to classify them and predict their level of need.
We need to predict the poverty level (Target variable) of household-based information. The target variable is an ordinal variable indicating groups of income level.
    1= extreme poverty, 
    2= moderate poverty, 
    3= vulnerable households, 
    4= non vulnerable households.

# DATA PREPRATION
HANDLING MISSING VALUES
1. Replacing missing values with Average of column or zero.
2. Removing variable from the data set which has more missing values.
HANDLING OULIERS
1.    Removing the outliers(Z- score value to identify the outliers )
2.    Consider outliers and rest of the data separately.
3.    Removing and replacing with NA.
DATA TRANSFORMATION
1. used label encoding by assigning unique value in the feature column and we converted it as ordered variable

Performed Exploratory Data Analysis

# ORDINAL LOGSTIC REGRESSION 

WE HAVE USED EXTENSION OF LOGISTIC REGRESSION TO PREDICT THE POVERTY LEVEL.
model summary, we get information about 
Model equation
The regression coefficients with their values, standard error, Z value and level of significance.
INTERCEPT VALUES,AIC and other values, which are used in comparing the model performance.
The estimate in the output are in units of ordered logits, or ordered log odds, so for ‘computer” variable, we would say that one unit increase in ‘computer’ will expect 0.73 increase in he expected value of “Target” variable on odd log scale, given that all other variables are held constant .
WE HAVE CLACULTED P-VALUE TO IDENTIFY THE SIGNIFICANT VARIBLES.
WE HAVE TRAINED MODEL USING ORDINAL LOGISTICS REGRESSION MODEL TESTED AND USED TO TESTING DATA TO PREDICT THE VALUES BASED ON LEARNING MODEL.
WE FOUND THE ACCURCEY OF MODEL 66% FROM CONFUSION MATRIX.

# MODEL SELECTION

USED stepwise regression for finding  all combination of variables and AIC(Akaike Information Criterion) values for selecting the best model
The model with lowest AIC value i.e. 13179.65 is best fit model
We have built new ordinal logistic regression model by using the features selected on stepwise regression method and found model accuracy 65%.
Comparison between above two model we didn’t observe much improvement in performance and  also observed that performance deviance compared to old model.

# MODEL OPTIMIZATION

Used gradient boosting algorithm to build the model.
Gradient boosting is an approach where new models are created that predicts the residuals or errors of prior models and then added together to make the final prediction.
It’s an implementation of gradient boosted decision trees designed for performance and speed.
363 iterations is optimum number of rounds for model where test error is minimum and no overfitting in the training model 
we found that accuracy of 84 % from XGBoosting model compared to traditional models and found that positive predictive value and negative predictive value percentages are looks good.

# CONCLUSION

Based on the accuracy rate, sensitivity and specificity, we can propose this model to inter-American development bank which will help them to analyze socio-economic conditions on household’s characteristics and governments can identify which households have the highest need for social welfare assistance. 
They can allocate funds for such people who are below poverty line. This solution will also help other countries beyond the Costa Rican for assessing the social need
