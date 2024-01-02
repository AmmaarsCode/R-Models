#Build a multiple linear model to predict car price
#

#Read in the data file
car.df <- read.csv("ToyotaCorolla.csv")

#Just use the first 1000 rows
car.df <- car.df[1:1000, ]

#Narrow down the possible variables for use in building the model
#Only look at variables that might work with the model
selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)

#Now partition the data into training and validation sets

#Establish the starting point for reproducing the partitions
set.seed(1)

#Build the partitions
#600 for training data
#400 for validation data
train.index <- sample(c(1:1000), 600)
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]

#Now determine fields that might work for our model
#Create a subset of numeric fields to check for correlations
corcol <- c(1, 2, 3, 5, 8, 9, 10, 11)

#Check correlations with the subset
cor(train.df[ ,corcol])

#Selecting Age, KM, and Weight as the three potential fields to build our model

#Now run plots to see how well each one matchs the price variable
#We are checking each independent variable against the dependent variable

plot(Price ~ Age_08_04, data = train.df)
plot(Price ~ KM, data = train.df)
plot(Price ~ Weight, data = train.df)

#Now we will plot the independent variables against each other
#to check for colinearity
plot(Age_08_04 ~ KM, data = train.df)
plot(Age_08_04 ~ Weight, data = train.df)
plot(Weight ~ KM, data = train.df)

#One more test for correlation betweewn IVs and DV
cor.test(train.df$Price, train.df$Age_08_04)
cor.test(train.df$Price, train.df$KM)
cor.test(train.df$Price, train.df$Weight)

#Test correlations between IVs
cor.test(train.df$Age_08_04, train.df$KM)
cor.test(train.df$Age_08_04, train.df$Weight)
cor.test(train.df$KM, train.df$Weight)

#Now that that is done, what decisions can we make about our model?
#How do we build our model?

#Let's assume that all of these fields were good candidates for our model
#and that there was no colinearity

#We can build our model

cars.model <- lm(Price ~ Age_08_04 + KM + Weight, data = train.df)

#limit the number of decimal places in the display
options(scipen = 999)

#show the results
summary(cars.model)

#We now have an intercept and a coefficient or slope for each variable
#We can build our model by plugging in the intercepts and coefficients directly into our model
#We are establishing a variable in our train.df called Pred.Value and populating it with our equation

train.df$Pred.Value = 2095.185143 + (train.df$Age_08_04 * -135.626079) + (train.df$KM * -0.023021) + (train.df$Weight * 16.418368)

#How does that compare to our actual Price variable
View(train.df[ ,c(1, 12)])

#Calculate the error for each row by subtracking Price from Pred.Value
train.df$Error <- train.df$Pred.Value - train.df$Price
View(train.df[ ,c(1, 12, 13)])

#Now average the error
#We will talk about more sophisticated ways of using error later
mean(train.df$Error)

#Lets plot our predicted value against the price
plot(Price ~ Pred.Value, data = train.df)

#What is the correlation?
cor.test(train.df$Price, train.df$Pred.Value)

# Your assignment this week:
# Modify the model to drop weight.  Apply it to the training data set and give me the intercept and coefficients
# Calculate Adj.Price by applying the model in the same way that I calculated Pred.Value (just without Weight)
# Calculate Adj.Error by subtracting Price from Adj.Price
# Calculate the mean of Adj.Error and turn that in.

# Now, you must check your (first) model against the validation data. Use the model that has all three variables in it.
# Calcualte Pred.Value and Error in your valid.df data frame
# Average the error
# How does it compare to the average error in train.df?

# Six things to turn into me:
# 1 Average error in train.df (with all three variables in the model) - I gave you that one.
# 2 Intercept and Coefficients when you drop Weight out of the model and apply it to train.df
# 3 Average Error in train.df with just Age_08_04 and KM in the model

# Do these things using valid.df
# Run the three variable model against valid.df
# Calculate Pred.Value and Error using in valid.df
# 4 Average Error in valid.df with all three values in the original model
# 5 Plot of Price against Pred.Value in valid.df 
# 6 corelation of Price and Pred.Value in valid.df



> car.df <- read.csv("C:\\RImport\\ToyotaCorolla.csv")
> 
> View(car.df)
> #Just use the first 1000 rows
> car.df <- car.df[1:1000, ]
> 
> #Narrow down the possible variables for use in building the model
> #Only look at variables that might work with the model
> selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)
> 
> #Now partition the data into training and validation sets
> 
> #Establish the starting point for reproducing the partitions
> set.seed(1)
> 
> #Build the partitions
> #600 for training data
> #400 for validation data
 train.index <- sample(c(1:1000), 600)
> train.df <- car.df[train.index, selected.var]
> valid.df <- car.df[-train.index, selected.var]
> 
> #Now determine fields that might work for our model
> #Create a subset of numeric fields to check for correlations
> corcol <- c(1, 2, 3, 5, 8, 9, 10, 11)
> 
> #Check correlations with the subset
> cor(train.df[ ,corcol])
                   Price  Age_08_04          KM          HP          CC       Doors Quarterly_Tax      Weight
Price          1.0000000 -0.8679513 -0.61814490  0.34238270  0.12297865  0.17953238     0.2274236  0.60211850
Age_08_04     -0.8679513  1.0000000  0.55769269 -0.16820001 -0.09681370 -0.15556551    -0.1960435 -0.48812991
KM            -0.6181449  0.5576927  1.00000000 -0.37000458  0.03954556 -0.03081387     0.2580081 -0.08818461
HP             0.3423827 -0.1682000 -0.37000458  1.00000000  0.01438708  0.04673949    -0.3249934  0.05324659
CC             0.1229786 -0.0968137  0.03954556  0.01438708  1.00000000  0.07900870     0.2243177  0.23676881
Doors          0.1795324 -0.1555655 -0.03081387  0.04673949  0.07900870  1.00000000     0.1214289  0.28685214
Quarterly_Tax  0.2274236 -0.1960435  0.25800813 -0.32499338  0.22431768  0.12142891     1.0000000  0.62563467
Weight         0.6021185 -0.4881299 -0.08818461  0.05324659  0.23676881  0.28685214     0.6256347  1.00000000
> 
> #Selecting Age, KM, and Weight as the three potential fields to build our model
> 
> #Now run plots to see how well each one matchs the price variable
> #We are checking each independent variable against the dependent variable
> 
> plot(Price ~ Age_08_04, data = train.df)
> plot(Price ~ KM, data = train.df)
> plot(Price ~ Weight, data = train.df)
> 
> #Now we will plot the independent variables against each other
> #to check for colinearity
> plot(Age_08_04 ~ KM, data = train.df)
> plot(Age_08_04 ~ Weight, data = train.df)
> plot(Weight ~ KM, data = train.df)
> 
> #One more test for correlation betweewn IVs and DV
> cor.test(train.df$Price, train.df$Age_08_04)

	Pearson's product-moment correlation

data:  train.df$Price and train.df$Age_08_04
t = -42.736, df = 598, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 -0.8864125 -0.8467334
sample estimates:
       cor 
-0.8679513 

> cor.test(train.df$Price, train.df$KM)

	Pearson's product-moment correlation

data:  train.df$Price and train.df$KM
t = -19.23, df = 598, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 -0.6652723 -0.5661111
sample estimates:
       cor 
-0.6181449 

> cor.test(train.df$Price, train.df$Weight)

	Pearson's product-moment correlation

data:  train.df$Price and train.df$Weight
t = 18.442, df = 598, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.5485102 0.6507969
sample estimates:
      cor 
0.6021185 

> 
> #Test correlations between IVs
> cor.test(train.df$Age_08_04, train.df$KM)

	Pearson's product-moment correlation

data:  train.df$Age_08_04 and train.df$KM
t = 16.43, df = 598, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.4999669 0.6104849
sample estimates:
      cor 
0.5576927 

> cor.test(train.df$Age_08_04, train.df$Weight)

	Pearson's product-moment correlation

data:  train.df$Age_08_04 and train.df$Weight
t = -13.677, df = 598, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 -0.5468093 -0.4246786
sample estimates:
       cor 
-0.4881299 

> cor.test(train.df$KM, train.df$Weight)

	Pearson's product-moment correlation

data:  train.df$KM and train.df$Weight
t = -2.1649, df = 598, p-value = 0.03079
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 -0.167049830 -0.008198099
sample estimates:
        cor 
-0.08818461 

> 
> #Now that that is done, what decisions can we make about our model?
> #How do we build our model?
> 
> #Let's assume that all of these fields were good candidates for our model
> #and that there was no colinearity
> 
> #We can build our model
> 
> cars.model <- lm(Price ~ Age_08_04 + KM + Weight, data = train.df)
> 
> #limit the number of decimal places in the display
> options(scipen = 999)
> 
> #show the results
> summary(cars.model)

Call:
lm(formula = Price ~ Age_08_04 + KM + Weight, data = train.df)

Residuals:
    Min      1Q  Median      3Q     Max 
-9562.7  -782.7    -1.9   782.6  6379.6 

Coefficients:
               Estimate  Std. Error t value            Pr(>|t|)    
(Intercept) -230.333467 1396.450090  -0.165               0.869    
Age_08_04   -129.787621    5.259261 -24.678 <0.0000000000000002 ***
KM            -0.028881    0.002118 -13.638 <0.0000000000000002 ***
Weight        18.635204    1.188284  15.682 <0.0000000000000002 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1517 on 596 degrees of freedom
Multiple R-squared:  0.8439,	Adjusted R-squared:  0.8431 
F-statistic:  1074 on 3 and 596 DF,  p-value: < 0.00000000000000022

> 
> #We now have an intercept and a coefficient or slope for each variable
> #We can build our model by plugging in the intercepts and coefficients directly into our model
> #We are establishing a variable in our train.df called Pred.Value and populating it with our equation
> 
> train.df$Pred.Value = 2095.185143 + (train.df$Age_08_04 * -135.626079) + (train.df$KM * -0.023021) + (train.df$Weight * 16.418368)
> 
> #How does that compare to our actual Price variable
> View(train.df[ ,c(1, 12)])
> 
> #Calculate the error for each row by subtracking Price from Pred.Value
> train.df$Error <- train.df$Pred.Value - train.df$Price
> View(train.df[ ,c(1, 12, 13)])
> 
> #Now average the error
> #We will talk about more sophisticated ways of using error later
> mean(train.df$Error)
[1] 19.72111
> 
> #Lets plot our predicted value against the price
> plot(Price ~ Pred.Value, data = train.df)
> 
> #What is the correlation?
> cor.test(train.df$Price, train.df$Pred.Value)

	Pearson's product-moment correlation

data:  train.df$Price and train.df$Pred.Value
t = 56.49, df = 598, p-value < 0.00000000000000022
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.9040696 0.9294719
sample estimates:
      cor 
0.9177037 


do the assingment now : Your assignment this week:
Modify the model to drop weight.  Apply it to the training data set and give me the intercept and coefficients
Calculate Adj.Price by applying the model in the same way that I calculated Pred.Value (just without Weight)
Calculate Adj.Error by subtracting Price from Adj.Price
Calculate the mean of Adj.Error and turn that in.

Now, you must check your (first) model against the validation data. Use the model that has all three variables in it.
Calcualte Pred.Value and Error in your valid.df data frame
Average the error
How does it compare to the average error in train.df?

Six things to turn into me:
1 Average error in train.df (with all three variables in the model) - I gave you that one.
2 Intercept and Coefficients when you drop Weight out of the model and apply it to train.df
3 Average Error in train.df with just Age_08_04 and KM in the model

Do these things using valid.df
Run the three variable model against valid.df
Calculate Pred.Value and Error using in valid.df
4 Average Error in valid.df with all three values in the original model
5 Plot of Price against Pred.Value in valid.df 
6 corelation of Price and Pred.Value in valid.df

what you as chatgpt need to do is give me the R code each of the 6 items for the assignment and  read the file using the 'ISO-8859-1' encoding



1. Average Error in train.df with All Three Variables in the Model
You already have this value as 19.72111.

2. Intercept and Coefficients When You Drop Weight Out of the Model and Apply it to train.df
R
Copy code
cars_model_adj <- lm(Price ~ Age_08_04 + KM, data = train.df)
summary(cars_model_adj)
3. Average Error in train.df with Just Age_08_04 and KM in the Model
R
Copy code
train.df$Adj.Price <- predict(cars_model_adj, newdata = train.df)
train.df$Adj.Error <- train.df$Adj.Price - train.df$Price
mean(train.df$Adj.Error)
4. Average Error in valid.df with All Three Values in the Original Model
Ensure you have the cars.model (with all three variables) created:

R
Copy code
cars_model <- lm(Price ~ Age_08_04 + KM + Weight, data = train.df)
valid.df$Pred.Value <- predict(cars_model, newdata = valid.df)
valid.df$Error <- valid.df$Pred.Value - valid.df$Price
mean(valid.df$Error)
5. Plot of Price against Pred.Value in valid.df
R
Copy code
plot(Price ~ Pred.Value, data = valid.df)
6. Correlation of Price and Pred.Value in valid.df
R
Copy code
cor.test(valid.df$Price, valid.df$Pred.Value)
Make sure to run these codes in sequence and after you have defined train.df and valid.df with the appropriate data and structure as per your initial R script. This will ensure the models and subsequent calculations are based on the correct datasets.









































#__________________________________________________
> car.df <- read.csv("C:\\RImport\\ToyotaCorolla.csv")
> 
> View(car.df)
> #Just use the first 1000 rows
> car.df <- car.df[1:1000, ]
> 
> #Narrow down the possible variables for use in building the model
> #Only look at variables that might work with the model
> selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)
> 
> #Now partition the data into training and validation sets
> 
> #Establish the starting point for reproducing the partitions
> set.seed(1)
> 
> #Build the partitions
> #600 for training data
> #400 for validation data
> train.index <- sample(c(1:1000), 600)
> train.df <- car.df[train.index, selected.var]
> valid.df <- car.df[-train.index, selected.var]
> 
> #Now determine fields that might work for our model
> #Create a subset of numeric fields to check for correlations
> corcol <- c(1, 2, 3, 5, 8, 9, 10, 11)
> 
> #Check correlations with the subset
> cor(train.df[ ,corcol])
                   Price  Age_08_04          KM          HP          CC       Doors Quarterly_Tax      Weight
Price          1.0000000 -0.8679513 -0.61814490  0.34238270  0.12297865  0.17953238     0.2274236  0.60211850
Age_08_04     -0.8679513  1.0000000  0.55769269 -0.16820001 -0.09681370 -0.15556551    -0.1960435 -0.48812991
KM            -0.6181449  0.5576927  1.00000000 -0.37000458  0.03954556 -0.03081387     0.2580081 -0.08818461
HP             0.3423827 -0.1682000 -0.37000458  1.00000000  0.01438708  0.04673949    -0.3249934  0.05324659
CC             0.1229786 -0.0968137  0.03954556  0.01438708  1.00000000  0.07900870     0.2243177  0.23676881
Doors          0.1795324 -0.1555655 -0.03081387  0.04673949  0.07900870  1.00000000     0.1214289  0.28685214
Quarterly_Tax  0.2274236 -0.1960435  0.25800813 -0.32499338  0.22431768  0.12142891     1.0000000  0.62563467
Weight         0.6021185 -0.4881299 -0.08818461  0.05324659  0.23676881  0.28685214     0.6256347  1.00000000
> 
> #Selecting Age, KM, and Weight as the three potential fields to build our model
> 
> #Now run plots to see how well each one matchs the price variable
> #We are checking each independent variable against the dependent variable
> 
> plot(Price ~ Age_08_04, data = train.df)
> plot(Price ~ KM, data = train.df)
> plot(Price ~ Weight, data = train.df)
> 
> #Now we will plot the independent variables against each other
> #to check for colinearity
> plot(Age_08_04 ~ KM, data = train.df)
> plot(Age_08_04 ~ Weight, data = train.df)
> plot(Weight ~ KM, data = train.df)
> 
> #One more test for correlation betweewn IVs and DV
> cor.test(train.df$Price, train.df$Age_08_04)

	Pearson's product-moment correlation

data:  train.df$Price and train.df$Age_08_04
t = -42.736, df = 598, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 -0.8864125 -0.8467334
sample estimates:
       cor 
-0.8679513 

> cor.test(train.df$Price, train.df$KM)

	Pearson's product-moment correlation

data:  train.df$Price and train.df$KM
t = -19.23, df = 598, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 -0.6652723 -0.5661111
sample estimates:
       cor 
-0.6181449 

> cor.test(train.df$Price, train.df$Weight)

	Pearson's product-moment correlation

data:  train.df$Price and train.df$Weight
t = 18.442, df = 598, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.5485102 0.6507969
sample estimates:
      cor 
0.6021185 

> 
> #Test correlations between IVs
> cor.test(train.df$Age_08_04, train.df$KM)

	Pearson's product-moment correlation

data:  train.df$Age_08_04 and train.df$KM
t = 16.43, df = 598, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.4999669 0.6104849
sample estimates:
      cor 
0.5576927 

> cor.test(train.df$Age_08_04, train.df$Weight)

	Pearson's product-moment correlation

data:  train.df$Age_08_04 and train.df$Weight
t = -13.677, df = 598, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 -0.5468093 -0.4246786
sample estimates:
       cor 
-0.4881299 

> cor.test(train.df$KM, train.df$Weight)

	Pearson's product-moment correlation

data:  train.df$KM and train.df$Weight
t = -2.1649, df = 598, p-value = 0.03079
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 -0.167049830 -0.008198099
sample estimates:
        cor 
-0.08818461 

> 
> #Now that that is done, what decisions can we make about our model?
> #How do we build our model?
> 
> #Let's assume that all of these fields were good candidates for our model
> #and that there was no colinearity
> 
> #We can build our model
> 
> cars.model <- lm(Price ~ Age_08_04 + KM + Weight, data = train.df)
> 
> #limit the number of decimal places in the display
> options(scipen = 999)
> 
> #show the results
> summary(cars.model)

Call:
lm(formula = Price ~ Age_08_04 + KM + Weight, data = train.df)

Residuals:
    Min      1Q  Median      3Q     Max 
-9562.7  -782.7    -1.9   782.6  6379.6 

Coefficients:
               Estimate  Std. Error t value            Pr(>|t|)    
(Intercept) -230.333467 1396.450090  -0.165               0.869    
Age_08_04   -129.787621    5.259261 -24.678 <0.0000000000000002 ***
KM            -0.028881    0.002118 -13.638 <0.0000000000000002 ***
Weight        18.635204    1.188284  15.682 <0.0000000000000002 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1517 on 596 degrees of freedom
Multiple R-squared:  0.8439,	Adjusted R-squared:  0.8431 
F-statistic:  1074 on 3 and 596 DF,  p-value: < 0.00000000000000022

> 
> #We now have an intercept and a coefficient or slope for each variable
> #We can build our model by plugging in the intercepts and coefficients directly into our model
> #We are establishing a variable in our train.df called Pred.Value and populating it with our equation
> 
> train.df$Pred.Value = 2095.185143 + (train.df$Age_08_04 * -135.626079) + (train.df$KM * -0.023021) + (train.df$Weight * 16.418368)
> 
> #How does that compare to our actual Price variable
> View(train.df[ ,c(1, 12)])
> 
> #Calculate the error for each row by subtracking Price from Pred.Value
> train.df$Error <- train.df$Pred.Value - train.df$Price
> View(train.df[ ,c(1, 12, 13)])
> 
> #Now average the error
> #We will talk about more sophisticated ways of using error later
> mean(train.df$Error)
[1] 19.72111
> 
> #Lets plot our predicted value against the price
> plot(Price ~ Pred.Value, data = train.df)
> 
> #What is the correlation?
> cor.test(train.df$Price, train.df$Pred.Value)

	Pearson's product-moment correlation

data:  train.df$Price and train.df$Pred.Value
t = 56.49, df = 598, p-value < 0.00000000000000022
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.9040696 0.9294719
sample estimates:
      cor 
0.9177037 


do the assingment now : Your assignment this week:
Modify the model to drop weight.  Apply it to the training data set and give me the intercept and coefficients
Calculate Adj.Price by applying the model in the same way that I calculated Pred.Value (just without Weight)
Calculate Adj.Error by subtracting Price from Adj.Price
Calculate the mean of Adj.Error and turn that in.

Now, you must check your (first) model against the validation data. Use the model that has all three variables in it.
Calcualte Pred.Value and Error in your valid.df data frame
Average the error
How does it compare to the average error in train.df?

Six things to turn into me:
1 Average error in train.df (with all three variables in the model) - I gave you that one.
2 Intercept and Coefficients when you drop Weight out of the model and apply it to train.df
3 Average Error in train.df with just Age_08_04 and KM in the model

Do these things using valid.df
Run the three variable model against valid.df
Calculate Pred.Value and Error using in valid.df
4 Average Error in valid.df with all three values in the original model
5 Plot of Price against Pred.Value in valid.df 
6 corelation of Price and Pred.Value in valid.df

what you need to do is give me the R code each of the 6 items for the assignment and  read the file using the 'ISO-8859-1' encoding