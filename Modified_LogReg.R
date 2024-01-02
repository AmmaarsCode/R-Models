
### Using Logistic Regression to determine if a customer is likley, or not, to accept an offer of a bank loan.
### Logistic Regression is very useful in determining the value of a binary variable (0/1, yes/no, approve/not approve, fraud/valid)
### We will load in 5000 records showing information on bank customers to build a model that we can then apply to new data sets
### We can validat our model using histograms but there are many other ways to validate the mode.

### In this case we have training and validation data that show which customers accepted the loan offer and which ones did not.
### We will look at some of the variables for each customer and see if we can use them to help us classify new customers.
### As new customers come in, we won't know if they will accept our offer or not.  
### But, by building a model and applying it to our data, we can eliminate a large number of customers from consideration
### This saves a lot of time and money

### Lets start by loading our data set

bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)]  # Drop ID and zip code columns.
# treat Education as categorical (R will create dummy variables)
bank.df$Education <- factor(bank.df$Education, levels = c(1, 2, 3), 
                            labels = c("Undergrad", "Graduate", "Advanced/Professional"))

### Now we divide the data into training (60% of records) and validation data (the rest) just like we have before

# partition data
set.seed(2)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

### Next we run logistic regression on all of the fields to build a model.  
### This model can be used against new data using the predict function. 
### But let's look at the values that came out of the model first.


# run logistic regression to build a model than we can apply to data
# use glm() (general linear model) with family = "binomial" to fit a logistic 
# regression.
logit.reg <- glm(Personal.Loan ~ ., data = train.df, family = "binomial") 
options(scipen=999) #for readability
summary(logit.reg) # show us the details of the mnodel

### Here we look at the p values to determine the best variables to use in our model
### Those with very low p values are going to be more useful in helping us build a model that will classify the records in our new data sets
### So let's review which fields have low p values and select some of those variables to use

### What do we see?  Which variables might be the best to use in our model?
### Remember we are trying to get the best classification we can

### Both Income and Education (among others) have low p values
### Lets narrow down the field to Inocome and Education and see how that works

### Run glm against more than one variable using the + sign to add them

logit.reg <- glm(Personal.Loan ~ Income + Education, data = train.df, family = "binomial")

### Lets look at the details of our model.

summary(logit.reg)

### Now how does our model look?  How do we know how well it might work?
### Lets apply it to our training data and build a new data frame.
### It contains the actual value of Personal.Loan and the value that our model would have returned to us
### We can compare them to see how well they match.

### To do this, we apply our logit.reg model to the training data set and put that result into logit.reg.pred
### We use the predict funtion to do this

logit.reg.pred <- predict(logit.reg, train.df, type = "response")

### Now we will create a data frame that allows us to merge and compare the predicted against the actual
### We will combine actual from our original train.df file and the predicted from the newly created logit.reg.pred file

forplots <- data.frame(actual = train.df$Personal.Loan, predicted = logit.reg.pred)

### How do the predictions compare to the actuals?
### Let's compare them with historgrams
### But we will arrange to show the historgrams to show side by side (or top to bottom)

# Fist - attach to the dataframe
attach(forplots)

# Now - set the parameters for display (2 rows and 1 column)
par(mfrow = c(2,1))

# Last - create the plots
hist(actual)
hist(predicted)

# How do they compare?
# Do you see the need to set a threshold?

####  Time to validate our model agains the valid.df to see if there are any large differences

# It would appear that things look okay in train.df and we have narrowed down our number of variables.
# Let's test out the model we have just created against the validation data and see how it looks.

# use predict() with type = "response" to compute predicted probabilities. 
logit.reg.pred <- predict(logit.reg, valid.df, type = "response")

# Build a new data frame from actual and predicted records
forplots2 <- data.frame(actual = valid.df$Personal.Loan, predicted = logit.reg.pred)

### How do the predictions compare to the actuals?
### Let's compare them with historgrams
### but we will arrange to show the historgrams side by side (or top to bottom)

# Fist - attach to the dataframe
attach(forplots2)

# Now - set the parameters for display (2 rows and 1 column)
par(mfrow = c(2,1))

# Last - create the plots
hist(actual)
hist(predicted)

# How do they compare?
# Do you see the need to set a threshold?

### What can you say about the usefulness of our model?






# Summary of the model
model_summary <- summary(logit.reg)
print(model_summary)
