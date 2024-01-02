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

#Your assignment this week:
#Modify the model to drop weight.  Apply it to the training data set and give me the intercept and coefficients
#Calculate Adj.Price by applying the model in the same way that I calculated Pred.Value (just without Weight)
#Calculate Adj.Error by subtracting Price from Adj.Price
#Calculate the mean of Adj.Error and turn that in.

#Now, you must check your (first) model against the validation data. Use the model that has all three variables in it.
#Calcualte Pred.Value and Error in your valid.df data frame
#Average the error
#How does it compare to the average error in train.df?

#Six things to turn into me:
# 1 Average error in train.df (with all three variables in the model) - I gave you that one.
# 2 Intercept and Coefficients when you drop Weight out of the model and apply it to train.df
# 3 Average Error in train.df with just Age_08_04 and KM in the model

#Do these things using valid.df
#Run the three variable model against valid.df
#Calculate Pred.Value and Error using in valid.df
# 4 Average Error in valid.df with all three values in the original model
# 5 Plot of Price against Pred.Value in valid.df 
# 6 corelation of Price and Pred.Value in valid.df




