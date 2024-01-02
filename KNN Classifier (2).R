####k-nn classifier

#In this script, we will load up a data file that will be divided into training data and validate data
#Then we will load a second file and check these records against what is in our training file
#To make a classification of whether the new customers are owners or non-owners of riding mowers

#First, lets load our files
mower.df <- read.csv("C:\\RImport\\RidingMowers.csv")
new.df <- read.csv("C:\\RImport\\NewCustomer.csv")

#Now we are going to divide the mower dataframe into two mutually exclusive files
#These will be names train and valid 
#We will build train by taking 60% of the mower.df dataframe
#We will build valid by taking the records not put into frame

#First set.seed for the random number generator
set.seed(111)

#Now build indices that separate mower.df into train.df and valid.df
train.index <- sample(row.names(mower.df), 0.6*dim(mower.df)[1])  
valid.index <- setdiff(row.names(mower.df), train.index)  

#Use the indices to build the new dataframes
train.df <- mower.df[train.index, ]
valid.df <- mower.df[valid.index, ]

## scatter plot

#Now do a visualiztion of the data points in the train dataframe.  Some are owners and some arent
#use pch=ifelse to run a "if" then "else" command on your plot
#this will set the pch to 1 if the data point is an owner and to 3 for a non-owner
#those records with pch set to 1 will be indicated with a circle
#those records with pch set to 3 will be indicated with a plus sign
#note that == is "not equal to"

plot(Lot_Size ~ Income, data=train.df, pch=ifelse(train.df$Ownership=="Owner", 1, 3))

#The text command is used to modify the scatter plot.  The first command adds row names.
#The second text command adds in the four new data points from your new customer file.  These are the ones you want to classify
#The legend fuction builds a legend in the top right of your scatter plot

text(train.df$Income, train.df$Lot_Size, rownames(train.df), pos=4)
text(new.df$Income, new.df$Lot_Size, "X")
#text(60, 20, "X")
legend("topright", c("owner", "non-owner", "newhousehold"), pch = c(1, 3, 4))

#Now, given the visual, what predictions can you make about how the four data points will be classified?

#### Run k-NN classifiers on the four new data points to classify them

## But first we need to normalize the data.  We will use z scores which is the distance from the mean in standard deviations.

# initialize normalized training, validation data, complete data frames to originals
# We will set up our new normalized data frames

train.norm.df <- train.df
valid.norm.df <- valid.df
mower.norm.df <- mower.df

# use preProcess() from the caret package to normalize Income and Lot_Size.
# we need to load two packages.  Lets do that now.
library(caret)
library(FNN)
#Set up our normlization template which is to build z scores for the values in train.df

norm.values <- preProcess(train.df[, 1:2], method=c("center", "scale"))

#Use predict to apply the z-score calculations to the three normalized data frames (train.norm, valid,norm, mower.norm)
train.norm.df[, 1:2] <- predict(norm.values, train.df[, 1:2])
valid.norm.df[, 1:2] <- predict(norm.values, valid.df[, 1:2])
mower.norm.df[, 1:2] <- predict(norm.values, mower.df[, 1:2])

#Also run normlization on the new data file so that it is on the same scale as the training and validation data
new.norm.df <- predict(norm.values, new.df)

# Now we are ready to use knn() to compute knn. 
# knn() is available in library FNN (provides a list of the nearest neighbors).  It has already been loaded.
# and library class (allows a numerical output variable).

nn <- knn(train = train.norm.df[, 1:2], test = new.norm.df, 
          cl = train.norm.df[, 3], k = 3)

#Let's look at the results
summary(nn)
print(nn)

#Match the results of our classification with the datat that was in our original new customer file
View(new.df)



