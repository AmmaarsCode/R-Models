#### We are going to run two types of clusters in this script using the Utilities.csv file.

#### The first type of cluster will be a hierarchical agglomerative cluster which we ususally view in a dendogram.



## First, lets read in our file

utilities.df <- read.csv("Utilities.csv")

# set row names to the utilities column
row.names(utilities.df) <- utilities.df[,1]

# remove the utility column
utilities.df <- utilities.df[,-1]

# compute Euclidean distance
# (to compute other distance measures, change the value in method = )
d <- dist(utilities.df, method = "euclidean")

d

#### Now we have to normalize just like we did in kNN so that the big values don't overpower the clustering algorithm

# normalize input variables by converting them to z scores
utilities.df.norm <- sapply(utilities.df, scale)

# add row names: utilities
row.names(utilities.df.norm) <- row.names(utilities.df) 

# compute normalized distance based on variables Sales and FuelCost
d.norm <- dist(utilities.df.norm[,c(6,8)], method = "euclidean")

d.norm

#### Now that we  have normalized data, we are ready to run the cluster and plot the dendograms

# compute normalized distance based on all 8 variables - this is a repeated step
d.norm <- dist(utilities.df.norm, method = "euclidean")

# We will try two different types of distance measures - single and average
# and generate two dendograms
# Capture each dendogram and put it into a word document
# Remember to right click on the plot and save as bitmap

hc1 <- hclust(d.norm, method = "single")
plot(hc1, hang = -1, ann = FALSE)
hc2 <- hclust(d.norm, method = "average")
plot(hc2, hang = -1, ann = FALSE)





####  Now we will run a k-means cluster assuming that there are six clusters - What do we thing that?  Take a look at the dendograms.


# reload and preprocess data 
utilities.df <- read.csv("Utilities.csv")
row.names(utilities.df) <- utilities.df[,1]
utilities.df <- utilities.df[,-1]

# normalized distance:
utilities.df.norm <- sapply(utilities.df, scale)
row.names(utilities.df.norm) <- row.names(utilities.df) 

# run kmeans algorithm 
set.seed(2)
km <- kmeans(utilities.df.norm, 6)

# show cluster membership
km$cluster
#### Capture the cluster membership chart and put it into the Word document along with your dendograms


#### Also capture the centrioids chart and put it into the Word document
# centroids
km$centers

# Lastly, run a summary of km
summary(km)

#### Put that into your Word document too.  Submit the Word document as this week's assignment.



