# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle.data","NbClust"))
library(cluster)
library(rattle.data)
library(NbClust)

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function

df <- scale(wine[-1])

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(df, nc=15, seed=1234){
	              wss <- (nrow(df)-1)*sum(apply(df,2,var))
               	      for (i in 2:nc){
		        set.seed(1234)
	                wss[i] <- sum(kmeans(df, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(df)

# Exercise 2:
#   * How many clusters does this method suggest?
      # Answer: it suggests 3 clusters.
#   * Why does this method work? What's the intuition behind it?
      # Answer: It looks for the optimal point where both the sum of squares within a group
                # and the number of clusters are lowest.
#   * Look at the code for wssplot() and figure out how it works
      # Answer: The first line calculates the within sum of squares (wss) for centers=1.
                # (The formula for calculating variance is "wss/(n-1)", so multiplying var with nrow(df)-1 will get the wss for centers=1.)
                # The next for loop is calculating wss from 2 to 15.

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
  # Answer: This method also suggests 3 clusters.

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(df, 3, nstart=25)
fit.km$size
fit.km$centers
aggregate(wine[-1], by=list(cluster=fit.km$cluster), mean)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

ct.km <- table(wine$Type, fit.km$cluster)
ct.km

# Answer: It's quite good clustering because only the 2nd type of wine has (minimal)
#         spillovers to other clusters. For the most part, each type of wine is solidly assigned to
#         its own cluster.

# Check the adjusted Rand index
library(flexclust)
randIndex(ct.km)

# The Rand index is 0.9, which is close to perfect agreement between the partitions.

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

# Run a PAM clustering
set.seed(1234)
fit.pam <- pam(wine[-1], k=3, stand=TRUE)
fit.pam$medoids

# Visualize using clusplot

clusplot(fit.pam, main="Bivariate Cluster Plot")

# It's clear from the plot that there is considerable overlap between clusters.
# This suggests this isn't a very good partitioning.

# Check the adjusted Rand index

ct.pam <- table(wine$Type, fit.pam$clustering)
randIndex(ct.pam)

# Table comparing clusters and wine types shows much more overlap compared to the k-means table.
# Adjusted Rand index is 0.7 --considerably lower than the k-means clustering.

