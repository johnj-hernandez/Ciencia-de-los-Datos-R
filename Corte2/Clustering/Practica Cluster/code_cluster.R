######    CLUSTER ANALYSIS   #########33

### Calculate & plot the distance between two players
### You've obtained the coordinates relative to the center of the field for two players 
### in a soccer match and would like to calculate the distance between them.

#In this exercise you will plot the positions of the 2 players and manually calculate the distance
#between them by using the euclidean distance formula.

two_players<- data.frame( x = c(5, 15), y =c(4, 10))

## E1 Plot their positions from the two_players dataframe using ggplot
install.packages("ggplot2")
#library(ggplot2)
ggplot(two_players, aes(x = x, y = y)) + 
  geom_point() +
 # Assuming a 40x60 field
 lims(x = c(-30,30), y = c(-20, 20))


## E2 Extract the positions of the players into two data frames player1 and player2


player1 <- two_players[1,]
player2 <- two_players[2,]

## E3 Calculate the distance between player1 and player2 by using the euclidean distance formula

player_distance <- sqrt( (player1$x - player2$x)^2 + (player1$y - player2$y)^2 )
player_distance


# Using the euclidean formula manually may be practical for 2 observations but can get more complicated
#rather quickly when measuring the distance between many observations.

# The dist() function simplifies this process by calculating distances between our observations 
# (rows) using their features (columns). In this case the observations are the player positions and the dimensions are their x and y coordinates.

#Note: The default distance calculation for the dist() function is euclidean distance

## E4 Calculate the distance between two players using the dist() function for the dataframe two_players

dist_two_players <- dist(two_players)
dist_two_players

#Calculate the distance between three players for the dataframe three_players
three_players<- data.frame( x = c(5, 15, 0), y =c(4, 10, 20))

# Calculate the Distance Between three_players
dist_three_players <- dist(three_players)
dist_three_players

#Effects of scale
# You have learned that when a variable is on a larger scale than other variables in your data 
# it may disproportionately influence the resulting distance calculated between your observations. 
# Lets see this in action by observing a sample of data from the trees data set.

# You will leverage the scale() function which by default centers & scales our column features.

# Our variables are the following:
#  Girth - tree diameter in inches
#  Height - tree height in inches
# Calculate the distance matrix for the dataframe three_trees and store it as dist_trees

three_trees <- data.frame(Girth = c(8.3, 8.6, 10.5), Height = c(840, 780, 864))


# Calculate distance for three_trees 
dist_trees <- dist(three_trees)

# Create a new variable scaled_three_trees where the three_trees data is centered & scaled

# Scale three trees & calculate the distance  
scaled_three_trees <- scale(three_trees)


# Calculate and print the distance matrix for scaled_three_trees and store this as dist_scaled_trees
# Output the results of both Matrices
dist_scaled_trees <- dist(scaled_three_trees)
dist_scaled_trees

# Output both dist_trees and dist_scaled_trees matrices and observe the change of which observations 
# have the smallest distance between the two matrices (hint: they have changed)
print('Without Scaling')
dist_trees
print('With Scaling')
dist_scaled_trees

## Calculating distance between categorical variables
## In this exercise you will explore how to calculate binary (Jaccard) distances.
#In order to calculate distances will first have to dummify our categories using the dummy.data.frame() from the library dummies

## You will use a small collection of survey observations stored in the data frame job_survey 
## with the following columns:
  
##  job_satisfaction Possible options: "Hi", "Mid", "Low"
##  is_happy Possible options: "Yes", "No"

job_survey<- data.frame(job_satisfaction = c('Low', 'Low', 'Hi', 'Low', 'Mid'), is_happy = c('No', 'No', 'Yes', 'No', 'No'))


## Create a dummified dataframe dummy_survey
# Dummify the Survey Data
# install.packages('dummies')
library(dummies)
dummy_survey <- dummy.data.frame(job_survey)

## Generate a Jaccard distance matrix for the dummified survey data dist_survey
## using the dist() function using the parameter method = 'binary'
  # Calculate the Distance
dist_survey <-dist(dummy_survey,method='binary') 

# Print the original data and the distance matrix
# Note the observations with a distance of 0 in the original data (1, 2, and 4)

job_survey
dist_survey


## Extract the distance values between all three pairs of players into individual variables
readRDS('lineup.RDS')
# Extract the pair distances of dist_players

dist_players<- floor(dist_three_players)
distance_1_2<-as.matrix(dist_players)["1","2"]
distance_1_3 <- as.matrix(dist_players)["1","3"]
distance_2_3 <- as.matrix(dist_players)["2","3"]

## Calculate the distance from player 3 to the group of players 1 & 2 using the following three 
## linkage methods

# Calculate the complete distance between group 1-2 and 3
## Complete: the resulting distance is based on the maximum
max(distance_1_3,distance_2_3)


## Single: the resulting distance is based on the minimum
# Calculate the single distance between group 1-2 and 3
max(distance_1_3,distance_2_3)

## Average: the resulting distance is based on the average

# Calculate the average distance between group 1-2 and 3
mean(c(distance_1_3,distance_2_3))


########## Assign cluster membership ################
## In this exercise you will leverage the hclust() function to calculate the iterative
## linkage steps and you will use the cutree() function to extract the cluster assignments 
## for the desired number (k) of clusters.
lineup<- readRDS('lineup.RDS')
# Calculate the Distance
dist_players <- dist(lineup) 

## You are given the positions of 12 players at the start of a 6v6 soccer match. 
## This is stored in the lineup dataframe.
## You know that this match has two teams (k = 2), let's use the clustering methods your
## learned to assign which team each player belongs in based on their position.

#Notes:
#The linkage method can be passed via the method parameter: hclust(distance_matrix, method = "complete")
#Remember that in soccer opposing teams start on their half of the field.
#Because these positions are measured using the same scale we do not need to re-scale our data.
hclust(dist_players,method = "complete")


#Calculate the euclidean distance matrix dist_players among all twelve players

dist_players <- dist(lineup,method = "euclidean") 


#Perform the complete linkage calculation for hierarchical clustring using hclust and store this as hc_players
# Perform the hierarchical clustering using the complete linkage
hc_players <-  hclust(dist_players   , method ="complete")


#Build the cluster assignment vector clusters_k2 using cutree() with a k = 2
#Calculate the assignment vector with a k of 2
clusters_k2 <- cutree(hc_players,k=2)



#Append the cluster assignments as a column cluster to the lineup data frame and save the results to
#a new dataframe called lineup_k2_complete
library(dplyr)
# Create a new dataframe storing these results
lineup_k2_complete <- mutate(lineup, cluster = clusters_k2)


########  Exploring the clusters   ######3
## Because clustering analysis is always in part qualitative, it is incredibly important to have the necessary tools to explore the results of the clustering.

## In this exercise you will explore that data frame you created in the previous exercise lineup_k2_complete.

## Reminder: The lineup_k2_complete dataframe contains the x & y positions of 12 players at the start of a 6v6 soccer game to which you have added clustering assignments based on the following parameters:
  
##  Distance: Euclidean
## Number of Clusters (k): 2
## Linkage Method: Complete

## Using count() from dplyr, count the number of players assigned to each cluster.
# Count the cluster assignments
count(lineup_k2_complete, cluster)

## Using ggplot(), plot the positions of the players and color them by cluster assignment.

# Plot the positions of the players and color them using their cluster
ggplot(lineup_k2_complete, aes(x = x, y = y, color = factor(cluster))) +
  geom_point()


## Comparing average, single & complete linkage ###

#You are now ready to analyze the clustering results of the lineup dataset using 
#the dendrogram plot. This will give you a new perspective on the effect the decision
#of the linkage method has on your resulting cluster analysis.


# Perform the linkage calculation for hierarchical clustring using the linkages: complete, 
# single and average
# Prepare the Distance Matrix
dist_players <- dist(lineup)

# Generate hclust for complete, single & average linkage methods
hc_complete <- hclust(dist_players, method = "complete")
hc_single <- hclust(dist_players, method ="single" )
hc_average <- hclust(dist_players, method = "average")



# Plot the three dendrograms side by side and review the changes
# Plot & Label the 3 Dendrograms Side-by-Side
# Hint: To see these Side-by-Side run the 4 lines together as one command
par(mfrow = c(1,3))
plot(hc_complete, main = 'Complete Linkage')
plot(hc_single, main = 'Single Linkage')
plot(hc_average, main = 'Average Linkage')



#########
#install.packages("dendextend")
library(dendextend)
dist_players <- dist(lineup, method = 'euclidean')
hc_players <- hclust(dist_players, method = "complete")

# Create a dendrogram object from the hclust variable
dend_players <- as.dendrogram(hc_players)

# Plot the dendrogram
plot(dend_players)

# Color branches by cluster formed from the cut at a height of 20 & plot
dend_20 <- color_branches(dend_players, h = 20)

# Plot the dendrogram with clusters colored below height 20
plot(dend_20)

# Color brances by cluster formed from the cut at a height of 40 & plot
dend_40 <- color_branches(dend_players, h = 40)

# Plot the dendrogram with clusters colored below height 40

plot(dend_40)

###########HASTA AQUI 
## read the RDS file ws_customer and call it customer_spend
customers_spend<- readRDS('ws_customerS.RDS')

dist_customers <- dist(customers_spend)
hc_customers <- hclust(dist_customers)
clust_customers <- cutree(hc_customers, h = 15000)
segment_customers <- mutate(customers_spend, cluster = clust_customers)

# Count the number of customers that fall into each cluster
count(segment_customers, cluster)

# Color the dendrogram based on the height cutoff
dend_customers <- as.dendrogram(hc_customers)
dend_colored <- color_branches(dend_customers, h = 15000)

# Plot the colored dendrogram
plot(dend_colored)

# Calculate the mean for each category
segment_customers %>% group_by(cluster) %>% summarise_all(funs(mean(.)))
segment_customers %>% group_by(cluster) %>% summarise(
  mean(Milk),
  mean(Grocery),
  mean(Frozen)
)
##hasta aqui----------------------------------

#Given the results of these clusters all 4 statements are resonable statements. Whether this is meaningful would heavily depend on the business context it is used for. Ask yourself these questions:
 # Are clusters of 6 or less meaningful for my question?
#Likewise, does the large cluster of 29 need to be split up?
#Are the distance, linkage and height decisions employed appropriate to your problem?
#The key to mastering clustering analysis is to keep asking these questions!

# Go back to the lineup dataset
# Build a kmeans model
model_km2 <- kmeans(lineup, centers = 2)

# Extract the cluster assignment vector from the kmeans model
clust_km2 <- model_km2$cluster

library(dplyr)
# Create a new dataframe appending the cluster assignment
lineup_km2 <- mutate(lineup, cluster = clust_km2)

# Plot the positions of the players and color them using their cluster
ggplot(lineup_km2, aes(x = x, y = y, color = factor(cluster))) +
  geom_point()

# Does this result make sense? Remember we only have 2 teams on the field. It's very 
# important to remember that k-means will run with any k that is more than 2 and less 
# than your total observations, but it doesn't always mean the results will be meaningful.


# Build a kmeans model
model_km3 <- kmeans(lineup, centers = 3)

# Extract the cluster assignment vector from the kmeans model
clust_km3 <- model_km3$cluster

# Create a new dataframe appending the cluster assignment
lineup_km3 <- mutate(lineup, cluster = clust_km3)

# Plot the positions of the players and color them using their cluster
ggplot(lineup_km3, aes(x = x, y = y, color = factor(cluster))) +
  geom_point()

library(purrr)

# Use map_dbl to run many models with varying value of k (centers)
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(lineup, centers = k)
  model$tot.withinss
})

# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10 ,
  tot_withinss = tot_withinss
)


# Use map_dbl to run many models with varying value of k (centers)
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = lineup, centers = k)
  model$tot.withinss
})

# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

# Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)

## That is correct, you can see that there is a sharp change in the slope of this line that
## makes an “elbow” shape. Furthermore, this is supported by the prior knowledge that there 
## two teams in this data and a k of 2 is desired.


## Silhouette analysis
## Silhouette analysis allows you to calculate how similar each observations is with the cluster it is 
## assigned relative to other clusters. This metric (silhouette width) ranges from -1 to 1 for each 
##observation in your data and can be interpreted as follows:
  
# Values close to 1 suggest that the observation is well matched to the assigned cluster
# Values close to 0 suggest that the observation is borderline matched between two clusters
# Values close to -1 suggest that the observations may be assigned to the wrong cluster
# In this exercise you will leverage the pam() and the silhouette() functions from the clusters library to perform silhouette analysis to compare the results of models with a k of 2 an a k of 3. You'll continue working with the lineup dataset.


# Use map_dbl to run many models with varying value of k
sil_width <- map_dbl(2:10,  function(k){
  model <- pam(x = customers_spend, k = k)
  model$silinfo$avg.width
})

# Generate a data frame containing both k and sil_width
sil_df <- data.frame(
  k = 2:10,
  sil_width = sil_width
)

# Plot the relationship between k and sil_width
ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 2:10)

## You're doing great! From the plot I hope you noticed that k = 2 has the highest average 
## sillhouette width and is the “best” value of k we will move forward with.


# Build a k-means model for the customers_spend with a k of 2
model_customers <- kmeans(customers_spend, centers = 2)

# Extract the vector of cluster assignments from the model
clust_customers <- model_customers$cluster

# Build the segment_customers dataframe
segment_customers <- mutate(customers_spend, cluster = clust_customers)

# Calculate the size of each cluster
count(segment_customers, cluster)

# Calculate the mean for each category
segment_customers %>% 
  group_by(cluster) %>% 
  summarise_all(funs(mean(.)))

## Well done! It seems that in this case cluster 1 constists of individuals who proportionally 
## spend more on Frozen food while cluster 2 customers spent more on Milk and Grocery. 
## Did you notice that when you explored this data using hierarchical clustering, the method 
## resulted in 4 clusters while using k-means got you 2. Both of these results are valid, but
## which one is appropriate for this would require more subject matter expertise. Before you 
## proceed with the next chapter, remember that: Generating clusters is a science, but interpreting
## them is an art.


# Lets use the Occuational Employment Statistics OES
# Read the RDS FILE oes
# Calculate euclidean distance between the occupations
oes<- readRDS('oes.RDS')
dist_oes <- dist(oes, method = 'euclidean')

# Generate an average linkage analysis 
hc_oes <- hclust(dist_oes, method = 'average')

# Create a dendrogram object from the hclust variable
dend_oes <- as.dendrogram(hc_oes)

# Plot the dendrogram
plot(dend_oes)

# Color brances by cluster formed from the cut at a height of 100000
dend_colored <- color_branches(dend_oes, h = 100000)

# Plot the colored dendrogram
plot(dend_colored)


# Hierarchical clustering: Preparing for exploration
# You have now created a potential clustering for the oes data, before you can explore these 
# clusters with ggplot2 you will need to process the oes data matrix into a tidy data frame with 
# each occupation assigned its cluster.


# Create the df_oes data frame from the oes data.matrix, making sure to store the rowname as a 
#column (use rownames_to_column() from the tibble library)
dist_oes <- dist(oes, method = 'euclidean')
hc_oes <- hclust(dist_oes, method = 'average')

library(tibble)
library(tidyr)

# Use rownames_to_column to move the rownames into a column of the data frame
df_oes <- rownames_to_column(as.data.frame(oes), var = 'occupation')



#Build the cluster assignment vector cut_oes using cutree() with a h = 100,000

# Create a cluster assignment vector at h = 100,000
cut_oes <- cutree(hc_oes, h = 100000)


#Append the cluster assignments as a column cluster to the df_oes data frame and save the results to a new dataframe called clust_oes
# Generate the segmented the oes dataframe
clust_oes <- mutate(df_oes, cluster = cut_oes)

# Create a tidy data frame by gathering the year and values into two columns

df_oes <- rownames_to_column(as.data.frame(oes), var = 'occupation')

#Use the gather() function from the tidyr() library to rehape the data into a format amenable for ggplot2 analysis and save the tidied data frame as gather_oes

gathered_oes <- gather(data = clust_oes, 
                       key = year, 
                       value = mean_salary, 
                       -occupation, -cluster)

## You have succesfully created all the parts necessary to explore the results of this
## hierarchical clustering work. In this exercise you will leverage the named assignment
# vector cut_oes and the tidy data frame gathered_oes to analyze the resulting clusters.

## View the assignments of each occupation to their clustering by sorting the cut_oes vector using sort()
sort(cut_oes)

##Use ggplot2 to plot each occupation's average income by year and color the lines by the occupation's 
#assigned cluster.

ggplot(gathered_oes, aes(x = year, y = mean_salary, color = factor(cluster))) + 
  geom_line(aes(group = occupation))


# Use map_dbl to run many models with varying value of k (centers)
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = oes, centers = k)
  model$tot.withinss
})

# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

# Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)


#K-means: Average Silhouette Widths
## So hierarchical clustering resulting in 3 clusters and the elbow method suggests 2. 
## In this exercise use average silhouette widths to explore what the "best" value of k should be.

#Use map_dbl() to run pam() using the oes data for k values ranging from 2 to 10 and extract the average
#silhouette width value from each model: model$silinfo$avg.width Store the resulting vector as sil_width
sil_width <- map_dbl(2:10,  function(k){
  model <- pam(oes, k = k)
  model$silinfo$avg.width
})


#Build a new dataframe sil_df containing the values of k and the vector of average silhouette widths
sil_df <- data.frame(
  k = 2:10,
  sil_width = sil_width
)

## Use the values in sil_df to plot a line plot showing the relationship between k and average
## silhouette width

ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 2:10)

#Great work! It seems that this analysis results in another value of k, this time 7 is the 
#top contender (although 2 comes very close).

#All 3 statements are correct but there is no quantitative way to determine which of these clustering approaches is the 
#right one without futher exploration.

