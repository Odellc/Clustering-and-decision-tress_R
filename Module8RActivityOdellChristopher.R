# ST 558: Multivariate Analytics
# Module 8 R Activity 
# Christopher Odell


#Set Directory

#setwd("C:/Users/codel/Documents/Masters Program/ST_558_400/week 8")

library(mclust)

###########
# Question 1  #
###########

auto82 <- read.csv('Auto82MPGData.csv')
head(auto82) # First 6 rows of the data


###########
# Question 2  #
###########

pairs(auto82)


###########
# Question 3  #
###########


auto82.distEuc <- dist(auto82[,1:6]) #Euclidean Distance

auto82.hcEuc <- hclust(auto82.distEuc, method="complete") #Complete linkage

plot(auto82.hcEuc, labels=auto82[,7], hang=-1, sub="", main=" Unstandardized Complete Linkage")


###########
# Question 4  #
###########


auto82.sc <- scale(auto82[,1:6]) #Standardize the data

auto82.distEuc.sc <- dist(auto82.sc) #Euclidean Distance

auto82.hcEuc.sc <- hclust(auto82.distEuc.sc, method="complete") #Complete linkage

plot(auto82.hcEuc.sc, labels=auto82[,7], hang=-1, sub="", main="Standardized Complete Linkage")


###########
# Question 5  #
###########

auto82.hsEuc.sc <- hclust(auto82.distEuc.sc, method="single") #Single linkage

plot(auto82.hsEuc.sc, labels=auto82[,7], hang=-1, sub="", main="Standardized Single Linkage")



###########
# Question 6  #
###########


cutree(auto82.hcEuc, k=4)
auto82.hcEuc.cut <- cutree(hclust(auto82.distEuc, method="complete"), k=4) #Complete linkage
pairs(auto82[,-7], col=cutree(auto82.hcEuc,k=4)+1)



###########
# Question 7  #
###########

#k means clustering

auto82.km4 <- kmeans(auto82[,-7], centers=4)

names(auto82.km4)

# Print the membership for each cluster:

auto82[auto82.km4$clus==1,7]
auto82[auto82.km4$clus==2,7]
auto82[auto82.km4$clus==3,7]
auto82[auto82.km4$clus==4,7]

###########
# Question 8  #
###########


table(auto82.km4$clus, cutree(auto82.hcEuc, k=4))

###########
# Question 9  #
###########

# Produce pairs plot with points colored by k-means cluster assignment

pairs(auto82[,-7], col=auto82.km4$clus+1)

###########
# Question 10  #
###########

auto82.mc <- Mclust(auto82[,-7])
auto82.mc
names(auto82.mc)
pairs(auto82[,-7], col=auto82.mc$clas+1)


###########
# Question 11  #
###########


# Fit a model-based clustering with a specified number of clusters
# (in this case, 3 clusters)

auto82.mc4 <- Mclust(auto82[,-7], G=4)

table(auto82.mc4$clas, auto82.km4$clus)

