seattle.df <- read.csv("seattle_listings.csv")

#Removing dollar sign from rows
seattle.df$price = as.numeric(gsub("\\$", "", seattle.df$price))
#Finding out which row contains an NA and needs to be removed
#which (is.na(seattle.df$price))
#[1] 3121

#na.omit: Drop out any rows with missing values anywhere in them and forgets them forever.
#na.exclude: Drop out rows with missing values, but keeps track of where they were 
#  (so that when you make predictions, for example, you end up with a vector whose length is that of the original response.)
na.omit(seattle.df$price)

#Trying to replace blanks with 0 
seattle.df$cleaning_fee = as.numeric(gsub("\\$", "", seattle.df$cleaning_fee))
seattle.df[["cleaning_fee"]][is.na(seattle.df[["cleaning_fee"]])] <- 0

#Chossing the predictors
selected.var <- c(11,12,13,15,16)
selected.df <- seattle.df[, selected.var]


#Checking correlations directly, first individually plots
library(ggplot2) 
ggplot(data = seattle.df) + geom_point(aes(x = cleaning_fee, y = price)) 
ggplot(data = seattle.df) + geom_point(aes(x = guests_included, y = price)) 
ggplot(data = seattle.df) + geom_point(aes(x = minimum_nights, y = price)) 
ggplot(data = seattle.df) + geom_point(aes(x = maximum_nights, y = price)) 

ggplot(data = seattle.df) + geom_point(aes(x = guests_included, y = price, color = cleaning_fee))

#Using correlation function, but something is wrong
#cor(seattle.df$price, seattle.df$price)
cor(seattle.df$price, seattle.df$cleaning_fee, use = "complete.obs")
cor(seattle.df$price, seattle.df$guests_included, use = "complete.obs")
cor(seattle.df$price, seattle.df$minimum_nights, use = "complete.obs")
cor(seattle.df$price, seattle.df$maximum_nights, use = "complete.obs")

# Multiple linear regression
selected.var <- c(11,12,13)
linreg.df <- seattle.df[, selected.var]     

set.seed(1)  # set seed for reproducing the partition
# Random sample indexes
train.index <- sample(1:nrow(linreg.df), nrow(linreg.df)*0.7)  

# Build training and validation set by indexing
train.df <- linreg.df[train.index, ]
valid.df <- linreg.df[-train.index, ]

# use lm() to run a linear regression of Spending on all 6 predictors in the training set
linreg.lm <- lm(price~., data = train.df)

#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999, digits=3)
summary(linreg.lm)

# use predict() to make predictions on a new set. 
linreg.lm.pred <- predict(linreg.lm, valid.df)

library(forecast)
accuracy(linreg.lm.pred, valid.df$price)


###consolidation, k-means
library(caret)

kmeans.df <- c(11, 8)
new.df <- seattle.df[, kmeans.df]

norm.values <- preProcess(new.df, method=c("center", "scale"))
# we perform the transformation/normalization
seattle.df.norm <- predict(norm.values, new.df)

# set seed for reproducibility
set.seed(1)
km <- kmeans(na.omit(seattle.df.norm), 6)

km$cluster
km$centers

# Kmeans clustre analysis
plotcluster(na.omit(seattle.df.norm), km$cluster)

clusplot(na.omit(seattle.df.norm), km$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)



##now with all the predictors
kmeans.df <- c(11, 8, 12, 13, 7, 9, 6, 19)
new.df <- seattle.df[, kmeans.df]

norm.values <- preProcess(new.df, method=c("center", "scale"))
# we perform the transformation/normalization
seattle.df.norm <- predict(norm.values, new.df)

# set seed for reproducibility
set.seed(1)
km <- kmeans(na.omit(seattle.df.norm), 5)

km$cluster
km$centers

# Kmeans clustre analysis
plotcluster(na.omit(seattle.df.norm), km$cluster)

clusplot(na.omit(seattle.df.norm), km$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)


plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0, 7))

# label x-axes
axis(1, at = c(1:8), labels = names(seattle.df.norm))

# plot centroids
for (i in c(1:6))
  lines(km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3,5),
                                                       "black", "dark grey"))

# name clusters
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:5)))


##now for the linear regression with all the predictors

seattle.df$cancellation_policy = as.numeric(seattle.df$cancellation_policy)
kmeans.df <- c(11, 8, 12, 13, 7, 9, 6, 19)
linreg.df <- seattle.df[, kmeans.df]     

set.seed(1)  # set seed for reproducing the partition
# Random sample indexes
train.index <- sample(1:nrow(linreg.df), nrow(linreg.df)*0.7)  

# Build training and validation set by indexing
train.df <- linreg.df[train.index, ]
valid.df <- linreg.df[-train.index, ]

# use lm() to run a linear regression of Spending on all 6 predictors in the training set
linreg.lm <- lm(price~., data = train.df)

#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999, digits=3)
summary(linreg.lm)

# use predict() to make predictions on a new set. 
linreg.lm.pred <- predict(linreg.lm, valid.df)
summary(linreg.lm.pred)


library(forecast)
accuracy(linreg.lm.pred, valid.df$price)



###heatmap testing
# 1: native palette from R
kmeans.df <- c(11, 8, 13, 7)
linreg.df <- seattle.df[, kmeans.df]

heatmap(as.matrix(linreg.df), scale="column", col = heat.colors(256))


#trying to change the matrix rows
kmeans.df <- c(11, 8)
atest.df <- seattle.df[, kmeans.df]

kmeans.df <- c(13, 7)
btest.df <- seattle.df[, kmeans.df]

new <- as.matrix(na.omit(atest.df, btest.df))
heatmap(new, scale="column", col = heat.colors(256))

