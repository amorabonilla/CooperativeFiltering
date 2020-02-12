
# https://rstudio-pubs-static.s3.amazonaws.com/288836_388ef70ec6374e348e32fde56f4b8f0e.html

# Library for loading CSV data
library(RCurl)
# Library for data tidying
library(tidyr)
# Library for data structure operations
library(dplyr)
library(knitr)
# Library for plotting
library(ggplot2)
# Library for data display in tabular format
library(DT)
library(pander)


data(MovieLense, package = "recommenderlab")

movielense <- MovieLense
class(movielense)

nrow(movielense)
ncol(movielense)

moviemeta <- MovieLenseMeta

nrow(moviemeta)
ncol(moviemeta)
pander(head(moviemeta), caption = "Sample Movie Meta Data")


#Data Preparation
#Since it is a large dataset,and sparse as well, 
# there might be users that might have hardly rated any movies 
# (may be watched or not) and many a movies which may not be rated 
# to a good extent. To maintain a healthy baseline on which recommendations 
# could be made we will take into consideration those users who have rated at 
# least 20 movies and those movies that are rated b atleast 50 users.



# Extracting data tha comprises of at least 20 ratings per user and 50 ratings
# per movie

movielenseorig <- movielense
movielense <- movielense[rowCounts(movielense) > 20, colCounts(movielense) > 50]
minrowcnt <- min(rowCounts(movielense))
nrow(movielense)

#Forming Train / Test Sets

set.seed(101)
which_train <- sample(x = c(TRUE, FALSE), size = nrow(movielense), replace = TRUE, 
                      prob = c(0.8, 0.2))

recc_data_train <- movielense[which_train, ]
recc_data_test <- movielense[!which_train, ]


# Model : Item-Based Collaborative Filtering
# Item-rBased collaborative filtering algorithm is applied with 
# Cosine similarity to identify 25 neighbouring items wiht similar 
# genre profile and base recommendations on that basis

# Find top 10 recomm movies with Item based collab filter
recc_model1 <- Recommender(data = recc_data_train, method = "IBCF", parameter = list(k = 25, 
                                                                                     method = "Cosine"))
recc_model1

# Applying model to test
num_rec <- 10  # Lets recommend top 5 movies to each of users

recc_predicted1 <- predict(object = recc_model1, newdata = recc_data_test, n = num_rec)
recc_predicted1


# The recc_predicted object contains the recommendations which is topN
# recommendations for each of the users.The slots are: . items: This is the list
# with the indices of the recommended items for each user . itemLabels: This is
# the name of the items . n: This is the number of recommendations . ratings
# predicted

# We try to find the latest among those predicted for each user as most
# recommended.


recdf <- data.frame(user = sort(rep(1:length(recc_predicted1@items), recc_predicted1@n)), 
                    rating = unlist(recc_predicted1@ratings), index = unlist(recc_predicted1@items))

# Recommendations from IBCF model With Period Context Added

recdf$title <- recc_predicted1@itemLabels[recdf$index]
recdf$year <- moviemeta$year[recdf$index]
recdf <- recdf %>% group_by(user) %>% top_n(5, recdf$rating)
# recdf
datatable(recdf[recdf$user %in% (1:10), ])

# Model : User-Based Collaborative Filtering
# UserBased collaborative filtering algorithm is applied with 
# Cosine similarity to identify 25 neiighbouring users wiht similar 
# profile and base recommendations on that basis


# Find top 10 recomm movies with Item based collab filter
recc_model2 <- Recommender(data = recc_data_train, method = "UBCF", parameter = list(k = 25, 
                                                                                     method = "Cosine"))
recc_model2

# Applying model to test
num_rec <- 10  # Lets recommend top 5 movies to each of users

recc_predicted2 <- predict(object = recc_model2, newdata = recc_data_test, n = num_rec)
recc_predicted2

recdfub <- data.frame(user = sort(rep(1:length(recc_predicted2@items), recc_predicted2@n)), 
                      rating = unlist(recc_predicted2@ratings), index = unlist(recc_predicted2@items))

# Recommendations from UBCF model With Period Context Added

recdfubnew <- recdfub[with(recdfub, order(recdfub$user, -recdfub$year, -round(recdfub$rating))), 
                      c(1, 2, 5, 4)]
recdfubnew <- recdfubnew %>% group_by(user) %>% top_n(5, recdfubnew$year)
datatable(recdfubnew[recdfubnew$user %in% (1:10), ])

