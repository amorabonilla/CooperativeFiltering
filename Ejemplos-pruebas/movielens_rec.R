library(recommenderlab) 
library(ggplot2) 
library(data.table)
library(reshape2)
data("MovieLense")
 
MovieLense
image(sample(MovieLense, 500), main = "Raw ratings")
qplot(getRatings(MovieLense), binwidth = 1,
       main = "Histogram of ratings", xlab = "Rating")
summary(getRatings(MovieLense))
# How about after normalization?
 qplot(getRatings(normalize(MovieLense, method = "Z-score")),
         main = "Histogram of normalized ratings", xlab = "Rating")
 summary(getRatings(normalize(MovieLense, method = "Z-score")))

 # How many movies did people rate on average
 qplot(rowCounts(MovieLense), binwidth = 10, main = "Movies Rated on average", xlab = "# of users",
       ylab = "# of movies rated")

 # What is the mean rating of each movie
 qplot(colMeans(MovieLense), binwidth = .1, main = "Mean rating of Movies", xlab = "Rating",
       ylab = "# of movies")
 # The big spike on 1 suggests that this could also be intepret # In other words, some people don't want to see certain movies #Sameon5andon3.
 # We will give it the binary treatment later
 
 recommenderRegistry$get_entries(dataType = "realRatingMatrix")
 
 
 