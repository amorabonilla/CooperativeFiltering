

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


# movies<- data.frame(scale(movielense, center=T, scale=T))
movies <- as(movielense, "data.frame")
dim(movies)


# We would need the item as numeric , hence doing factors to uniquely identify
# item of movie by assigning as itemid
movies <- transform(movies, itemid = as.numeric(factor(item)))
colnames(movies) <- c("user", "item", "rating", "itemid")
dim(movies)


# View(movies)

# Forming the movieid/moviename mapping table for later reference
moviename <- movies %>% select(item, itemid)

# Since ths is made form the long format useritem table , we have duplicate rows.
# Deleting duplicate rows, taking only distince
moviename <- moviename %>% distinct(item, itemid)
# Verify the movie
dim(moviename)


# We further select the data on basis on itemid , removing the named movie column
moviesdata <- movies %>% select(-item)
# View(moviesdata) View(moviename)

# We need to do this as Spark which is later conencted to for ALS factorization
# requires the user /item column be numeric
moviesdata$user <- as.numeric(moviesdata$user)
moviesdata$itemid <- as.numeric(moviesdata$itemid)


# Now arrangeing by user and to wide format
moviesdatawide <- reshape(moviesdata, idvar = "user", timevar = "itemid", direction = "wide") %>% 
  arrange(user)
dim(moviesdatawide)


# View(moviesdatawide)


# Store the userids as rownames for later use
rownames(moviesdatawide) <- moviesdatawide$user
moviesdatawide <- moviesdatawide %>% select(-user)

# Store the moview ids for later use
library(stringr)
colnames(moviesdatawide) <- str_replace(colnames(moviesdatawide), "rating.", "")


# Now we have both rating tables moviesdata in long form and moviesdatawide in
# wide form

# Since this is in long format, as needed but is mixed, we would prep the data in
# such a way so as to identify the movies with a numerical value . This is
# because in order to use Spark library functions we would need to use movie
# lense data as dataframe and with the user and item as numeric columns



mometa <- as(moviemeta, "data.frame")



## look at the first few ratings of the first user
head(as(MovieLense[1,], "list")[[1]])

## visualize part of the matrix
image(MovieLense[1:100,1:100])

## number of ratings per user
hist(rowCounts(MovieLense))

## number of ratings per movie
hist(colCounts(MovieLense))
## mean rating (averaged over users)
mean(rowMeans(MovieLense))
## available movie meta information
head(MovieLenseMeta)


