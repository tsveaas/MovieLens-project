##########################################
# Capstone Project
##########################################

# RMarkdown settings
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.width=5, fig.height=2.5, fig.align="center") 
knitr::opts_chunk$set(fig.pos = "H", out.extra = "")

# required libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(knitr)
library(magrittr)

#Load required packages used in project
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(scales)
library(recosystem)

#############################################################
# Create edx set, validation set (final hold-out test set)
#############################################################


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

#####################
# Explore the data
#####################

# Here we explore the edx data set

# Dimensions of the edx data set
dim(edx)

# head of edx set
head(edx)

# The number of unique users and number of unique movies
edx %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

# The number of ratings and the range
edx %>%
  summarize(distinct_ratings = n_distinct(rating),
            rating_range = range(rating))

# Number of unique genres
edx %>%
  summarize(distinct_genres = n_distinct(genres))

# Explore the genres
head(edx$genres)

# The time span in which the ratings have taken place
# Convert the time stamp and find the range
edx <- edx %>% mutate(date = as_datetime(timestamp))
year(max(edx$date)) - year(min(edx$date))

################################
# Analyze and Visualize the data
################################

# Movie Data
# Plot a histogram of rating distribution of movies
edx %>% group_by(movieId) %>%
  summarise(mean_rating = mean(rating)) %>% 
  ggplot(aes(mean_rating)) +
  geom_histogram(bins = 20, color = "white") +
  ggtitle("Mean Ratings of Movies") +
  xlab("Mean Rating") +
  ylab("Number of Movies") +
  theme_stata()
  

# Plot a histogram of the number of ratings per movie
edx %>% count(movieId) %>% ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "white") +
  scale_x_log10() + 
  ggtitle("Distribution of ratings per movie") +
  xlab("Number of Ratings") +
  ylab("Number of Movies") +
  theme_stata()

# Create a table with the number of ratings for the movies with the top 5 and bottom 5 mean rating
edx %>% group_by(title) %>% 
  summarise(mean_rating = mean(rating), count = n()) %>%
  slice_max(mean_rating, n = 5, with_ties = FALSE)

edx %>% group_by(title) %>% 
  summarise(mean_rating = mean(rating), count = n()) %>%
  slice_min(mean_rating, n = 5, with_ties = FALSE)

# User Data
# Plot a histogram of average rating from different users
edx %>% group_by(userId) %>%
  summarise(mean_rating = mean(rating)) %>% 
  ggplot(aes(mean_rating)) +
  geom_histogram(bins = 20, color = "white") +
  ggtitle("Mean Ratings from users") +
  xlab("Mean Rating") +
  ylab("Number of Users") +
  theme_stata()

# Plot a histogram of the number of ratings per user
edx %>% count(userId) %>% ggplot(aes(n)) +
  geom_histogram(bins = 20, color = "white") +
  scale_x_log10() +
  ggtitle("Distribution of Ratings per User") +
  xlab("Number of Ratings") +
  ylab("Number of Users") +
  theme_stata()

# Create a table with the number of ratings for the users with the top 5 and bottom 5 mean rating
edx %>% group_by(userId) %>% 
  summarise(mean_rating = mean(rating), count = n()) %>%
  slice_max(mean_rating, n = 5, with_ties = FALSE)

edx %>% group_by(userId) %>% 
  summarise(mean_rating = mean(rating), count = n()) %>%
  slice_min(mean_rating, n = 5, with_ties = FALSE)

# Date data
# Trend in average rating per week over time 
edx %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarise(mean_rating = mean(rating)) %>%
  ggplot(aes(date,mean_rating)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average Rating per over Time") +
  theme_stata()

# Genres data

# Plot a histogram of rating distribution of genres
edx %>% group_by(genres) %>%
  summarise(mean_rating = mean(rating)) %>% 
  ggplot(aes(mean_rating)) +
  geom_histogram(bins = 20, color = "white") +
  ggtitle("Mean Ratings of Genres") +
  xlab("Mean Rating") +
  ylab("Number of Genres") +
  theme_stata()

# Plot a histogram of the number of ratings per Genre
edx %>% count(movieId) %>% ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "white") +
  scale_x_log10() + 
  ggtitle("Distribution of ratings per Genre") +
  xlab("Number of Ratings") +
  ylab("Number of Genres") +
  theme_stata()

# Create a table with the number of ratings for the genres with the top 5 and bottom 5 mean rating
edx %>% group_by(genres) %>% 
  summarise(mean_rating = mean(rating), count = n()) %>%
  slice_max(mean_rating, n = 5, with_ties = FALSE)

edx %>% group_by(genres) %>% 
  summarise(mean_rating = mean(rating), count = n()) %>%
  slice_min(mean_rating, n = 5, with_ties = FALSE)

################################
# Creating the Model
################################

# Create train set and test set for developing our model

# Test set set will be 10% of edx data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure movieId and userId in test set are also in train set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

# RMSE function to test our models
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Just the mean
# Start model building by using the mean rating of all movies

mu <- mean(train_set$rating)

# Calculate RMSE and put into rmse results data frame

mu_rmse <- RMSE(test_set$rating, mu)
rmse_results <- data.frame(model = "Just mean", RMSE = mu_rmse)


# Movie Bias
# Add in a bias for average rating of each movie using regularization as well
# find the best lambda value for regularization equation
lambdas <- seq(0, 10, 0.25)

# find best lambda value for movie bias
rmses <- sapply(lambdas, function(l){
 
  # creating bias for average movie rating
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu)/ (n() + l))
  
  # predict the ratings on the test_set
  predicted_ratings <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  
  return(RMSE(predicted_ratings, test_set$rating))
})

# get the lambda value which returns the lowest rmse
lambda_b_i <- lambdas[which.min(rmses)]

# create movie bias using the lambda chosen
b_i <- train_set %>%
  group_by(movieId) %>%
  summarise(b_i = sum(rating - mu)/ (n() + lambda_b_i))

# add movie bias to train set
train_set <- train_set %>%
  left_join(b_i, by = "movieId")

# find rmse with movie bias added
# predict the ratings on the test_set
rmse_movie <- test_set %>%
  left_join(b_i, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  summarize(rmse = RMSE(pred, rating)) %>%
  pull(rmse)

# add the rmse of the of the model which includes movie bias to the table
rmse_results <- rmse_results %>%
  add_row(model = "Movie bias", RMSE = rmse_movie)

# User Bias
# find best lambda value for user bias
rmses <- sapply(lambdas, function(l){
 
   # create bias for user rating
  b_u <- train_set %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n() + l))
  
  # predict the ratings on the test_set
  predicted_ratings <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(predicted_ratings, test_set$rating))
})

lambda_b_u <- lambdas[which.min(rmses)]

# create user bias using the lambda chosen
b_u <- train_set %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n() + lambda_b_u))

# add user bias to train set
train_set <- train_set %>%
  left_join(b_u, by = "userId")

# find rmse with both user bias and movie bias added
# predict the ratings on the test_set
rmse_movie_user <- test_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  summarize(rmse = RMSE(pred, rating)) %>%
  pull(rmse)

# add the rmse of the of the model which includes movie and user bias to the table
rmse_results <- rmse_results %>%
  add_row(model = "Movie + User bias", RMSE = rmse_movie_user)

# Genres bias
# find best lambda value for genres bias
rmses <- sapply(lambdas, function(l){
  
  # create bias for user rating
  b_g <- train_set %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - b_u - mu)/(n() + l))
  
  # predict the ratings on the test_set
  predicted_ratings <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    .$pred
  
  return(RMSE(predicted_ratings, test_set$rating))
})

lambda_b_g <- lambdas[which.min(rmses)]

# create genres bias
b_g <- train_set %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_i - b_u - mu)/(n() + lambda_b_g))

# add genres bias to train set
train_set <- train_set %>%
  left_join(b_g, by = "genres")

# find rmse with user bias, movie bias and genres added
# predict the ratings on the test_set
rmse_movie_user_genres <- test_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  summarize(rmse = RMSE(pred, rating)) %>%
  pull(rmse)

# add the rmse of the of the model which includes movie and user bias to the table
rmse_results <- rmse_results %>%
  add_row(model = "Movie + User + Genres bias", RMSE = rmse_movie_user_genres)


# Compare the different bias'
 train_set %>% select(b_i, b_u, b_g) %>%
   gather(key = "bias" , value = "value") %>%
   ggplot(aes(value)) +
   geom_histogram(bins=30) +
   xlim(-2,2) +
   facet_grid(~factor(bias, levels = c('b_i', 'b_u', 'b_g'))) +
   scale_y_continuous(labels = comma) +
   xlab("Bias Amount") +
   ylab("Count")
   
# Matrix factorisation
# Center the data to leave only the remaining variation by removing mu, movie bias and user bias'
train_set <- train_set %>% mutate(residual = rating - mu - b_i - b_u)

# Create recosystem
r <- Reco()
train_reco <- data_memory(user_index = train_set$userId, item_index = train_set$movieId,
                          rating = train_set$residual, index1 = TRUE)

# Tune the recosystem model
# This part includes multithreading using 6 thread son my PC
# Change "nthread" if you do not have enough
reco_tune <- r$tune(train_reco, opts = list(dim      = c(10L, 20L),
                                            costp_l1 = 0,
                                            costp_l2 = c(0.01, 0.1),
                                            costq_l1 = 0,
                                            costq_l2 = c(0.01, 0.1),
                                            lrate    = c(0.01, 0.1),
                                            niter = 10,
                                            nthreads = 6,
                                            verbose = TRUE))

# Train the recosystem model using optimal tune
r$train(train_reco, opts = c(reco_tune$min, niter = 50)) 

# Export model
reco_mat <- r$output(out_P = out_memory(), out_Q = out_memory())

# Use recosystem model to predict test set
test_reco <- data_memory(user_index = test_set$userId, item_index = test_set$movieId,
                         index1 = TRUE)
test_residual_pred <- r$predict(test_reco, out_memory())
rmse_mat_fact <- test_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u + test_residual_pred) %>%
  summarize(rmse = RMSE(pred, rating)) %>%
  pull(rmse)

# Add the rmse of the of the model which includes matrix factorization
rmse_results <- rmse_results %>%
  add_row(model = "Matrix Factorisation", RMSE = rmse_mat_fact)

############################
## Results
############################

# Use final model on the validation set
valid_reco <- data_memory(user_index = validation$userId, item_index = validation$movieId,
                          index1 = TRUE)
valid_residual_pred <- r$predict(valid_reco, out_memory())
rmse_valid <- validation %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u + valid_residual_pred) %>%
  summarize(rmse = RMSE(pred, rating)) %>%
  pull(rmse)
rmse_valid