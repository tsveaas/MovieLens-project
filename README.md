# MovieLens-project
Data Science: Capstone (HarvardX PH125.9x)

In this project we develop a recommendation system that predicts the rating a specific user will give a specific movie in terms of stars. The star rating system ranges from the lowest rating possible of 0.5 stars meaning the user would hate the movie to the highest possible rating of 5 stars meaning the user would love the movie.

The data used in this project is the MovieLens 10m movie ratings data set which can be found here https://grouplens.org/datasets/movielens/10m/ . The data set contains 10 million movie ratings and 100,000 tag applications applied to 10,000 movies by 72,000 users. Released in 2009.

The objective of this project is to train a model that achieves a Root Mean Squared Error (RMSE) below 0.86499 when predicting the movie ratings on the validation set. For this purpose a linear model was developed which attempts the convey the movie, user and genre specific effects on the rating. These effects were also regularized to account for the movies, users and genres with small sample sizes. Lastly matrix factorization was added on to account for the remaining variability in ratings . This model achieved an RMSE of .7946383 which fell well below the target value.
