################################################
#
# Capstione Movielens Project
# by
# Andras Gelencser
#
###############################################


##############################################
#Load the necessary packages
##############################################
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate") 
if(!require(stringr)) install.packages("stringr") 


#############################################
#Functions used in the projects
############################################

#function to extract the year information from the movie title
#the movie title contains the year at the end of the title in brackets (eg. "Movietitel (1999)")
#parameters:
#     title: the title of the movie containing the year
#
#return:
#     only the year 

extract_movie_year <- function(title){
  #cut the year from the end of the title
  as.integer(str_sub(title, str_length(title) - 4, str_length(title)-1))
}


#function for cross validation
#parameters:
#     trainset: the train set to use for the cross validation
#     cv_n:     the count of the cross validation
#     FUNC:     the function to call for the actual cross validation train and test set (calculated from the param trainset)
#     ...:      additional parameter necessary for calling the provided function
#
#return:
#     dataframe with the function result for the cross validations (the data frame has cv_n items)

cross_validation <- function(trainset, cv_n, FUNC,...){
  
  #get the count of the data rows on the train set
  data_count = nrow(trainset)
  
  #initialize the data frame for the result
  values_from_cv = data.frame()
  
  #randomise the trainset. 
  #If the train set is ordered (not randomised, like the movielens dataset) the cross validation
  #will not be independent and provide wrong result
  trainset_randomised <- trainset[sample(nrow(trainset)),]
  
  #create the train- and testset for the cross validation
  #we need cv_n run, therefore we use a loop 
  for (i in c(1:cv_n)){
    #evaulate the size of the test set. This will be the 1/cv_n part of the data
    part_count = data_count / cv_n
    
    #select the data from the parameter train set
    #we get the part_count size elements from the parameter train set 
    idx = c(   (trunc((i-1) * part_count) + 1) : trunc(i * part_count) )
    
    #tmp holds the new test set
    tmp = trainset_randomised[idx,]
    #train holds the new test set
    train = trainset_randomised[-idx,]
    
    #we remove the elements from the test set, where either the movie or the user is missing in the train set
    test <- tmp %>% 
      semi_join(train, by = "movieId") %>%
      semi_join(train, by = "userId")
    removed <- anti_join(tmp, test, by=c("movieId", "userId"))
    
    #add the removed elements back to the train set 
    train <- rbind(train, removed)
    
    #call the provided function to the actual train and test set.
    akt_value <- FUNC(train, test,...)
    
    #add the result to the data frame
    #the column 'cv' contains the idx of the cross validation run
    values_from_cv <- bind_rows(values_from_cv, akt_value %>% mutate(cv = i))
  }
  
  #return the results of each cross validation
  return(values_from_cv)
}


#function for calculating the root mean squared error for a prediction
#parameters:
#     actual_rating:      the real rating values (from the test set)
#     predicted_rating:   the predicted ratings (prediction calculated on the test set with the prediction algorithm)
#return:
#     the root mean squared error of the prediction

RMSE <- function(actual_rating, predicted_rating){
  sqrt(mean((actual_rating - predicted_rating)^2))
}


#############################################################
# Create edx set, validation set, and submission file
#############################################################

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

#flag for marking file download
file_downloaded <- FALSE

#download the zip file only if not done yet
if (!dir.exists("ml-10M100K")){
  dl <- tempfile()
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
  unzip(dl, "ml-10M100K/ratings.dat")
  unzip(dl, "ml-10M100K/movies.dat")
  
  #mark the download in the flag
  file_downloaded <- TRUE
}

fl <- file("ml-10M100K/ratings.dat")
ratings <- read.table(text = gsub("::", "\t", readLines(fl)),
                      col.names = c("userId", "movieId", "rating", "timestamp"))
close(fl)

fl <- file("ml-10M100K/movies.dat")
movie_data <- str_split_fixed(readLines(fl), "\\::", 3)
close(fl)

colnames(movie_data) <- c("movieId", "title", "genres")
movie_data <- as.data.frame(movie_data) %>% 
  mutate(movieId = as.numeric(levels(movieId))[movieId],
         title = as.character(title),
         genres = as.factor(genres))

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = ratings$rating, times = 1, p = 0.1, list = FALSE)
edx <- ratings[-test_index,]
temp <- ratings[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation, by=c("userId", "movieId"))
edx <- rbind(edx, removed)

#add the genres data to the test and validation set
edx <- edx %>% left_join(movie_data,by="movieId") %>% select(userId, movieId, rating, genres)
validation <- validation %>% left_join(movie_data,by="movieId") %>% select(userId, movieId, rating, genres)

#remove temp file if download done
if (file_downloaded) rm(dl)

#remove temporary variables
rm(fl, ratings, test_index, temp, removed, file_downloaded)

#############################################################
# Calculate penalty terms for the bias parts
#############################################################

#we look for the optimum penalty values in the range 0 up to 10 (step by 0.25)
lambdas <- seq(0, 10, 0.25)

#function for calculating the RMSE value for different penalty terms for the movie genres bias 
#this function will be used for the k-fold cross validation
rmses_for_genres_lambdas <- function(train, test){
  
  #data frame for the result
  RMSE_all = data.frame(lambda = numeric(), rmse = numeric())
  #overall average for the train set
  avg <- mean(train$rating)
  #the sum of the genre rating average from the overall average
  genre_sum <- train %>% group_by(genres) %>% summarise(s=sum(rating-avg), n_i=n())
  
  #loop for the defined range
  for (l in lambdas){
    #calculate predictions for the actual penalty value
    predicted_ratings <- test %>%
      left_join(genre_sum, by='genres') %>%
      mutate(b_g = s / (n_i + l)) %>%
      mutate(pred = avg + b_g) %>%
      pull(pred)
    
    #calculate the prediction performance for the actual penalty value
    rmse_akt =RMSE(predicted_ratings, test$rating)
    #add the performance result to the result data frame
    RMSE_all <- bind_rows(RMSE_all, data.frame(lambda = l, rmse = rmse_akt))
  }
  return(RMSE_all)
}

#call the penalty calculation with 5-fold cross validation
res <- cross_validation(edx, 5, rmses_for_genres_lambdas)
#select the best penalty term and store it in a variable
res <- res %>% group_by(lambda) %>% summarise(avg_rmse=mean(rmse))
genres_lambda = lambdas[which.min(res$avg_rmse)]

#function for calculating the RMSE value for different penalty terms for the movie bias 
#this function will be used for the k-fold cross validation
#the function logic is the same like for the functino rmse_for_genres_lambda
#please take a look for the comments in those function
rmses_for_movie_lambdas <- function(train, test){
  
  RMSE_all = data.frame(lambda = numeric(), rmse = numeric())
  avg <- mean(train$rating)
  b_genres <- train %>%  
    group_by(genres) %>%
    summarize(b_genres = sum(rating - avg) / (n() + genres_lambda))
  
  movie_sum <- train %>% 
    left_join(b_genres, "genres") %>%
    group_by(movieId) %>%
    summarize(s = sum(rating - b_genres - avg), n_i= n())
  
  for (l in lambdas){
    predicted_ratings <- test %>%
      left_join(b_genres, by='genres') %>%
      left_join(movie_sum, by='movieId') %>%
      mutate(b_i = s / (n_i + l)) %>%
      mutate(pred = avg + b_genres + b_i) %>%
      pull(pred)
    
    rmse_akt =RMSE(predicted_ratings, test$rating)
    RMSE_all <- bind_rows(RMSE_all, data.frame(lambda = l, rmse = rmse_akt))
  }
  return(RMSE_all)
}

#call the penalty calculation with 5-fold cross validation
res <- cross_validation(edx, 5,rmses_for_movie_lambdas)
#select the best penalty term
res <- res %>% group_by(lambda) %>% summarise(avg_rmse=mean(rmse))
#and store it in a variable
movie_lambda = lambdas[which.min(res$avg_rmse)]

#function for calculating the RMSE value for different penalty terms for the user bias 
#this function will be used for the k-fold cross validation
#the function logic is the same like for the functino rmse_for_genres_lambda
#please take a look for the comments in those function
rmses_for_user_lambdas <- function(train, test){
  
  RMSE_all = data.frame(lambda = numeric(), rmse = numeric())
  avg <- mean(train$rating)
  b_genres <- train %>%  
    group_by(genres) %>%
    summarize(b_genres = sum(rating - avg) / (n() + genres_lambda))
  
  b_movie <- train %>%
    left_join(b_genres, by="genres") %>%
    group_by(movieId) %>%
    summarize(b_movie = sum(rating - b_genres - avg) / (n() + movie_lambda))
  
  user_sum <- train %>% 
    left_join(b_genres, by="genres") %>%
    left_join(b_movie, by="movieId") %>%
    group_by(userId) %>%
    summarize(s = sum(rating - b_movie - b_genres - avg), n_i= n())
  
  for (l in lambdas){
    predicted_ratings <- test %>%
      left_join(b_genres, by="genres") %>%
      left_join(b_movie, by='movieId') %>%
      left_join(user_sum, by='userId') %>%
      mutate(b_u = s / (n_i + l)) %>%
      mutate(pred = avg + b_genres + b_movie + b_u) %>%
      pull(pred)
    
    rmse_akt =RMSE(predicted_ratings, test$rating)
    RMSE_all <- bind_rows(RMSE_all, data.frame(lambda = l, rmse = rmse_akt))
  }
  return(RMSE_all)
}

#call the penalty calculation with 5-fold cross validation
res <- cross_validation(edx, 5, rmses_for_user_lambdas)
#select the best penalty term and store it in a variable
res <- res %>% group_by(lambda) %>% summarise(avg_rmse=mean(rmse))
user_lambda = lambdas[which.min(res$avg_rmse)]

#function for calculating the RMSE value for different sd limit values 
#this function will be used for the k-fold cross validation
#the function logic is the same like for the functino rmse_for_genres_lambda
#please take a look for the comments in those function
rmses_for_sd_ranges <- function(train, test, range){
  
  RMSE_all = data.frame(r = numeric(), rmse = numeric())
  avg <- mean(train$rating)
  b_genres <- train %>%  
    group_by(genres) %>%
    summarize(b_genres = sum(rating - avg) / (n() + genres_lambda))
  
  b_movie <- train %>%
    left_join(b_genres, by="genres") %>%
    group_by(movieId) %>%
    summarize(b_movie = sum(rating - b_genres - avg) / (n() + movie_lambda))
  
  b_user <- train %>% 
    left_join(b_movie, by="movieId") %>%
    left_join(b_genres, by="genres") %>%
    group_by(userId) %>%
    summarize(b_user = sum(rating - b_movie - b_genres - avg) / (n() + user_lambda), 
              sd_userrating=sd(rating), avg_user=mean(rating))
  
  for (r in range){
    predicted_ratings <- test %>%
      left_join(b_genres, by="genres") %>%
      left_join(b_movie, by='movieId') %>%
      left_join(b_user, by='userId') %>%
      mutate(pred = ifelse(sd_userrating < r, avg_user, 
                           avg + b_genres + b_movie + b_user)) %>%
      pull(pred)
    
    rmse_akt =RMSE(predicted_ratings, test$rating)
    RMSE_all <- bind_rows(RMSE_all, data.frame(r = r, rmse = rmse_akt))
  }
  return(RMSE_all)
}

#we look for the optimum sd limit value in the range 0 up to 0.5 (step by 0.01)
sd_range <- seq(0, 0.5, 0.01)

#call the penalty calculation with 5-fold cross validation
res <- cross_validation(edx, 5, rmses_for_sd_ranges, sd_range)
#select the best penalty term and store it in a variable
res <- res %>% group_by(r) %>% summarise(avg_rmse=mean(rmse)) 
sd = sd_range[which.min(res$avg_rmse)]


#############################################################
# Implements the prediction model and calculate the
# the prediction for the validation set
#############################################################

avg_rating = mean(edx$rating)

#bias based on the genres
genre_bias_reg <- edx %>% group_by(genres) %>% 
  summarise(b_genre = sum(rating - avg_rating)/(n() + genres_lambda))


#bias based on the movies
movie_bias_reg <- edx %>% left_join(genre_bias_reg, by="genres") %>% 
  group_by(movieId) %>% 
  summarise(b_movie = sum(rating - avg_rating - b_genre) / (n() + movie_lambda))

user_bias_reg <- edx %>% left_join(genre_bias_reg, by="genres") %>% 
  left_join(movie_bias_reg, by="movieId") %>%  group_by(userId) %>% 
  summarise(b_user = sum(rating - avg_rating - b_genre - b_movie)/(n()+user_lambda), 
            avg_user=mean(rating), sd_userrating=sd(rating))

user_avg_rating <- edx %>% group_by(userId) %>% summarise(avg_rating = mean(rating))

#prediction based on avg, genre, movie and user bias regulated
prediction <- validation %>% left_join(genre_bias_reg, by="genres") %>% 
  left_join(movie_bias_reg,by="movieId") %>% 
  left_join(user_bias_reg, by = "userId")  %>% 
  mutate(pred = ifelse(sd_userrating < 0.4, 
                       avg_user, avg_rating + b_genre + b_movie + b_user))

prediction[prediction$pred > 5,]$pred = 5 
prediction[prediction$pred < 0.5,]$pred = 0.5 


#############################################################
# Calculate the prediction model performance and
# write the result to the console
#############################################################
cat("The model prediction RMSE value: ", RMSE(validation$rating, prediction$pred))