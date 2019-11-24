lambdas <- seq(0, 10, 0.25)

#Movie genres lambda with cross validation
rmses_for_genres_lambdas <- function(train, test){
  
  RMSE_all = data.frame(lambda = numeric(), rmse = numeric())
  avg <- mean(train$rating)
  genre_sum <- train %>% group_by(genres) %>% summarise(s=sum(rating-avg), n_i=n())
  
  
  for (l in lambdas){
    predicted_ratings <- test %>%
      left_join(genre_sum, by='genres') %>%
      mutate(b_g = s / (n_i + l)) %>%
      mutate(pred = avg + b_g) %>%
      pull(pred)
    
    rmse_akt =RMSE(predicted_ratings, test$rating)
    RMSE_all <- bind_rows(RMSE_all, data.frame(lambda = l, rmse = rmse_akt))
  }
  return(RMSE_all)
}

#Movie lambda with cross validation
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

#User lambda with cross validation
rmses_for_user_lambdas <- function(train, test){
  
  RMSE_all = data.frame(lambda = numeric(), rmse = numeric())
  avg <- mean(train$rating)
  b_genres <- train %>%  
    group_by(genres) %>%
    summarize(b_genres = sum(rating - avg) / (n() + genres_lambda))
  
  b_movie <- train %>%
    left_join(b_genres, by="genres") %>%
    group_by(movieId) %>%
    summarize(b_movie = sum(rating - avg - b_genres) / (n() + movie_lambda))
  
  user_sum <- train %>% 
    left_join(b_genres, by="genres") %>%
    left_join(b_movie, by="movieId") %>%
    group_by(userId) %>%
    summarize(s = sum(rating - avg - b_genres - b_movie), n_i= n())
  
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

#Usergenre lambda with cross validation
rmses_for_usergenre_lambdas <- function(train, test){
  
  RMSE_all = data.frame(lambda = numeric(), rmse = numeric())
  avg <- mean(train$rating)
  b_genres <- train %>%  
    group_by(genres) %>%
    summarize(b_genres = sum(rating - avg) / (n() + genres_lambda))
  
  b_movie <- train %>%
    left_join(b_genres, by="genres") %>%
    group_by(movieId) %>%
    summarize(b_movie = sum(rating - avg - b_genres) / (n() + movie_lambda))
  
  b_user <- train %>% 
    left_join(b_genres, by="genres") %>% 
    left_join(b_movie, by="movieId") %>%  
    group_by(userId) %>% 
    summarise(b_user = sum(rating - avg - b_genres - b_movie)/(n()+user_lambda))
  
  usergenre_sum <- train %>% 
    left_join(b_genres, by="genres") %>%
    left_join(b_movie, by="movieId") %>%
    group_by(userId, genres) %>%
    summarize(s = sum(rating - avg - b_genres - b_movie - b_user), n_i= n())
  
  for (l in lambdas){
    predicted_ratings <- test %>%
      left_join(b_genres, by="genres") %>%
      left_join(b_movie, by='movieId') %>%
      left_join(b_user, by='userId') %>%
      left_join(usergenre_sum, by=c('userId', 'genres')) %>%
      mutate(b_ug = avg - b_genres - b_movie - b_user - s / (n_i + l)) %>%
      mutate(pred = avg + b_genres + b_movie + b_user + b_ug) %>%
      pull(pred)
    
    rmse_akt =RMSE(predicted_ratings, test$rating)
    RMSE_all <- bind_rows(RMSE_all, data.frame(lambda = l, rmse = rmse_akt))
  }
  return(RMSE_all)
}

#SD range with cross validation
rmses_for_sd_ranges <- function(train, test, range){
  
  RMSE_all = data.frame(lambda = numeric(), rmse = numeric())
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
    summarize(b_user = sum(rating - avg - b_genres - b_movie) / (n() + user_lambda), sd_userrating=sd(rating), avg_user=mean(rating))
  
  for (r in range){
    predicted_ratings <- test %>%
      left_join(b_genres, by="genres") %>%
      left_join(b_movie, by='movieId') %>%
      left_join(b_user, by='userId') %>%
      mutate(pred = ifelse(sd_userrating < r, avg_user, avg + b_genres + b_movie + b_user)) %>%
      pull(pred)
    
    rmse_akt =RMSE(predicted_ratings, test$rating)
    RMSE_all <- bind_rows(RMSE_all, data.frame(r = r, rmse = rmse_akt))
  }
  return(RMSE_all)
}

res <- cross_validation(edx, 5, rmses_for_genres_lambdas)
res <- res %>% group_by(lambda) %>% summarise(avg_rmse=mean(rmse))
qplot(main=c('Lambda for genres', 'RMSE', 'Lambda'), lambdas, res$avg_rmse)
genres_lambda = lambdas[which.min(res$avg_rmse)]
cat("Lambda for genres regulaisration: ", genres_lambda)


res <- cross_validation(edx, 5,rmses_for_movie_lambdas)
res <- res %>% group_by(lambda) %>% summarise(avg_rmse=mean(rmse))
qplot(main=c('Lambda for movies', 'RMSE', 'Lambda'), lambdas, res$avg_rmse)
movie_lambda = lambdas[which.min(res$avg_rmse)]
cat("Lambda for movie regulaisration: ", movie_lambda)


res <- cross_validation(edx, 5, rmses_for_user_lambdas)
res <- res %>% group_by(lambda) %>% summarise(avg_rmse=mean(rmse))
qplot(main=c('Lambda for user', 'RMSE', 'Lambda'), lambdas, res$avg_rmse)
user_lambda = lambdas[which.min(res$avg_rmse)]
cat("Lambda for user regulaisration: ", user_lambda)


#res <- cross_validation(edx, 5, rmses_for_usergenre_lambdas)
#res <- res %>% group_by(lambda) %>% summarise(avg_rmse=mean(rmse))
#qplot(main=c('Lambda for usergenre', 'RMSE', 'Lambda'), lambdas, res$avg_rmse)
#usergenre_lambda = lambdas[which.min(res$avg_rmse)]
#cat("Lambda for usergenre regulaisration: ", usergenre_lambda)


sd_range <- seq(0, 0.5, 0.01)
res <- cross_validation(edx, 5, rmses_for_sd_ranges, sd_range)
res <- res %>% group_by(r) %>% summarise(avg_rmse=mean(rmse))
qplot(main=c('Range for SD', 'RMSE', 'Range'), sd_range, res$avg_rmse)
sd = sd_range[which.min(res$avg_rmse)]
cat("SD range: ", sd)
