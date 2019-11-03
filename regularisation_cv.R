lambdas <- seq(0, 10, 0.25)

#Lambda with cross validation
rmses_for_movie_lambdas <- function(train, test){
  
  RMSE_all = data.frame(lambda = numeric(), rmse = numeric())
  avg <- mean(train$rating)
  movie_sum <- train %>% group_by(movieId) %>% summarise(s=sum(rating-avg), n_i=n())
  
  
  cat("Calculate rmse with movie lambdas\n")
  for (l in lambdas){
    predicted_ratings <- test %>%
      left_join(movie_sum, by='movieId') %>%
      mutate(b_i = s / (n_i + l)) %>%
      mutate(pred = avg + b_i) %>%
      pull(pred)
    
    rmse_akt =RMSE(predicted_ratings, test$rating)
    cat( "Lambda: ", str_pad(l,width = 4,side = "right"), " RMSE: ", rmse_akt, "\n")
    RMSE_all <- bind_rows(RMSE_all, data.frame(lambda = l, rmse = rmse_akt))
  }
  cat("\n RMSE calculated\n")
  cat("\n")
  return(RMSE_all)
}

#Lambda with cross validation
rmses_for_user_lambdas <- function(train, test, movie_lambda){
  
  RMSE_all = data.frame(lambda = numeric(), rmse = numeric())
  avg <- mean(train$rating)
  b_movie <- train %>%
    group_by(movieId) %>%
    summarize(b_movie = sum(rating - avg) / (n() + movie_lambda))
  
  user_sum <- train %>% 
    left_join(b_movie, by="movieId") %>%
    group_by(userId) %>%
    summarize(s = sum(rating - b_movie - avg), n_i= n())
  
  cat("Calculate rmse with user lambdas\n")
  for (l in lambdas){
    predicted_ratings <- test %>%
      left_join(b_movie, by='movieId') %>%
      left_join(user_sum, by='userId') %>%
      mutate(b_u = s / (n_i + l)) %>%
      mutate(pred = avg + b_movie + b_u) %>%
      pull(pred)
    
    rmse_akt =RMSE(predicted_ratings, test$rating)
    cat( "Lambda: ", str_pad(l,width = 4,side = "right"), " RMSE: ", rmse_akt, "\n")
    RMSE_all <- bind_rows(RMSE_all, data.frame(lambda = l, rmse = rmse_akt))
  }
  cat("\n RMSE calculated\n")
  cat("\n")
  return(RMSE_all)
}


cross_validation_movie <- function(trainset, lambdas, cv_n){
  
  data_count = nrow(edx)
  RMSE_all = data.frame(lambda = numeric(), cv = numeric(), rmse = numeric())
  
  trainset_randomised <- trainset[sample(nrow(trainset)),]
  
  for (i in c(1:cv_n)){
    
    part_count = data_count / cv_n
    cat( "Run: ", i, " Test Part: ", str_pad((trunc((i-1) * part_count) + 1),width = 8,side = "left"), " ",  trunc(i * part_count), "\n")
    idx = c(   (trunc((i-1) * part_count) + 1) : trunc(i * part_count) )
    tmp = trainset_randomised[idx,]
    train = trainset_randomised[-idx,]
    test <- tmp %>% 
      semi_join(train, by = "movieId") %>%
      semi_join(train, by = "userId")
    removed <- anti_join(tmp, test, by=c("movieId", "userId"))
    train <- rbind(train, removed)
    RMSE_Lambdas <- rmses_for_movie_lambdas(train, test)
    RMSE_all <- bind_rows(RMSE_all,RMSE_Lambdas %>% mutate(cv = i))
    cat("\n")
  }
  return(RMSE_all)
}

cross_validation_user <- function(trainset, lambdas, cv_n, movie_lambda){
  
  data_count = nrow(edx)
  RMSE_all = data.frame(lambda = numeric(), cv = numeric(), rmse = numeric())
  
  trainset_randomised <- trainset[sample(nrow(trainset)),]
  
  for (i in c(1:cv_n)){
    
    part_count = data_count / cv_n
    cat( "Run: ", i, " Test Part: ", str_pad((trunc((i-1) * part_count) + 1),width = 8,side = "left"), " ",  trunc(i * part_count), "\n")
    idx = c(   (trunc((i-1) * part_count) + 1) : trunc(i * part_count) )
    tmp = trainset_randomised[idx,]
    train = trainset_randomised[-idx,]
    test <- tmp %>% 
      semi_join(train, by = "movieId") %>%
      semi_join(train, by = "userId")
    removed <- anti_join(tmp, test, by=c("movieId", "userId"))
    train <- rbind(train, removed)
    RMSE_Lambdas <- rmses_for_user_lambdas(train, test, movie_lambda)
    RMSE_all <- bind_rows(RMSE_all,RMSE_Lambdas %>% mutate(cv = i))
    cat("\n")
  }
  return(RMSE_all)
}

res <- cross_validation_movie(edx, lambdas,5)
res <- res %>% group_by(lambda) %>% summarise(avg_rmse=mean(rmse))
qplot(main=c('Lambda for movies'), lambdas, res$avg_rmse)
movie_lambda = lambdas[which.min(res$avg_rmse)]
cat("Lambda for movie regulaisration: ", movie_lambda)

res <- cross_validation_user(edx, lambdas,5, movie_lambda)
res <- res %>% group_by(lambda) %>% summarise(avg_rmse=mean(rmse))
qplot(main=c('Lambda for user'), lambdas, res$avg_rmse)
user_lambda = lambdas[which.min(res$avg_rmse)]
cat("Lambda for user regulaisration: ", user_lambda)

