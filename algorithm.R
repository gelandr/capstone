#function for evaulationg the performance of the algorithm
RMSE <- function(actual_rating, predicted_rating){
  sqrt(mean((actual_rating - predicted_rating)^2))
}

avg_rating = mean(edx$rating)

#movie_lambda = 2.25
#user_lambda = 5

#bias based on the movies
movie_bias <- edx %>% group_by(movieId) %>% summarise(b_movie = mean(rating - avg_rating))
movie_bias_reg <- edx %>% group_by(movieId) %>% summarise(b_movie = sum(rating - avg_rating)/(n() + movie_lambda))

#prediction based on avg and movie bias
prediction1 <- validation %>% left_join(movie_bias,by="movieId") %>% mutate(pred = avg_rating + b_movie)
#prediction based on avg and movie bias regulated
prediction2 <- validation %>% left_join(movie_bias_reg,by="movieId") %>% mutate(pred = avg_rating + b_movie)

#performance
user_bias <- edx %>% left_join(movie_bias, by="movieId") %>%  group_by(userId) %>% summarise(b_user = mean(rating - avg_rating - b_movie), avg_user=mean(rating), sd_userrating=sd(rating))
user_bias_reg <- edx %>% left_join(movie_bias, by="movieId") %>%  group_by(userId) %>% summarise(b_user = sum(rating - avg_rating - b_movie)/(n()+user_lambda), avg_user=mean(rating), sd_userrating=sd(rating))

user_avg_rating <- edx %>% group_by(userId) %>% summarise(avg_rating = mean(rating))

#prediction based on avg, movie and user bias
prediction3 <- validation %>% left_join(movie_bias_reg,by="movieId") %>% left_join(user_bias, by = "userId")  %>% mutate(pred = avg_rating + b_movie + b_user)

#prediction based on avg, movie and user bias regulated
prediction4 <- validation %>% left_join(movie_bias_reg,by="movieId") %>% left_join(user_bias_reg, by = "userId")  %>% mutate(pred = avg_rating + b_movie + b_user)
prediction4[prediction4$pred > 5,]$pred = 5 
prediction4[prediction4$pred < 0.5,]$pred = 0.5 
#prediction4$pred = round(prediction4$pred,1)

#prediction based on avg, movie and user bias regulated
prediction5 <- validation %>% left_join(movie_bias_reg,by="movieId") %>% left_join(user_bias_reg, by = "userId")  %>% mutate(pred = ifelse(sd_userrating < 0.1, avg_user, avg_rating + b_movie + b_user))
prediction5[prediction5$pred > 5,]$pred = 5 
prediction5[prediction5$pred < 0.5,]$pred = 0.5 

#prediction based on user avg, movie bias and user bias
prediction6 <- validation %>% left_join(movie_bias_reg,by="movieId") %>% left_join(user_avg_rating, by = "userId")  %>% mutate(pred = avg_rating + b_movie)
prediction6[prediction6$pred > 5,]$pred = 5 
prediction6[prediction6$pred < 0.5,]$pred = 0.5 


#performance of the avarage
print("Avarage")
RMSE(validation$rating, avg_rating)
print("RMSE for movie effect")
RMSE(validation$rating, prediction1$pred)
print("RMSE for movie effect regulasired")
RMSE(validation$rating, prediction2$pred)
print("RMSE for movie and usert effect")
RMSE(validation$rating, prediction3$pred)
print("RMSE for movie effect regularised and usert effect")
RMSE(validation$rating, prediction4$pred)
print("RMSE for movie effect regularised, usert effect and genre effect")
RMSE(validation$rating, prediction5$pred)
#print("RMSE for usert avg, movie bias")
#RMSE(validation$rating, prediction6$pred)

