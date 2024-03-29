
avg_rating = mean(edx$rating)

#bias based on the genres
genre_bias <- edx %>% group_by(genres) %>% summarise(b_genre = mean(rating - avg_rating))
genre_bias_reg <- edx %>% group_by(genres) %>% summarise(b_genre = sum(rating - avg_rating)/(n() + genres_lambda))

#prediction based on avg und genres bias
prediction0 <- validation %>% left_join(genre_bias_reg, by="genres")  %>% mutate(pred = avg_rating + b_genre)


#bias based on the movies
movie_bias <- edx %>% left_join(genre_bias, by="genres") %>% 
      group_by(movieId) %>% summarise(b_movie = mean(rating - avg_rating - b_genre))

movie_bias_reg <- edx %>% left_join(genre_bias_reg, by="genres") %>% 
      group_by(movieId) %>% summarise(b_movie = sum(rating - avg_rating - b_genre) / (n() + movie_lambda))

#prediction based on avg, genre and movie bias
prediction1 <- validation %>% left_join(genre_bias, by="genres") %>% 
      left_join(movie_bias,by="movieId") %>% mutate(pred = avg_rating + b_genre + b_movie)

#prediction based on avg, genre and movie bias regularised
prediction2 <- validation %>% left_join(genre_bias_reg, by="genres") %>% 
      left_join(movie_bias_reg,by="movieId") %>% mutate(pred = avg_rating + b_genre + b_movie)

#bias based on the user
user_bias <- edx %>% left_join(genre_bias, by="genres") %>% 
             left_join(movie_bias, by="movieId") %>%  
             group_by(userId) %>% 
             summarise(b_user = mean(rating - avg_rating - b_genre - b_movie), 
                       avg_user=mean(rating), 
                       sd_userrating=sd(rating))

user_bias_reg <- edx %>% left_join(genre_bias_reg, by="genres") %>% 
                         left_join(movie_bias_reg, by="movieId") %>%  
                         group_by(userId) %>% 
                         summarise(b_user = sum(rating - avg_rating - b_genre - b_movie)/(n()+user_lambda), 
                         avg_user=mean(rating), sd_userrating=sd(rating))


#bias based on the usergenre
#usergenre_bias <- edx %>% left_join(genre_bias, by="genres") %>% 
#                  left_join(movie_bias, by="movieId") %>%  
#                  left_join(user_bias, by="userId") %>%  
#                  group_by(userId, genres) %>% 
#                  summarise(b_usergenre = mean(rating - avg_rating - b_genre - b_movie - b_user))
#
#usergenre_bias_reg <- edx %>% left_join(genre_bias_reg, by="genres") %>% 
#                  left_join(movie_bias_reg, by="movieId") %>%  
#                  left_join(user_bias_reg, by="userId") %>%  
#                  group_by(userId, genres) %>% 
#                  summarise(b_usergenre = sum(rating - avg_rating - b_genre - b_movie - b_user)/(n()+usergenre_lambda))


user_avg_rating <- edx %>% group_by(userId) %>% summarise(avg_rating = mean(rating))

#prediction based on avg, genre, movie and user bias
prediction3 <- validation %>% left_join(genre_bias, by="genres") %>% 
      left_join(movie_bias,by="movieId") %>% left_join(user_bias, by = "userId")  %>% 
      mutate(pred = avg_rating + b_genre + b_movie + b_user)


#prediction based on avg, movie and user bias regularised
prediction4 <- validation %>% left_join(genre_bias_reg, by="genres") %>% 
               left_join(movie_bias_reg,by="movieId") %>% 
               left_join(user_bias_reg, by = "userId")  %>% 
               mutate(pred = avg_rating + b_genre + b_movie + b_user)

#prediction based on avg, movie and user bias regularised with min/max
prediction4b <- prediction4
prediction4b[prediction4b$pred > 5,]$pred = 5 
prediction4b[prediction4b$pred < 0.5,]$pred = 0.5 

#prediction based on avg, genre, movie and user bias regulated with sd
prediction5 <- validation %>% left_join(genre_bias_reg, by="genres") %>% 
      left_join(movie_bias_reg,by="movieId") %>% left_join(user_bias_reg, by = "userId")  %>% 
      mutate(pred = ifelse(sd_userrating < 0.4, avg_user, avg_rating + b_genre + b_movie + b_user))

prediction5b <- prediction5
prediction5b[prediction5b$pred > 5,]$pred = 5 
prediction5b[prediction5b$pred < 0.5,]$pred = 0.5 

#prediction based on avg, genre, movie, and usergenres bias 
#prediction6 <- validation %>% left_join(genre_bias, by="genres") %>% 
#   left_join(movie_bias,by="movieId") %>% 
#   left_join(user_bias,by="userId") %>% 
#   left_join(usergenre_bias, by = c("userId", "genres")) %>% 
#   mutate(pred = avg_rating + b_genre + b_movie + b_user + ifelse(is.na(b_usergenre),0, b_usergenre))
#
#prediction based on avg, movie and user bias regularised
#prediction7 <- validation %>% left_join(genre_bias_reg, by="genres") %>% 
#   left_join(movie_bias_reg,by="movieId") %>% 
#   left_join(usergenre_bias_reg, by = c("userId", "genres")) %>% 
#   mutate(pred = avg_rating + b_genre + b_movie + b_usergenre)
#
#prediction based on avg, movie and usergenre bias regularised with sd und min/max
#prediction7b <- validation %>% left_join(genre_bias_reg, by="genres") %>% 
#   left_join(movie_bias_reg,by="movieId") %>% 
#   left_join(user_bias_reg, by = "userId") %>% 
#   left_join(usergenre_bias_reg, by = c("userId", "genres")) %>% 
#   mutate(pred = ifelse(sd_userrating < 0.4, avg_user, 
#                 avg_rating + b_genre + b_movie + ifelse(is.na(b_usergenre), b_user,b_usergenre)))
#
#prediction7b[prediction7b$pred > 5,]$pred = 5 
#prediction7b[prediction7b$pred < 0.5,]$pred = 0.5 


#performance of the avarage
print("Avarage")
RMSE(validation$rating, avg_rating)
print("RMSE for genre effect reg")
RMSE(validation$rating, prediction0$pred)
print("RMSE for genre and movie effect")
RMSE(validation$rating, prediction1$pred)
print("RMSE for genre and movie effect regulasired")
RMSE(validation$rating, prediction2$pred)
print("RMSE for genre, movie and usert effect")
RMSE(validation$rating, prediction3$pred)
print("RMSE for genre, movie and usert effect regularised")
RMSE(validation$rating, prediction4$pred)
print("RMSE for genre, movie and usert effect regularised mit min&max")
RMSE(validation$rating, prediction4b$pred)
print("RMSE for genre, movie and usert effect regularised with sd")
RMSE(validation$rating, prediction5$pred)
print("RMSE for genre, movie and usert effect regularised with sd and min&max")
RMSE(validation$rating, prediction5b$pred)
#print("RMSE for genre, movie and usergenre effect")
#RMSE(validation$rating, prediction6$pred)
#print("RMSE for genre, movie and usergenre effect regularised")
#RMSE(validation$rating, prediction7b$pred)

