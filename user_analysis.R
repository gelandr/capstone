#calculate rating count pro user
rating_summary_pro_user <- edx %>% group_by(userId) %>% summarise(n = n(), avgRating=mean(rating), sdRating=sd(rating))
max(rating_summary_pro_user$n)
min(rating_summary_pro_user$n)

rating_summary_pro_user %>% ggplot(aes(n)) + geom_histogram(color="black", binwidth = 5) +
  ggtitle("User rating count") +
  xlab("Rating count") + ylab("count")

rating_summary_pro_user %>% filter(n <= 500) %>% ggplot(aes(n)) + geom_histogram(color="black", binwidth = 5) +
  ggtitle("Movie rating count (count <= 500)") +
  xlab("Rating count") + ylab("count")


rating_summary_pro_user %>% filter(n > 500) %>% ggplot(aes(n)) + geom_histogram(color="black", binwidth = 5) +
  ggtitle("Movie rating count (count > 500)") +
  xlab("Rating count") + ylab("count")
  
rating_summary_pro_user %>% ggplot(aes(sdRating)) + geom_histogram(color="black", binwidth = 0.1) +
  ggtitle("User rating standard deviation") +
  xlab("Standard deviation") + ylab("count")

top_movie <- edx %>% 
        left_join(rating_summary_pro_user, "userId") %>%
        filter(movieId == rating_summary_pro_movie[which.max(rating_summary_pro_movie$n),]$movieId) 

top_movie %>% left_join(movie_data, by="movieId") %>% ggplot(aes(rating)) + geom_histogram(color="black", binwidth = 0.5) +
      ggtitle(paste("Rating of the movie ' ", title, "'"))

#calculate average ratings pro user. We will use it later
avg_ratings_pro_user <- edx %>% group_by(userId) %>% summarise(rating = mean(rating), n=n())

user_genre_ratings = rating_summary_pro_user %>% filter(n > 500) %>% inner_join(edx,by="userId") %>%
    group_by(userId, genres) %>% summarise(avg=mean(rating), n=n())

avg_ratings_pro_user %>% arrange(desc(n))

#plot the avg rating pro genre for userid 59269
user_genre_ratings %>% filter(userId==59269 & n > 20) %>% mutate(genres = reorder(genres, avg)) %>%  
    ggplot(aes(genres,avg)) + geom_point() +
    ggtitle("genres avg rating for userId 59269") +
    xlab("Genres") + ylab("Avg rating") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

user_genre_ratings %>% filter(userId==59269 & n > 20) %>% arrange(desc(avg))
user_genre_ratings %>% filter(userId==59269 & n > 20) %>% arrange(avg)

#plot the avg rating pro genre for userid 67385
user_genre_ratings %>% filter(userId==67385 & n > 20) %>% mutate(genres = reorder(genres, avg)) %>% 
  ggplot(aes(genres,avg)) + geom_point() +
  ggtitle("genres avg rating for userId 67385") +
  xlab("Genres") + ylab("Avg rating") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


user_genre_ratings %>% filter(userId==67385 & n > 20) %>% arrange(desc(avg))
user_genre_ratings %>% filter(userId==67385 & n > 20) %>% arrange(avg)