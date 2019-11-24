#calculate rating count pro user
rating_summary_pro_user <- edx %>% group_by(userId) %>% summarise(n = n(), avgRating=mean(rating), sdRating=sd(rating))
max(rating_summary_pro_user$n)
min(rating_summary_pro_user$n)

head(rating_summary_pro_user %>% arrange(desc(n)),2)$userId

rating_summary_pro_user %>% ggplot(aes(n)) + geom_histogram(color="black", binwidth = 5) +
  ggtitle("User rating count") +
  xlab("Rating count") + ylab("count")

rating_summary_pro_user %>% filter(n <= 500) %>% ggplot(aes(n)) + geom_histogram(color="black", binwidth = 5) +
  ggtitle("Movie rating count (count <= 500)") +
  xlab("Rating count") + ylab("count")


rating_summary_pro_user %>% filter(n >= 1000) %>% ggplot(aes(n)) + geom_histogram(color="black", binwidth = 100) +
  ggtitle("Movie rating count (count >= 1000)") +
  xlab("Rating count") + ylab("count")
  
rating_summary_pro_user %>% ggplot(aes(sdRating)) + geom_histogram(color="black", binwidth = 0.1) +
  ggtitle("ser rating standard deviation") +
  xlab("Standard deviation") + ylab("count")

top_movie <- edx %>% 
        left_join(rating_summary_pro_user, "userId") %>%
        filter(movieId == rating_summary_pro_movie[which.max(rating_summary_pro_movie$n),]$movieId) 

title_topmovie = movie_data[which.max(rating_summary_pro_movie$n),]$title

top_movie %>% left_join(movie_data, by="movieId") %>% ggplot(aes(rating)) + geom_histogram(color="black", binwidth = 0.5) +
      ggtitle(paste("Rating histogram of the movie '", title_topmovie, "'"))

#calculate average ratings pro user. We will use it later
avg_ratings_pro_user <- edx %>% group_by(userId) %>% summarise(rating = mean(rating), n=n())


user_genre_ratings = rating_summary_pro_user %>% filter(n > 500) %>% 
    inner_join(edx,by="userId") %>%
    group_by(userId, genres) %>% summarise(avg= mean(rating - avgRating), n=n())


top_user_count = 3

movies_to_compare <- head(rating_summary_pro_movie %>% arrange(desc(n)),50)
movies_user_rating <- data.frame(movieId=numeric(), type=character(), rating=numeric(), stringsAsFactors=FALSE)

for(u in head(rating_summary_pro_user %>% arrange(desc(n)),top_user_count)$userId)
{
  movies_user_rating <- bind_rows(movies_user_rating,edx %>% filter(userId==u & movieId %in% movies_to_compare$movieId) %>% 
                                    mutate(type=paste('User ', u)) %>%
                                    select(movieId, type, rating))
}      

movies_user_rating <- bind_rows(movies_user_rating, 
                                rating_summary_pro_movie %>% filter(movieId %in% movies_to_compare$movieId) %>%  
                                  mutate(type='Overall', rating=avg) %>% 
                                  select(movieId,type, rating))

movies_user_rating <- movies_user_rating %>% left_join(movie_data,by='movieId') %>%
                      left_join(rating_summary_pro_movie, "movieId") %>%
                      select(title, type, rating, avg) %>%
                      mutate(title= reorder(as.factor(title), (avg)))
         


movies_user_rating %>%
  ggplot(aes(title, rating, color=type))  +
  geom_point() +
  geom_line(data=movies_user_rating%>%filter(type=='Overall'),aes(title,rating), group=1) +
  ggtitle("Avg rating for top movies") +
  xlab("Movie title") + ylab("Avg rating") +  
  labs(color='Rating type') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = 'bottom')



genres_to_compare <- head(genres_rating %>% arrange(desc(n)),50)$genres
genres_user_rating <- data.frame(genres=character(), type=character(), avg=numeric(), stringsAsFactors=FALSE)

for(u in head(rating_summary_pro_user %>% arrange(desc(n)),top_user_count)$userId)
{
      genres_user_rating <- bind_rows(genres_user_rating,user_genre_ratings %>% filter(userId==u & genres %in% genres_to_compare) %>% 
                                      mutate(type=paste('User ', u)) %>% 
                                      ungroup() %>% select(genres, type, avg) %>% mutate(genres=as.character(genres)))
}      

genres_user_rating <- bind_rows(genres_user_rating, 
                                genres_rating %>% filter(genres %in% genres_to_compare) %>%  
                                mutate(type='Overall', avg=avg-avg_overall) %>% 
                                ungroup() %>% select(genres,type, avg) %>% mutate(genres=as.character(genres)))


genres_user_rating <- genres_user_rating %>% 
                      left_join(genres_rating %>% mutate(avg_o=avg) %>% 
                      ungroup() %>% select(genres, avg_o)  %>% mutate(genres=as.character(genres)), by="genres")

genres_user_rating <- genres_user_rating %>% mutate(genres = as.factor(genres))


genres_user_rating %>%  mutate(genres = reorder(genres,avg_o)) %>%
  ggplot(aes(genres, avg, color=type))  +
  geom_point() +
  geom_line(data=genres_user_rating%>%filter(type=='Overall') %>% 
              mutate(genres = reorder(genres,avg_o)),aes(genres,avg), group=1) +
  ggtitle("Avg rating bias for genre combinations") +
  xlab("Genres") + ylab("Bias") +  
  labs(color='Bias type') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = 'bottom')


genres_user_rating %>% filter(genres=='Action|Sci-Fi|Thriller')



head(user_genre_ratings %>% filter(userId==59269 & n > 20) %>% arrange(desc(avg)),10)
head(user_genre_ratings %>% filter(userId==59269 & n > 20) %>% arrange(avg),10)

head(user_genre_ratings %>% filter(userId==67385 & n > 20) %>% arrange(desc(avg)),10)
head(user_genre_ratings %>% filter(userId==67385 & n > 20) %>% arrange(avg),10)