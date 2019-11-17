
avg_overall = mean(edx$rating)

#calculate rating count avg rating and sd pro movie
rating_summary_pro_movie <- edx %>% group_by(movieId) %>% summarise(avg = mean(rating), avg_norm=mean(rating) - avg_overall, sd=sd(rating), n = n())
max(rating_summary_pro_movie$n)
min(rating_summary_pro_movie$n)

#plot the histogram for the rating
edx %>% ggplot(aes(rating)) + geom_histogram(color="black", binwidth = 0.5)+
  ggtitle("Movie rating historgram") +
  xlab("Rating") + ylab("count")

#plot the histogram for the avg
rating_summary_pro_movie %>% ggplot(aes(avg)) + geom_histogram(color="black", binwidth = 0.1) +
  ggtitle("Movie rating avg") +
  xlab("AVG Rating") + ylab("count")

#plot the histogram for the rating count
rating_summary_pro_movie %>% ggplot(aes(n)) + geom_histogram(color="black", binwidth = 200) +
  ggtitle("Movie rating count") +
  xlab("Rating count") + ylab("count")

#plot the histogram for the rating count (for count > 1000)
rating_summary_pro_movie %>% filter(n > 1000) %>% ggplot(aes(n)) + geom_histogram(color="black", binwidth = 100) +
  ggtitle("Movie rating count (count > 1.000)") +
  xlab("Rating count") + ylab("count")

#plot the histogram for the count (for count < 1000)
rating_summary_pro_movie %>% filter(n <= 1000) %>% ggplot(aes(n)) + geom_histogram(color="black", binwidth = 10) +
  ggtitle("Movie rating count (count <= 1.000)")+
  xlab("Rating count") + ylab("count")

#calculate the avg rating and count pro genre
genres_rating <- edx %>% group_by(genres) %>% summarise(avg=mean(rating), sd=sd(rating), n=n())

#plot the histogram for the avg
genres_rating %>% ggplot(aes(n)) + geom_histogram(color="black", binwidth = 10000) +
    ggtitle("Genres rating count") +
    xlab("Rating count") + ylab("count")

#plot the avg rating pro genre with more than 10000 ratings ordered by avg rating
genres_rating %>% filter(n>=25000) %>% mutate(genres = reorder(genres, avg)) %>% 
  ggplot(aes(genres,avg)) + geom_point() +
  ggtitle("genres avg rating (rating count >= 10.000") +
  xlab("Genres") + ylab("Avg rating") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#List of the genres order by avg rating for genres with more than 10.000 ratings
genres_rating %>% filter(n>=10000) %>% arrange(desc(avg))

#calculate the avg rating and count pro genre and movie age at rating
genres_rating_year <- edx %>% group_by(genres, movie_age_by_rating) %>% summarise(avg=mean(rating), n=n())

#plot the avg rating for genre "Drama" pro movie age at rating with more than 100 ratings
genres_rating_year %>% filter(genres=="Drama" & n>500) %>% ggplot(aes(movie_age_by_rating,avg)) + geom_line()

#plot the avg rating for genre "Drama|Romance" pro movie age at rating with more than 100 ratings
genres_rating_year %>% filter(genres=="Drama|Romance" & n>500) %>% ggplot(aes(movie_age_by_rating,avg)) + geom_line()

#plot the avg rating for genre "Drama|Thriller" pro movie age at rating with more than 100 ratings
genres_rating_year %>% filter(genres=="Drama|Thriller" & n>500) %>% ggplot(aes(movie_age_by_rating,avg)) + geom_line()

#plot the avg rating for genre "Comedy" pro movie age at rating with more than 100 ratings
genres_rating_year %>% filter(genres=="Comedy" & n>500) %>% ggplot(aes(movie_age_by_rating,avg)) + geom_line()

#plot the avg rating for genre "Comedy|Romance" pro movie age at rating with more than 100 ratings
genres_rating_year %>% filter(genres=="Comedy|Romance" & n>500) %>% ggplot(aes(movie_age_by_rating,avg)) + geom_line()

#plot the avg rating for genre "Comedy|Drama" pro movie age at rating with more than 100 ratings
genres_rating_year %>% filter(genres=="Comedy|Drama" & n>500) %>% ggplot(aes(movie_age_by_rating,avg)) + geom_line()

#plot the avg rating for genre "Action|Adventure|Sci-Fi" pro movie age at rating with more than 100 ratings
genres_rating_year %>% filter(genres=="Action|Adventure|Sci-Fi" & n>500) %>% ggplot(aes(movie_age_by_rating,avg)) + geom_line()

#plot the avg rating for genre "Action|Adventure|Thriller" pro movie age at rating with more than 100 ratings
genres_rating_year %>% filter(genres=="Action|Adventure|Thriller" & n>500) %>% ggplot(aes(movie_age_by_rating,avg)) + geom_line()

#plot the avg rating for genre "Action|Adventure|Sci-Fi|Thriller" pro movie age at rating with more than 100 ratings
genres_rating_year %>% filter(genres=="Action|Adventure|Sci-Fi|Thriller" & n>500) %>% ggplot(aes(movie_age_by_rating,avg)) + geom_line()