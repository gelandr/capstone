
avg_overall = mean(edx$rating)

#calculate rating count and avg rating pro movie
rating_count_avg_pro_movie <- edx %>% group_by(movieId) %>% summarise(avg = mean(rating), avg_norm=mean(rating) - avg_overall,n = n())
max(rating_count_avg_pro_movie$n)
min(rating_count_avg_pro_movie$n)

rating_count_avg_pro_movie %>% ggplot(aes(n)) + geom_histogram(color="black", binwidth = 5)
rating_count_avg_pro_movie %>% filter(n < 500) %>% ggplot(aes(n)) + geom_histogram(color="black", binwidth = 5)
rating_count_avg_pro_movie %>% filter(n > 500) %>% ggplot(aes(n)) + geom_histogram(color="black", binwidth = 5)


genres_avg <- edx %>% left_join(movie_data, by="movieId") %>% group_by(genres_txt) %>% summarise(avg=mean(rating), sd=sd(rating), n=n())
edx %>% left_join(movie_data, by="movieId") %>% qplot(factor(genres_txt), rating, data=., geom="boxplot")

genres_rating <- edx %>% left_join(movie_data, by="movieId") %>% group_by(genres_txt, movie_age_by_rating) %>% summarise(avg=mean(rating))


rating_pro_year_movie <- edx %>% group_by(movieId, movie_age_by_rating) %>% summarise(rating = mean(rating))
rating_pro_year_movie <- rating_pro_year_movie %>% left_join(rating_count_avg_pro_movie, by="movieId") %>% mutate(rating_d = rating - avg)

rating_pro_year_movie %>% left_join(movie_data, by="movieId") %>% filter(`Sci-Fi` == 1 & n >= 1000 & movie_age_by_rating < 20) %>% qplot(factor(movie_age_by_rating), rating_d, data=., geom="boxplot")
rating_pro_year_movie %>% left_join(movie_data, by="movieId") %>% filter(Comedy == 1 & n >= 1000 & movie_age_by_rating < 20) %>% qplot(factor(movie_age_by_rating), rating_d, data=., geom="boxplot")
rating_pro_year_movie %>% left_join(movie_data, by="movieId") %>% filter(Drama == 1 & n >= 1000 & movie_age_by_rating < 20) %>% qplot(factor(movie_age_by_rating), rating_d, data=., geom="boxplot")
rating_pro_year_movie %>% left_join(movie_data, by="movieId") %>% filter(Crime == 1 & n >= 1000 & movie_age_by_rating < 20) %>% qplot(factor(movie_age_by_rating), rating_d, data=., geom="boxplot")

genres_rating %>% filter(`Sci-Fi` == 1 & n >= 1000 & movie_age_by_rating < 20) %>% qplot(factor(movie_age_by_rating), rating_d, data=., geom="boxplot")



x <- movie_data %>% subset(select=-c(movieId, title))


data_pca <- prcomp(x)

pc <- 1:nrow(x)
qplot(pc, data_pca$sdev)

