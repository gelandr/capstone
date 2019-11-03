#calculate rating count pro user
rating_count_pro_user <- edx %>% group_by(userId) %>% summarise(n = n(), avgRating=mean(rating), sdRating=sd(rating))
max(rating_count_pro_user$n)
min(rating_count_pro_user$n)

rating_count_pro_user %>% ggplot(aes(n)) + geom_histogram(color="black", binwidth = 5)
rating_count_pro_user %>% filter(n < 500) %>% ggplot(aes(n)) + geom_histogram(color="black", binwidth = 5)
rating_count_pro_user %>% filter(n > 500) %>% ggplot(aes(n)) + geom_histogram(color="black", binwidth = 5)
rating_count_pro_user %>% ggplot(aes(sdRating)) + geom_histogram(color="black", binwidth = 0.1)


#calculate average ratings pro user. We will use it later
avg_ratings_pro_user <- edx %>% group_by(userId) %>% summarise(rating = mean(rating), n=n())

user_genre_ratings = edx %>% group_by(userId) %>% summarise(rating = mean(rating), n=n())

