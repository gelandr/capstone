#calculate rating count pro user
rating_summary_pro_user <- edx %>% group_by(userId) %>% summarise(n = n(), avgRating=mean(rating), sdRating=sd(rating))
max(rating_summary_pro_user$n)
min(rating_summary_pro_user$n)

rating_summary_pro_user %>% ggplot(aes(n)) + geom_histogram(color="black", binwidth = 5)
rating_summary_pro_user %>% filter(n < 500) %>% ggplot(aes(n)) + geom_histogram(color="black", binwidth = 5)
rating_summary_pro_user %>% filter(n > 500) %>% ggplot(aes(n)) + geom_histogram(color="black", binwidth = 5)
rating_summary_pro_user %>% ggplot(aes(sdRating)) + geom_histogram(color="black", binwidth = 0.1)

nrow(rating_summary_pro_user[rating_summary_pro_user$sdRating < 0.1,])

sum(rating_summary_pro_user$sdRating == 0)


#calculate average ratings pro user. We will use it later
avg_ratings_pro_user <- edx %>% group_by(userId) %>% summarise(rating = mean(rating), n=n())

user_genre_ratings = edx %>% group_by(userId) %>% summarise(rating = mean(rating), n=n())

