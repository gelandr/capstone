
edx %>%filter(!str_detect(genres, "\\|")) %>% group_by(genres) %>% summarise(rating = mean(rating))

edx %>% filter(str_detect(genres,"Drama")) %>% mutate(mix = lengths(str_split(genres, "\\|"))) %>% group_by(mix) %>% summarise(rating = mean(rating))