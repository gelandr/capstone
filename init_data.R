#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate") 
if(!require(stringr)) install.packages("stringr") 

source("functions.R")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

#flag for marking file download
file_downloaded <- FALSE

#download the zip file only if not done yet
if (!dir.exists("ml-10M100K")){
     dl <- tempfile()
     download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
     unzip(dl, "ml-10M100K/ratings.dat")
     unzip(dl, "ml-10M100K/movies.dat")

     #mark the download in the flag
     file_downloaded <- TRUE
}
     
fl <- file("ml-10M100K/ratings.dat")
ratings <- read.table(text = gsub("::", "\t", readLines(fl)),
                      col.names = c("userId", "movieId", "rating", "timestamp"))
close(fl)

fl <- file("ml-10M100K/movies.dat")
movie_data <- str_split_fixed(readLines(fl), "\\::", 3)
close(fl)

colnames(movie_data) <- c("movieId", "title", "genres")
movie_data <- as.data.frame(movie_data) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = ratings$rating, times = 1, p = 0.1, list = FALSE)
edx <- ratings[-test_index,]
temp <- ratings[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

#add movie year as column
movie_data <- movie_data %>% mutate(movie_year = extract_movie_year(title)) 

#select all unique genres into a new data frame
all_genres <- movie_data %>% select(genres) %>% unique()

#temporary data frame for the genre flag calculation
genres_data <- data.frame(genres=character(), genre=character(), flag=integer())

#loop for splitting all the existing genre combinations
for (i in 1 : nrow(all_genres)){
  #the list of the genres in the genre combination
  genres = str_split(all_genres$genres[i], "\\|")
  #add all genres separate to the genre combination as a flag
  for (genre in genres[[1]]){
    new_item = data.frame(genres=all_genres$genres[i],genre=genre, flag=1)
    genres_data <- rbind(genres_data, new_item)
  }
}

#remove temporary variables
rm(all_genres, genres, genre, new_item, i)

#select all the 'clean' genres
clean_genres = genres_data %>% select(genre) %>% unique()

##Spread the data to get the genre flags for the genre combinations in the data frame columns
genres_data <- genres_data %>% spread(genre,flag, fill=0)

#extend data frame with age of the movie at the rating time
edx <- edx %>% left_join(movie_data, by="movieId") %>% mutate(movie_age_by_rating =  year(as_datetime(timestamp))- movie_year)
validation <- validation %>% left_join(movie_data, by="movieId") %>% mutate(movie_age_by_rating = year(as_datetime(timestamp))-movie_year)

#select necessary columns only 
edx <- edx %>% select(movieId, genres, userId, rating, movie_age_by_rating)
validation <- validation %>% select(movieId, genres, userId, rating, movie_age_by_rating)

#remove temp file if download done
if (file_downloaded) rm(dl)

#remove temporary variables
rm(fl, ratings, test_index, temp, removed, file_downloaded)
