#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate") 
if(!require(stringr)) install.packages("stringr") 



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
movies <- str_split_fixed(readLines(fl), "\\::", 3)
close(fl)

colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

#function to extract the year information from the movie title
#the movie title contains the year at the end of the title in brackets (eg. "Movietitel (1999)")
extract_movie_year <- function(title){
  #cut the year from the end of the title
  as.integer(str_sub(title, str_length(title) - 4, str_length(title)-1))
}

#movies genres
movie_data <- data.frame(movieId = integer(), title=character(),genres_txt=character(), genre=character(), flag=integer())

for (i in 1 : nrow(movies)){
  genres = str_split(movies$genres[i], "\\|")
  for (genre in genres[[1]]){
    new_item = data.frame(movieId = movies$movieId[i], title = movies$title[i], genres_txt=movies$genres[i],genre=genre, flag=1)
    movie_data <- rbind(movie_data, new_item)
  }
}

#select all existing genres from the movies.
all_genres <- (movie_data %>% select(genre) %>% unique())$genre

#add movie year as column
movie_data <- movie_data %>% mutate(movie_year = extract_movie_year(movie_data$title)) 

#Spread the data
movie_data <- movie_data %>% spread(genre,flag, fill=0)


#add movie year column for both test and validation set
edx <- edx %>% mutate(movie_year = extract_movie_year(edx$title)) 
validation <- validation %>% mutate(movie_year = extract_movie_year(validation$title)) 

#extend data frame with year of the rating
edx <- edx %>% mutate(rating_year = year(as_datetime(timestamp)))
validation <- validation %>% mutate(rating_year = year(as_datetime(timestamp)))

#extend data frame with age of the movie at the rating time
edx <- edx %>% mutate(movie_age_by_rating = year(as_datetime(timestamp))-movie_year)
validation <- validation %>% mutate(movie_age_by_rating = year(as_datetime(timestamp))-movie_year)

#select necessary columns only
edx <- edx %>% select(movieId, userId, rating, movie_age_by_rating)
validation <- validation %>% select(movieId, userId, rating, movie_age_by_rating)


#remove temp file if download done
if (file_downloaded) rm(dl)

#remove temporary variables
rm(fl, ratings, movies, test_index, temp, movielens, removed, file_downloaded, extract_movie_year, genre, i, new_item)
