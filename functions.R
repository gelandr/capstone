
#function to extract the year information from the movie title
#the movie title contains the year at the end of the title in brackets (eg. "Movietitel (1999)")
#parameters:
#     title: the title of the movie containing the year
#
#return:
#     only the year 

extract_movie_year <- function(title){
  #cut the year from the end of the title
  as.integer(str_sub(title, str_length(title) - 4, str_length(title)-1))
}


#function for cross validation
#parameters:
#     trainset: the train set to use for the cross validation
#     cv_n:     the count of the cross validation
#     FUNC:     the function to call for the actual cross validation train and test set (calculated from the param trainset)
#     ...:      additional parameter necessary for calling the provided function
#
#return:
#     dataframe with the function result for the cross validations (the data frame has cv_n items)

cross_validation <- function(trainset, cv_n, FUNC,...){
  
  #get the count of the data rows on the train set
  data_count = nrow(trainset)
  
  #initialize the data frame for the result
  values_from_cv = data.frame()
  
  #randomise the trainset. 
  #If the train set is ordered (not randomised, like the movielens dataset) the cross validation
  #will not be independent and provide wrong result
  trainset_randomised <- trainset[sample(nrow(trainset)),]
  
  #create the train- and testset for the cross validation
  #we need cv_n run, therefore we use a loop 
  for (i in c(1:cv_n)){
    #evaulate the size of the test set. This will be the 1/cv_n part of the data
    part_count = data_count / cv_n
    
    #select the data from the parameter train set
    #we get the part_count size elements from the parameter train set 
    idx = c(   (trunc((i-1) * part_count) + 1) : trunc(i * part_count) )
    
    #tmp holds the new test set
    tmp = trainset_randomised[idx,]
    #train holds the new test set
    train = trainset_randomised[-idx,]
    
    #we remove the elements from the test set, where either the movie or the user is missing in the train set
    test <- tmp %>% 
      semi_join(train, by = "movieId") %>%
      semi_join(train, by = "userId")
    removed <- anti_join(tmp, test, by=c("movieId", "userId"))
    
    #add the removed elements back to the train set 
    train <- rbind(train, removed)
    
    #call the provided function to the actual train and test set.
    akt_value <- FUNC(train, test,...)
    
    #add the result to the data frame
    #the column 'cv' contains the idx of the cross validation run
    values_from_cv <- bind_rows(values_from_cv, akt_value %>% mutate(cv = i))
  }
  
  #return the results of each cross validation
  return(values_from_cv)
}


#function for calculating the root mean squared error for a prediction
#parameters:
#     actual_rating:      the real rating values (from the test set)
#     predicted_rating:   the predicted ratings (prediction calculated on the test set with the prediction algorithm)
#return:
#     the root mean squared error of the prediction

RMSE <- function(actual_rating, predicted_rating){
  sqrt(mean((actual_rating - predicted_rating)^2))
}
