if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if (!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if (!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(knitr)
library(kableExtra)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later

# set.seed(1) # if using R 3.5 or earlier

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set

final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set

removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Extract "year" from "title" in edx and final_holdout_test set

pattern <- "\\((\\d{4})\\)$"
edx <- edx %>% mutate(year=str_extract_all(edx$title, pattern,simplify=FALSE))
edx <- edx %>% mutate(title=str_remove_all(edx$title,pattern))

final_holdout_test <- final_holdout_test %>% mutate(year=str_extract_all(final_holdout_test$title, pattern,simplify=FALSE))
final_holdout_test <- final_holdout_test %>% mutate(title=str_remove_all(final_holdout_test$title,pattern))

final_holdout_test$year <- str_remove_all(final_holdout_test$year,"[(]")
final_holdout_test$year <- str_remove_all(final_holdout_test$year,"[)]")

edx$year <- str_remove_all(edx$year,"[(]")
edx$year <- str_remove_all(edx$year,"[)]")

# Change class of Year from character to numeric

final_holdout_test$year <- as.numeric(final_holdout_test$year)
edx$year <- as.numeric(edx$year)


# Check new variable "year" added

head(edx)
head(final_holdout_test)



#--------------------------2. Data preparation----------------------------------

#2.1. Data Description

#Structure of edx

str(edx)

# Summary of character variables

Character_variables <- c("title","genres")
Number_of_categories <- c(n_distinct(edx$title), n_distinct(edx$genres))
tab1 <- data.frame(Character_variables,Number_of_categories)
kbl(tab1, booktabs = T, caption = "Summary of character variables") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))

# Summary of numeric variables

Numeric_variables <- c("userId","movieId","rating","timestamp","year")

Min <- c(min(edx$userId), min(edx$movieId), min(edx$rating),
         min(edx$timestamp), min(edx$year))

Median <- c(median(edx$userId),median(edx$movieId),median(edx$rating),
            median(edx$timestamp),median(edx$year))

Mean <- c(mean(edx$userId), mean(edx$movieId), mean(edx$rating),
          mean(edx$timestamp), mean(edx$year))
Max <- c(max(edx$userId), max(edx$movieId), max(edx$rating),
         max(edx$timestamp), max(edx$year))

NA_number <- c(sum(is.na(edx$userId)), sum(is.na(edx$movieId)),
               sum(is.na(edx$rating)), sum(is.na(edx$timestamp)),
               sum(is.na(edx$year)))

tab2 <- data.frame(Numeric_variables, Min, Median, Mean, Max, NA_number)

kbl(tab2, booktabs = T, digits = 1, caption = "Summary of Numeric Variables") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))

#N/A values: No

#--------------------3. Data Exploration and Visualization----------------------

#3.1. Number of rating count

# Number of different movies

n_distinct(edx$movieId)

# Range of number of ratings per movie

movie_rating_count <- edx %>% 
  group_by(movieId) %>% 
  summarize(movie_rating_count = n()) 

range(movie_rating_count$movie_rating_count)

#Histogram of average rating per movie

avg_movie_rating <- edx %>%
  group_by(movieId) %>%
  summarize(avg_movie_rating = mean(rating))
  
avg_movie_rating %>% 
  ggplot(aes(avg_movie_rating)) +
  geom_histogram(fill = "blue", color = "black") +
  labs(x = "Average Rating", y = "Number of Movies") +
  ggtitle("Average Rating per Movie")

# Histogram of number of ratings per movie

movie_rating_count %>%
  ggplot(aes(movie_rating_count)) +
  geom_histogram(fill = "blue", color = "black", binwidth = 1000) +
  scale_y_log10() +
  labs(x="Number of Ratings", y="Number of Movies") +
  ggtitle("Number of Ratings per Movie")


# Number of different users

n_distinct(edx$userId)

# Range of number of ratings per user

user_rating_count <- edx %>% 
  group_by(userId) %>% 
  summarize(user_rating_count = n())

range(user_rating_count$user_rating_count)

#Histogram of average rating per user

avg_user_rating <- edx %>%
  group_by(userId) %>%
  summarize(avg_user_rating = mean(rating))

avg_user_rating %>% 
  ggplot(aes(avg_user_rating)) +
  geom_histogram(fill = "blue", color = "black") +
  labs(x = "Average Rating", y = "Number of Users") +
  ggtitle("Average Rating per User")

# Histogram of number of ratings per user

user_rating_count %>%
  ggplot(aes(user_rating_count)) +
  geom_histogram(fill = "blue", color = "black", bins = 30) +
  scale_y_log10() +
  labs(x="Number of Ratings", y="Number of Users") +
  ggtitle("Number of Ratings per User")

# Number of different years

n_distinct(edx$year)

# Range of number of ratings per year

year_rating_count <- edx %>%
  group_by(year) %>%
  summarize(year_rating_count = n())

range(year_rating_count$year_rating_count)

#Line plot of average rating per year

avg_year_rating <- edx %>%
  group_by(year) %>%
  summarize(avg_year_rating = mean(rating))

avg_year_rating %>% 
  ggplot(aes(year, avg_year_rating)) +
  geom_line(color = "blue", size = 1.3) +
  labs(x = "Release Year", y = "Average Rating") +
  ggtitle("Average Rating per Year")

# Line plot of number of ratings per year

year_rating_count %>%
  ggplot(aes(year, year_rating_count)) +
  geom_line(color = "blue", size = 1.3) +
  labs(x="Release Year", y="Number of Ratings") +
  ggtitle("Number of Ratings by Release Year")


#----------------------------4. Modelling-------------------------------------

#Split edx data into training and test set

set.seed(1)

test_index <- createDataPartition(edx$rating, times = 1, p = 0.1, list = FALSE)

train_set <- edx[-test_index, ]

temp <- edx[test_index, ]

test_set <- temp %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

removed <- anti_join(temp, test_set)

train_set <- rbind(train_set, removed)

#Define RMSE function

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


# Model 1: Just the Average  

mu <- mean(train_set$rating)

rmse1 <- RMSE(test_set$rating, mu)

result1 <- tibble(Model = "Just the Average", RMSE = rmse1)  

kbl(result1, booktabs = T, caption ="Result 1") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))

# Model 2: Average + Movie Effect

lambdas <- seq(0, 10, 0.1)

rmse2 <- sapply(lambdas, function(l) {
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l))
  
  y_hat <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    mutate(y_hat = mu + b_i) %>%
    pull(y_hat)
  
RMSE(test_set$rating, y_hat)

})

result2 <- tibble(Model = "Movie Effect",
                      Optimal_lambda = lambdas[which.min(rmse2)],
                      RMSE = min(rmse2))

# Model 3: Average + Movie Effect + User Effect

rmse3 <- sapply(lambdas, function(l) {
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - mu - b_i)/(n()+l))
  
  y_hat <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(y_hat = mu + b_i + b_u) %>%
    pull(y_hat)
  
  RMSE(test_set$rating, y_hat)
  
})

result3 <- tibble(Model = "Movie + User Effect",
                      Optimal_lambda = lambdas[which.min(rmse3)],
                      RMSE = min(rmse3))

#Model 4: Average + movie + user + year effect

  rmse4 <- sapply(lambdas, function(l) {
  
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))
  
  b_y <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - mu - b_i - b_u)/(n()+l))
  
  y_hat <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_y, by = "year") %>%
    mutate(y_hat = mu + b_i + b_u + b_y) %>%
    pull(y_hat)
  
  RMSE(test_set$rating, y_hat)
  
})

result4 <- tibble(Model = "Movie + User + Year Effect",
                  Optimal_lambda = lambdas[which.min(rmse4)],
                  RMSE = min(rmse4))

# Combined result table

result <- bind_rows(result1,result2,result3,result4)
kbl(result, booktabs = T, caption ="Combined result of all models") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))


  
rmse <- sapply(lambdas, function(l) {
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))
  
  b_y <- edx %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - mu - b_i - b_u)/(n()+l))
  
  y_hat <- final_holdout_test %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_y, by = "year") %>%
    mutate(y_hat = mu + b_i + b_u + b_y) %>%
    pull(y_hat)
  
  RMSE(final_holdout_test$rating, y_hat)
  
})
 

# Plot lambda and rmse of final_holdout_test set

qplot(lambdas, rmse)

# Final result table on validation set
result_final <- tibble(Model = "Movie + User + Year",
                           Optimal_lambda = lambdas[which.min(rmse)],
                           RMSE = min(rmse))

kbl(result_final, booktabs = T, 
    caption ="Final RMSE result on final_holdout_test set") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))



          
  



