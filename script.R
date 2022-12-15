rm(list = ls())
library(tidyverse)
library(stringr)
library(caret)
load("C:/Users/DRambaccussing/OneDrive - University of Dundee/HARVARDX/DOORUJRAMBACCUSSING/data1.RData")


# Loading datasets (Using EDX instructions) ------------------------
rm(list=ls())
library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp")) 

#If Edx code does not work, use the following:

#ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
#                      col.names = c("userId", "movieId", "rating", "timestamp"))


movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")



# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# We save the data locally to improve speed
save.image("~/data1.RData")






##Movie release date
Releaseyear =as.numeric(str_sub(edx$title, start =-5, end =-2))
edx = edx %>% mutate(Release_year = Releaseyear)
head(edx)
Releaseyear =as.numeric(str_sub(validation$title, start =-5, end =-2))
validation = validation %>% mutate(Release_year = Releaseyear)
edx = edx %>% mutate(char_length = nchar(title))
validation = validation %>% mutate(char_length = nchar(title))
edx = edx %>% mutate(MovieAge = 2022 - Release_year)
validation = validation %>% mutate(MovieAge = 2022 - Release_year)


## We can also learn about the movie release date:
Releaseyear =as.numeric(str_sub(edx$title, start =-5, end =-2))
edx = edx %>% mutate(Release_year = Releaseyear)
edx 
head(edx)








# Exploring the data ------------------------------------------------------
load("~/data1.RData")
# identify class and change to tibble
class(edx)
edx %>% as_tibble()
# Check for missing observations
which(is.na(edx))
# Check how the data looks like
head(edx)
glimpse(edx)
#Number of Users
length(unique(edx$userId))
#Number of movies
length(unique(edx$movieId))
# Table 
table(edx$rating)


# Data Wrangling ----------------------------------------------------------
# create the year of the rating and release date
edx <- edx %>% mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01",  
                                             tz = "UTC"))
edx$timestamp <- format(edx$timestamp, "%Y")
names(edx)[names(edx) == "timestamp"] <- "Year"
head(edx)
validation <- validation %>% mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01",  
                                                           tz = "UTC"))
validation$timestamp <- format(validation$timestamp, "%Y")
names(edx)[names(validation) == "timestamp"] <- "Year"
head(validation)
## Release year
Releaseyear =as.numeric(str_sub(edx$title, start =-5, end =-2))
edx = edx %>% mutate(Release_year = Releaseyear)
head(edx)
Releaseyear =as.numeric(str_sub(validation$title, start =-5, end =-2))
validation = validation %>% mutate(Release_year = Releaseyear)

edx = edx %>% mutate(MovieAge = 2022 - Release_year)
validation = validation %>% mutate(MovieAge = 2022 - Release_year)

edx = edx %>% mutate(char_length = nchar(title))
validation = validation %>% mutate(char_length = nchar(title))

# Plots -------------------------------------------------------------------


## some plots

movie = edx %>%
  group_by(movieId,title) %>%
  summarise(n_ratings = n(),
            avg_movie_rating = mean(rating) 
            )

## Scatterplot of number of times rated and average ratings
movie %>%
  ggplot(aes(n_ratings,avg_movie_rating))+
  geom_point(aes())+
  geom_smooth()+
  theme_bw()
## Correlation between numebr of times rated and rating scores.
cor(movie$n_ratings,movie$avg_movie_rating)

## Scatterplot of title length and rating

movie %>%
  ggplot(aes(nchar(title),avg_movie_rating))+
  geom_point(aes())+
  geom_smooth()+
  theme_bw()
## correlation 
cor(nchar(movie_add$title), movie_add$avg_movie_rating)

## Number of words in title
movie$nwords <- str_count(movie$title, "\\w+")
cor(movie$nwords,movie$avg_movie_rating)
## Usually the number of words/characters is positively albeit weakly correlated



# Plots -------------------------------------------------------------------

## Create a new dataframe with a unique genre category representing a row. 
genres_df <- edx %>%
  separate_rows(genres, sep = "\\|")  %>%
  group_by(genres) %>%
  summarise(number = n(), averageratings = mean(rating)) 

Genres = genres_df  %>%
  filter(number > 100000) %>%
  mutate(genres = fct_reorder(genres, averageratings)) %>%
  ggplot(aes(x = averageratings, y = genres)) +
  geom_col(aes(fill=genres), alpha = 0.9) +
  labs(y = "Genre", 
       x = "Average Rating") +
  theme_bw() +
  scale_x_continuous(limits = c(0, 5), 
                     breaks = 0:5,
                     labels = 0:5) +
  theme(panel.grid.major.x = element_line(linetype = "dashed", color = "blue"))
####

density = edx %>% 
  group_by(movieId) %>%
  summarise(ratings = mean(rating))
mean_rat = mean(density$ratings)
                
densit= ggplot(density, aes(x=ratings))+
  geom_density(fill="dodgerblue", alpha=0.5)+
  labs(x="Movie Ratings")+
  geom_vline(xintercept=mean_rat, size=1.5, color="red")+
  geom_text(aes(x=3.19, label=paste0("Mean\n",3.19), y=1.9))+
  theme_bw()
  

timeseries = edx %>% 
  group_by(Release_year) %>%
  summarise(ratings = mean(rating), count=n())

ggplot(data=timeseries, aes(x=Release_year, y=ratings, group=1)) +
  geom_line(linetype = "solid")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.2))+
  theme_bw()

###
##partitioning the dataset into test and training samples. 
test_index = createDataPartition(y = edx$rating, times= 1, p = 0.2,
                                   list = F
                                   )
test_set <- edx %>% slice(test_index)
train_set <- edx %>% slice(-test_index)
## free memory
rm(density, densit, genres_df, Genres, movie, test_index, timeseries)

#### solving Parameters of alpha (mean), bi(movie) and bu (user), 
### bt (release year), bg(genres) and bc(characters in titles)
alpha <- mean(train_set$rating)

bi <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - alpha))

bu <- train_set %>%
  left_join(bi, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - alpha - b_i))

bg <- train_set %>%
  left_join(bu, by = "userId") %>%
  left_join(bi, by = "movieId") %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - alpha - b_i - b_u))

bt <- train_set %>%
  left_join(bu, by = "userId") %>%
  left_join(bi, by = "movieId") %>%
  left_join(bg, by = "genres") %>%
  group_by(Release_year) %>%
  summarize(b_t = mean(rating - alpha - b_i - b_u -b_g))

bc <- train_set %>%
  left_join(bu, by = "userId") %>%
  left_join(bi, by = "movieId") %>%
  left_join(bg, by = "genres") %>%
  left_join(bt, by = "Release_year") %>%
  group_by(char_length) %>%
  summarize(b_c = mean(rating - alpha - b_i - b_u -b_g -b_t))



## Predictions

n_rmse <- RMSE(test_set$rating, alpha)
n_rmse

rmse_results <- data_frame(method = "Average movie rating", RMSE = n_rmse)

pred_bi <- alpha + test_set %>%
  left_join(bi, by = "movieId") %>%
  pull(b_i)

model1_rmse <- RMSE(pred = pred_bi,
                    obs = test_set$rating,
                    na.rm = TRUE)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie effect ",  
                                     RMSE = model1_rmse ))

pred_bu <- test_set %>%
  left_join(bi, by = "movieId") %>%
  left_join(bu, by = "userId") %>%
  mutate(pred_bu = alpha + b_i + b_u) %>%
  pull(pred_bu)
model2_rmse <- RMSE(pred = pred_bu,
                    obs = test_set$rating,
                    na.rm = TRUE)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie and User effect",  
                                     RMSE = model2_rmse))
# Free some space
rm(pred_bi, pred_bu)

pred_bg <- test_set %>%
  left_join(bi, by = "movieId") %>%
  left_join(bu, by = "userId") %>%
  left_join(bg, by = "genres") %>%
  mutate(pred_bg = alpha + b_i + b_u + b_g) %>%
  pull(pred_bg)

model3_rmse <- RMSE(pred = pred_bg,
                    obs = test_set$rating,
                    na.rm = TRUE)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie, User and Genre effect",  
                                     RMSE = model3_rmse))

pred_bt <- test_set %>%
  left_join(bu, by = "userId") %>%
  left_join(bi, by = "movieId") %>%
  left_join(bg, by = "genres") %>%
  left_join(bt, by = "Release_year") %>%
  mutate(pred_bt = alpha + b_i + b_u + b_g+ b_t) %>%
  pull(pred_bt)

model4_rmse <- RMSE(pred = pred_bt,
                    obs = test_set$rating,
                    na.rm = TRUE)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie, User, Genre and Release date effect",  
                                     RMSE = model4_rmse))


pred_bc <- test_set %>%
  left_join(bu, by = "userId") %>%
  left_join(bi, by = "movieId") %>%
  left_join(bg, by = "genres") %>%
  left_join(bt, by = "Release_year") %>%
  left_join(bc, by = "char_length") %>%
  mutate(pred_bc = alpha + b_i + b_u + b_g+ b_t +b_c) %>%
  pull(pred_bc)
  
model5_rmse <- RMSE(pred = pred_bc,
                      obs = test_set$rating,
                      na.rm = TRUE)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie, User, Genre, Release date and Character length effect",  
                                     RMSE = model4_rmse))
rmse_results %>% knitr::kable()

######################### Validation Dataset
### Simple Mean
mu <- mean(edx$rating)
N_rmse <- RMSE(validation$rating, mu)
rmse_results1 <- data_frame(method = "Average movie rating model", RMSE = N_rmse)
### Movie ID
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
predicted_ratings <- mu + validation %>%
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
model_1_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results1 <- bind_rows(rmse_results1,
                          data_frame(method="Movie effect model",
                                     RMSE = model_1_rmse ))

#### User average
user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
predicted_ratings <- validation%>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
model_2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results1 <- bind_rows(rmse_results1,
                          data_frame(method="Movie and user effect model", RMSE = model_2_rmse))

##########Genre Effect##########
genre_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))
predicted_ratings <- validation%>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)
model_3_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results1 <- bind_rows(rmse_results1,
                          data_frame(method="Movie, user and Genre effect model", RMSE = model_3_rmse))
rmse_results1 %>% knitr::kable()




##########################Regularisation###
# lambda is a tuning parameter
# Use cross-validation to choose it.
lambdas <- seq(0, 10, 0.5)


# For each lambda,find b_i & b_u, followed by rating prediction & testing
# note:the below code could take some time  
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_g <- edx %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - b_u - mu)/(n()+l))
  
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})
rmse_results1 <- bind_rows(rmse_results1,
                          data_frame(method="Regularized movie,user and genre effect",
                                     RMSE = min(rmses)))
rmse_results1 %>% knitr::kable()

# Plot rmses vs lambdas to select the optimal lambda                                                             
qplot(lambdas, rmses)  

#Determine which lambda minimizes the RMSE
lambda <- lambdas[which.min(rmses)]

