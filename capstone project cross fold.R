if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(class)) install.packages("class", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")


library(dplyr)
library(caret)
library(class)
library(tidyverse)
library(data.table)
library(xgboost)

  # Data Wrangling
dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

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

  # Analysis proper
# Checking data
str(edx)

# Deriving the average rating per movie
avg_rating_per_movie <- edx %>% group_by(movieId) %>% summarize(avg_movie_rating = mean(rating))
str(avg_rating_per_movie)

# Visualizing the distribution of average movie ratings
hist(avg_rating_per_movie$avg_movie_rating, 
     xlab = "Average rating of a movie", 
     main = "Histogram of average movie ratings")

boxplot(avg_rating_per_movie$avg_movie_rating, 
        main = "Boxplot of average movie ratings", 
        ylab = "Average movie rating")

# Inserting the average rating per movie for each observation in the edx dataset
data <- inner_join(edx, avg_rating_per_movie)
str(data)


# Visualizing the distribution of average movie rating given by each reviewer
hist(avgratings_per_user$avg_rating_per_user,
     xlab = "Average movie rating given by reviewers",
     main = "HIstogram of the average movie rating per reviewer")

boxplot(avgratings_per_user$avg_rating_per_user, 
        ylab = "Average movie rating per reviewer",
        main = "Boxplot of average movie ratings per user")

# Inserting the average rating per user for each observation in the edx dataset
data <- inner_join(data, avgratings_per_user)
str(data)

# Discovering all possible genres listed in the edx dataset
data_genres <- str_split(data$genres, "\\|", simplify = TRUE)
genre_vec <- data_genres %>% factor() 
list_genres <- genre_vec %>% levels()

# Counts of genres
genre_vec <- as.data.frame(genre_vec)
genre_count <- genre_vec %>% group_by(genre_vec) %>% count()
genre_count

# Visualizing counts of genres

# We take out the empty character strings and "(no genres listed)" strings
genre_vec <- genre_vec[genre_vec != "" ]
genre_vec <- genre_vec[genre_vec != "(no genres listed)"]

# We visualize the counts of the different genres by giving each its own barplot
barplot(table(genre_vec), 
        main = "Barplots of counts of different genres",
        width = 1, 
        cex.names = 0.7
        )

list_genres

# Transforming each genre into a binary variable for each observation in the data
# dataset

data <- as.data.table(data)

data[ , `:=` (ng = data$genres %>% 
                str_detect("(no genres listed)") %>% 
                as.numeric(),
              action = data$genres %>% 
                str_detect("Action") %>% 
                as.numeric(),
              adventure = data$genres %>%
                str_detect("Adventure") %>%
                as.numeric(),
              animation = data$genres %>% 
                str_detect("Animation") %>% 
                as.numeric(),
              children = data$genres %>% 
                str_detect("Children") %>% 
                as.numeric(),
              comedy = data$genres %>% 
                str_detect("Comedy") %>% 
                as.numeric(),
              crime = data$genres %>% 
                str_detect("Crime") %>% 
                as.numeric(),
              documentary = data$genres %>% 
                str_detect("Documentary") %>% 
                as.numeric(),
              drama = data$genres %>% 
                str_detect("Drama") %>% 
                as.numeric(), 
              fantasy = data$genres %>% 
                str_detect("Fantasy") %>% 
                as.numeric(),
              film_noir = data$genres %>% 
                str_detect("Film-Noir") %>%
                as.numeric(),
              horror = data$genres %>% 
                str_detect("Horror") %>% 
                as.numeric(), 
              imax = data$genres %>% 
                str_detect("IMAX") %>% 
                as.numeric(), 
              musical = data$genres %>% 
                str_detect("Musical") %>%
                as.numeric(),
              mystery = data$genres %>% 
                str_detect("Mystery") %>%
                as.numeric(), 
              romance = data$genres %>% 
                str_detect("Romance") %>%
                as.numeric(), 
              scifi = data$genres %>% 
                str_detect("Sci-Fi") %>% 
                as.numeric(), 
              thriller = data$genres %>% 
                str_detect("Thriller") %>% 
                as.numeric(), 
              war = data$genres %>% 
                str_detect("War") %>% 
                as.numeric(), 
              western = data$genres %>% 
                str_detect("Western") %>% 
                as.numeric()
              )
      ]

# Finalizing dataset to be used in the cross-validated XGBoost model
final_data <- data[ , 7:28]
str(final_data)

  # Validation data wrangling

# Deriving the average rating per movie
val_avg_rating_per_movie <- validation %>% group_by(movieId) %>% 
  summarize(avg_movie_rating = mean(rating))

# Inserting the average rating per movie for each observation in the validation dataset
val_data <- inner_join(validation, val_avg_rating_per_movie)

# Deriving the average ratings per user
val_avgratings_per_user <- validation %>% group_by(userId) %>%
  summarize(avg_rating_per_user = mean(rating))

#Inserting the average rating per user in the validation dataset
val_data <- inner_join(val_data, val_avgratings_per_user)

# Making validation genres into binary variables
val_data <- as.data.table(val_data)

val_data[ , `:=` (ng = val_data$genres %>% 
                    str_detect("(no genres listed)") %>% 
                    as.numeric(),
                  action = val_data$genres %>% 
                    str_detect("Action") %>% 
                    as.numeric(),
                  adventure = val_data$genres %>% 
                    str_detect("Adventure") %>% 
                    as.numeric(),
                  animation = val_data$genres %>% 
                    str_detect("Animation") %>%
                    as.numeric(),
                  children = val_data$genres %>% 
                    str_detect("Children") %>% 
                    as.numeric(),
                  comedy = val_data$genres %>% 
                    str_detect("Comedy") %>%
                    as.numeric(),
                  crime = val_data$genres %>% 
                    str_detect("Crime") %>% 
                    as.numeric(),
                  documentary = val_data$genres %>% 
                    str_detect("Documentary") %>% 
                    as.numeric(),
                  drama = val_data$genres %>% 
                    str_detect("Drama") %>% 
                    as.numeric(), 
                  fantasy = val_data$genres %>% 
                    str_detect("Fantasy") %>% 
                    as.numeric(),
                  film_noir = val_data$genres %>% 
                    str_detect("Film-Noir") %>% 
                    as.numeric(),
                  horror = val_data$genres %>% 
                    str_detect("Horror") %>% 
                    as.numeric(), 
                  imax = val_data$genres %>% 
                    str_detect("IMAX") %>% 
                    as.numeric(), 
                  musical = val_data$genres %>% 
                    str_detect("Musical") %>% 
                    as.numeric(),
                  mystery = val_data$genres %>% 
                    str_detect("Mystery") %>% 
                    as.numeric(), 
                  romance = val_data$genres %>%
                    str_detect("Romance") %>% 
                    as.numeric(), 
                  scifi = val_data$genres %>% 
                    str_detect("Sci-Fi") %>% 
                    as.numeric(), 
                  thriller = val_data$genres %>% 
                    str_detect("Thriller") %>% 
                    as.numeric(), 
                  war = val_data$genres %>% 
                    str_detect("War") %>% 
                    as.numeric(), 
                  western = val_data$genres %>% 
                    str_detect("Western") %>% 
                    as.numeric()
                  )
          ]

# Finalizing validation dataset
fin_val_data <- val_data[ , 7:28]
str(fin_val_data)

# Setting parameters for XGBoost
grid_tune <- expand.grid(nrounds = c(100, 200, 300),
                         max_depth = c(2, 6, 8), 
                         eta = c(0.1, 0.3, 0.5),
                         gamma = 0, 
                         colsample_bytree = 1,
                         min_child_weight = 1, 
                         subsample = 1
)

# Setting parameters for cross-validation
train_control <- trainControl(method = "cv",
                              number= 10,
                              verboseIter = FALSE,
                              allowParallel = TRUE)

# Running the XGBoost model on the final_data dataset
xgb_model <- train(x = final_data,
                   y = data$rating,
                   trControl = train_control,
                   tuneGrid = grid_tune,
                   method= "xgbTree",
                   verbose = FALSE,
                   verbosity = 0)

xgb_model

predict(xgb_tune, fin_val_data)

# Prediction:
xgb_pred <- predict(xgb_model, fin_val_data)

mse <- mean((validation$rating - xgb_pred)^2)
mae <- caret::MAE(validation$rating, xgb_pred)
rmse <- caret::RMSE(validation$rating, xgb_pred)

model_eval <- c(mse, mae, rmse)
model_eval

