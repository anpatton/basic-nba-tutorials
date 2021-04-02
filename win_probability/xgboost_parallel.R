library(tidyverse)
library(arrow)
library(knitr)
set.seed(42)

pbp <- arrow::read_parquet("data/pbp.parquet") %>% 
  filter(is.na(nameTeam) == FALSE) %>% 
  filter(is.na(eventGeneral) == FALSE) %>% 
  replace_na(list(shotResultPoints = 0)) %>% 
  mutate(home_points_scored = ifelse(nameTeam == homeTeam, shotResultPoints, 0)) %>% 
  
  mutate(away_points_scored = ifelse(nameTeam == awayTeam, shotResultPoints, 0)) %>% 
  mutate(home_foul = ifelse(nameTeam == homeTeam & grepl("foul", .$eventType), 1, 0)) %>%
  mutate(away_foul = ifelse(nameTeam == awayTeam & grepl("foul", .$eventType), 1, 0)) %>%
  mutate(time_remaining = 2880 - gametime) %>% 
  group_by(idGame) %>% 
  mutate(home_score = cumsum(home_points_scored)) %>% 
  mutate(away_score = cumsum(away_points_scored)) %>% 
  mutate(home_margin = home_score - away_score) %>% 
  mutate(home_foul_totals = cumsum(home_foul)) %>%
  mutate(away_foul_totals = cumsum(away_foul)) %>%
  mutate(home_win = ifelse(max(home_score) > max(away_score), 1, 0)) %>% 
  ungroup() %>% 
  select(idGame, time_remaining, home_score, away_score, home_margin, 
         home_foul_totals, away_foul_totals, home_win) 


library(splitTools)

games <- unique(pbp$idGame) 

train_games <- sample(games, 0.8 * length(games))

train <- pbp %>% 
  filter(idGame %in% train_games)

test <- pbp %>% 
  filter(!idGame %in% train_games)

x_train <- train %>% 
  select(-idGame, -home_win) %>% 
  as.matrix() ## xgboost necessary step, although you can use other ways

y_train <- as.numeric(train$home_win)

x_test <- test %>% 
  select(-idGame, -home_win) %>% 
  as.matrix() ## xgboost necessary step, although you can use other ways

y_test <- test$home_win

#grouped_folds <-  map(0:4, function(x) { ## this was 100% written by Ben Baldwin and is a manual splitter
#  fold_indices <- which(train$idGame %in% games[(1 + 80 * x) : (80 + (x * 80))])
#  return(fold_indices)
#  })

grouped_folds <- splitTools::create_folds(train$idGame, k = 5, type = "grouped", invert = TRUE)

param_grid = list()

for(i in 1:10){
  
  param_grid[[i]] <- list(booster = "gbtree",
                          objective = "binary:logistic",
                          max_depth = sample(c(3:10), 1),
                          eta = runif(1, .01, .3),
                          subsample = runif(1, .7, 1),
                          colsample_bytree = runif(1, .6, 1),
                          min_child_weight = sample(0:10, 1))
  
}

par_xgboost <- function(params, data, labels, folds) {
  
  xgbcv <- xgboost::xgb.cv(params = params, 
                           data = x_train, 
                           label = y_train,
                           nrounds = 1000, ## somewhat arbitrary, can let early stopping solve this
                           folds = folds,
                           early_stop_round = 5,
                           eval_metric = "error")
  
  test_error <- xgbcv$evaluation_log %>% 
    filter(test_error_mean == min(test_error_mean)) %>% 
    pull(test_error_mean)
  
  return(list(test_error, params, xgbcv$niter))
  
}

cores_minus_one <- parallel::detectCores() - 1 

cl <- parallel::makeCluster(spec = cores_minus_one) 

parallel::clusterExport(cl, c("par_xgboost", "x_train", "y_train", "grouped_folds"))

parallel::clusterEvalQ(cl, {
  library(dplyr)
  library(xgboost)
})

res <- parallel::parLapply(cl = cl, param_grid, 
                           function(x) par_xgboost(params = x, data = x_train, labels = y_train, folds = grouped_folds))

parallel::stopCluster(cl) 

best_error_index <- which.min(sapply(res, function(x) x[[1]]))
best_error <- res[[best_error_index]][[1]] 
best_param_set <- res[[best_error_index]][[2]] 
best_num_iterations <- res[[best_error_index]][[3]]

xgb_tuned <- xgboost::xgboost(params = best_param_set,
                              data = x_train,
                              label = y_train,
                              nrounds = best_num_iterations,
                              print_every_n = 10,
                              eval_metric = "error",
                              early_stopping_rounds = 5)

preds <- predict(xgb_tuned, x_test)
preds_01 <- round(preds, 0)

hist(preds)

#test <- test %>% 
#  mutate(home_win_prob = preds) %>% 
#  mutate(home_win_pred = preds_01)

#print(paste0("Training Error %: ", round(100 * best_error, 1)))
#print(paste0("Testing Error %: ", round(100 * Metrics::accuracy(y_test, !preds_01), 1)))

