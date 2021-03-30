How to Calculate RAPM
================
Andrew Patton

## What is RAPM?

Regularized Adjusted Plus Minus, or RAPM, is a modeled estimate of a
player’s plusminus per-100 hopefully independent of their teammates,
production, etc. As seen in the SPM tutorial, the amount of “stuff” a
player does impacts a player’s SPM rating. With RAPM, a player can score
but up literally no box score stats and have a great rating, and score
30 ppg and have a terrible rating. This is possible because RAPM is
based on play-by-play data which tracks two things on a possession
level, what ten players are on court (split offense and defense), and
how many points the offense scored.

For a highly technical explanation of how RAPM is calculated, check out
[this
link](https://squared2020.com/2017/09/18/deep-dive-on-regularized-adjusted-plus-minus-i-introductory-example/).
We will get into a vastly simplified mini-explanation in a section or
two. First we read in data from [Ryan Davis’
GitHub](https://github.com/rd11490/NBA_Tutorials), pretty much the best
place for more advanced python tutorials.

``` r
library(tidyverse)
library(Matrix)
library(glmnet)
library(knitr)

possesion_data <- read_csv("https://raw.githubusercontent.com/rd11490/NBA_Tutorials/master/rapm/data/rapm_possessions.csv") %>% 
  filter(possessions != 0) %>% 
  select(-X1)
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
player_data <- read_csv("https://raw.githubusercontent.com/rd11490/NBA_Tutorials/master/rapm/data/player_names.csv")

head(possesion_data)
```

    ## # A tibble: 6 x 12
    ##   offensePlayer1Id offensePlayer2Id offensePlayer3Id offensePlayer4Id
    ##              <dbl>            <dbl>            <dbl>            <dbl>
    ## 1           201142           201939           202691          1626172
    ## 2           101141           202339           203089           203114
    ## 3           201933           202704           203083           203922
    ## 4             2548           201609           202355           204020
    ## 5             2594           201567           201588           202684
    ## 6             2200           200746           201158           201942
    ## # … with 8 more variables: offensePlayer5Id <dbl>, defensePlayer1Id <dbl>,
    ## #   defensePlayer2Id <dbl>, defensePlayer3Id <dbl>, defensePlayer4Id <dbl>,
    ## #   defensePlayer5Id <dbl>, points <dbl>, possessions <dbl>

## Step 1: Get the player IDs

With the data read in, we need to convert it into the proper format. The
first step is to get all the unique player ids from the possession data.

``` r
get_players <- function(possesions) {
  
  possesions <- distinct(possesions)
  players <- unique(c(unique(possesions$offensePlayer1Id),
                      unique(possesions$offensePlayer2Id),
                      unique(possesions$offensePlayer3Id),
                      unique(possesions$offensePlayer4Id),
                      unique(possesions$offensePlayer5Id),
                      unique(possesions$defensePlayer1Id),
                      unique(possesions$defensePlayer2Id),
                      unique(possesions$defensePlayer3Id),
                      unique(possesions$defensePlayer4Id),
                      unique(possesions$defensePlayer5Id)))
  return(players)
  
}

players <- sort(get_players(possesions = possesion_data))

print(paste0("There are ", length(players), " unique players in the dataset."))
```

    ## [1] "There are 529 unique players in the dataset."

## Step 2: Convert the possessions into per/100

We do this because generally we interpret player level statistics on
this level. You could pick any number here you want (you probably
shouldn’t though), just make sure that you understand why and how you
picked it.

``` r
possesion_data <- possesion_data %>% 
  mutate(ppp100 = 100 * points/possessions)
```

## Step 3: Convert the possesion file into a sparse matrix

In order to set up our model properly, we need to change the format of
our possesion file. Instead of a column being “Player 1” with a
corresponding ID, we need to create a matrix that has **all** our 529
players as columns, and 1s, 0s, and -1s as the values.

Our first function `make_matrix_rows()`, is applied across the
possesions data frame and gives us a modified one hot matrix with all
the players.

``` r
make_matrix_rows <- function(lineup, players_in) {
  
  player1 <- lineup[1]
  player2 <- lineup[2]
  player3 <- lineup[3]
  player4 <- lineup[4]
  player5 <- lineup[5]
  player6 <- lineup[6]
  player7 <- lineup[7]
  player8 <- lineup[8]
  player9 <- lineup[9]
  player10 <- lineup[10]
  
  zeroRow <- rep(0, length(players_in) * 2)
  
  # OFFENSE #
  zeroRow[which(players_in == player1)] <- 1
  zeroRow[which(players_in == player2)] <- 1
  zeroRow[which(players_in == player3)] <- 1
  zeroRow[which(players_in == player4)] <- 1
  zeroRow[which(players_in == player5)] <- 1
  
  # DEFENSE #
  zeroRow[which(players_in == player6) + length(players_in)] <- -1
  zeroRow[which(players_in == player7) + length(players_in)] <- -1
  zeroRow[which(players_in == player8) + length(players_in)] <- -1
  zeroRow[which(players_in == player9) + length(players_in)] <- -1
  zeroRow[which(players_in == player10) + length(players_in)] <- -1
  
  return(zeroRow)
  
}
```

Extremely important point here. This line
`as(player_matrix, "dgCMatrix")`, converts the matrix from a ‘regular’
matrix to a sparse matrix which is a data structure where enormous
amounts of 0 data is restructured for efficiency. If you don’t do this
step, your model down the line might choke and die. This chunk will take
a second to run. If you want to get fancy, you can create the
`player_matrix` directly as a sparse matrix.

``` r
player_matrix <- t(apply(possesion_data[, 1:10], 1, 
                         function(x) make_matrix_rows(lineup = x, players_in = players)))

player_matrix <- as(player_matrix, "dgCMatrix")

target <- possesion_data$ppp100

print(dim(player_matrix))
```

    ## [1] 243312   1058

You’ll note that the matrix is 529 \* 2 columns wide, as each player
needs an offensive column and defensive column, with the same number of
columns as our possessions data - this is exactly what we want. \#\#
Step 4: Model!

We’re going to use a ridge regression here for our model. If you do not
know what a ridge regression is, I *strongly* recommend buying this
book, [Introduction to Statistical
Learning](https://www.statlearning.com/). Our ridge regression is a
linear model that using some statistical techniques, squishes the
coefficient estimates towards zero and handles multicollinearity well.
The model takes a parameter *λ* (lambda) that controls how much
squishing (the real term is regularization) is conducted. When *λ* = 0,
the model is just OLS, and when the *λ* gets vary large (differs by
model) the coefficients are more regularized. Therefore, we will tune
the values of *λ* to determine what is best.

HUGE BIG IMPORTANT NOTE -&gt; The alpha and lambda parameters for
`sklearn.linear_model.RidgeCV()` and `glmnet()` are not equivalent. [For
a statsy explainer check this
out.](https://stats.stackexchange.com/questions/160096/what-are-the-differences-between-ridge-regression-using-rs-glmnet-and-pythons)

``` r
## this is a cross validated model to pick the lambda
cv_model <- glmnet::cv.glmnet(x = player_matrix, ##ncol = 1058
                           y = target,
                           alpha = 0, 
                           standardize = FALSE)

lam <- cv_model$lambda.min ## best lambda

## this is the model refit using that lambda
coef_model <- glmnet::glmnet(x = player_matrix, 
                          y = target,
                          alpha = 0, 
                          standardize = FALSE,
                          lambda = lam)

player_coefficients <- coef_model$beta ## length = 1058

o_rapm <- player_coefficients[1:length(players)]

d_rapm <- player_coefficients[length(players) + 1:length(players) * 2]

o_rapm_frame <- data.frame("player_id" = players,
                           "o_rapm" = o_rapm)

d_rapm_frame <- data.frame("player_id" = players,
                           "d_rapm" = d_rapm)

rapm <- left_join(o_rapm_frame, d_rapm_frame, by = "player_id") %>% 
  left_join(player_data, by = c("player_id" = "playerId")) %>% 
  mutate(rapm = o_rapm + d_rapm) %>% 
  select(Player = playerName,
         RAPM = rapm,
         `O-RAPM` = o_rapm,
         `D-RAPM` = d_rapm) %>% 
  arrange(-RAPM)

kable(rapm[1:5, ] %>% 
          mutate(across(where(is.numeric), function(x) round(x, 1))), align = "c") 
```

|     Player     | RAPM | O-RAPM | D-RAPM |
|:--------------:|:----:|:------:|:------:|
|  Kevin Durant  | 4.5  |  4.7   |  -0.2  |
| Stephen Curry  | 4.2  |  3.4   |  0.8   |
|  Paul George   | 4.1  |  2.8   |  1.3   |
| Christian Wood | 4.1  |  -0.6  |  4.7   |
| Malik Beasley  | 4.0  |  1.3   |  2.8   |

And there you have it. Very important caveat - single year RAPM, which
is what this is, is extremely unstable. When at all possible, use three
or five year. Further, on Ryan’s site, you see luck adjusted RAPM which
reduces some of the variance, but still aspire to use three or five
years. Some of the leading SPM models such as [LEBRON from Basketball
Index](https://www.bball-index.com/lebron-introduction/), are advanced
combinations of RAPM, box score statistics, and other data.

## Thanks

[Ryan Davis](https://twitter.com/rd11490) [Jake
Flancer](https://twitter.com/JakeFlancer)
