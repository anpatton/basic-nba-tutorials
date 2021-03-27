How to Make a Player Level Team Ratings and Win Projections
================
Andrew Patton

## What are Player Level Team Ratings?

Team ratings are a way to determine the strength of a team based on
player level ratings, often statistical plusminus models (SPM). For a
primer on those, check out [this
link](https://github.com/anpatton/basic-nba-tutorials/blob/main/spm/how_to_make_spm_R.md).
Doing team ratings on the player level means that you are not just
adding up points scored per 100 and subtracting points against per 100,
as that is team performance data. The player level team ratings are
often used for projection, gambling, etc. You might have seen versions
of these from PIPM (RIP), DARKO, 538, etc. Like most basketball
analytics approaches, there are a variety of ways to accomplish the
task, and this is but one simplified version. Let’s get into it.

## What are Win Projections?

Simply, win projections are estimates of how many games a team will win
over \_\_\_ period of time. Can be season long projections, daily
projections, etc. Our projections will be based on our player level team
ratings that we will calculate first.

## Data

1.  player\_ratings\_minutes.csv" - This is DARKO player ratings from
    day one of the 2018-19 season with projected minutes as well.
2.  “schedule.csv” - This is the league schedule from 2018-19.

``` r
library(tidyverse)
library(knitr)

ratings <- read_csv("data/player_ratings_minutes.csv") %>% 
  mutate(dpm = o_dpm + d_dpm) %>% 
  arrange(-dpm) %>% 
  mutate(team_name = ifelse(team_name == "Los Angeles Clippers", "LA Clippers", team_name))
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   nba_id = col_double(),
    ##   player_name = col_character(),
    ##   team_name = col_character(),
    ##   o_dpm = col_double(),
    ##   d_dpm = col_double(),
    ##   minutes = col_double()
    ## )

``` r
kable(ratings[1:10, ] %>% 
          mutate(across(where(is.numeric), function(x) round(x, 1)))) 
```

| nba\_id | player\_name      | team\_name             | o\_dpm | d\_dpm | minutes | dpm |
|--------:|:------------------|:-----------------------|-------:|-------:|--------:|----:|
|  201939 | Stephen Curry     | Golden State Warriors  |    6.4 |    1.1 |    39.2 | 7.5 |
|  101108 | Chris Paul        | Houston Rockets        |    4.7 |    1.8 |    20.3 | 6.5 |
|  201566 | Russell Westbrook | Oklahoma City Thunder  |    5.6 |    0.4 |    40.4 | 6.0 |
|    2544 | LeBron James      | Los Angeles Lakers     |    5.7 |    0.3 |    37.4 | 6.0 |
|  201935 | James Harden      | Houston Rockets        |    6.7 |   -0.7 |    39.7 | 6.0 |
|  201142 | Kevin Durant      | Golden State Warriors  |    6.0 |   -0.5 |    39.7 | 5.5 |
|  203999 | Nikola Jokic      | Denver Nuggets         |    3.8 |    1.7 |    39.2 | 5.5 |
|  202695 | Kawhi Leonard     | Toronto Raptors        |    3.2 |    1.7 |    22.4 | 4.8 |
|  203506 | Victor Oladipo    | Indiana Pacers         |    2.9 |    1.7 |    37.6 | 4.6 |
|  202710 | Jimmy Butler      | Minnesota Timberwolves |    3.2 |    1.3 |    32.8 | 4.5 |

You’ll note that Russ has an extremely high DPM. It starts to decrease
somewhat quickly following this point due to three years of strong
on-off data.

## Step 1: Aggregate player ratings to the team level and calculate team ratings

This is a deceptively simple step that does literally does everything!
We multiply a player’s minutes projection by their O-DPM and D-DPM, and
then by team, add up the offensive and defensive values as well as the
total minutes projected. Then, we divide the team offensive and
defensive values by the teams’ total projected minutes and multiply by
five (for five players on court). That’s it! Add up the offensive and
defensive ratings and you have your net team ratings.

``` r
team_ratings <- ratings %>% 
  replace_na(list(minutes = 0)) %>% 
  mutate(o_value = minutes * o_dpm) %>% 
  mutate(d_value = minutes * d_dpm) %>%
  group_by(team_name) %>% 
  summarise(team_o_value = sum(o_value),
            team_d_value = sum(d_value),
            team_minutes = sum(minutes)) %>% 
  mutate(ortg = (team_o_value/team_minutes) * 5,
         drtg = (team_d_value/team_minutes) * 5,
         nrtg = ortg + drtg) %>% 
  mutate(across(where(is.numeric), function(x) round(x, 1))) %>% 
  ungroup() %>% 
  select(team_name, ortg, drtg, nrtg) %>% 
  arrange(-nrtg) 

kable(team_ratings)
```

| team\_name             | ortg | drtg | nrtg |
|:-----------------------|-----:|-----:|-----:|
| Golden State Warriors  |  8.5 |  3.8 | 12.3 |
| Oklahoma City Thunder  |  4.5 |  2.2 |  6.7 |
| Toronto Raptors        |  2.7 |  2.7 |  5.4 |
| Milwaukee Bucks        |  2.7 |  1.9 |  4.6 |
| New Orleans Pelicans   |  2.6 |  1.6 |  4.3 |
| Utah Jazz              | -1.0 |  4.9 |  3.9 |
| Houston Rockets        |  3.7 |  0.1 |  3.7 |
| Boston Celtics         |  0.6 |  2.4 |  3.0 |
| Denver Nuggets         |  4.3 | -1.3 |  3.0 |
| Minnesota Timberwolves |  3.3 | -0.5 |  2.8 |
| Portland Trail Blazers |  1.7 |  0.8 |  2.5 |
| Philadelphia 76ers     |  1.0 |  1.1 |  2.1 |
| Indiana Pacers         |  0.4 |  1.6 |  2.0 |
| Washington Wizards     |  2.0 | -0.6 |  1.4 |
| Los Angeles Lakers     |  0.6 | -0.2 |  0.4 |
| Miami Heat             | -1.4 |  1.8 |  0.4 |
| San Antonio Spurs      | -0.9 |  0.1 | -0.8 |
| Detroit Pistons        | -0.5 | -0.4 | -0.9 |
| Charlotte Hornets      | -0.2 | -2.0 | -2.2 |
| LA Clippers            | -0.4 | -2.4 | -2.8 |
| Dallas Mavericks       | -1.9 | -1.1 | -3.0 |
| Brooklyn Nets          | -1.2 | -2.0 | -3.2 |
| Memphis Grizzlies      | -2.0 | -1.5 | -3.4 |
| Cleveland Cavaliers    | -2.4 | -2.5 | -4.9 |
| Orlando Magic          | -3.7 | -1.4 | -5.1 |
| Sacramento Kings       | -4.1 | -2.1 | -6.2 |
| New York Knicks        | -3.1 | -3.8 | -6.9 |
| Chicago Bulls          | -4.2 | -3.2 | -7.4 |
| Atlanta Hawks          | -6.0 | -1.6 | -7.6 |
| Phoenix Suns           | -3.8 | -4.2 | -8.0 |

## Step 2: Get What we Need to Project Wins

In order to figure out the number of wins a team should have, we first
need the schedule. I have scraped that already for the 2018-19 season in
the “schedule.csv” and conveniently formatted it for our purposes.

``` r
schedule <- read_csv("data/schedule.csv") 

head(schedule)
```

    ## # A tibble: 6 x 3
    ##    game_id away_team             home_team            
    ##      <dbl> <chr>                 <chr>                
    ## 1 21800001 Philadelphia 76ers    Boston Celtics       
    ## 2 21800002 Oklahoma City Thunder Golden State Warriors
    ## 3 21800008 Cleveland Cavaliers   Toronto Raptors      
    ## 4 21800003 Milwaukee Bucks       Charlotte Hornets    
    ## 5 21800011 Utah Jazz             Sacramento Kings     
    ## 6 21800005 Memphis Grizzlies     Indiana Pacers

You’ll note that home and away teams are there, as home teams are more
likely to win. The raw team ratings assume a neutral court scenario.
Let’s join in the team ratings to the schedule.

``` r
schedule_with_ratings <- schedule %>% 
  left_join(team_ratings, by = c("home_team" = "team_name")) %>% 
  left_join(team_ratings, by = c("away_team" = "team_name"), suffix = c("_home", "_away")) 

head(schedule_with_ratings)
```

    ## # A tibble: 6 x 9
    ##   game_id away_team home_team ortg_home drtg_home nrtg_home ortg_away drtg_away
    ##     <dbl> <chr>     <chr>         <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
    ## 1  2.18e7 Philadel… Boston C…       0.6       2.4       3         1         1.1
    ## 2  2.18e7 Oklahoma… Golden S…       8.5       3.8      12.3       4.5       2.2
    ## 3  2.18e7 Clevelan… Toronto …       2.7       2.7       5.4      -2.4      -2.5
    ## 4  2.18e7 Milwauke… Charlott…      -0.2      -2        -2.2       2.7       1.9
    ## 5  2.18e7 Utah Jazz Sacramen…      -4.1      -2.1      -6.2      -1         4.9
    ## 6  2.18e7 Memphis … Indiana …       0.4       1.6       2        -2        -1.5
    ## # … with 1 more variable: nrtg_away <dbl>

Ok, with that done, our setup is ready to go. All we need to do is
actually figure out how to translate ratings to win probability. We’re
going to cheat a bit here and use some previously conducted research on
the topic by some of my NBA Twitter colleagues/friends/collaborators.

1.  Rating difference and win/loss was plugged into a logistic
    regression across many many games and home/away splits. The
    resulting function takes the coefficients and manually conducts a
    prediction.

``` r
get_win_prob <- function(home_rating, away_rating, sd) {
  
  hca <- 2.7 ## THIS IS DIFFERENT FOR COVID SEASONS
  home_rating_adj <- rnorm(mean = home_rating, sd = sd, n = 1)
  away_rating_adj <- rnorm(mean = away_rating, sd = sd, n = 1)
  delta <- home_rating_adj - away_rating_adj - hca
  coef1 <- 0.1483737284
  coef2 <- 0.4257284470
  home_win_prob <- 1/(1 + exp(-(coef2 + coef1 * delta)))
  
  return(home_win_prob)
  
}
```

You’ll note that there is an “sd” parameter that we have not discussed.
This is approximately the standard deviation of the team ratings based
on the uncertainty in the player ratings based again on prior research.
Essentially, when we say that Steph is a +7.0, we also imply that he is
+7.0 +/- some value. No player is *exactly* what they are predicted at.

Next, we need to build a function that simulates a season of games. This
one is kind of hacky but does the trick. Like always, I’m sure there’s a
better/faster/simpler way to do some of this, but this is my first pass
and it seems to work well. It takes in the schedule dataframe and the
value for the sd and outputs a list of 1s and 0s where the home team is
predicted to win across all the games. Important note - if we assume
that the team ratings are normally distributed we can actually skip the
simulation. However, I wanted to show the simulation code since that’s
probably more of use to people than various forms of \_norm.

``` r
simulate_season <- function(schedule, sd = 2.2) {
  
  home_rating_col <- which(names(schedule) == "nrtg_home")
  away_rating_col <- which(names(schedule) == "nrtg_away")
  home_win_probs <- unname(apply(schedule, 1, 
                                 function(x) get_win_prob(as.numeric(x[home_rating_col]),
                                                          as.numeric(x[away_rating_col]), 
                                                          sd = sd)))
  home_win_checks <- runif(n = length(home_win_probs))
  home_win <- as.numeric(home_win_probs >= home_win_checks)
  ## Good suggestion by Jake Flancer to just use rbinom() and save some code!
  
  return(home_win)
  
}
```

Now, we’re going to do a Monte Carlo (simulation) where we just run the
simulation a few hundred times to see what shakes out. If we assumed the
team ratings were deterministic, point predictions with no variace, this
wouldn’t be necessary. This is even more important when you aren’t just
doing Baby’s First Win Projections.

We’re going to do this in parallel as it’s a perfect use case of
completely separate trials that can be split across your cores. I’ll
annotate the code here if you’re new to parallelization.

``` r
cores_minus_one <- parallel::detectCores() - 1 ## How many cores you have minus one (don't want to overdo it)
cl <- parallel::makeCluster(spec = cores_minus_one) ## Creating a cluster instance
parallel::clusterExport(cl, c("get_win_prob", "schedule_with_ratings", "simulate_season")) ## export our two functions and the data to the cluster
home_wins <- parallel::parSapply(cl, 1:100, function(i) simulate_season(schedule = schedule_with_ratings)) ## literally just sapply, but on our cluster for 100 trials
parallel::stopCluster(cl) ## close down the cluster when done
```

Great, that hopefully worked on your machine. Each column is a season
and each row is a game. If you’re a Mac user, check out
parallel::mclapply which is specifically targeted for Monte Carlos but
does not work on Windows due to CS terms over my head. Important note
here, while we don’t know the order in which the parallelized trials
will occur, *within* the simulate season function, the game order is
preserved.

Next, we tally up wins and losses by each team and do some summing

``` r
home_wins_frame <- cbind(select(schedule_with_ratings, home_team), 
                         as.data.frame(home_wins)) %>% 
  group_by(home_team) %>% 
  summarise_all(sum) %>% 
  rename(team = home_team)


away_wins_frame <- cbind(select(schedule_with_ratings, away_team), 
                         as.data.frame(1 * !home_wins)) %>% 
  group_by(away_team) %>% 
  summarise_all(sum) %>% 
  rename(team = away_team)

wins_frame <- bind_rows(home_wins_frame, away_wins_frame) %>%
  ungroup() %>% 
  group_by(team) %>%
  summarise_all(sum)
```

With our one column per season and one team per row result, we can now
take a look at the variation within a team how many games a they might
win.

``` r
projections <- apply(wins_frame[, -1], 1, function(x) data.frame(mean = mean(x),
                                                                 sd = sd(x))) %>% 
  bind_rows() %>% 
  bind_cols(wins_frame[, 1], .) %>% 
  mutate(best_case = qnorm(p = 0.95, mean = .$mean, sd = .$sd)) %>% 
  mutate(worst_case = qnorm(p = 0.05, mean = .$mean, sd = .$sd)) %>% 
  mutate(across(where(is.numeric), function(x) round(x, 1))) %>% 
  arrange(-mean)

kable(projections)  
```

| team                   | mean |  sd | best\_case | worst\_case |
|:-----------------------|-----:|----:|-----------:|------------:|
| Golden State Warriors  | 69.3 | 3.4 |       74.9 |        63.8 |
| Oklahoma City Thunder  | 58.0 | 3.9 |       64.4 |        51.6 |
| Toronto Raptors        | 55.9 | 4.2 |       62.7 |        49.0 |
| Milwaukee Bucks        | 55.2 | 3.8 |       61.5 |        49.0 |
| New Orleans Pelicans   | 52.6 | 4.6 |       60.1 |        45.0 |
| Utah Jazz              | 52.3 | 4.5 |       59.8 |        44.9 |
| Boston Celtics         | 50.7 | 3.8 |       56.9 |        44.5 |
| Houston Rockets        | 49.7 | 4.1 |       56.4 |        43.1 |
| Indiana Pacers         | 49.1 | 3.9 |       55.6 |        42.7 |
| Philadelphia 76ers     | 48.4 | 4.4 |       55.6 |        41.1 |
| Denver Nuggets         | 47.7 | 4.5 |       55.2 |        40.3 |
| Portland Trail Blazers | 47.3 | 3.8 |       53.4 |        41.1 |
| Minnesota Timberwolves | 47.2 | 4.4 |       54.4 |        39.9 |
| Washington Wizards     | 46.6 | 4.6 |       54.1 |        39.1 |
| Miami Heat             | 42.5 | 4.6 |       50.1 |        35.0 |
| Los Angeles Lakers     | 41.4 | 4.5 |       48.9 |        33.9 |
| Detroit Pistons        | 39.8 | 4.1 |       46.5 |        33.2 |
| San Antonio Spurs      | 39.2 | 3.8 |       45.5 |        32.9 |
| Charlotte Hornets      | 36.4 | 4.3 |       43.5 |        29.3 |
| Brooklyn Nets          | 33.4 | 4.5 |       40.7 |        26.1 |
| LA Clippers            | 32.7 | 4.2 |       39.7 |        25.8 |
| Dallas Mavericks       | 31.8 | 4.1 |       38.5 |        25.1 |
| Memphis Grizzlies      | 31.1 | 3.9 |       37.6 |        24.6 |
| Orlando Magic          | 29.0 | 3.8 |       35.3 |        22.8 |
| Cleveland Cavaliers    | 28.8 | 4.5 |       36.3 |        21.4 |
| Sacramento Kings       | 24.0 | 4.4 |       31.2 |        16.9 |
| Chicago Bulls          | 23.7 | 3.8 |       29.9 |        17.4 |
| New York Knicks        | 23.3 | 4.0 |       29.8 |        16.7 |
| Atlanta Hawks          | 22.3 | 4.3 |       29.3 |        15.3 |
| Phoenix Suns           | 20.5 | 4.2 |       27.4 |        13.5 |

And there you have it, win projections done as simply as possible!
