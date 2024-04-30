library (tidyverse)
library(ggthemes)
library(ranger)
library(vip)
library(caret)
library(xgboost)
library(randomForest)
library(DiagrammeR)
options(scipen = 9999)
library(dplyr)
library(stringr)

# read in 2019 data
pbp_2019 <- read.csv("/Users/colemanbahr/Downloads/retrosheets_events_2019.csv") # Read Data
# pull only cols you need
pbp_2019 <- pbp_2019 %>%
  select(game_id, event_id, result_batter_id, result_batter_hand, result_pitcher_id, result_pitcher_hand, batter_event, ab,
         hit_val, sac_fly, batted_ball_type, bunt)
pbp_2019 <- pbp_2019 %>%
  filter(batter_event == "t" & bunt == "f")

pbp_2019 <- pbp_2019 %>%
  mutate(ab_numeric = ifelse(ab == "t", 1, 0))


#pull in park data from fangraphs 
park_data <- read.csv("/Users/colemanbahr/Downloads/Fangraphs Leaderboard.csv")
#pull in park data from retrosheet
retrosheet_parks <- read.csv("/Users/colemanbahr/Downloads/retrosheets_parks.csv")
#pull in game info
game_info <- read.csv("/Users/colemanbahr/Downloads/retrosheets_game_info.csv")
game_info <- game_info %>%
  rename(game_id = id)

# Perform a left join to add park_id to pbp_2019
pbp_2019 <- pbp_2019 %>%
  left_join(game_info %>% select(game_id, park_id), by = "game_id")

#pull in team data from retrosheet
retrosheet_teams <- read.csv("/Users/colemanbahr/Downloads/retrosheets_teams.csv")
retrosheet_teams <- retrosheet_teams %>%
  filter(last_date == "")

retrosheet_teams <- retrosheet_teams %>%
  rename(Team = name2)

park_data <- park_data %>%
  left_join(retrosheet_teams %>% select(id, Team), by = "Team")

retrosheet_parks$teamid <- substr(retrosheet_parks$id, 1, 3)
retrosheet_parks$parknum <- substr(retrosheet_parks$id, nchar(retrosheet_parks$id) - 1, nchar(retrosheet_parks$id))

retrosheet_parks <- retrosheet_parks %>%
  filter(league_id != "" & end_date == "")
team_info <- read.csv("/Users/colemanbahr/Downloads/retrosheets_game_info.csv")
team_info_2019 <- team_info %>%
  filter(year(game_date) == 2019)

team_info_2019 <- team_info_2019 %>%
  rename(id2 = home_team)

park_data <- park_data %>%
  rename(id2 = id)

park_data <- park_data %>%
  left_join(team_info_2019 %>% select(id2, park_id), by = "id2")
library(dplyr)


park_data <- park_data %>%
  filter(park_id != "")

park_data <- park_data %>%
  distinct(Season, Team,park_id,.keep_all = TRUE)

pbp_2019 <- pbp_2019 %>%
  left_join(park_data %>% select(park_id, HR.as.R), by = "park_id")

pbp_2019 <- pbp_2019 %>%
  left_join(game_info %>% select(game_id, temperature, wind_speed, wind_dir), by = "game_id")

pbp_2019 <- pbp_2019 %>%
  left_join(game_info %>% select(game_id,game_date), by = "game_id")


pbp_2019 <- pbp_2019 %>%
  mutate(hit = ifelse(hit_val > 0, 1, 0))

pbp_2019 <- pbp_2019 %>%
  arrange(result_batter_id, game_date, event_id) %>%
  group_by(result_batter_id) %>%
  mutate(cumulative_hits = cumsum(hit))

pbp_2019 <- pbp_2019 %>%
  arrange(result_batter_id, game_date, event_id) %>%
  group_by(result_batter_id) %>%
  mutate(cumulative_abs = cumsum(ab_numeric))

# Assuming pbp_2019 is your data frame
result <- pbp_2019 %>%
  arrange(result_batter_id, game_date, event_id) %>%
  group_by(result_batter_id) %>%
  mutate(
    cumulative_bathits_last_500 = cumsum(row_number() > n() - 600 & ab == "t" & hit == 1),
    cumulative_batab_last_500 = cumsum(row_number() > n() - 600 & ab == "t"),
    cumulative_batavg_last_500 = cumulative_bathits_last_500 / cumulative_batab_last_500,
    cumulative_batbases_last_500 = cumsum(hit_val * (row_number() > n() - 600 & ab == "t")),
    cumulative_batslg_last_500 = cumulative_batbases_last_500 / cumulative_batab_last_500) %>%
  ungroup() %>%
  filter(!is.na(cumulative_batavg_last_500) & !is.na(cumulative_batslg_last_500))

result <- result %>%
  mutate(isolatedpower = cumulative_batslg_last_500 - cumulative_batavg_last_500)

result2 <- pbp_2019 %>%
  arrange(result_pitcher_id, game_date) %>%
  group_by(result_pitcher_id) %>%
  mutate(
    cumulative_pithits_last_500 = cumsum(row_number() > n() - 600 & ab == "t" & hit == 1),
    cumulative_pitab_last_500 = cumsum(row_number() > n() - 600 & ab == "t"),
    cumulative_pitavg_last_500 = cumulative_pithits_last_500 / cumulative_pitab_last_500,
    cumulative_pitbases_last_500 = cumsum(hit_val * (row_number() > n() - 600 & ab == "t")),
    cumulative_pitslg_last_500 = cumulative_pitbases_last_500 / cumulative_pitab_last_500) %>%
  ungroup() %>%
  filter(!is.na(cumulative_pitavg_last_500) & !is.na(cumulative_pitslg_last_500))

result2 <- result2 %>%
  mutate(pitchiso = cumulative_pitslg_last_500 - cumulative_pitavg_last_500)

result2 <- result2 %>%
  mutate(link = paste(game_id, event_id, sep = "-"))

result <- result %>%
  mutate(link = paste(game_id, event_id, sep = "-"))
  
result<- result %>%
  left_join(result2 %>% select(link, cumulative_pitavg_last_500,pitchiso ), by = "link")


result <- result %>%
  mutate(hr_factor = ifelse(result_batter_hand == "l",HR.as.L.y,HR.as.R.y))

# Assuming df is your data frame
final_result <- result %>%
  filter(cumulative_batab_last_500 >= 450)

# Assuming df is your data frame
final_result <- final_result %>%
  mutate(hr = ifelse(hit_val == 4, 1, 0))

colSums(is.na(final_result))
final_result <- na.omit(final_result)

sample_data <- final_result[sample(nrow(final_result), 0.7 * nrow(final_result)), ]
homerun_rf <- randomForest::randomForest(as.factor(hr) ~ cumulative_batavg_last_500 + temperature + wind_speed + wind_dir + cumulative_pitavg_last_500.y + pitchiso.y + isolatedpower+hr_factor, data = final_result)
vip(homerun_rf)
hr_preds <- predict(homerun_rf, type = "prob")
hr_preds_joined <- cbind(final_result, hr_preds)


write.csv(hr_preds_joined, "hrpreds.csv")
# putting off woba for now







