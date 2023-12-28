library(readr)
library(dplyr)
library(caret)
library(xgboost)
library(vip)
library(gt)

wimbledon_pbp <- data.frame()
wimbledon_matches <- data.frame()

for (year in 2017:2023) {
    if(!(year == 2020)) {
      comp_data <- read_csv(url(paste0("https://raw.githubusercontent.com/JeffSackmann/tennis_slam_pointbypoint/master/", year, "-wimbledon-points.csv")))
      match_data <- read_csv(url(paste0("https://raw.githubusercontent.com/JeffSackmann/tennis_slam_pointbypoint/master/", year, "-wimbledon-matches.csv")))
    }
    wimbledon_pbp <- rbind(wimbledon_pbp, comp_data)
    wimbledon_matches <- rbind(wimbledon_matches, match_data)
}

wimbledon_pbp <- wimbledon_pbp %>%
  distinct(match_id, PointNumber, .keep_all = TRUE)

data <- wimbledon_pbp %>%
  mutate(gender = ifelse(as.numeric(substring(match_id, 16, 16)) == 1, "M", "F"), year = as.numeric(substring(match_id, 1, 4)), ace = P1Ace + P2Ace, game_score = ifelse(PointServer == 1, paste0(P1Score, " vs ", P2Score), paste0(P2Score, " vs ", P1Score))) %>%
  group_by(match_id) %>%
  mutate(momentum_diff = ifelse(PointServer == 1, lag(P1Momentum - P2Momentum), lag(P2Momentum - P1Momentum)), double_faults = ifelse(PointServer == 1, cumsum(P1DoubleFault) - P1DoubleFault, cumsum(P2DoubleFault) - P2DoubleFault)) %>%
  filter(PointServer != 0) %>%
  mutate(serves = ifelse(PointServer == 1, cumsum(PointServer == 1) - 1, cumsum(PointServer == 2) - 1), double_fault_pct = double_faults/serves) %>%
  ungroup() %>%
  select(match_id, gender, num = PointNumber, server = PointServer, year, ace, set = SetNo, speed = Speed_KMH, momentum_diff, double_fault_pct, serve_width = ServeWidth, serve_depth = ServeDepth)

data$double_fault_pct[which(is.na(data$double_fault_pct))] <- 0
data$set <- as.factor(data$set)
data$gender <- as.factor(data$gender)
data$serve_width <- as.factor(data$serve_width)
data$serve_depth <- as.factor(data$serve_depth)

dummy <- dummyVars(" ~ .", data = data[,c(2,7,11,12)])
factor <- data.frame(predict(dummy, newdata = data[,c(2,7,11,12)]))

data <- data %>%
  select(-gender, -set, -serve_width, -serve_depth)

data <- cbind(data, factor)

data[(is.na(data))] <- 0

xgboost_train <- data %>%
  filter(year != 2023)

xgboost_test <- data %>%
  filter(year == 2023)

labels_train <- as.matrix(xgboost_train[,5])
xgboost_trainfinal <- as.matrix(xgboost_train[, c(6:22)])
xgboost_testfinal <- as.matrix(xgboost_test[, c(6:22)])

aoe_model <- xgboost(data = xgboost_trainfinal, label = labels_train, nrounds = 100, objective = "binary:logistic", early_stopping_rounds = 10, max_depth = 6, eta = 0.3)

vip(aoe_model)
vi(aoe_model)

a_predict <- predict(aoe_model, xgboost_testfinal)
a_actual <- as.matrix(xgboost_test[,5])
postResample(a_predict, a_actual)

a_predictions <- as.data.frame(
  matrix(predict(aoe_model, as.matrix(data[,c(6:22)])))
)

stats_with_pred <- cbind(data, a_predictions) 

matches <- wimbledon_matches %>%
  select(match_id, player1, player2)

stats_matches <- left_join(stats_with_pred, matches, by = "match_id")

stats_matches <- stats_matches %>%
  distinct(match_id, num, .keep_all = TRUE)

stats <- stats_matches %>%
  mutate(gender = ifelse(gender.F == 1, "F", "M"), server = ifelse(server == 1, player1, player2)) %>%
  group_by(year, server, gender) %>%
  summarize(pts_played = n(), aces = sum(ace), pred_aces = sum(V1), ace_pct = aces/pts_played * 100, pred_ace_pct = pred_aces/pts_played * 100, aoe = ace_pct - pred_ace_pct)

male_stats_2023 <- stats %>%
  filter(year == 2023, gender == "M") %>%
  mutate(pred_aces = round(pred_aces, 2), ace_pct = round(ace_pct, 2), pred_ace_pct = round(pred_ace_pct, 2), aoe = round(aoe, 2)) 

female_stats_2023 <- stats %>%
  filter(year == 2023, gender == "F") %>%
  mutate(pred_aces = round(pred_aces, 2), ace_pct = round(ace_pct, 2), pred_ace_pct = round(pred_ace_pct, 2), aoe = round(aoe, 2)) 

maletop <- male_stats_2023 %>%
  arrange(-aoe) %>%
  head(10) %>%
  ungroup() %>%
  select(-year, -gender)

femaletop <- female_stats_2023 %>%
  arrange(-aoe) %>%
  head(10) %>%
  ungroup() %>%
  select(-year, -gender)

malebot <- male_stats_2023 %>%
  arrange(aoe) %>%
  head(10) %>%
  ungroup() %>%
  select(-year, -gender)

femalebot <- female_stats_2023 %>%
  arrange(aoe) %>%
  head(10) %>%
  ungroup() %>%
  select(-year, -gender)

maletop %>% gt() %>% 
  cols_align(
    align = "center",
    everything()
  ) %>%
  data_color(
    columns = setdiff(names(maletop), "server"),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    server = md("**Server**"),
    pts_played = md("**Points Played**"),
    aces = md("**Aces**"),
    pred_aces = md("**Pred. Aces**"),
    ace_pct = md("**Ace %**"),
    pred_ace_pct = md("**Pred. Ace %**"),
    aoe = md("**AOE**")
  ) %>%
  tab_header(
    title = md("**2023 Male Wimbledon Top 10 AOE (Aces Over Expected)**"),
    subtitle = "Trained Data From 2017 to 2022 (No 2020)"
  ) 

femaletop %>% gt() %>% 
  cols_align(
    align = "center",
    everything()
  ) %>%
  data_color(
    columns = setdiff(names(femaletop), "server"),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    server = md("**Server**"),
    pts_played = md("**Points Played**"),
    aces = md("**Aces**"),
    pred_aces = md("**Pred. Aces**"),
    ace_pct = md("**Ace %**"),
    pred_ace_pct = md("**Pred. Ace %**"),
    aoe = md("**AOE**")
  ) %>%
  tab_header(
    title = md("**2023 Female Wimbledon Top 10 AOE (Aces Over Expected)**"),
    subtitle = "Trained Data From 2017 to 2022 (No 2020)"
  ) 

malebot %>% gt() %>% 
  cols_align(
    align = "center",
    everything()
  ) %>%
  data_color(
    columns = setdiff(names(malebot), c("server", "aoe")),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL,
    )
  ) %>%
  data_color(
    columns = aoe,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL,
      reverse = TRUE
    )
  ) %>%
  cols_label(
    server = md("**Server**"),
    pts_played = md("**Points Played**"),
    aces = md("**Aces**"),
    pred_aces = md("**Pred. Aces**"),
    ace_pct = md("**Ace %**"),
    pred_ace_pct = md("**Pred. Ace %**"),
    aoe = md("**AOE**")
  ) %>%
  tab_header(
    title = md("**2023 Male Wimbledon Bottom 10 AOE (Aces Over Expected)**"),
    subtitle = "Trained Data From 2017 to 2022 (No 2020)"
  ) 

femalebot %>% gt() %>% 
  cols_align(
    align = "center",
    everything()
  ) %>%
  data_color(
    columns = setdiff(names(femalebot), c("server", "aoe")),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL,
    )
  ) %>%
  data_color(
    columns = aoe,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL,
      reverse = TRUE
    )
  ) %>%
  cols_label(
    server = md("**Server**"),
    pts_played = md("**Points Played**"),
    aces = md("**Aces**"),
    pred_aces = md("**Pred. Aces**"),
    ace_pct = md("**Ace %**"),
    pred_ace_pct = md("**Pred. Ace %**"),
    aoe = md("**AOE**")
  ) %>%
  tab_header(
    title = md("**2023 Female Wimbledon Bottom 10 AOE (Aces Over Expected)**"),
    subtitle = "Trained Data From 2017 to 2022 (No 2020)"
  ) 

