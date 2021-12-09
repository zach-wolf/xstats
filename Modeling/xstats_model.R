library(baseballr)
library(dplyr)
library(tidyverse)
library(DBI)
library(RPostgreSQL)
library(RPostgres)
library(data.table)
library(ggplot2)


#### DATA ####

# statcast db was built with statcast_scrape.R script in the Data folder
statcast_db <- dbConnect(RPostgres::Postgres(), dbname = "statcast")

tbl(statcast_db, 'statcast') %>%
  group_by(game_year) %>%
  count() %>%
  collect()

all_data <- tbl(statcast_db, 'statcast') %>%
  filter(game_date >= '2017-01-01', # first year of Trackman
  spray_angle >= -45,
  spray_angle <= 45 # fair batted balls
  ) %>%
  mutate(IsHit = case_when(is.na(hit) ~ 0,
                           TRUE ~ 1)) %>%
  filter(!is.na(launch_angle) & !is.na(launch_speed)) %>%
  select(#pitch_type, release_speed, spin_dir, stand, p_throws, pfx_x, pfx_z, plate_x, plate_z, release_spin_rate, spin_axis, 
    launch_speed, launch_angle, IsHit) %>%
  collect()

gc()

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.05, .95), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

all_data <- all_data %>%
  mutate_at(all_of(colnames(all_data)), funs(remove_outliers))

all_data$IsHit <- as.factor(all_data$IsHit)

#### BUILDING MODEL ####

library(tidymodels)

set.seed(123)
bb_split <- initial_split(all_data, strata = IsHit)
bb_train <- training(bb_split)
bb_test <- testing(bb_split)

bb_recipe <- recipe(IsHit ~ ., data = bb_train) %>%
  prep()

# xgboost model without tuning
xgb_mod <- boost_tree(
  trees = 1000,
  mode = "classification"
) %>% 
  set_engine("xgboost", nthread = 2)

xgb_wf <- workflow() %>%
  add_model(xgb_mod) %>%
  add_recipe(bb_recipe)

start <- Sys.time()
xgb_fit <- xgb_wf %>%
  fit(data = bb_train)
Sys.time() - start # 3 minutes

xgb_fit %>%
  extract_fit_parsnip()

xgb_pred <-
  predict(xgb_fit, bb_test, type = "prob") %>%
  bind_cols(bb_test)

xgb_pred %>%
  roc_auc(truth = IsHit, .pred_0) # 0.869

xgb_pred %>%
  ppv(truth = IsHit, as.factor(ifelse(xgb_pred$.pred_1 > 0.5, 1, 0))) # 0.830

xgb_fit %>%
  extract_fit_parsnip() %>%
  vip::vip(geom = "point", n = 20)

# xgboost model with tuning
xgb_mod <- 
  boost_tree(
    trees = 1000, 
    tree_depth = tune(), 
    min_n = tune(), 
    loss_reduction = tune(),                     ## first three: model complexity
    learn_rate = tune()                          ## step size
  ) %>% 
  set_engine("xgboost", nthread = 3) %>% 
  set_mode("classification")

xgb_wf <- 
  workflow() %>%
  add_recipe(bb_recipe) %>%
  add_model(xgb_mod)

xgb_params <-
  parameters(
    tree_depth(),
    min_n(),
    loss_reduction(),
    learn_rate()
  )

xgb_grid <- 
  grid_max_entropy(
    xgb_params,
    size = 20
  )

perf_metrics <- metric_set(roc_auc, ppv, npv, mn_log_loss)
ctrl_grid <- control_grid(save_pred = TRUE, save_workflow = TRUE, verbose = TRUE)

set.seed(123)
bb_folds <- vfold_cv(bb_train, strata = IsHit, v = 3)

start <- Sys.time()
set.seed(234)
xgb_tuned <- 
  xgb_wf %>%
  tune_grid(
    resamples = bb_folds,
    grid = xgb_grid,
    metrics = perf_metrics,
    control = ctrl_grid
  )
Sys.time() - start # 3 hours

xgb_tuned %>% show_best("roc_auc", n = 10)

best_xgb <- xgb_tuned %>% select_best("roc_auc")

final_xgb <- finalize_workflow(xgb_wf, best_xgb)
xgb_fit <- final_xgb %>% fit(bb_train)

xgb_fit %>%
  pull_workflow_fit() %>%
  vip::vip(geom = "point", num_features = 20)

last_fit(
  final_xgb,
  bb_split,
  metrics = perf_metrics
) %>%
  collect_metrics()
