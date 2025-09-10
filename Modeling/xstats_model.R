library(baseballr)
library(dplyr)
library(tidyverse)
library(tidymodels)
library(DBI)
library(RPostgreSQL)
library(RPostgres)
library(data.table)
library(ggplot2)

#### DATA ####

# Pull each season separately
seasons <- 2021:2024

get_season_data <- function(year) {
  start_date <- paste0(year, "-03-01")  # Spring training
  end_date <- paste0(year, "-10-01")    # World Series
  
  # Break season into months
  monthly_starts <- seq(as.Date(start_date), as.Date(end_date), by = "month")
  monthly_ends <- monthly_starts + months(1) - days(1)
  
  map2_dfr(monthly_starts, monthly_ends, safe_statcast)
}

all_seasons <- rbind(all_seasons, all_seasons1)

all_data <- all_seasons %>%
  filter(!str_detect(description, regex("foul", ignore_case = TRUE)) # fair batted balls
  ) %>%
  mutate(IsHit = ifelse(events == "single", 1,
                        ifelse(events == "double", 1,
                               ifelse(events == "triple", 1, 
                                      ifelse(events == "home_run", 1, 0))))) %>%
  filter(!is.na(launch_angle) & !is.na(launch_speed)) %>%
  select(#pitch_type, release_speed, spin_dir, stand, p_throws, pfx_x, pfx_z, plate_x, plate_z, release_spin_rate, spin_axis, 
    launch_speed, launch_angle, IsHit)

remove_outliers <- function(x, na.rm = TRUE) {
  if(!is.numeric(x)) return(x)
  
  bounds <- quantile(x, probs = c(0.01, 0.99), na.rm = na.rm)
  y <- x
  y[x < bounds[1] | x > bounds[2]] <- NA
  return(y)
}

all_data <- all_data %>%
  mutate(across(where(is.numeric), remove_outliers)) %>%
  filter(!is.na(launch_speed), !is.na(launch_angle))

all_data$IsHit <- as.factor(all_data$IsHit)

#### BUILDING MODEL ####

set.seed(123)
bb_split <- initial_split(all_data, strata = IsHit)
bb_train <- training(bb_split)
bb_test <- testing(bb_split)

bb_recipe <- recipe(IsHit ~ ., data = bb_train)

# xgboost model without tuning
xgb_mod <- boost_tree(
  trees = 1000,
  mode = "classification"
) %>% 
  set_engine("xgboost", nthread = 10)

xgb_wf <- workflow() %>%
  add_model(xgb_mod) %>%
  add_recipe(bb_recipe)

start <- Sys.time()
xgb_fit <- xgb_wf %>%
  fit(data = bb_train)
Sys.time() - start

xgb_fit %>%
  extract_fit_parsnip()

xgb_pred <-
  predict(xgb_fit, bb_test, type = "prob") %>%
  bind_cols(bb_test)

xgb_pred %>%
  roc_auc(truth = IsHit, .pred_0) # 0.853

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
  set_engine("xgboost", nthread = 10) %>% 
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
bb_folds <- vfold_cv(bb_train, strata = IsHit, v = 5)

start <- Sys.time()
set.seed(123)
xgb_tuned <- 
  xgb_wf %>%
  tune_grid(
    resamples = bb_folds,
    grid = xgb_grid,
    metrics = perf_metrics,
    control = ctrl_grid
  )
Sys.time() - start

xgb_tuned %>% show_best(metric = "roc_auc", n = 10)

best_xgb <- xgb_tuned %>% select_best(metric = "roc_auc")

final_xgb <- finalize_workflow(xgb_wf, best_xgb)
xgb_fit <- final_xgb %>% fit(bb_train)

vip_plot <- xgb_fit %>%
  extract_fit_parsnip() %>%
  vip::vip(geom = "point", num_features = 20)

ggsave(plot = vip_plot,
       filename = "Visualizations/vip_plot.png",
       width = 7,
       height = 5)

last_fit(
  final_xgb,
  bb_split,
  metrics = perf_metrics
) %>%
  collect_metrics()

# ppv:      0.836
# npv:      0.737
# AUC:      0.868
# logloss:  0.415

saveRDS(xgb_fit, "Modeling/xstats_xgb_tuned.Rds")

xgb_pred <-
  predict(xgb_fit, bb_test, type = "prob") %>%
  bind_cols(bb_test)

prediction_plot <- xgb_pred %>%
  mutate(Xcoord = launch_speed / 120 *
           cos(launch_angle * pi / 180),
         Ycoord = launch_speed / 120 *
           sin(launch_angle * pi / 180)) %>%
  ggplot(aes(x = Xcoord, y = Ycoord, color = .pred_1)) +
  geom_point() +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5) +
  
  # Launch angle reference lines
  geom_segment(x = 0, y = 0, xend = 0.985, yend = 0.174, color = "black", linetype = "dashed") +
  geom_segment(x = 0, y = 0, xend = 0.906, yend = 0.423, color = "black", linetype = "dashed") +
  geom_segment(x = 0, y = 0, xend = 0.643, yend = 0.766, color = "black", linetype = "dashed") +
  
  # Exit velocity reference lines
  geom_curve(x = 0, y = 0.83333, xend = 0, yend = -0.83333, curvature = -1, color = "black", linetype = "dashed") +
  geom_curve(x = 0, y = 0.5, xend = 0, yend = -0.5, curvature = -1, color = "black", linetype = "dashed") +
  
  # Launch angle labels - positioned slightly outside the segments
  annotate("text", x = 1.05, y = 0.186, label = "10°", size = 3, color = "black", angle = 10) +
  annotate("text", x = 0.97, y = 0.453, label = "25°", size = 3, color = "black", angle = 25) +
  annotate("text", x = 0.685, y = 0.815, label = "50°", size = 3, color = "black", angle = 50) +
  
  # Exit velocity labels - positioned on the curves
  annotate("text", x = -0.125, y = 0.83333, label = "100 mph", size = 3, color = "black") +
  annotate("text", x = -0.125, y = 0.5, label = "60 mph", size = 3, color = "black") +
  
  coord_fixed() +
  scale_x_continuous(limits = c(-0.25, 1.25)) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(
    title = "Expected Hit Probability by Launch Angle & Exit Velocity",
    color = "Hit Probability"
  )

ggsave(plot = prediction_plot,
       filename = "Visualizations/prediction_plot.png",
       width = 5,
       height = 5)
