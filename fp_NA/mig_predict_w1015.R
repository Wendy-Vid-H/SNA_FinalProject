library(tigris)
library(sf)
library(units)
library(tidyverse)
library(stringr)
library(lubridate)
library(here)
library(tidycensus)
library(censusapi)
library(httr)
library(jsonlite)
library(testthat)
library(readr)
library(rsample)
library(recipes)
library(dials)
library(parsnip)
library(workflows)
library(tidymodels)
library(vip)
library(ranger)
install.packages("ranger")

library(readr)
mig_fulldemo <- read_csv("mig_fulldemo.csv")


mig_predata_raw <- mig_fulldemo |>
  select(-state1_name, -state2_name) |>
  select(-state_comb) |>
  rename(migcount = n) |>
  select(migcount, everything()) 

mig_predata <- mig_predata_raw |>
  select(-state1_fips, -state2_fips)

mig_predata2 <- mig_predata |>
  select(-statefip) |>
  select(-starts_with("in_median_hh_inc_")) |>
  select(-starts_with("in_female_labor")) |>
  select(-starts_with("out_median_hh_inc_")) |>
  select(-starts_with("out_female_labor")) |>
  select(-starts_with("out_tot_civilian_labor")) |>
  select(-starts_with("in_tot_civilian_labor"))

mig_predata3 <- mig_predata2 |>
  select(-mig_5yago, -mig_current)

### Random Forest Model

set.seed(20250501)

mig1015_split <- initial_split(data = mig_predata3)
mig1015_training <- training(mig1015_split)
mig1015_testing <- testing(mig1015_split)

mig1015_folds <- vfold_cv(data = mig1015_training, v = 10)

mig1015_recipe <-
  recipe(formula = migcount ~ ., data = mig1015_training) |>
  step_normalize(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors())

rf_mod <- rand_forest(
  trees = 150,          
  mtry = tune(),        
  min_n = tune()        
) |>
  set_mode("regression") |>
  set_engine("ranger", importance = "impurity", num.threads = 4)

rf_wf <- workflow() |>
  add_recipe(mig1015_recipe) |>
  add_model(rf_mod)

rf_grid <- grid_regular(
  mtry(range = c(5, 20)),
  min_n(range = c(2, 10)),
  levels = 4
)


rf_tuned <- rf_wf |>
  tune_grid(resamples = mig1015_folds,
  grid = rf_grid,
  metrics = metric_set(rmse)
)

best_rf_params <- rf_tuned |>
  select_best(metric = "rmse")

final_rf <- rf_wf |>
  finalize_workflow(best_rf_params)
final_fit <- final_rf |>
  fit(data = mig1015_training)

rf_coefs <- final_fit |>
  extract_fit_parsnip()

# 9. 变量重要性可视化
final_fit |>
  extract_fit_parsnip() %>%
  vip(num_features = 15)

