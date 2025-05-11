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
library(glmnet)

install.packages("glmnet")
install.packages("ranger")

library(poissonreg)
install.packages("poissonreg")

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

### Passion+LASSO

set.seed(20230501)

mig1015_split <- initial_split(data = mig_predata3)
mig1015_training <- training(mig1015_split)
mig1015_testing <- testing(mig1015_split)

mig1015_folds <- vfold_cv(data = mig1015_training, v = 10)

mig1015_pasl_recipe <-
  recipe(formula = migcount ~ ., data = mig1015_training) |>
  step_normalize(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

# 3. 定义泊松 + L1 模型 ----------------------------------------------
poissonl_mod <- poisson_reg(
  penalty = tune(),   # 自动调 λ
  mixture = 1         # Lasso（L1 正则化）
) |>
  set_engine("glmnet") |>
  set_mode("regression")

# 4. 构建 workflow ---------------------------------------------------
poissonl_wf <- workflow() |>
  add_recipe(mig1015_pasl_recipe) |>
  add_model(poissonl_mod)

lasso_grid <- grid_regular(penalty(), levels = 25)

poissonl_cv <- poissonl_wf |>
  tune_grid(
  resamples = mig1015_folds,
  grid = lasso_grid,
  metrics = metric_set(rmse, rsq)
)


poissonl_best <- poissonl_cv |>
  select_best(metric = "rmse")

poissonl_final <- poissonl_wf |>
  finalize_workflow(
  parameters = poissonl_best
)


poissonl_final_fit <- poissonl_final |>
  fit(data = mig1015_training) 

poissonl_coef <- poissonl_final_fit|>
  extract_fit_parsnip() 

poissonl_final_fit|>
  extract_fit_parsnip() |>
  tidy() |>
  filter(estimate != 0) |>
  arrange(desc(estimate))

poissonl_final_fit |>
  extract_fit_parsnip() |>
  vip(num_features = 15)

