library(tidyverse)
library(tidycensus)
library(sf)
library(stringr)
library(lubridate)
library(tigris)
library(here)
library(censusapi)
library(httr)
library(jsonlite)
library(testthat)

library(igraph)

library(haven)
yr5_movestate <- read_dta("5y_movestate.dta")

mig_count5 <- yr5_movestate |>
  mutate(state_comb = migsta5*100 + statefip) |>
  group_by(state_comb) |>
  summarize(n = n())

mig_count5 |>
  arrange(desc(n))

yr5_movestate |>
  select(year, statefip, migsta5) |>
  count(migsta5) |>
  arrange(desc(n))

library(haven)
state_cd <- read_dta("state_cent_d_v1.dta")

## state_sr1: for the not existing state combination of migration, n = "na"
state_sr1_y5 <- state_cd |>
  left_join(mig_count5, by = "state_comb")
state_sr2_y5 <- state_sr1_y5 |>
  mutate(mig_5yago = state1_name) |>
  mutate(mig_current = state2_name)

state_sr3_y5 <- state_sr2_y5 |>
  filter(is.na(n) == FALSE)
state_sr3_y5 |>
  write.csv("state_sr3_y5.csv", row.names = FALSE)

library(tidyverse)
library(igraph)
library(ggraph)

library(statnet)
library(ergm)
library(intergraph)
library(stargazer)


## Create a network data set ##
state_yr5_viz <- data.frame(
  from = c(state_sr3_y5$mig_5yago),
  to = c(state_sr3_y5$mig_current),
  weight = c(state_sr3_y5$n)
)


acs1_2010_sein <- read_csv("acs1_2010_sein.csv")
acs1_2015_sein <- read_csv("acs1_2015_sein.csv")
acs1_2010_seout <- read_csv("acs1_2010_seout.csv")
acs1_2015_seout <- read_csv("acs1_2015_seout.csv")

acs1_2010_sein <- acs1_2010_sein |>
  mutate(state2_fips = statefip)

in1 <- state_sr3_y5 |>
  left_join(acs1_2010_sein, by = "state2_fips") |>
  select(-state.x) |>
  select(-state.y)

acs1_2015_sein <- acs1_2015_sein |>
  mutate(state2_fips = statefip)
in2 <- in1 |>
  left_join(acs1_2015_sein, by = "state2_fips") |>
  select(-State.x) |>
  select(-State.y) |>
  select(-statefip.y)

acs1_2010_seout <- acs1_2010_seout |>
  mutate(state1_fips = statefip)

in3 <- in2|>
  left_join(acs1_2010_seout, by = "state1_fips") |>
  select(-state.x) |>
  select(-state.y) |>
  select(-statefip.x)

acs1_2015_seout <- acs1_2015_seout |>
  mutate(state1_fips = statefip)
in4 <- in3 |>
  left_join(acs1_2015_seout, by = "state1_fips") |>
  select(-State.x) |>
  select(-State.y) |>
  select(-statefip.y) |>
  rename(statefip = statefip.x)

mig_fulldemo <- in4
mig_fulldemo |>
  write.csv("mig_fulldemo.csv", row.names = FALSE)

