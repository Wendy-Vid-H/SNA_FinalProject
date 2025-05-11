library(tidyverse)

library(haven)
yr1_movestate <- read_dta("1y_movestate.dta")

mig_count <- yr1_movestate |>
  mutate(state_comb = migsta1*100 + statefip) |>
  count(state_comb)
  group_by(state_comb) |>
  summarize(n = n())

yr1_movestate |>
  count(migsta1)

library(haven)
state_cd <- read_dta("state_cent_d_v1.dta")
View(state_cd)

## state_sr1: for the not existing state combination of migration, n = "na"
state_sr1 <- state_cd |>
  left_join(mig_count, by = "state_comb")
state_sr2_y1 <- state_sr1|>
  mutate(mig_1yago = state1_name) |>
  mutate(mig_current = state2_name)

write.csv(state_sr2_y1, "mig_1y_15.csv", row.names = FALSE)

## state_sr1: for the not existing state combination of migration, n = 0
state_sr2 <- state_sr1
state_sr2$n <- replace_na(state_sr2$n, 0)

# WE USE STATE_SR2 BELOW ###

# generate vairable for residency now vs 1 year ago



