library(tidyverse)

library(readr)
acs1_2010_bvar <- read_csv("2010_acs1_bvar.csv")

acs1_2010_dvar <- read_csv("2010_acs1_dvar.csv")
acs1_2010_dvar <- acs1_2010_dvar |>
  rename(statefip = statefips)

acs1_2010_final <- acs1_2010_bvar |>
  left_join(acs1_2010_dvar,
            by = "statefip")

acs1_2010_final |>
  write.csv("acs1_2010_se.csv", row.names = FALSE)