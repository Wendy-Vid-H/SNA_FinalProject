library(tidyverse)

library(readr)
acs1_2015_bvar <- read_csv("acs1_2015_bvar1.csv")

acs1_2015_dvar <- read_csv("acs1_2015_dvar1.csv")

acs1_2015_final <- acs1_2015_bvar |>
  left_join(acs1_2015_dvar,
            by = "statefip")

acs1_2015_final |>
  write.csv("acs1_2015_se.csv", row.names = FALSE)