library(tidyverse)

library(readr)
acs1_2014_bvar <- read_csv("acs1_2014_bvar1.csv")

acs1_2014_dvar <- read_csv("acs1_2014_dvar1.csv")

acs1_2014_final <- acs1_2014_bvar |>
  left_join(acs1_2014_dvar,
            by = "statefip")

acs1_2014_final |>
  write.csv("acs1_2014_se.csv", row.names = FALSE)
