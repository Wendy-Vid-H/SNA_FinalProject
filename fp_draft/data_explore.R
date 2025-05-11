library(tidyverse)

library(tidymodels)

library(haven)
non_usbirth <- read_dta("non_usbirth.dta")

nusb_indta <- non_usbirth |>
  mutate(find = as.factor(ind))
nusb_indta <- nusb_indta |>
  filter(ind != 0) 

ef_ind <- nusb_indta |>
  group_by(ind) |>
  summarise(numf = n()) |>
  filter(numf > 100) |>
  select(ind)

nusb_indta_st <- nusb_indta |>
  filter(ind %in% ef_ind$ind)

nusb_indta_st |>
  group_by(ind) |>
  summarise(numf = n())

