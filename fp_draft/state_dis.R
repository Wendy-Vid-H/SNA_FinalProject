# Load packages
library(tigris)
library(sf)
library(tidycensus)

# Use cache to speed up repeated runs
options(tigris_use_cache = TRUE)

# Get US states geometry
states <- states(cb = TRUE, resolution = "20m", year = 2020) %>%
  st_transform(crs = 4326) %>%
  select(NAME, STATEFP, geometry)

states <- states |>
  filter(STATEFP != 72)
  

# Compute centroids
states_centroids <- states %>%
  mutate(centroid = st_centroid(geometry)) %>%
  select(NAME, STATEFP, centroid)

# Create pairwise combinations (excluding self-pairs)
state_pairs <- expand_grid(
  state1 = states_centroids,
  state2 = states_centroids
) %>%
  filter(state1$NAME != state2$NAME)  # To avoid duplicate and self-pairs

# Compute distances between centroids
state_pairs <- state_pairs %>%
  mutate(
    distance_km = as.numeric(st_distance(state1$centroid, state2$centroid, by_element = TRUE)) / 1000,
    state1_name = state1$NAME,
    state2_name = state2$NAME,
    state1_fips = state1$STATEFP,
    state2_fips = state2$STATEFP
  ) %>%
  select(state1_name, state2_name, state1_fips, state2_fips, distance_km)

# View result
print(head(state_pairs))

# Optionally write to CSV
# write.csv(state_pairs, "state_centroid_distances.csv", row.names = FALSE)

write.csv(state_pairs, "state_centroid_distances.csv", row.names = FALSE)

