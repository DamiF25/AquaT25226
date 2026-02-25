# Install required packages if not already installed
install.packages(c("tidyverse", "sf", "rnaturalearth", "rnaturalearthdata", "ggrepel"))

library(tidyverse)
library(sf)                # for spatial data
library(rnaturalearth)     # for world map data
library(rnaturalearthdata) # supporting data
library(ggrepel)           # for better text labels

#swd

# -------------------------------
# 1. Load your dataset
# -------------------------------
studies <- read_csv("Study_locations.csv", show_col_types = FALSE)

# -------------------------------
# 2. Calculate % proportion per country
# -------------------------------
country_counts <- studies %>%
  group_by(Country) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(percent = 100 * n / sum(n))

# -------------------------------
# 3. Get world map data
# -------------------------------
world <- ne_countries(scale = "medium", returnclass = "sf")

# -------------------------------
# 4. Join study data with map data
# -------------------------------
map_data <- world %>%
  left_join(country_counts, by = c("name" = "Country"))

# -------------------------------
# 5. Plot world map with % proportion
# -------------------------------
ggplot(data = map_data) +
  geom_sf(aes(fill = percent), color = "white", size = 0.2) +
  scale_fill_gradient(low = "skyblue", high = "red", na.value = "grey90") +
  geom_text_repel(
    data = map_data %>% filter(!is.na(percent)),
    aes(
      x = st_coordinates(st_centroid(geometry))[,1],
      y = st_coordinates(st_centroid(geometry))[,2],
      label = paste0(name, "\n", round(percent, 1), "%")
    ),
    size = 3
  ) +
  theme_classic() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = c(0.12, 0.25),        # move legend inside map
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = alpha("white", 0.6), color = NA),
    legend.title = element_text(face = "bold")
  ) +
  labs(
    title = NULL,
    subtitle = NULL,
    fill = "% of studies"
  )

# Save map
  ggsave(filename = "Study_location_map_by_country_and_percentage_proportion_of_studies_map.tif",
         width=16, height=6, dpi=400, units='in', compression = "lzw")
  