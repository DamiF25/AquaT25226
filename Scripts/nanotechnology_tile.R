# Install required packages (if not installed)

install.packages("tidyverse")
install.packages("ggplot2")

# Load libraries
library(tidyverse)
library(ggplot2)

# Load the CSV data
data <- read_csv(
  "nanotechnology_data.csv",
  col_types = cols(
    Nanotechnology = col_character(),
    `Bacterial pathogen` = col_character(),
    Efficacy = col_character(),
    Parameter = col_character(),
    `Side effect` = col_character()
  )
)

# Convert variables to factors
data <- data %>%
  mutate(
    Nanotechnology = factor(Nanotechnology),
    `Bacterial pathogen` = factor(`Bacterial pathogen`),
    Efficacy = factor(Efficacy, levels = c("High", "Moderate", "Low")),
    Parameter = as.character(Parameter),
    `Side effect` = as.character(`Side effect`)
  )

# Optional: Add space after commas for readability
data$Parameter <- gsub(",", ", ", data$Parameter)

# Convert side effects to short codes: L, H, N
data <- data %>%
  mutate(
    Side_effect_short = case_when(
      `Side effect` == "Low" ~ "L",
      `Side effect` == "Mild" ~ "M",
      `Side effect` == "High" ~ "H",
      TRUE ~ "N"   # Not clear
    )
  )

# Clean Parameter column: remove NA text
data$Parameter[is.na(data$Parameter) | data$Parameter == "NA"] <- ""

# Build the tile label safely
data <- data %>%
  mutate(
    TileLabel = ifelse(
      Parameter == "",
      Side_effect_short,
      paste0(Parameter, "\n", Side_effect_short)
    )
  )

# Custom colours for efficacy
efficacy_cols <- c(
  "High" = "#0D97C5",
  "Moderate" = "#BAA4A0",
  "Low" = "#000000"
)

# Create tile plot with parameter + side-effect labels
p <- ggplot(data, aes(x = Nanotechnology,
                      y = `Bacterial pathogen`,
                      fill = Efficacy)) +
  geom_tile(color = "white", linewidth = 0.3) +
  
  # Add two-line label inside tiles
  geom_text(aes(label = TileLabel),
            color = "black",
            size = 3,
            fontface = "bold",
            lineheight = 0.9) +
  
  scale_fill_manual(values = efficacy_cols, name = "Efficacy") +
  theme_light(base_size = 20) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(face = "italic", color = "black"),
    axis.title.x = element_text(size = 24, face = "bold"),
    axis.title.y = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 24, face = "bold"),
    legend.position = "top",
  ) +
  labs(
    x = "Nanotechnology",
    y = "Pathogen",
    title = NULL
  )

# Print plot to screen
print(p)

# Save high-resolution output
ggsave(
  filename = "nanotechnology_tile_plot.tif",
  plot = p,
  width = 13, height = 5.7, dpi = 600, units = "in",
  compression = "lzw"
)
