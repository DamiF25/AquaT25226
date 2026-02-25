# Load packages if required
install.packages("ggplot2")
install.packages("tidyverse")

# Load required libraries
library(ggplot2)
library(tidyverse)

# Set working directory

# Load the data
data <- read.csv("intervention_cat.csv", header = TRUE, stringsAsFactors = FALSE)

# Clean whitespace and fix typos
data <- data %>%
  mutate(
    Category = str_trim(Category),
    Intervention = str_trim(Intervention),
    Intervention = recode(Intervention,
                          "Nanotechnology " = "Nanotechnology",
                          "Probiotic " = "Probiotic",
                          "Vaccine " = "Vaccine",
                          "Phytogenic " = "Phytogenic")
  )

# Summarize counts
summary_data <- data %>%
  group_by(Category, Intervention) %>%
  summarise(Count = n(), .groups = "drop")

# Plot with manual colors
plot <- ggplot(summary_data, aes(x = Category, y = Count, fill = Intervention)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_grey(base_size = 20) +
  labs(
    x = "Alternative non-antibiotics",
    y = "Number of studies",
    fill = "Intervention method"
  ) +
  theme(
    axis.text.x = element_text(color = "black", angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "top",              # move legend above plot
    legend.direction = "horizontal",      # make legend horizontal
    legend.justification = "center"       # center it above the plot
    ) +
  scale_fill_manual(
    values = c(
      "Prophylactic" = "#F5A30A",   # orange
      "Therapeutic"  = "#0C7105",   # green
      "Combination"  = "#000000"    # black
    )
  )

print(plot)

# Save high-resolution plot
ggsave("intervention_cat_stacked_bar.tif", 
       plot = plot, 
       width = 10, 
       height = 8, 
       dpi = 600,
       units = "in",
       compression = "lzw")








# Load required libraries
library(ggplot2)
library(tidyverse)

# Load the data
data <- read.csv("intervention_cat.csv", header = TRUE, stringsAsFactors = FALSE)

# Clean whitespace in both columns
data <- data %>%
  mutate(
    Category = trimws(Category),
    Intervention = trimws(Intervention)
  )

# Summarize counts
summary_data <- data %>%
  group_by(Category, Intervention) %>%
  summarise(Count = n(), .groups = "drop")

# Plot with manual colors
plot <- ggplot(summary_data, aes(x = Category, y = Count, fill = Intervention)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_grey(base_size = 20) +
  labs(
    x = "Alternative non-antibiotics",
    y = "Number of studies",
    fill = "Intervention approach"
  ) +
  theme(
    axis.text.x = element_text(color = "black", angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = "center"
  ) +
  scale_fill_manual(
    values = c(
      "Prophylactic" = "#F5A30A",
      "Therapeutic"  = "#0C7105",
      "Combination"  = "#000000"
    )
  )

print(plot)

# Save high-resolution plot
ggsave(
  "intervention_cat_stacked_bar.tif",
  plot = plot,
  width = 10,
  height = 8,
  dpi = 600,
  units = "in",
  compression = "lzw"
)
