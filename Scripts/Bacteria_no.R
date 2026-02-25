#if required, install necessary packages first
#install.packages("ggplot2")
#install.packages("tidyverse")

# Load required libraries
library(ggplot2)
library(tidyverse)

# Read the CSV file
bact <- read.csv("Bacteria_no.csv", stringsAsFactors = FALSE)

# Ensure Bacteria is a factor ordered by Number
bact$Bacteria <- factor(bact$Bacteria, levels = bact$Bacteria[order(bact$Number)])

# Plot
ggplot(bact, aes(x = Number, y = Bacteria)) +
  geom_bar(stat = "identity", fill = "black") +
  geom_text(aes(label = Number),
            hjust = -0.2,          # pushes text slightly to the right of the bar
            size = 6,              # adjust text size
            colour = "#BA4A4A") +      # <-- change annotation font colour here
  theme_grey() +
  theme(
    axis.title = element_text(size = 24, colour = "black"),
    axis.text.x = element_text(size = 18, colour = "black"),
    axis.text.y = element_text(size = 18, face = "italic", colour = "black")
  ) +
  labs(
    x = "Number",
    y = "Bacteria tested"
  )


# Save plot
ggsave(
  filename = "Bacteria_barplot.tif",   # File name + format
  plot = last_plot(),                  # Saves the most recent ggplot object
  width = 12,                          # Width in inches
  height = 8,                          # Height in inches
  dpi = 600,                           # High-resolution output
  units = "in",                         # Units for width/height
  compression = "lzw"
)
