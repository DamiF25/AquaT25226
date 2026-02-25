# if required, install necessary packages first
# install.packages("ggplot2")
# install.packages("tidyverse")

# Load required libraries
library(ggplot2)
library(tidyverse)

# Set working directory

# Load the CSV file
year_df <- read_csv("Year_no.csv", show_col_types = FALSE)

# Ensure Year is numeric (if imported as character)
year_df$Year <- as.numeric(year_df$Year)

# Create the line plot
p <- ggplot(year_df, aes(x = Year, y = Number)) +
  geom_line(color = "red", linewidth = 1.2) +        # Red line
  geom_point(color = "black", size = 7) +       # Black dots
  theme_grey () +
  theme(
    axis.text.x = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.title = element_text(size = 24, face = "bold")
  ) +
  labs(
    x = "Year",
    y = "Number of publications"
  )

# Display the plot
print(p)

# Save the plot
ggsave(
  filename = "Year_lineplot.tif", 
  plot = p,
  width = 10,        # inches
  height = 6,        # inches
  dpi = 600,         # resolution
  units = "in",
  compression = "lzw" # compression
)
