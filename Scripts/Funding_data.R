# Load required packages
library(tidyverse)
library(ggplot2)

# Read CSV data
df <- read.csv("funding_data.csv", stringsAsFactors = FALSE)

# Prepare summary table (counts and percentages)
summary_df <- df %>%
  count(Funding) %>%
  mutate(
    Percent = n / sum(n) * 100,
    Label = paste0(round(Percent, 1), "%")
  )

# Compute label positions for donut annotation
summary_df <- summary_df %>%
  arrange(desc(Funding)) %>%
  mutate(
    ymax = cumsum(Percent),
    ymin = lag(ymax, default = 0),
    label_pos = (ymax + ymin) / 2
  )

# Donut chart parameters
inner_radius <- 0.55   # Controls donut hole size (0 = pie chart, closer to 1 = thinner ring)
font_size <- 16        # Global font size
my_colors <- c("Funded" = "#60435D", "Not funded" = "#FFB62E")

# Build donut chart
donut_plot <- ggplot(summary_df, aes(x = 2, y = Percent, fill = Funding)) +
  geom_col(width = 1, color = "white") +
  geom_text(
    aes(
      x = inner_radius + 1.0 * (2 - inner_radius),
      y = label_pos,
      label = Label
    ),
    color = "white",
    size = font_size / 2,
    fontface = "bold"
  ) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = my_colors) +
  xlim(0.5, 2.5) +
  theme_void(base_size = font_size) +
  theme(
    legend.key.height = unit(0.8, "cm"),
    legend.title = element_blank(),
    legend.position = c(1.08, 0.5),   # x, y in [0,1]
    legend.text = element_text(size = 20)
    ) +
  annotate("text",
           x = 0 + inner_radius,
           y = 0,
           label = "Funding",
           size = font_size / 2,
           lineheight = 1.0
  ) 

print(donut_plot)

# Save high‑resolution image
###############################################
ggsave(
  filename = "Funding_percentage.tif",
  plot = donut_plot,
  width = 9,
  height = 6,
  dpi = 600,
  units = "in",
  compression = "lzw"
)
