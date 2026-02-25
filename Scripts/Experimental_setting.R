# Required packages
library(tidyverse)   # includes ggplot2, readr, dplyr
library(ggplot2)

# Load CSV file
df <- read_csv("Exp_setting.csv",show_col_types = FALSE)

# Reshape to long format
df_long <- df %>%
  pivot_longer(cols = everything(),
               names_to = "System",
               values_to = "Count")

# Set custom order of bars
df_long$System <- factor(df_long$System,
                         levels = c("Lab-intensive", "Semi-intensive", "Extensive"))
# Custom bar colors
custom_cols <- c("Lab-intensive" = "#BAA4A0",
                 "Semi-intensive" = "#0D97C5",
                 "Extensive" = "#FFC352")

# Bar plot
p <- ggplot(df_long, aes(x = System, y = Count, fill = System)) +
  geom_col(width = 0.8) +
  geom_text(aes(label = paste0("N = ", Count)),
            vjust = -0.5, size = 8) +
  scale_fill_manual(values = custom_cols) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(df_long$Count) * 1.15)) +
  labs(x = "Experimental setting",
       y = "Count",
       title = NULL) +
  theme_classic() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 26, color="black"),
    axis.text.y = element_text(size = 24, color="black"),
    axis.title.x = element_text(size = 28, face = "bold"),
    axis.title.y = element_text(size = 28, face = "bold"),
    axis.line = element_line(linewidth = 1),
    legend.position = "none"
  )

# Display plot
print(p)

# Save high-resolution outputs
ggsave("Experimental_setting_barplot.pdf", p,
       width = 8, height = 8, units = "in")

ggsave("Experimental_setting_barplot.png", p,
       width = 8, height = 8, units = "in",
       dpi = 600)

ggsave("Experimental_setting_barplot.tif", p,
       width = 8, height = 8, units = "in",
       dpi = 600, compression = "lzw")
