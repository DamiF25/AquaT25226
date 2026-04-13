# Install required packages (if not installed)

install.packages("tidyverse")
install.packages("ggplot2")

# Load libraries
library(tidyverse)
library(ggplot2)

# Read data
data <- read.csv("phytogenic_tile_data.csv", stringsAsFactors = FALSE)

# Clean potential hidden spaces
data$Alternative <- trimws(data$Alternative)
data$Pathogen <- trimws(data$Pathogen)

# Convert Outcome factors with defined order
data$Outcome_key <- factor(
  data$Outcome_key,
  levels = c("Survival", "Immunity", "Gut function")
)

data$Outcome_result <- factor(
  data$Outcome_result,
  levels = c(
    "Significant",
    "Not significant",
    "No stat test",
    "Not reported"
  )
)

# Remove NA
data$Effect[is.na(data$Effect)] <- ""
data$Side_effect_assessment[is.na(data$Side_effect_assessment)] <- ""

# Order alternatives alphabetically
data$Alternative <- factor(
  as.character(data$Alternative),
  levels = sort(unique(as.character(data$Alternative)))
)

# Order pathogens alphabetically
data$Pathogen <- factor(
  as.character(data$Pathogen),
  levels = sort(unique(as.character(data$Pathogen)))
)

# Create combined label
data$label <- paste0(data$Effect, "\n", data$Side_effect_assessment)

# Plot
p <- ggplot(data,
            aes(x = Alternative,
                y = Pathogen,
                fill = Outcome_result)) +
  
  geom_tile(color = "white", linewidth = 0.3) +
  
  geom_text(aes(label = label),
            size = 2,
            fontface = "bold",
            lineheight = 0.9) +
  
  # Facet by outcome key
  facet_grid(Outcome_key ~ .) +
  
  # Reverse y-axis for correct alphabetical ordering
  scale_y_discrete(limits = rev(levels(data$Pathogen))) +
  
  scale_fill_manual(values = c(
    "Significant" = "#0D97C5",
    "Not significant" = "#BAA4A0",
    "No stat test" = "#BF5A5A",
    "Not reported" = "#000000"
  )) +
  
    theme_light(base_size = 12) +
    
    theme(
      axis.text = element_text(colour = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(face = "italic"),
      axis.title = element_text(size = 13, face = "bold"),
      panel.grid = element_blank(),
      
      strip.text.y.right = element_text(color = "black", face = "bold"),
      strip.background = element_rect(fill = "grey85"),
      
      strip.placement = "outside",
      
      legend.position = "top",
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 16)
    ) +
  
  labs(
    x = "Phytogenic alternative",
    y = "Pathogen",
    fill = "Outcome result",
    title = NULL, #"Reported effects of phytogenic alternatives against tilapia pathogens"
    subtitle = NULL #"+ / − = direction of effect; Y/N = side-effect assessment"
  )

# Print plot
print(p)

# Save landscape
ggsave(
  filename = "phytogenic_tile_plot.tif",
  plot = p,
  width = 25,
  height = 5.6,
  dpi = 600,
  units = "in",
  compression = "lzw"
)
