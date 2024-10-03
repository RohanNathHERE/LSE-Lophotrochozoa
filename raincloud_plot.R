# Load necessary libraries
library(ggplot2)         # For plotting
library(ggbeeswarm)      # For beeswarm plots (jittered points)
library(ggplotify)       # For geom_flat_violin (violins)
library(RColorBrewer)    # For generating color palettes
library(tidyverse)       # For data manipulation
library(tidyquant)       # For additional ggplot themes
library(ggdist)          # For distributional plots
library(ggthemes)        # Additional themes for ggplot
library(ggridges)        # For ridge plots

# Generate a color palette using RColorBrewer
# 'n_colors' is the number of levels/groups to plot, adjust based on the levels in your data
n_colors <- nlevels(data$group_column)  # Replace 'group_column' with your actual column representing groups
palette <- brewer.pal(n = 9, name = "Set1")  # Generate a 9-color palette; adjust 'n' as needed

# Create the raincloud plot
# 'data' is the dataset, 'group_column' represents categorical groups, 'value_column' is the continuous variable
p <- ggplot(data, aes(y = group_column, x = value_column, fill = group_column)) +  # Generic variable names
  geom_density_ridges(alpha = 0.7, color = "black") +  # Ridge plot for densities, with transparency
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.5) +  # Boxplot overlay, removing outliers
  geom_quasirandom(aes(color = group_column), size = 0.5, alpha = 0.7, orientation = "y") +  # Jittered points
  scale_fill_manual(values = palette) +  # Fill color for density ridges
  scale_color_manual(values = palette) +  # Color for jittered points
  scale_y_discrete(expand = c(0.5, 0)) +  # Adjust y-axis to provide space for plot
  theme_minimal() +  # Apply minimal theme for a clean look
  labs(title = "Raincloud Plot of Variable by Group",
       x = "Value",  # X-axis label (generic)
       y = "Group") +  # Y-axis label (generic)
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centered bold title
        axis.text.y = element_text(size = 12),  # Y-axis text size
        axis.text.x = element_text(size = 12),  # X-axis text size
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.border = element_blank()) +  # Remove border
  scale_y_discrete(limits = rev)  # Reverse y-axis order for better visual

# Print the plot
print(p)
