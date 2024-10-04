# Load necessary libraries
library(ggplot2)       # For data visualization
library(reshape2)      # For reshaping data
library(RColorBrewer)  # For color palettes
library(tidyr)        # For data tidying functions, like complete
library(dplyr)        # For data manipulation functions

# Example input data
# data <- tibble::tribble(
#   ~number, ~characteristic, ~tm,      ~structure, ~label, ~taxa,
#       0, "AC-AL", "TM1-TM2", 0, "0/0", "annelida",
#       0, "AC-AR", "TM1-TM2", 0, "0/0", "annelida",
#       0, "AC-BA", "TM1-TM2", 0, "0/0", "annelida",
#       0, "AC-OH", "TM1-TM2", 0, "0/0", "annelida",
#       0, "AC-S",  "TM1-TM2", 0, "0/0", "annelida"
# )

# Define the substitutions for characteristics
substitutions <- c(
  "Hydroxyl" = "OH",
  "Polar" = "P",
  "Uncharged" = "UC",
  "Aliphatic" = "AL",
  "Nonpolar" = "NP",
  "Hydrophobic" = "HP",
  "Aromatic" = "AR",
  "Sulfur" = "SU",
  "Basic" = "BA",
  "Charged" = "CH",
  "Hydrophilic" = "HI",
  "Acidic" = "AC",
  "Amide" = "AM",
  "Small" = "S"
)

# Perform substitutions in the 'characteristic' column
data$characteristic <- as.character(data$characteristic)
for (pattern in names(substitutions)) {
  data$characteristic <- gsub(pattern, substitutions[pattern], data$characteristic)
}

# Ensure that all combinations of characteristic, tm, and taxa are present
# Fill missing combinations with 0
all_combinations <- expand.grid(
  characteristic = unique(data$characteristic),
  tm = unique(data$tm),
  taxa = unique(data$taxa)
)

# Merge with the original data and fill missing values with 0
data_complete <- left_join(all_combinations, data, by = c("characteristic", "tm", "taxa"))
data_complete[is.na(data_complete)] <- 0

# Get all unique properties
all_properties <- unique(data_complete$characteristic)

# Ensure each taxon has all properties
for (taxon in unique(data_complete$taxa)) {
  for (property in all_properties) {
    if (!any(data_complete$taxa == taxon & data_complete$characteristic == property)) {
      # If the property is missing, add it with a value of 0 for all TM
      new_rows <- data.frame(taxa = taxon, characteristic = property, tm = unique(data_complete$tm), number = 0)
      data_complete <- rbind(data_complete, new_rows)
    }
  }
}

# Re-check data completeness
data_complete <- data_complete %>%
  group_by(taxa, characteristic, tm) %>%
  summarise(number = sum(number), .groups = "drop")

# Calculate the total number for each taxa and characteristic combination
taxa_char_totals <- data_complete %>%
  group_by(taxa, characteristic) %>%
  summarise(total = sum(number), .groups = "drop")

# Filter out characteristic values where the total number is 0 for a specific taxa
data_filtered <- data_complete %>%
  semi_join(taxa_char_totals %>% filter(total != 0), by = c("taxa", "characteristic"))

# Define the new order of the taxa
taxa_order <- c("gastropoda", "bivalvia", "cephalopoda", "minor_molluscan_classes", "annelida", "platyhelminthes", "other_lophotrochozoan_phyla")

# Convert the taxa column to a factor with the specified levels
data_filtered$taxa <- factor(data_filtered$taxa, levels = taxa_order)

# Define the colors for the heatmap
colors <- c("#ffffff", "#f7f9cb", "#c7edc0", "#92e0c3", "#56d0d0", "#00bdde", "#00afef", "#539df4", "#9085e8", "#cd6ace", "#f54a9d", "#ff3d5e", "#f0550d", "#fc8d59")

# Create the heatmap with taxa on the top
ggplot(data_complete, aes(x = tm, y = characteristic, fill = as.numeric(number))) +
  geom_tile(color = "black", size = 0.2) +
  scale_fill_gradientn(colors = colors, na.value = "white") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.line = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10, face = "bold"),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10, face = "bold"),
    legend.box.background = element_rect(color = "black", size = 1),
    legend.position = "bottom",
    legend.direction = "horizontal",
    strip.background = element_blank(),
    strip.text.x = element_text(size = 10, face = "bold"),  # Taxa on top
    panel.spacing = unit(0, "lines")
  ) +
  labs(
    title = "Heatmap of Amino Acid Properties by Taxa",
    x = "Transmembrane Regions (TM)",
    y = "Amino Acid Properties",
    fill = "Number of Residual Pairs"
  ) +
  facet_grid(. ~ taxa, scales = "free_x", space = "free_x")
