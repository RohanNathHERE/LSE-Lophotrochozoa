# Load necessary library
library(igraph)

# The dataset should be a data frame named 'gpcr_data' with the following structure:
#  - Column 1: TM1 (numeric) - Represents the first transmembrane domain (TM).
#  - Column 2: TM2 (numeric) - Represents the second transmembrane domain (TM).
#  - Column 3: connections (numeric) - Number of connections between the TM domains.
# Example:
# gpcr_data <- data.frame(
#   TM1 = c(1, 1, 2, ...),  # Numeric identifiers for TM
#   TM2 = c(2, 3, 3, ...),  # Numeric identifiers for TM
#   connections = c(5, 3, 4, ...)  # Number of connections between TMs
# )

# Ensure TM7 appears in the graph even without connections
all_tms <- c(1, 2, 3, 4, 5, 6, 7)

# Create a graph from the edge list
g <- graph_from_data_frame(d = gpcr_data, directed = FALSE, vertices = data.frame(name = all_tms))

# Add the weight of the connections based on the third column (number of connections)
E(g)$weight <- gpcr_data[, 3]

# Define colors for each TM node using the rainbow palette
node_colors <- rainbow(length(all_tms))

# Define custom coordinates for a circular layout manually
node_coords <- matrix(
  c(
    1, 0,      # TM1 coordinates
    0.71, 0.71, # TM2 coordinates
    0, 1,      # TM3 coordinates
    -0.71, 0.71,# TM4 coordinates
    -1, 0,     # TM5 coordinates
    -0.71, -0.71,# TM6 coordinates
    0, -1      # TM7 coordinates
  ), 
  ncol = 2, byrow = TRUE  # Organize coordinates into two columns (x, y)
)

# Plot the graph with customized attributes
plot(
  g,
  vertex.size = 35,  # Size of TM circles for better visibility
  vertex.label = paste0("TM", V(g)$name),  # Label each TM inside the node
  vertex.label.cex = 1.2,  # Font size for labels
  vertex.label.color = "black",  # Color of the labels
  edge.width = E(g)$weight,  # Thickness of the edges based on connections
  edge.color = "gray50",  # Color of the edges for visual clarity
  vertex.color = node_colors,  # Different colors for each node
  vertex.shape = "circle",  # Shape of the vertices
  layout = node_coords,  # Use custom coordinates for circular layout
  edge.curved = 0.2  # Curvature of the edges for aesthetic appeal
)
