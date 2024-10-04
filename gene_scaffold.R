# Load required packages
library(htmlwidgets)  # For saving interactive web content
library(webshot2)     # For taking screenshots of web content
library(geneviewer)   # For creating gene visualization charts

# Define the custom color scheme
custom_colors <- c(
  "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c",
  "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#ffff99", "#b15928",
  "#d9d9d9", "#636363", "#bc80bd", "#6b6ecf", "#ccebc5", "#41ab5d",
  "#ffed6f", "#9e0142", "#fbb4ae", "#de2d26", "#b3cde3", "#377eb8",
  "#fdcdac", "#e31a1c", "#cbd5e8", "#5288b7", "#f4cae4", "#984ea3",
  "#e6f5c9", "#4daf4a", "#fff2ae", "#ff7f00", "#fddbc7", "#d95f02",
  "#e5d8bd", "#8c510a", "#f2f0f7", "#8073ac", "#abdda4", "#313695",
  "#91bfdb", "#fc8d59"
)

# Calculate the number of unique clusters
unique_clusters <- length(unique(data$cluster))

# Set the height of the chart dynamically based on the number of unique clusters
dynamic_height <- paste0(70 * unique_clusters, "px")

# Create the main GC_chart with various components
main_chart <- GC_chart(data, 
                       cluster = "cluster",  # Specify the clustering variable
                       group = "species",    # Specify the grouping variable
                       height = dynamic_height,  # Use the dynamic height calculated
                       width = "1000px") %>%
  GC_normalize(group = "name",    # Normalize by name
               gap = 0.01,         # Set gap between genes
               preserve_gene_length = FALSE) %>%
  GC_tooltip(formatter = "<b>Start:</b> {original_start}<br><b>End:</b> {original_end}") %>%
  GC_genes(show = TRUE,           # Show gene details
           stroke = "black",      # Set stroke color
           strokeWidth = 1,      # Set stroke width
           marker = "triangle",   # Set marker shape
           marker_size = list(type = "custom", scale = 0.4)) %>%
  GC_legend(FALSE) %>%             # Disable legend
  GC_scale(
    padding = 0,                   # No padding
    hidden = FALSE,                # Show scale
    ticksCount = 20,               # Number of ticks on scale
    axis_type = "bottom",          # Position axis at bottom
    ticksFormat = ".3s",           # Format for ticks
    y = NULL                       # Do not show y-axis
  )

# Enhance the main_chart with additional components and create the cluster plot
cluster_plot <- main_chart %>%
  GC_color(customColors = custom_colors)  # Apply custom colors

# Display the plot
cluster_plot

# Set the path to the Chrome executable for webshot
Sys.setenv(CHROMOTE_CHROME = "PATH_TO_YOUR_BROWSER_EXECUTABLE")  # Replace with your browser path

# Save the plot to a temporary HTML file
htmlwidgets::saveWidget(cluster_plot, "temp.html", selfcontained = TRUE)

# Use webshot2 to save the plot as a PDF
webshot2::webshot("temp.html", "output.pdf", selector = ".geneviewer",
                  cliprect = "viewport", vwidth = 5000, vheight = 2160)
