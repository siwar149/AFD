# Step 1: Install and load the igraph package
install.packages("igraph")
library(igraph)

# Step 2: Prepare the input-output table
# This is an example input-output table (adjacency matrix)
# Rows and columns represent sectors, and values represent the flow between them
input_output_table <- matrix(
  c(0, 2, 0, 1,
    1, 0, 3, 0,
    0, 1, 0, 4,
    2, 0, 1, 0),
  nrow = 4, ncol = 4, byrow = TRUE
)

# Optionally, name the rows and columns
rownames(input_output_table) <- colnames(input_output_table) <- c("Sector A", "Sector B", "Sector C", "Sector D")

# Step 3: Create the graph from the adjacency matrix
network_graph <- graph_from_adjacency_matrix(input_output_table, mode = "directed", weighted = TRUE)

# Step 4: Plot the graph
plot(network_graph,
     vertex.label = V(network_graph)$name,   # Label the vertices with sector names
     edge.width = E(network_graph)$weight,   # Adjust edge width based on the weight
     edge.arrow.size = 0.5,                  # Size of the arrows for directed edges
     main = "Network Graph from Input-Output Table")
