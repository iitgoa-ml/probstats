library(available) # Check if the Title of a Package is Available,
# Appropriate and Interesting
# Check for potential names
available::suggest("Probability Concept Visualizer")
# Check whether it's available
available::available("ProbabilityViz", browse = FALSE)
