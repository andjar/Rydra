# Example for visualize_rydra_model_as_mermaid
# ---------------------------------------------
#
# This script demonstrates how to use the `visualize_rydra_model_as_mermaid`
# function from the Rydra package to generate a Mermaid flowchart definition
# for a model specified in a YAML configuration file.

# --- Prerequisites ---
# 1. Ensure the Rydra package is loaded. You might need to build and install
#    it first if you are developing it.
#    If Rydra is installed:
#    library(Rydra)
#
#    If running from source (e.g., during development, assuming your working
#    directory is the package root):
#    devtools::load_all() # Load all functions from the current package source
#
# 2. Ensure the 'yaml' package is installed to read the configuration file:
#    if (!requireNamespace("yaml", quietly = TRUE)) {
#      install.packages("yaml")
#    }
#    library(yaml)
#
# --- Script ---

# Function to execute the example
run_visualization_example <- function() {
  # For development: ensure Rydra functions are available
  # If you've built and installed Rydra, you can comment out devtools::load_all()
  # and uncomment library(Rydra)
  if (requireNamespace("devtools", quietly = TRUE) && file.exists("DESCRIPTION")) {
    message("Loading Rydra using devtools::load_all() from package root.")
    devtools::load_all()
  } else if (requireNamespace("Rydra", quietly = TRUE)) {
    message("Loading installed Rydra package.")
    library(Rydra)
  } else {
    stop("Rydra package not found. Please install it or run this script from the package root with devtools available.")
  }

  # Ensure yaml package is available
  if (!requireNamespace("yaml", quietly = TRUE)) {
    message("yaml package not found. Please run: install.packages('yaml')")
    return(invisible(NULL))
  }

  # 1. Define the path to your configuration file
  #    Using the example_config.yaml from the Rydra package itself
  #    This path might need adjustment depending on how/where you run the script.
  #    system.file() is robust for finding files within an installed package.
  config_file_path <- system.file("extdata", "example_config.yaml", package = "Rydra")

  if (config_file_path == "" || !file.exists(config_file_path)) {
    # Fallback for when system.file might not work (e.g. before installation, running from source)
    alt_config_path <- file.path("inst", "extdata", "example_config.yaml")
    if(file.exists(alt_config_path)) {
        config_file_path <- alt_config_path
        message(paste("Using local config path:", config_file_path))
    } else {
        message("Could not find example_config.yaml. Searched with system.file and locally at inst/extdata/.")
        return(invisible(NULL))
    }
  } else {
     message(paste("Using config file from system.file:", config_file_path))
  }

  # 2. Read the YAML configuration file
  parsed_yaml_config <- yaml::read_yaml(config_file_path)

  # 3. Specify the model you want to visualize from the YAML file
  model_to_visualize_name <- "main_model" # This is a key in example_config.yaml

  if (!model_to_visualize_name %in% names(parsed_yaml_config)) {
    message(paste("Model '", model_to_visualize_name, "' not found in the YAML configuration.", sep = ""))
    return(invisible(NULL))
  }

  model_configuration <- parsed_yaml_config[[model_to_visualize_name]]

  # 4. Generate the Mermaid string
  #    The visualize_rydra_model_as_mermaid function is part of Rydra
  mermaid_definition_string <- visualize_rydra_model_as_mermaid(
    model_config = model_configuration,
    model_name = model_to_visualize_name
  )

  # 5. Print the Mermaid string
  cat("\n--- Mermaid Flowchart Definition ---\n\n")
  cat(mermaid_definition_string)
  cat("\n\n--- How to Use ---\n")
  cat("1. Copy the text between the '--- Mermaid Flowchart Definition ---' lines.\n")
  cat("2. Paste it into a Mermaid live editor, such as:\n")
  cat("   - https://mermaid.live\n")
  cat("   - Typora (if Mermaid is enabled)\n")
  cat("   - VS Code with a Mermaid preview extension.\n")
  cat("3. The editor will render the flowchart.\n")
  cat("Alternatively, in R, you can use the DiagrammeR package:\n")
  cat("# if (!requireNamespace(\"DiagrammeR\", quietly = TRUE)) install.packages(\"DiagrammeR\")\n")
  cat("# DiagrammeR::mermaid(mermaid_definition_string)\n")

  return(invisible(mermaid_definition_string))
}

# Run the example:
# To run this, you can source this file in R, then call:
# run_visualization_example()
# Or, if Rydra is installed and R session started fresh:
# library(Rydra)
# source("examples/visualize_model_example.R") # Adjust path if needed
# run_visualization_example()

# If the script is run directly (e.g. Rscript examples/visualize_model_example.R)
if (interactive()) {
  message("Example script loaded. Call 'run_visualization_example()' to execute.")
} else {
  # Non-interactive mode, try to run
  message("Running visualization example script non-interactively...")
  run_visualization_example()
}

message("\nTo run the example, call: run_visualization_example()")
