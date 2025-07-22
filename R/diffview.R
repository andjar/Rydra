#' Compare two Rydra configuration files and display differences
#'
#' This function compares two YAML configuration files and outputs a nicely
#' formatted diff, ignoring whitespace changes. It shows additions, deletions,
#' and modifications between the two configuration files.
#'
#' @param config_path1 Path to the first YAML configuration file.
#' @param config_path2 Path to the second YAML configuration file.
#' @param ignore_whitespace Logical, whether to ignore whitespace differences.
#'        Defaults to TRUE.
#' @return A character vector containing the formatted diff output.
#'         Each element represents a line of the diff.
#' @export
#' @examples
#' \dontrun{
#' # Create two sample config files for demonstration
#' config1_path <- tempfile(fileext = ".yaml")
#' config2_path <- tempfile(fileext = ".yaml")
#' 
#' # Write sample configs
#' writeLines(c(
#'   "model_name: test_model",
#'   "centering:",
#'   "  age: 30",
#'   "main_model:",
#'   "  intercepts:",
#'   "    baseline: 1.0"
#' ), config1_path)
#' 
#' writeLines(c(
#'   "model_name: test_model",
#'   "centering:",
#'   "  age: 35", 
#'   "main_model:",
#'   "  intercepts:",
#'   "    baseline: 1.5",
#'   "    new_param: 0.2"
#' ), config2_path)
#' 
#' # Compare the configs
#' diff_result <- diffview(config1_path, config2_path)
#' cat(diff_result, sep = "\n")
#' 
#' # Clean up
#' unlink(c(config1_path, config2_path))
#' }
diffview <- function(config_path1, config_path2, ignore_whitespace = TRUE) {
  # Load both configurations
  config1 <- load_config(config_path1)
  config2 <- load_config(config_path2)
  
  # Compare the configurations
  diff_result <- .compare_configs(config1, config2, ignore_whitespace)
  
  # Format the output
  formatted_output <- .format_diff_output(diff_result, config_path1, config_path2)
  
  return(formatted_output)
}

#' Internal function to compare two configuration objects
#' 
#' @param config1 First configuration list
#' @param config2 Second configuration list
#' @param ignore_whitespace Whether to ignore whitespace differences
#' @return A list containing the comparison results
.compare_configs <- function(config1, config2, ignore_whitespace = TRUE) {
  # Create a list to store differences
  differences <- list()
  
  # Get all unique keys from both configs
  all_keys <- unique(c(names(config1), names(config2)))
  
  for (key in all_keys) {
    key_diff <- .compare_values(config1[[key]], config2[[key]], 
                               paste0(key), ignore_whitespace)
    if (!is.null(key_diff)) {
      differences[[key]] <- key_diff
    }
  }
  
  return(differences)
}

#' Internal function to compare two values (recursive)
#' 
#' @param val1 Value from first config
#' @param val2 Value from second config  
#' @param path Current path in the configuration tree
#' @param ignore_whitespace Whether to ignore whitespace differences
#' @return Difference information or NULL if no differences
.compare_values <- function(val1, val2, path, ignore_whitespace = TRUE) {
  # Handle case where one value is missing
  if (is.null(val1) && !is.null(val2)) {
    return(list(type = "added", path = path, value = val2))
  }
  if (!is.null(val1) && is.null(val2)) {
    return(list(type = "removed", path = path, value = val1))
  }
  if (is.null(val1) && is.null(val2)) {
    return(NULL)
  }
  
  # Handle lists (nested structures)
  if (is.list(val1) && is.list(val2)) {
    nested_diffs <- list()
    all_nested_keys <- unique(c(names(val1), names(val2)))
    
    for (nested_key in all_nested_keys) {
      nested_path <- paste0(path, ".", nested_key)
      nested_diff <- .compare_values(val1[[nested_key]], val2[[nested_key]], 
                                   nested_path, ignore_whitespace)
      if (!is.null(nested_diff)) {
        nested_diffs[[length(nested_diffs) + 1]] <- nested_diff
      }
    }
    
    if (length(nested_diffs) > 0) {
      return(list(type = "nested", path = path, differences = nested_diffs))
    }
    return(NULL)
  }
  
  # Handle case where types don't match
  if (class(val1)[1] != class(val2)[1]) {
    return(list(type = "changed", path = path, old_value = val1, new_value = val2))
  }
  
  # Handle strings with whitespace normalization
  if (is.character(val1) && is.character(val2)) {
    if (ignore_whitespace) {
      # Normalize whitespace by trimming and collapsing multiple spaces
      norm_val1 <- gsub("\\s+", " ", trimws(val1))
      norm_val2 <- gsub("\\s+", " ", trimws(val2))
      
      if (!identical(norm_val1, norm_val2)) {
        return(list(type = "changed", path = path, old_value = val1, new_value = val2))
      }
    } else {
      if (!identical(val1, val2)) {
        return(list(type = "changed", path = path, old_value = val1, new_value = val2))
      }
    }
    return(NULL)
  }
  
  # Handle other data types (numeric, logical, etc.)
  if (!identical(val1, val2)) {
    return(list(type = "changed", path = path, old_value = val1, new_value = val2))
  }
  
  return(NULL)
}

#' Internal function to format diff output
#' 
#' @param differences List of differences from comparison
#' @param config_path1 Path to first config file
#' @param config_path2 Path to second config file
#' @return Character vector with formatted diff output
.format_diff_output <- function(differences, config_path1, config_path2) {
  if (length(differences) == 0) {
    return("No differences found between configuration files.")
  }
  
  output <- c()
  output <- c(output, paste("=== Configuration Diff ==="))
  output <- c(output, paste("File 1:", config_path1))
  output <- c(output, paste("File 2:", config_path2))
  output <- c(output, "")
  
  # Process differences
  for (diff_group in differences) {
    output <- c(output, .format_single_diff(diff_group))
  }
  
  return(output)
}

#' Internal function to format a single difference
#' 
#' @param diff Single difference object
#' @return Character vector with formatted lines for this difference
.format_single_diff <- function(diff) {
  output <- c()
  
  if (diff$type == "nested") {
    # Handle nested differences
    for (nested_diff in diff$differences) {
      output <- c(output, .format_single_diff(nested_diff))
    }
  } else if (diff$type == "added") {
    output <- c(output, paste0("+ ", diff$path, ": ", .format_value(diff$value)))
  } else if (diff$type == "removed") {
    output <- c(output, paste0("- ", diff$path, ": ", .format_value(diff$value)))
  } else if (diff$type == "changed") {
    output <- c(output, paste0("~ ", diff$path, ":"))
    output <- c(output, paste0("  - ", .format_value(diff$old_value)))
    output <- c(output, paste0("  + ", .format_value(diff$new_value)))
  }
  
  return(output)
}

#' Internal function to format values for display
#' 
#' @param value Value to format
#' @return Character representation of the value
.format_value <- function(value) {
  if (is.null(value)) {
    return("NULL")
  } else if (is.list(value)) {
    # For lists, show a summary
    if (length(value) == 0) {
      return("{}")}
    return(paste0("{", length(value), " items}"))
  } else if (is.character(value) && length(value) == 1) {
    return(paste0('"', value, '"'))
  } else if (is.numeric(value) && length(value) == 1) {
    return(as.character(value))
  } else if (is.logical(value) && length(value) == 1) {
    return(as.character(value))
  } else {
    # For other types or vectors, show the first few elements
    if (length(value) > 1) {
      return(paste0("[", paste(value[1:min(3, length(value))], collapse = ", "), 
                   if (length(value) > 3) ", ..." else "", "]"))
    }
    return(as.character(value))
  }
}