# This file will contain the function to visualize a Rydra model as a Mermaid flowchart.

#' Visualize Rydra Model Configuration as Mermaid Flowchart
#'
#' Parses a Rydra model's configuration (a list derived from YAML) and generates
#' a Mermaid.js flowchart definition string. This string can be used to render
#' a diagram of the model's structure and calculation flow.
#'
#' @param model_config A list representing the configuration for a single Rydra
#'   model (e.g., the content under a specific model name like `main_model` in
#'   the parsed YAML).
#' @param model_name A string, the name of the model (e.g., "main_model"). This
#'  is used for titling the diagram.
#'
#' @return A string containing the Mermaid flowchart definition.
#'
#' @examples
#' \dontrun{
#' # Assuming 'parsed_yaml' is the full list from yaml::read_yaml("config.yaml")
#' # And 'model_key' is the name of the model in the YAML, e.g., "main_model"
#' # model_conf <- parsed_yaml[[model_key]]
#' # mermaid_string <- visualize_rydra_model_as_mermaid(model_conf, model_key)
#' # cat(mermaid_string)
#' # Then, paste the string into mermaid.live or use DiagrammeR to render.
#' }
visualize_rydra_model_as_mermaid <- function(model_config, model_name = "Rydra Model") {
  if (!is.list(model_config)) {
    stop("model_config must be a list.")
  }
  if (!is.character(model_name) || length(model_name) != 1) {
    stop("model_name must be a single string.")
  }

  mermaid_lines <- c(
    paste0("graph TD;"),
    paste0("    subgraph ", sanitize_mermaid_label(model_name))
  )

  # To keep track of defined nodes and avoid re-defining inputs
  # and to manage connections.
  defined_nodes <- character(0)
  all_mermaid_parts <- list()

  # 0. Define a box for all raw inputs that will be discovered
  # We will collect them first from transformations, factors, and conditions
  raw_inputs <- c() # Will be populated by helpers

  # Helper function to sanitize labels and IDs for Mermaid
  sanitize_mermaid_id <- function(id_str) {
    id_str <- gsub("[^A-Za-z0-9_]", "_", id_str)
    # Ensure it doesn't start with a number, which can be problematic
    if (grepl("^[0-9]", id_str)) {
      id_str <- paste0("id_", id_str)
    }
    return(id_str)
  }

  sanitize_mermaid_label <- function(label_str) {
    # Replace problematic characters for labels, keep it readable
    label_str <- gsub("\"", "#quot;", label_str) # Escape quotes
    label_str <- gsub("(", "#lpar;", label_str)  # Escape parentheses
    label_str <- gsub(")", "#rpar;", label_str)  # Escape parentheses
    label_str <- gsub("[", "#lsqb;", label_str) # Escape square brackets
    label_str <- gsub("]", "#rsqb;", label_str) # Escape square brackets
    label_str <- gsub("{", "#lcub;", label_str) # Escape curly brackets
    label_str <- gsub("}", "#rcub;", label_str) # Escape curly brackets
    return(label_str)
  }


  # 1. Transformations
  # transform_mermaid <- generate_transformations_mermaid(model_config$transformations,
  #                                                       defined_nodes,
  #                                                       raw_inputs,
  #                                                       sanitize_mermaid_id,
  #                                                       sanitize_mermaid_label)
  transform_mermaid_result <- .generate_transformations_mermaid(
    model_config$transformations,
    defined_nodes,
    raw_inputs,
    sanitize_mermaid_id,
    sanitize_mermaid_label
  )
  all_mermaid_parts <- c(all_mermaid_parts, list(transform_mermaid_result$mermaid))
  defined_nodes <- transform_mermaid_result$defined_nodes
  raw_inputs <- transform_mermaid_result$raw_inputs

  # 2. Factors
  factors_mermaid_result <- .generate_factors_mermaid(
    model_config$factors,
    model_config$coefficients, # Or however coefficients are accessed
    defined_nodes,
    raw_inputs,
    sanitize_mermaid_id,
    sanitize_mermaid_label
  )
  all_mermaid_parts <- c(all_mermaid_parts, list(factors_mermaid_result$mermaid))
  defined_nodes <- factors_mermaid_result$defined_nodes
  raw_inputs <- factors_mermaid_result$raw_inputs

  # 3. Conditions
  conditions_mermaid_result <- .generate_conditions_mermaid(
    model_config$conditions,
    model_config$coefficients, # Or however coefficients are accessed
    defined_nodes,
    raw_inputs,
    sanitize_mermaid_id,
    sanitize_mermaid_label
  )
  all_mermaid_parts <- c(all_mermaid_parts, list(conditions_mermaid_result$mermaid))
  defined_nodes <- conditions_mermaid_result$defined_nodes
  raw_inputs <- conditions_mermaid_result$raw_inputs

  # X. Define Raw Inputs collected (needs to be before they are used in links)
  # This should be done *after* all components that might identify raw inputs have run,
  # but *before* those raw inputs are visually used in links if not already defined.
  # The current structure adds links like `input_X --> some_node` assuming `input_X` will be defined.
  # So, the call to .generate_inputs_mermaid should ideally come after processing other components
  # that discover these inputs, but its output should be prepended to the mermaid lines.

  # Let's adjust the order of operations slightly:
  # All component processing first to gather all raw_inputs.
  # Then generate raw_inputs_mermaid.
  # Then combine.

  # 4. Calculation steps (intercepts, direct coefficients, aggregation)
  calc_steps_mermaid_result <- .generate_calculation_steps_mermaid(
    model_config, # pass the whole model_config
    defined_nodes,
    sanitize_mermaid_id,
    sanitize_mermaid_label
  )
  all_mermaid_parts <- c(all_mermaid_parts, list(calc_steps_mermaid_result$mermaid))
  defined_nodes <- calc_steps_mermaid_result$defined_nodes

  # 5. Output Transformation
  output_trans_mermaid_result <- .generate_output_transformation_mermaid(
    model_config$output_transformation,
    sanitize_id("total_score_pre_output"), # Expected ID from calc_steps
    defined_nodes,
    sanitize_mermaid_id,
    sanitize_mermaid_label
  )
  all_mermaid_parts <- c(all_mermaid_parts, list(output_trans_mermaid_result$mermaid))
  defined_nodes <- output_trans_mermaid_result$defined_nodes # Update defined_nodes

  # Now that all components have been processed and raw_inputs collected,
  # generate the Mermaid code for the raw input nodes.
  # This needs to be prepended to ensure inputs are defined before being linked.
  raw_inputs_mermaid_result <- .generate_inputs_mermaid(
    unique(raw_inputs),
    defined_nodes, # Pass current defined_nodes to avoid re-defining if an input somehow got defined elsewhere
    sanitize_mermaid_id,
    sanitize_mermaid_label
  )
  # Update defined_nodes, though less critical as this is the last definition part.
  defined_nodes <- raw_inputs_mermaid_result$defined_nodes

  # Prepend input definitions to all other parts
  all_mermaid_parts <- c(list(raw_inputs_mermaid_result$mermaid), all_mermaid_parts)

  # Combine all parts
  mermaid_lines <- c(mermaid_lines, unlist(all_mermaid_parts))

  mermaid_lines <- c(mermaid_lines, "    end") # End subgraph
  mermaid_lines <- c(mermaid_lines, "") # Extra line for readability

  # Placeholder for actual logic using helper functions
  # For now, just returns a very simple diagram to show structure.
  if (length(unlist(all_mermaid_parts)) == 0) {
      mermaid_lines <- c(mermaid_lines, "    EmptyModel[No components found to visualize];")
  }

  return(paste(mermaid_lines, collapse = "\n"))
}

# Helper functions
# ----------------

# Sanitize string for Mermaid ID
# (already defined within visualize_rydra_model_as_mermaid,
#  but could be global if preferred and passed or re-defined in helpers)

# Sanitize string for Mermaid Label
# (already defined within visualize_rydra_model_as_mermaid)


#' Extracts variable names from a formula string.
#' This is a simplified parser. It looks for words that are not numbers
#' and are not common operators or function names used in formulas.
#' It might need to be made more robust based on typical formula complexity.
#' @param formula_str The formula string (e.g., "center_variable(age, centering.age)")
#' @return A character vector of potential variable names.
.extract_variables_from_formula <- function(formula_str) {
  # Remove function calls like some_func(...) to get arguments
  # This regex tries to capture content within parentheses if it's part of a function call pattern
  # It's tricky; for now, let's focus on simpler extraction.

  # Remove content within quotes
  formula_str <- gsub("\"[^\"]*\"", "", formula_str)
  formula_str <- gsub("'[^']*'", "", formula_str)

  # Replace parentheses, commas, and common operators with spaces
  cleaned_formula <- gsub("[(),=*/+-]+", " ", formula_str)

  # Split by space and filter
  potential_vars <- unlist(strsplit(cleaned_formula, "\\s+"))

  # Filter out numbers, empty strings, and known keywords/functions if necessary
  # For now, this is very basic. A more sophisticated approach might involve
  # checking against a list of known transformation function names.
  potential_vars <- potential_vars[potential_vars != ""]
  potential_vars <- potential_vars[!grepl("^[0-9\\.]+$", potential_vars)] # Remove numeric values

  # Remove R's built-in keywords that might appear (very basic list)
  r_keywords <- c("TRUE", "FALSE", "NULL", "NA", "if", "else", "for", "while", "in")
  potential_vars <- potential_vars[!potential_vars %in% r_keywords]

  # Remove common Rydra function names (this list might need to be expanded)
  # Or, better, identify the main function call and exclude it.
  # For example, in "center_variable(age, centering.age)", "center_variable" is the function.
  # This is complex. A simpler heuristic for now:
  # The first "word" is often the function name.
  # However, formulas can be "var1 + var2".

  # Let's assume variables are typically standalone words or words with dots/underscores.
  # This regex will find sequences of letters, numbers, dots, and underscores.
  vars <- gregexpr("[a-zA-Z_][a-zA-Z0-9_\\.]*", formula_str, perl = TRUE)
  extracted <- regmatches(formula_str, vars)[[1]]

  # Filter out any purely numeric strings again, and known function names if possible.
  # This needs to be context-aware (e.g. `log(income)` -> `income` is input)
  # `center_variable(age, centering.age)` -> `age` and `centering.age` are inputs.

  # A very basic assumption: the first word in a `func(arg1, arg2)` structure is the function itself.
  # And things like `centering.age` are usually config paths, not direct data inputs.
  # This is the hardest part to generalize perfectly without a full parser.

  # For transformation formulas like "center_variable(age, centering.age)":
  # "age" is an input variable. "centering.age" is a config lookup.
  # We are interested in "age".

  # For factor/condition formulas like "age > 10": "age" is an input.

  # Let's try to extract all "words" and then filter known config paths or function names.
  # This is still heuristic.

  # Remove the function name itself from the list of variables
  # e.g., for "center_variable(age, centering.age)", "center_variable" should not be listed as an input.
  # This simple regex looks for the function name at the start of the formula.
  function_name_match <- regexpr("^[a-zA-Z_][a-zA-Z0-9_]*\\s*\\(", formula_str)
  if (attr(function_name_match, "match.length") > 0) {
    func_name <- gsub("\\s*\\($", "", regmatches(formula_str, function_name_match))
    extracted <- extracted[extracted != func_name]
  }

  # Filter out paths that look like they are from 'coefficients.' or 'intercepts.' or 'centering.'
  # as these are config values, not primary data inputs from 'data'.
  config_like_paths <- grepl("^(coefficients|intercepts|centering)\\.", extracted)
  extracted <- extracted[!config_like_paths]

  # Filter out common function names that might be part of the formula but not inputs
  # (e.g., if a formula was "my_func(other_func(input_var))"). This is hard.
  # For now, we rely on the above filters.

  return(unique(extracted))
}


#' Generates Mermaid syntax for raw input nodes.
#' @param input_names Character vector of raw input variable names.
#' @param defined_nodes_ref A character vector (passed by reference via environment or list element)
#'                          of already defined Mermaid node IDs. Updated by this function.
#' @param sanitize_id Function to sanitize strings for Mermaid IDs.
#' @param sanitize_label Function to sanitize strings for Mermaid labels.
#' @return A list with $mermaid (character vector of Mermaid lines) and $defined_nodes (updated).
.generate_inputs_mermaid <- function(input_names, defined_nodes_ref, sanitize_id, sanitize_label) {
  mermaid_lines <- c()

  if (length(input_names) > 0) {
    mermaid_lines <- c(mermaid_lines, "    subgraph Raw Inputs")
    for (input_name in unique(input_names)) {
      input_id <- sanitize_id(paste0("input_", input_name))
      if (!input_id %in% defined_nodes_ref) {
        mermaid_lines <- c(mermaid_lines, paste0("        ", input_id, "[(\"", sanitize_label(input_name), "\")]"))
        defined_nodes_ref <- c(defined_nodes_ref, input_id)
      }
    }
    mermaid_lines <- c(mermaid_lines, "    end")
  }
  return(list(mermaid = mermaid_lines, defined_nodes = defined_nodes_ref))
}

#' Generates Mermaid syntax for transformations.
#' @param transformations List of transformation configurations from the model.
#' @param defined_nodes_ref A character vector of already defined Mermaid node IDs. Updated.
#' @param raw_inputs_ref A character vector to collect raw input names found. Updated.
#' @param sanitize_id Function to sanitize strings for Mermaid IDs.
#' @param sanitize_label Function to sanitize strings for Mermaid labels.
#' @return List with $mermaid, $defined_nodes, $raw_inputs.
.generate_transformations_mermaid <- function(transformations, defined_nodes_ref, raw_inputs_ref, sanitize_id, sanitize_label) {
  mermaid_lines <- c()
  if (is.null(transformations) || length(transformations) == 0) {
    return(list(mermaid = c(), defined_nodes = defined_nodes_ref, raw_inputs = raw_inputs_ref))
  }

  mermaid_lines <- c(mermaid_lines, "    subgraph Transformations")

  for (i in seq_along(transformations)) {
    transform <- transformations[[i]]
    if (!is.list(transform) || is.null(transform$name) || is.null(transform$formula)) {
      warning(paste("Skipping invalid transformation entry at index", i))
      next
    }

    # Transformed variable node (output of the transformation)
    # Style: rectangle with rounded corners for operations
    output_var_name <- transform$name
    output_var_id <- sanitize_id(paste0("tvar_", output_var_name))
    # Label includes the formula
    label <- paste0(output_var_name, " = ", transform$formula)
    mermaid_lines <- c(mermaid_lines, paste0("        ", output_var_id, "[\"", sanitize_label(label), "\"]"))
    defined_nodes_ref <- c(defined_nodes_ref, output_var_id)

    # Identify input variables from the formula
    # This is a heuristic and might need refinement.
    # We assume variables are words not part of `centering.X` or `coefficients.Y`
    formula_inputs <- .extract_variables_from_formula(transform$formula)

    for (input_name in formula_inputs) {
      # Determine if this input is a raw input or an output of a previous transformation
      potential_prev_tvar_id <- sanitize_id(paste0("tvar_", input_name))
      potential_input_id <- sanitize_id(paste0("input_", input_name))

      if (potential_prev_tvar_id %in% defined_nodes_ref) { # It's an output of a previous transform
        mermaid_lines <- c(mermaid_lines, paste0("        ", potential_prev_tvar_id, " --> ", output_var_id))
      } else { # Assume it's a raw input
        raw_inputs_ref <- c(raw_inputs_ref, input_name) # Collect it
        # Link will be from potential_input_id (defined by .generate_inputs_mermaid later)
        mermaid_lines <- c(mermaid_lines, paste0("        ", potential_input_id, " --> ", output_var_id))
        # We don't add potential_input_id to defined_nodes_ref here; .generate_inputs_mermaid does that
      }
    }
  }
  mermaid_lines <- c(mermaid_lines, "    end")

  return(list(mermaid = mermaid_lines,
              defined_nodes = defined_nodes_ref,
              raw_inputs = unique(raw_inputs_ref)))
}

#' Generates Mermaid syntax for factors.
#' @param factors List of factor configurations from the model.
#' @param coefficients The coefficients block from the model config (for looking up values).
#' @param defined_nodes_ref A character vector of already defined Mermaid node IDs. Updated.
#' @param raw_inputs_ref A character vector to collect raw input names found. Updated.
#' @param sanitize_id Function to sanitize strings for Mermaid IDs.
#' @param sanitize_label Function to sanitize strings for Mermaid labels.
#' @return List with $mermaid, $defined_nodes, $raw_inputs.
.generate_factors_mermaid <- function(factors, coefficients, defined_nodes_ref, raw_inputs_ref, sanitize_id, sanitize_label) {
  mermaid_lines <- c()
  if (is.null(factors) || length(factors) == 0) {
    return(list(mermaid = c(), defined_nodes = defined_nodes_ref, raw_inputs = raw_inputs_ref))
  }

  mermaid_lines <- c(mermaid_lines, "    subgraph Factors")

  # Define a central node for all factor contributions
  factors_sum_id <- sanitize_id("factors_sum")
  if (!factors_sum_id %in% defined_nodes_ref) {
    mermaid_lines <- c(mermaid_lines, paste0("        ", factors_sum_id, "[\"Sum of Factor Contributions\"]"))
    defined_nodes_ref <- c(defined_nodes_ref, factors_sum_id)
  }

  for (i in seq_along(factors)) {
    factor <- factors[[i]]
    if (!is.list(factor) || is.null(factor$name) || is.null(factor$levels)) {
      warning(paste("Skipping invalid factor entry at index", i))
      next
    }

    factor_name <- factor$name
    factor_node_id <- sanitize_id(paste0("factor_", factor_name))

    # Node for the factor itself (as a decision point)
    # Style: diamond for decision/choice based on input variable
    mermaid_lines <- c(mermaid_lines, paste0("        ", factor_node_id, "{{\"", sanitize_label(paste("Factor:", factor_name)), "\"}}"))
    defined_nodes_ref <- c(defined_nodes_ref, factor_node_id)

    # The factor's input variable
    # This variable name is what's looked up in the data.
    # It could be a raw input or a transformed variable.
    factor_input_var_name <- factor_name # By convention in Rydra, factor name is the variable name

    potential_prev_tvar_id <- sanitize_id(paste0("tvar_", factor_input_var_name))
    potential_input_id <- sanitize_id(paste0("input_", factor_input_var_name))

    if (potential_prev_tvar_id %in% defined_nodes_ref) { # It's an output of a transform
      mermaid_lines <- c(mermaid_lines, paste0("        ", potential_prev_tvar_id, " --> ", factor_node_id))
    } else { # Assume it's a raw input
      raw_inputs_ref <- c(raw_inputs_ref, factor_input_var_name) # Collect it
      mermaid_lines <- c(mermaid_lines, paste0("        ", potential_input_id, " --> ", factor_node_id))
    }

    # Each level of the factor leads to a coefficient
    for (level in factor$levels) {
      if (!is.list(level) || is.null(level$value) || is.null(level$coefficient)) {
        warning(paste("Skipping invalid level in factor", factor_name))
        next
      }

      level_value_label <- sanitize_label(paste0(factor_name, " is ", level$value))
      coeff_path <- level$coefficient

      # Coefficient node (values are not shown directly in this graph part, but path is)
      # We could try to resolve the coefficient value if `coefficients` list is simple.
      # For now, just the path.
      coeff_label <- sanitize_label(paste("Coeff:", coeff_path))
      coeff_node_id <- sanitize_id(paste0("coeff_", gsub("\\.", "_", coeff_path))) # Create a unique ID from path

      if (!coeff_node_id %in% defined_nodes_ref) {
        # Style: parallelogram for data/coefficients
        mermaid_lines <- c(mermaid_lines, paste0("        ", coeff_node_id, "[/", sanitize_label(coeff_path), "/]"))
        defined_nodes_ref <- c(defined_nodes_ref, coeff_node_id)
      }

      # Link from factor decision to the coefficient applied for that level
      mermaid_lines <- c(mermaid_lines, paste0("        ", factor_node_id, " -- \"", level_value_label, "\" --> ", coeff_node_id))
      # Link from this specific coefficient contribution to the sum of factors
      mermaid_lines <- c(mermaid_lines, paste0("        ", coeff_node_id, " --> ", factors_sum_id))
    }
  }
  mermaid_lines <- c(mermaid_lines, "    end")

  return(list(mermaid = mermaid_lines,
              defined_nodes = defined_nodes_ref,
              raw_inputs = unique(raw_inputs_ref)))
}

#' Generates Mermaid syntax for conditions.
#' @param conditions List of condition configurations from the model.
#' @param coefficients The coefficients block from the model config.
#' @param defined_nodes_ref A character vector of already defined Mermaid node IDs. Updated.
#' @param raw_inputs_ref A character vector to collect raw input names found. Updated.
#' @param sanitize_id Function to sanitize strings for Mermaid IDs.
#' @param sanitize_label Function to sanitize strings for Mermaid labels.
#' @return List with $mermaid, $defined_nodes, $raw_inputs.
.generate_conditions_mermaid <- function(conditions, coefficients, defined_nodes_ref, raw_inputs_ref, sanitize_id, sanitize_label) {
  mermaid_lines <- c()
  if (is.null(conditions) || length(conditions) == 0) {
    return(list(mermaid = c(), defined_nodes = defined_nodes_ref, raw_inputs = raw_inputs_ref))
  }

  mermaid_lines <- c(mermaid_lines, "    subgraph Conditions")

  # Define a central node for all conditional contributions
  conditions_sum_id <- sanitize_id("conditions_sum")
  if (!conditions_sum_id %in% defined_nodes_ref) {
    mermaid_lines <- c(mermaid_lines, paste0("        ", conditions_sum_id, "[\"Sum of Conditional Contributions\"]"))
    defined_nodes_ref <- c(defined_nodes_ref, conditions_sum_id)
  }

  for (i in seq_along(conditions)) {
    condition_item <- conditions[[i]]
    if (!is.list(condition_item) || is.null(condition_item$name) || is.null(condition_item$condition) || is.null(condition_item$coefficient)) {
      warning(paste("Skipping invalid condition entry at index", i))
      next
    }

    condition_name <- condition_item$name
    condition_logic_str <- condition_item$condition # The R expression string
    condition_id <- sanitize_id(paste0("cond_", condition_name))

    # Node for the condition logic (diamond for decision)
    # Label shows the R expression
    mermaid_lines <- c(mermaid_lines, paste0("        ", condition_id, "{{\"", sanitize_label(condition_logic_str), "\"}}"))
    defined_nodes_ref <- c(defined_nodes_ref, condition_id)

    # Identify input variables from the condition string (using the same helper as for transformations)
    condition_inputs <- .extract_variables_from_formula(condition_logic_str)

    for (input_name in condition_inputs) {
      potential_prev_tvar_id <- sanitize_id(paste0("tvar_", input_name))
      potential_input_id <- sanitize_id(paste0("input_", input_name))

      if (potential_prev_tvar_id %in% defined_nodes_ref) {
        mermaid_lines <- c(mermaid_lines, paste0("        ", potential_prev_tvar_id, " --> ", condition_id))
      } else {
        raw_inputs_ref <- c(raw_inputs_ref, input_name)
        mermaid_lines <- c(mermaid_lines, paste0("        ", potential_input_id, " --> ", condition_id))
      }
    }

    # Coefficient applied if condition is true
    coeff_path <- condition_item$coefficient
    coeff_label <- sanitize_label(paste("Coeff:", coeff_path))
    coeff_node_id <- sanitize_id(paste0("coeff_cond_", gsub("\\.", "_", coeff_path), "_",condition_name)) # Ensure unique for conditions

    if (!coeff_node_id %in% defined_nodes_ref) {
      mermaid_lines <- c(mermaid_lines, paste0("        ", coeff_node_id, "[/", sanitize_label(coeff_path), "/]"))
      defined_nodes_ref <- c(defined_nodes_ref, coeff_node_id)
    }

    # Link from condition (if true) to the coefficient
    mermaid_lines <- c(mermaid_lines, paste0("        ", condition_id, " -- \"If TRUE\" --> ", coeff_node_id))
    # Link from this coefficient to the sum of conditions
    mermaid_lines <- c(mermaid_lines, paste0("        ", coeff_node_id, " --> ", conditions_sum_id))
  }
  mermaid_lines <- c(mermaid_lines, "    end")

  return(list(mermaid = mermaid_lines,
              defined_nodes = defined_nodes_ref,
              raw_inputs = unique(raw_inputs_ref)))
}

#' Generates Mermaid syntax for calculation summary steps.
#' This includes intercepts, direct coefficients, and aggregation points.
#' @param model_config The full model configuration.
#' @param defined_nodes_ref A character vector of already defined Mermaid node IDs. Updated.
#' @param sanitize_id Function to sanitize strings for Mermaid IDs.
#' @param sanitize_label Function to sanitize strings for Mermaid labels.
#' @return List with $mermaid, $defined_nodes.
.generate_calculation_steps_mermaid <- function(model_config, defined_nodes_ref, sanitize_id, sanitize_label) {
  mermaid_lines <- c()
  if (is.null(model_config)) {
    return(list(mermaid = c(), defined_nodes = defined_nodes_ref))
  }

  mermaid_lines <- c(mermaid_lines, "    subgraph CalculationSummary")

  # --- Intercepts ---
  intercept_node_ids <- c()
  if (!is.null(model_config$intercepts) && length(model_config$intercepts) > 0) {
    # Overall intercept contribution node
    intercepts_sum_id <- sanitize_id("intercepts_sum")
    mermaid_lines <- c(mermaid_lines, paste0("        ", intercepts_sum_id, "[\"Sum of Intercepts\"]"))
    defined_nodes_ref <- c(defined_nodes_ref, intercepts_sum_id)
    intercept_node_ids <- c(intercept_node_ids, intercepts_sum_id)

    for (intercept_name in names(model_config$intercepts)) {
      intercept_val <- model_config$intercepts[[intercept_name]]
      # Node for individual intercept
      ind_intercept_id <- sanitize_id(paste0("intercept_", intercept_name))
      label <- paste0(intercept_name, ": ", intercept_val)
      mermaid_lines <- c(mermaid_lines, paste0("        ", ind_intercept_id, "[/", sanitize_label(label), "/]"))
      defined_nodes_ref <- c(defined_nodes_ref, ind_intercept_id)
      # Link individual intercept to the sum of intercepts
      mermaid_lines <- c(mermaid_lines, paste0("        ", ind_intercept_id, " --> ", intercepts_sum_id))
    }
  }

  # --- Direct Coefficients (applied to transformed variables) ---
  # These are coefficients not part of factors or conditions, but applied directly.
  # The Rydra structure primarily uses coefficients through transformations (implicitly), factors, and conditions.
  # Direct coefficients are those in the `coefficients` block that are used by name in formulas
  # or are implicitly part of the `base_score` if a variable with the same name exists.
  # This part is tricky to visualize perfectly without knowing how `rydra_calculate`
  # exactly distinguishes "direct" application vs. use within transformations.
  # For now, we will assume transformed variables are the primary way continuous variables contribute,
  # and their "coefficient" is embedded in the transformation formula itself or how it's used.
  # The README's "Base Score" calculation implies direct multiplication:
  # `base_score = intercept + sum(coefficient * transformed_variable_value)`
  # So, we need to show transformed variables linking to a "Base Score Calculation" node,
  # possibly via their respective coefficient nodes.

  base_score_id <- sanitize_id("base_score_calc")
  mermaid_lines <- c(mermaid_lines, paste0("        ", base_score_id, "[\"Base Score Calculation\"]"))
  defined_nodes_ref <- c(defined_nodes_ref, base_score_id)

  if(length(intercept_node_ids) > 0) {
      mermaid_lines <- c(mermaid_lines, paste0("        ", intercepts_sum_id, " --> ", base_score_id))
  }

  # Link transformed variables that are used with direct coefficients.
  # The `model_config$coefficients` list contains all coefficients.
  # We need to identify which of these are *not* already handled by factors/conditions.
  # And how they pair with transformed variables.
  if (!is.null(model_config$coefficients) && !is.null(model_config$transformations)) {
    # Get names of all defined coefficients
    all_coeff_names <- names(model_config$coefficients)

    # Get names of coefficients used in factors
    factor_coeff_paths <- c()
    if (!is.null(model_config$factors)) {
      for (factor in model_config$factors) {
        if(!is.null(factor$levels)) {
          for (level in factor$levels) factor_coeff_paths <- c(factor_coeff_paths, level$coefficient)
        }
      }
    }
    factor_coeff_names <- gsub("^(coefficients\\.|intercepts\\.)", "", factor_coeff_paths)

    # Get names of coefficients used in conditions
    condition_coeff_paths <- c()
    if (!is.null(model_config$conditions)) {
      for (cond in model_config$conditions) condition_coeff_paths <- c(condition_coeff_paths, cond$coefficient)
    }
    condition_coeff_names <- gsub("^(coefficients\\.|intercepts\\.)", "", condition_coeff_paths)

    # Identify direct coefficients: those in `coefficients` block not used by factors/conditions
    # This logic might be too simplistic if coefficients can be reused.
    # A common pattern is `age_centered: 0.05` in YAML, and `age_centered` is a transformed var.
    # The link is often by name matching.

    # Iterate through transformed variables. If a coefficient with the same name exists,
    # and it's not solely a factor/condition coefficient, link it.
    for (transform in model_config$transformations) {
      tvar_name <- transform$name
      if (tvar_name %in% all_coeff_names) {
        # Check if this coefficient is used *only* for factors/conditions or also directly
        # This is still a heuristic. If 'tvar_name' coefficient is listed, it's likely direct.
        coeff_val <- model_config$coefficients[[tvar_name]]
        coeff_node_id <- sanitize_id(paste0("direct_coeff_", tvar_name))
        label <- paste0("Coeff: ", tvar_name, ": ", coeff_val)

        if(!coeff_node_id %in% defined_nodes_ref) {
            mermaid_lines <- c(mermaid_lines, paste0("        ", coeff_node_id, "[/", sanitize_label(label), "/]"))
            defined_nodes_ref <- c(defined_nodes_ref, coeff_node_id)
        }

        # Link transformed variable to its direct coefficient node
        tvar_id <- sanitize_id(paste0("tvar_", tvar_name))
        if(tvar_id %in% defined_nodes_ref) { # Transformed var should exist
             # This forms: TransformedVar --> ItsDirectCoefficient --> BaseScoreCalc
            mermaid_lines <- c(mermaid_lines, paste0("        ", tvar_id, " --> ", coeff_node_id))
            mermaid_lines <- c(mermaid_lines, paste0("        ", coeff_node_id, " --> ", base_score_id))
        }
      }
    }
  }

  # --- Aggregation Point: Total Score Pre-Output Transform ---
  total_score_pre_output_id <- sanitize_id("total_score_pre_output")
  mermaid_lines <- c(mermaid_lines, paste0("        ", total_score_pre_output_id, "([\"Total Score (Pre-Output)\"])"))
  defined_nodes_ref <- c(defined_nodes_ref, total_score_pre_output_id)

  # Link Base Score to Total Score
  mermaid_lines <- c(mermaid_lines, paste0("        ", base_score_id, " --> ", total_score_pre_output_id))

  # Link Sum of Factor Contributions (if exists) to Total Score
  factors_sum_id <- sanitize_id("factors_sum")
  if (factors_sum_id %in% defined_nodes_ref) {
    mermaid_lines <- c(mermaid_lines, paste0("        ", factors_sum_id, " --> ", total_score_pre_output_id))
  }

  # Link Sum of Conditional Contributions (if exists) to Total Score
  conditions_sum_id <- sanitize_id("conditions_sum")
  if (conditions_sum_id %in% defined_nodes_ref) {
    mermaid_lines <- c(mermaid_lines, paste0("        ", conditions_sum_id, " --> ", total_score_pre_output_id))
  }

  mermaid_lines <- c(mermaid_lines, "    end")

  return(list(mermaid = mermaid_lines, defined_nodes = defined_nodes_ref))
}

#' Generates Mermaid syntax for the output transformation step.
#' @param output_transformation_str The output transformation string from YAML (e.g., "truncate_variable(result, 0, 100)").
#' @param prev_node_id The ID of the node that feeds into this transformation (e.g., "total_score_pre_output").
#' @param defined_nodes_ref A character vector of already defined Mermaid node IDs. Updated.
#' @param sanitize_id Function to sanitize strings for Mermaid IDs.
#' @param sanitize_label Function to sanitize strings for Mermaid labels.
#' @return List with $mermaid, $defined_nodes.
.generate_output_transformation_mermaid <- function(output_transformation_str, prev_node_id, defined_nodes_ref, sanitize_id, sanitize_label) {
  mermaid_lines <- c()
  if (is.null(output_transformation_str) || nchar(output_transformation_str) == 0) {
    # If no output transformation, the prev_node_id is effectively the final result before this step.
    # We can create a "Final Result" node and link prev_node_id to it.
    final_result_node_id <- sanitize_id("final_result")
    if (!final_result_node_id %in% defined_nodes_ref) {
        mermaid_lines <- c(mermaid_lines, paste0("    ", final_result_node_id, "([\"Final Result\"])")) # Stadium shape for end
        defined_nodes_ref <- c(defined_nodes_ref, final_result_node_id)
    }
    if (prev_node_id %in% defined_nodes_ref) { # prev_node_id should exist
        mermaid_lines <- c(mermaid_lines, paste0("    ", prev_node_id, " --> ", final_result_node_id))
    }
    return(list(mermaid = mermaid_lines, defined_nodes = defined_nodes_ref))
  }

  # Node for the output transformation itself
  # Style: rectangle with rounded corners for operations
  output_trans_id <- sanitize_id("output_transformation_step")
  label <- sanitize_label(paste("Output Transform:", output_transformation_str))
  mermaid_lines <- c(mermaid_lines, paste0("    ", output_trans_id, "[\"", label, "\"]"))
  defined_nodes_ref <- c(defined_nodes_ref, output_trans_id)

  # Link from the pre-output total score to this transformation
  if (prev_node_id %in% defined_nodes_ref) { # prev_node_id should exist
    mermaid_lines <- c(mermaid_lines, paste0("    ", prev_node_id, " --> ", output_trans_id))
  }

  # Node for the final result
  final_result_node_id <- sanitize_id("final_result")
  # Style: stadium shape for start/end
  mermaid_lines <- c(mermaid_lines, paste0("    ", final_result_node_id, "([\"Final Result\"])"))
  defined_nodes_ref <- c(defined_nodes_ref, final_result_node_id)

  # Link from output transformation to final result
  mermaid_lines <- c(mermaid_lines, paste0("    ", output_trans_id, " --> ", final_result_node_id))

  return(list(mermaid = mermaid_lines, defined_nodes = defined_nodes_ref))
}
