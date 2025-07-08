#' Rydra: A Lite, Hydra-Inspired Configuration Package for R
#'
#' Rydra allows you to define complex calculation configurations in YAML files,
#' including transformations, conditional logic, and model parameters. It then
#' provides functions to use these configurations to perform calculations on
#' input data.
#'
#' @keywords internal
#' @aliases Rydra
#' @details
#' Key Functions:
#' \itemize{
#'   \item \code{\link{load_config}}: Loads a YAML configuration file.
#'   \item \code{\link{hydra_calculate}}: Performs calculations based on a loaded
#'     configuration and input data.
#' }
#'
#' @examples
#' \dontrun{
#' # 1. Create a YAML configuration file (e.g., "my_model_config.yaml")
#' #    similar to the structure shown in the package documentation or vignettes.
#'    config_path <- system.file("extdata", "example_config.yaml", package = "Rydra")
#'    if (config_path == "") {
#'      # Fallback for when not installed, e.g. during dev
#'      # Assuming current dir is package root
#'      if(file.exists("inst/extdata/example_config.yaml")) {
#'          config_path <- "inst/extdata/example_config.yaml"
#'      } else {
#'          stop("Example config not found. Ensure package is built or path is correct.")
#'      }
#'    }
#'
#' # 2. Prepare your input data as a list or single-row data frame
#' input_data <- list(
#'   biochemical_ga = 12, # weeks
#'   weight = 70,         # kg
#'   age = 30,            # years
#'   plgf_machine = 1,    # Corresponds to 'Delfia' in example config
#'   race = 1,            # e.g., Caucasian/Other
#'   smoking = 0,         # 0 for No, 1 for Yes
#'   diabetes_type_i = 0, # 0 for No, 1 for Yes
#'   diabetes_type_ii = 0,# 0 for No, 1 for Yes
#'   conception = 1,      # e.g., 1 for Spontaneous, 3 for IVF
#'   previous = 0         # e.g., 0 for Nulliparous
#'   # ... any other variables required by your config's transformations or conditions
#' )
#'
#' # 3. Perform the calculation
#' if (file.exists(config_path)) {
#'   result <- hydra_calculate(config_path = config_path, data = input_data)
#'   print(paste("Calculated result:", result))
#' } else {
#'   print("Could not find example_config.yaml")
#' }
#'
#' # Example of accessing the config directly
#' if (file.exists(config_path)) {
#'   my_config <- load_config(config_path)
#'   # print(my_config$plgf_model$reference_paper)
#' }
#'
#' }
"_PACKAGE"
