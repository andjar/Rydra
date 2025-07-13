#' ---
#' title: "Vignette 1: Simple Pricing Model"
#' ---
#'
#' This vignette demonstrates a basic use case of `Rydra` for creating a simple pricing model.
#'
#' ## 1. The Pricing Model
#'
#' The pricing model calculates the total price based on the number of users and whether a discount is applied.
#'
#' The model is defined in the `pricing_model.yaml` file:
#'
#' ```yaml
#' model_name: "basic_pricing"
#'
#' centering:
#'   base_price: 100
#'
#' main_model:
#'   intercepts:
#'     baseline: 0
#'
#'   coefficients:
#'     price_per_user: 5
#'     discount_percentage: -0.1
#'
#'   transformations:
#'     - name: "user_cost"
#'       formula: "multiply_by(users, coefficients.price_per_user)"
#'     - name: "total_price"
#'       formula: "add_value(centering.base_price, user_cost)"
#'     - name: "discount_factor"
#'       formula: "add_value(1, has_discount)"
#'
#'   factors:
#'     - name: "has_discount"
#'       levels:
#'         - value: 0
#'           coefficient: "intercepts.baseline"
#'         - value: 1
#'           coefficient: "coefficients.discount_percentage"
#'
#'   output_transformation: "multiply_by(total_price, discount_factor)"
#' ```
#'
#' ## 2. Using the Pricing Model
#'
#' We can use the `rydra_calculate` function to calculate the price for different scenarios.
#'
#' ### Scenario 1: 10 users, no discount
#'
#' ```{r}
#' library(Rydra)
#'
#' input_data <- list(
#'   users = 10,
#'   has_discount = 0
#' )
#'
#' result <- rydra_calculate(
#'   config_path = system.file("extdata", "pricing_model.yaml", package = "Rydra"),
#'   data = input_data,
#'   model_name = "main_model"
#' )
#'
#' print(result)
#' ```
#'
#' ### Scenario 2: 20 users, with discount
#'
#' ```{r}
#' input_data <- list(
#'   users = 20,
#'   has_discount = 1
#' )
#'
#' result <- rydra_calculate(
#'   config_path = system.file("extdata", "pricing_model.yaml", package = "Rydra"),
#'   data = input_data,
#'   model_name = "main_model"
#' )
#'
#' print(result)
#' ```
#'
#' ## 3. Conclusion
#'
#' This simple example demonstrates how to use `Rydra` to create a versionable and transparent pricing model. The model logic is clearly defined in the YAML file, making it easy to understand, modify, and audit.
