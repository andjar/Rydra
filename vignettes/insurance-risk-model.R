#' ---
#' title: "Vignette 2: Insurance Risk Model"
#' ---
#'
#' This vignette demonstrates a more complex use of `Rydra` for creating an insurance risk model.
#'
#' ## 1. The Insurance Risk Model
#'
#' This model calculates the risk score for an individual based on their age, BMI, and smoking status. It also includes a high-risk adjustment for older individuals with a high BMI.
#'
#' The model is defined in the `insurance_risk_model.yaml` file:
#'
#' ```yaml
#' model_name: "insurance_risk"
#'
#' centering:
#'   age: 40
#'   bmi: 25
#'
#' main_model:
#'   intercepts:
#'     baseline: 0.5
#'
#'   coefficients:
#'     age_centered: 0.02
#'     bmi_log: 0.1
#'     smoker_modifier: 0.3
#'     high_risk_adjustment: 0.2
#'
#'   transformations:
#'     - name: "age_centered"
#'       formula: "center_variable(age, centering.age)"
#'     - name: "bmi_log"
#'       formula: "log_transform(bmi)"
#'
#'   factors:
#'     - name: "smoker"
#'       levels:
#'         - value: 0
#'           coefficient: "intercepts.baseline"
#'         - value: 1
#'           coefficient: "coefficients.smoker_modifier"
#'
#'   conditions:
#'     - name: "high_risk_age_bmi"
#'       condition: "age > 50 && bmi > 30"
#'       coefficient: "coefficients.high_risk_adjustment"
#'
#'   output_transformation: "truncate_variable(result, 0, 1)"
#' ```
#'
#' ## 2. Using the Insurance Risk Model
#'
#' We can use the `rydra_calculate` function to calculate the risk score for different individuals.
#'
#' ### Scenario 1: Young, healthy non-smoker
#'
#' ```{r}
#' library(Rydra)
#'
#' input_data <- list(
#'   age = 30,
#'   bmi = 22,
#'   smoker = 0
#' )
#'
#' result <- rydra_calculate(
#'   config_path = system.file("extdata", "insurance_risk_model.yaml", package = "Rydra"),
#'   data = input_data,
#'   model_name = "main_model"
#' )
#'
#' print(result)
#' ```
#'
#' ### Scenario 2: Older, smoker with high BMI
#'
#' ```{r}
#' input_data <- list(
#'   age = 55,
#'   bmi = 32,
#'   smoker = 1
#' )
#'
#' result <- rydra_calculate(
#'   config_path = system.file("extdata", "insurance_risk_model.yaml", package = "Rydra"),
#'   data = input_data,
#'   model_name = "main_model"
#' )
#'
#' print(result)
#' ```
#'
#' ## 3. Conclusion
#'
#' This example demonstrates how `Rydra` can be used to create more complex models with conditional logic. The use of YAML makes the model easy to understand and maintain, even as the complexity grows. The output transformation ensures that the final risk score is always within a valid range.
