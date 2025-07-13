#' ---
#' title: "Vignette 3: Clinical Trial Patient Selection"
#' ---
#'
#' This vignette demonstrates a complex and realistic use case of `Rydra` for creating a model for selecting patients for a clinical trial.
#'
#' ## 1. The Patient Selection Model
#'
#' This model calculates a score for a patient based on their demographics, lab results, and medical history. The score is used to determine if the patient is a good candidate for the clinical trial.
#'
#' The model is defined in the `clinical_trial_selection.yaml` file:
#'
#' ```yaml
#' model_name: "clinical_trial_selection"
#'
#' centering:
#'   age: 50
#'   creatinine: 1.0
#'
#' main_model:
#'   intercepts:
#'     baseline: 0
#'
#'   coefficients:
#'     age_above_60: 0.1
#'     creatinine_high: 0.2
#'     comorbidity_score_high: 0.3
#'     previous_treatment_failed: 0.4
#'
#'   transformations:
#'     - name: "age_category"
#'       formula: "ifelse(age > 60, 1, 0)"
#'     - name: "creatinine_category"
#'       formula: "ifelse(creatinine > 1.5, 1, 0)"
#'
#'   factors:
#'     - name: "comorbidity_score"
#'       levels:
#'         - value: "low"
#'           coefficient: "intercepts.baseline"
#'         - value: "medium"
#'           coefficient: "intercepts.baseline"
#'         - value: "high"
#'           coefficient: "coefficients.comorbidity_score_high"
#'     - name: "previous_treatment"
#'       levels:
#'         - value: "naive"
#'           coefficient: "intercepts.baseline"
#'         - value: "failed"
#'           coefficient: "coefficients.previous_treatment_failed"
#'         - value: "success"
#'           coefficient: "intercepts.baseline"
#'
#'   conditions:
#'     - name: "age_above_60_cond"
#'       condition: "age_category == 1"
#'       coefficient: "coefficients.age_above_60"
#'     - name: "creatinine_high_cond"
#'       condition: "creatinine_category == 1"
#'       coefficient: "coefficients.creatinine_high"
#'
#'   output_transformation: "pmin(result, 1.0)"
#' ```
#'
#' ## 2. Using the Patient Selection Model
#'
#' We can use the `rydra_calculate` function to calculate the score for different patients.
#'
#' ### Scenario 1: Ideal Candidate
#'
#' ```{r}
#' library(Rydra)
#'
#' input_data <- list(
#'   age = 55,
#'   creatinine = 1.2,
#'   comorbidity_score = "low",
#'   previous_treatment = "naive"
#' )
#'
#' result <- rydra_calculate(
#'   config_path = system.file("extdata", "clinical_trial_selection.yaml", package = "Rydra"),
#'   data = input_data,
#'   model_name = "main_model"
#' )
#'
#' print(result)
#' ```
#'
#' ### Scenario 2: High-Risk Candidate
#'
#' ```{r}
#' input_data <- list(
#'   age = 65,
#'   creatinine = 1.8,
#'   comorbidity_score = "high",
#'   previous_treatment = "failed"
#' )
#'
#' result <- rydra_calculate(
#'   config_path = system.file("extdata", "clinical_trial_selection.yaml", package = "Rydra"),
#'   data = input_data,
#'   model_name = "main_model"
#' )
#'
#' print(result)
#' ```
#'
#' ## 3. Conclusion
#'
#' This example demonstrates how `Rydra` can be used to create highly complex and realistic models. The ability to define transformations, factors, and conditions in a structured YAML file makes it possible to build and manage sophisticated calculation logic. The versionable nature of the configuration file is crucial in a clinical setting, where traceability and reproducibility are paramount.
