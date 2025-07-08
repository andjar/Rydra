# Rydra

**A Hydra-inspired configuration management tool for R**

`Rydra` provides a simple and flexible way to manage configurations for complex calculations in R. It is inspired by the popular Python library [Hydra](https://hydra.cc/), and it allows you to define your calculations in a YAML file, which makes them easy to read, modify, and share.

## Getting Started

To get started with `Rydra`, you first need to install the package from GitHub:

```r
# install.packages("devtools")
devtools::install_github("andjar/Rydra")
```

Once the package is installed, you can use the `rydra_calculate()` function to perform your calculations. This function takes the following arguments:

*   `config_path`: The path to your YAML configuration file.
*   `data`: A list or a single-row data frame containing the input data for your calculation.
*   `model_name`: (Optional) The name of the model configuration block in your YAML file to use (defaults to a common name like "plgf_model" or the first model found, check package documentation for specifics if not using `model_name` from the YAML root).
*   `transformations`: (Optional) A named list of R functions to be made available for use in the `transformations` section of your YAML configuration.
    *   By default, `Rydra` provides a set of base transformation functions: `center_variable`, `square_variable`, `log_transform`, and `exp_transform`.
    *   If you provide your own list to this argument, it will *replace* the default set. This means if you want to use a base function alongside your custom ones, you must include it in the list you provide.
    *   Example: `list(my_custom_func = function(x) x*2, log_transform = Rydra::log_transform)`
    *   Provide `transformations = list()` to use *no* pre-defined R functions from Rydra, relying only on functions globally available in your R session or defined directly in sufficiently complex YAML transformation formulas (though passing functions is cleaner).

Here is an example of how to use the `rydra_calculate()` function:

```r
# Load the Rydra package
library(Rydra)

# Create a sample input data
input_data <- list(
  age = 35,
  income = 60000,
  student = 1,
  employment_status = "Unemployed"
)

# Perform the calculation using default base transformations
# The `transformations` argument is omitted, so Rydra uses its defaults:
# center_variable, square_variable, log_transform, exp_transform.
# The example_config.yaml uses center_variable, square_variable, and log_transform.
result <- rydra_calculate(
  config_path = system.file("extdata", "example_config.yaml", package = "Rydra"),
  data = input_data,
  model_name = "main_model" # example_config.yaml uses 'main_model'
)

# Print the result
print(result)

# Example of providing custom transformations (this would replace defaults):
my_special_log <- function(x) log(x, base = 100)
custom_transform_list <- list(
  log_transform = my_special_log, # Overrides default log_transform
  center_variable = Rydra::center_variable # Still need center_variable for example_config
  # square_variable would be missing here if example_config needed it and we didn't add it
)
# result_custom <- rydra_calculate(
#   config_path = system.file("extdata", "example_config.yaml", package = "Rydra"),
#   data = input_data,
#   model_name = "main_model",
#   transformations = custom_transform_list
# )
# print(result_custom)

# Example of providing no Rydra transformations (use if functions in YAML are globally defined):
# result_no_defaults <- rydra_calculate(
#   config_path = system.file("extdata", "example_config.yaml", package = "Rydra"),
#   data = input_data,
#   model_name = "main_model",
#   transformations = list() # This would likely cause errors for example_config.yaml
# )
# print(result_no_defaults)
```

## YAML Configuration File

The YAML configuration file is the heart of the `Rydra` package. It allows you to define all the parameters, transformations, factors, and coefficients for your specific model. The configuration file has the following structure:

```yaml
model_name: "simplified_model"

centering:
  age: 30
  income: 50000

main_model:
  intercepts:
    baseline: 1.25

  coefficients:
    # Continuous variable coefficients
    age_centered: 0.05
    age_squared_centered: -0.02
    income_log: 0.15

    # Coefficients for categorical variables
    student_modifier: -0.50
    employment_unemployed: -0.25
    employment_self_employed: 0.10

  transformations:
    - name: "age_centered"
      formula: "center_variable(age, centering.age)"
    - name: "age_squared_centered"
      formula: "square_variable(age_centered)"
    - name: "income_log"
      formula: "log_transform(income)"

  factors:
    - name: "student"
      levels:
        - value: 0
          coefficient: "coefficients.baseline" # Assuming 0 is the baseline, so no additional coefficient
        - value: 1
          coefficient: "coefficients.student_modifier"
    - name: "employment_status"
      levels:
        - value: "Employed"
          coefficient: "coefficients.baseline" # Baseline for employment status
        - value: "Unemployed"
          coefficient: "coefficients.employment_unemployed"
        - value: "Self-employed"
          coefficient: "coefficients.employment_self_employed"

  output_transformation: "truncate_variable(result, 0, 100)"
```

### Transformations

`Rydra` provides a set of built-in transformation functions that you can use in your YAML configuration file. These functions are:

*   `center_variable(x, center)`: Centers a variable by subtracting a centering value.
*   `square_variable(x)`: Squares a variable.
*   `log_transform(x, base = exp(1))`: Log-transforms a variable (natural log by default).
*   `exp_transform(x, base = exp(1))`: Exponentiates a variable (e^x by default).

You can also use your own custom transformation functions. As explained in the "Getting Started" section, you can pass a named list of your functions via the `transformations` argument to `rydra_calculate`. If you do so, remember that this list *replaces* the default set of base transformations, so include any base functions you still need (e.g., `list(my_func = ..., log_transform = Rydra::log_transform)`).

### Factors

The `factors` section of the YAML configuration file allows you to define and manage categorical variables. Each factor has a `name` and a list of `levels`. Each `level` has a `value` (the actual value of the factor in your data) and a `coefficient` (the path to the coefficient in the `coefficients` or `intercepts` section that should be applied when this level is present). You should explicitly define all levels, including the baseline, to ensure proper validation and clarity.

### Conditions

The `conditions` section allows you to apply additional coefficients based on logical expressions evaluated against the input data (after transformations). This is useful for implementing interaction effects, applying specific adjustments, or other conditional logic not easily covered by direct factors or transformations.

Each item in the `conditions` list must have:
*   `name`: A descriptive name for the condition.
*   `condition`: An R expression (as a string) that evaluates to `TRUE` or `FALSE`. This expression can reference any variable in your input `data` (including those created by `transformations`) and also elements from the model's `intercepts` or `coefficients` blocks (e.g., `age > intercepts.age_threshold`).
*   `coefficient`: A path string (e.g., `"coefficients.high_risk_adj"` or `"intercepts.special_bonus"`) to a numeric value within the current model's configuration. If the condition evaluates to `TRUE`, this numeric value is added to the total score.

**Example of a `conditions` block:**

```yaml
# Inside your model_name block (e.g., main_model)
# ...
  intercepts:
    baseline: 1.25
    special_bonus: 0.75
    age_threshold: 50
  coefficients:
    age_centered: 0.05
    # ... other coefficients ...
    high_risk_adj: 0.25

  transformations:
    - name: "age_centered"
      formula: "center_variable(age, centering.age)"
    # ... other transformations ...

  factors:
    # ... your factors ...

  conditions:
    - name: "high_risk_adjustment"
      condition: "age_centered > 10 && some_other_variable == 'CategoryA'"
      coefficient: "coefficients.high_risk_adj" # Adds 0.25 if condition is met
    - name: "senior_bonus_if_employed"
      condition: "age > intercepts.age_threshold && employment_status == 'Employed'"
      coefficient: "intercepts.special_bonus"    # Adds 0.75 if condition is met
# ...
```

If a condition's expression is TRUE, the numeric value found at the specified `coefficient` path is added to the score. If FALSE, it contributes nothing.

### Output Transformation

The `output_transformation` section of the YAML configuration file allows you to apply a final transformation to the calculated score. This transformation **must** be a call to a function that is available in the `transformations` list provided to (or defaulted by) the `rydra_calculate` function.

The transformation string should be in the format `"function_name(arguments)"`. The special variable `result` (which holds the total aggregated score before this final step) **must** be used as one of the arguments to the function. This mechanism is intended for final scaling, unit conversions, or applying caps/floors using predefined and allowed transformation functions. Raw R code or arbitrary expressions are no longer permitted here to ensure better control and clarity.

Available built-in transformation functions (which can be used if they are part of the active `transformations` list) suitable for output transformations include:
*   `multiply_by(value, multiplier)`: Multiplies the `value` by `multiplier`.
*   `add_value(value, term)`: Adds `term` to `value`.
*   Standard R functions like `log()`, `exp()`, `pmin()`, `pmax()` can also be used if they are part of the `transformations` list (note: base R functions are not included by default in the `transformations` list; only those explicitly defined in `Rydra:::.default_rydra_transformations` or user-provided lists). To use base R functions, you would typically wrap them or add them to the list passed to `rydra_calculate`. For simplicity, `log_transform` and `exp_transform` are provided by default.

**Examples in YAML:**

To truncate the result to a minimum of 0 and a maximum of 100:
```yaml
output_transformation: "truncate_variable(result, 0, 100)"
```

To scale the result by 100:
```yaml
output_transformation: "multiply_by(result, 100)"
```

To add 5 to the result:
```yaml
output_transformation: "add_value(result, 5)"
```

The `log_transform` function (available by default) can also be used:
```yaml
output_transformation: "log_transform(result, base = 10)"
```

**Important:**
*   The function used (e.g., `multiply_by`) must be present in the named list of functions provided via the `transformations` argument to `rydra_calculate` (or be part of Rydra's default set).
*   The expression must be a single function call.
*   The `result` variable must be explicitly passed as an argument to the function.

### Example: Score Calculation Walkthrough

Let's walk through how a score is calculated with a medium-complex example. We'll use a hypothetical model configuration similar to `main_model` found in `inst/extdata/example_config.yaml`.

**Sample Input Data:**

*   `age`: 40
*   `income`: 70000
*   `student`: 1 (true/active student)
*   `employment_status`: "Unemployed"

**Relevant YAML Configuration Snippets:**

```yaml
centering:
  age: 30
  income: 50000

# model_name: example_calculation_model (hypothetical)
intercepts:
  baseline: 1.25

coefficients:
  age_centered: 0.05
  age_squared_centered: -0.02
  income_log: 0.15
  student_modifier: -0.50             # Used by factors
  employment_unemployed: -0.25      # Used by factors
  # (other coefficients for different factor levels or conditions might exist)

transformations:
  - name: "age_centered"
    formula: "center_variable(age, centering.age)"
  - name: "age_squared_centered"
    formula: "square_variable(age_centered)"
  - name: "income_log"
    formula: "log_transform(income)"

factors:
  - name: "student"
    levels:
      - value: 0
        coefficient: "coefficients.baseline_student_effect" # Hypothetical baseline effect
      - value: 1
        coefficient: "coefficients.student_modifier"
  - name: "employment_status"
    levels:
      - value: "Employed"
        coefficient: "coefficients.baseline_employment_effect" # Hypothetical
      - value: "Unemployed"
        coefficient: "coefficients.employment_unemployed"
      # (other levels)

# conditions: (Assume no conditions for this specific example for simplicity, or they evaluate to 0)
#   - name: "some_condition"
#     condition: "age_centered > 15"
#     coefficient: "coefficients.conditional_add_on"

output_transformation: "truncate_variable(result, 0, 100)"
```

**Calculation Steps:**

1.  **Apply Transformations:**
    *   `age_centered`: `center_variable(40, centering.age=30)` = `40 - 30` = `10`
    *   `age_squared_centered`: `square_variable(age_centered=10)` = `10^2` = `100`
    *   `income_log`: `log_transform(income=70000)` (natural log) ≈ `11.15625`
    *   The `transformed_data` available for subsequent steps will include these values alongside the original data.

2.  **Calculate Base Score:**
    This score includes the baseline intercept plus the sum of (direct coefficient * transformed variable value).
    *   `intercept`: `1.25` (from `intercepts.baseline`)
    *   Direct coefficient contributions:
        *   `age_centered`: `0.05 * 10` = `0.5`
        *   `age_squared_centered`: `-0.02 * 100` = `-2.0`
        *   `income_log`: `0.15 * 11.15625` = `1.6734375`
    *   `base_score` = `1.25 + 0.5 - 2.0 + 1.6734375` = `1.4234375`

3.  **Calculate Factor Coefficients Sum (`factor_coeffs_sum`):**
    This sum comes from looking up the coefficients associated with the active levels of categorical variables.
    *   **Factor "student"**: Input `student` is `1`.
        *   The YAML maps level `1` to `coefficients.student_modifier`, which is `-0.50`.
    *   **Factor "employment_status"**: Input `employment_status` is `"Unemployed"`.
        *   The YAML maps this level to `coefficients.employment_unemployed`, which is `-0.25`.
    *   `factor_coeffs_sum` = `-0.50 + (-0.25)` = `-0.75`

4.  **Calculate Conditional Coefficients Sum (`conditional_coeffs_sum`):**
    This sum comes from any `conditions` that evaluate to true. For this example, we assume no conditions are met or defined.
    *   `conditional_coeffs_sum` = `0`

5.  **Calculate Total Score (before output transformation):**
    `total_score` = `base_score + factor_coeffs_sum + conditional_coeffs_sum`
    `total_score` = `1.4234375 + (-0.75) + 0` = `0.6734375`

6.  **Apply Output Transformation:**
    The `output_transformation` is `truncate_variable(result, 0, 100)`.
    *   `final_result` = `truncate_variable(0.6734375, 0, 100)` = `0.6734375` (since it is within the range)

This `final_result` of `0.6734375` is what `rydra_calculate()` would return for this input data and configuration.

## Logging Calculations

`Rydra` supports optional logging of calculation details to JSON files. This can be useful for debugging, auditing, or reproducing specific calculations.

To enable logging, add a `logging` section to the root of your YAML configuration file:

```yaml
# At the root of your config.yaml, alongside model_name, centering, etc.
logging:
  enabled: true  # Set to true to enable logging
  path: "calculation_logs" # Optional: directory to store log files
                           # Defaults to "./rydra_logs/" if enabled and path is not specified
                           # Path is relative to the current working directory when rydra_calculate is run.

model_name: "simplified_model"
centering:
  # ... rest of your configuration
```

### Log File Content

When logging is enabled, `Rydra` will create a JSON file for each call to `rydra_calculate`. The files are named using a timestamp and a unique UUID, for example: `20231027153000123456_xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx.json`.

Each log file contains the following information:

*   `timestamp`: The date and time (ISO 8601 format) when the calculation was performed.
*   `invocation_params`:
    *   `config_path`: Path to the YAML configuration file used.
    *   `model_name`: The name of the model used from the configuration.
    *   `data`: The original input data provided to `rydra_calculate`.
*   `model_config_used`: The specific part of the configuration that corresponds to the `model_name` used for the calculation.
*   `intermediate_values`:
    *   `input_data_processed_for_calc`: Input data after initial processing (e.g., if a multi-row data frame was input, only the first row is taken and converted to a list).
    *   `transformed_data`: The data after all transformations have been applied.
    *   `factor_coeffs_sum`: The sum of coefficients derived from the `factors` section.
    *   `base_score`: The score calculated from intercepts and direct coefficients multiplied by transformed data values.
    *   `conditional_coeffs_sum`: The sum of coefficients applied due to met `conditions`.
    *   `total_score_pre_output_transform`: The total score before the final `output_transformation` is applied.
*   `final_result`: The final result of the calculation after all steps, including the output transformation.

### Dependencies for Logging

The logging feature uses the following R packages, which will be installed as dependencies of `Rydra`:
*   `jsonlite`: For writing data to JSON format.
*   `uuid`: For generating unique identifiers for log filenames.

If logging fails for any reason (e.g., the specified path is not writable), `Rydra` will issue a warning, but the main calculation will proceed as normal.
