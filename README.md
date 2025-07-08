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

  output_transformation: "result * 100"
```

### Transformations

`Rydra` provides a set of built-in transformation functions that you can use in your YAML configuration file. These functions are:

*   `center_variable(x, center)`: Centers a variable by subtracting a centering value.
*   `square_variable(x)`: Squares a variable.
*   `log_transform(x, base = exp(1))`: Log-transforms a variable (natural log by default).
*   `exp_transform(x, base = exp(1))`: Exponentiates a variable (e^x by default).

You can also use your own custom transformation functions. As explained in the "Getting Started" section, you can pass a named list of your functions via the `transformations` argument to `rydra_calculate`. If you do so, remember that this list *replaces* the default set of base transformations, so include any base functions you still need (e.g., `list(my_func = ..., log_transform = Rydra::log_transform)`).

Here is an example of how to use a custom transformation function:

```r
# Define a custom transformation function
my_custom_transformation <- function(x) {
  x * 2
}

# Add the custom transformation function to the transformations list
transformations$my_custom_transformation <- my_custom_transformation

# Use the custom transformation function in the YAML configuration file
# transformations:
#   - name: "my_transformed_variable"
#     formula: "my_custom_transformation(my_variable)"

```

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

The `output_transformation` section of the YAML configuration file allows you to apply a final transformation to the calculated score. This can be any valid R expression.
