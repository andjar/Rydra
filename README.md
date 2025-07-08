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
*   `transformations`: A named list of functions that can be used in the `transformations` section of the YAML configuration file.

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

# Define the transformation functions
transformations <- list(
  center_variable = center_variable,
  square_variable = square_variable,
  log_transform = log_transform
)

# Perform the calculation
result <- rydra_calculate(
  config_path = system.file("extdata", "example_config.yaml", package = "Rydra"),
  data = input_data,
  transformations = transformations,
  model_name = "main_model"
)

# Print the result
print(result)
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
*   `log_transform(x, base = exp(1))`: Log-transforms a variable.

You can also use your own custom transformation functions in your YAML configuration file. To do this, you simply need to define the function in your R environment and pass it to the `rydra_calculate()` function in the `transformations` list.

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

The `factors` section of the YAML configuration file allows you to define and manage categorical variables. Each factor has a `name` and a list of `levels`. Each `level` has a `value` (the actual value of the factor in your data) and a `coefficient` (the path to the coefficient in the `coefficients` section that should be applied when this level is present). You should explicitly define all levels, including the baseline, to ensure proper validation and clarity.

### Output Transformation

The `output_transformation` section of the YAML configuration file allows you to apply a final transformation to the calculated score. This can be any valid R expression.
