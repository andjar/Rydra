test_that("diffview correctly identifies no differences", {
  # Create two identical temporary config files
  config1_path <- tempfile(fileext = ".yaml")
  config2_path <- tempfile(fileext = ".yaml")
  
  config_content <- c(
    "model_name: test_model",
    "centering:",
    "  age: 30",
    "main_model:",
    "  intercepts:",
    "    baseline: 1.0"
  )
  
  writeLines(config_content, config1_path)
  writeLines(config_content, config2_path)
  
  result <- diffview(config1_path, config2_path)
  
  expect_equal(result, "No differences found between configuration files.")
  
  # Clean up
  unlink(c(config1_path, config2_path))
})

test_that("diffview correctly identifies value changes", {
  # Create two different config files
  config1_path <- tempfile(fileext = ".yaml")
  config2_path <- tempfile(fileext = ".yaml")
  
  writeLines(c(
    "model_name: test_model",
    "centering:",
    "  age: 30",
    "main_model:",
    "  intercepts:",
    "    baseline: 1.0"
  ), config1_path)
  
  writeLines(c(
    "model_name: test_model",
    "centering:",
    "  age: 35",
    "main_model:",
    "  intercepts:",
    "    baseline: 1.5"
  ), config2_path)
  
  result <- diffview(config1_path, config2_path)
  
  expect_true(length(result) > 1)
  expect_true(any(grepl("centering.age", result)))
  expect_true(any(grepl("main_model.intercepts.baseline", result)))
  expect_true(any(grepl("~", result)))  # Should have change indicators
  
  # Clean up
  unlink(c(config1_path, config2_path))
})

test_that("diffview correctly identifies additions and removals", {
  # Create configs with additions and removals
  config1_path <- tempfile(fileext = ".yaml")
  config2_path <- tempfile(fileext = ".yaml")
  
  writeLines(c(
    "model_name: test_model",
    "centering:",
    "  age: 30",
    "  weight: 70",
    "main_model:",
    "  intercepts:",
    "    baseline: 1.0"
  ), config1_path)
  
  writeLines(c(
    "model_name: test_model",
    "centering:",
    "  age: 30",
    "  height: 180",
    "main_model:",
    "  intercepts:",
    "    baseline: 1.0",
    "    new_param: 0.5"
  ), config2_path)
  
  result <- diffview(config1_path, config2_path)
  
  expect_true(length(result) > 1)
  expect_true(any(grepl("\\+", result)))  # Should have addition indicators
  expect_true(any(grepl("\\-", result)))  # Should have removal indicators
  
  # Clean up
  unlink(c(config1_path, config2_path))
})

test_that("diffview ignores whitespace when ignore_whitespace = TRUE", {
  # Create configs with whitespace differences
  config1_path <- tempfile(fileext = ".yaml")
  config2_path <- tempfile(fileext = ".yaml")
  
  writeLines(c(
    "model_name: test_model",
    "centering:",
    "  age: 30",
    "  description: \"test value\""
  ), config1_path)
  
  writeLines(c(
    "model_name: test_model",
    "centering:",
    "  age: 30",
    "  description: \"  test   value  \""
  ), config2_path)
  
  result <- diffview(config1_path, config2_path, ignore_whitespace = TRUE)
  
  expect_equal(result, "No differences found between configuration files.")
  
  # Clean up
  unlink(c(config1_path, config2_path))
})

test_that("diffview detects whitespace when ignore_whitespace = FALSE", {
  # Create configs with whitespace differences
  config1_path <- tempfile(fileext = ".yaml")
  config2_path <- tempfile(fileext = ".yaml")
  
  writeLines(c(
    "model_name: test_model",
    "centering:",
    "  description: \"test value\""
  ), config1_path)
  
  writeLines(c(
    "model_name: test_model",
    "centering:",
    "  description: \"  test   value  \""
  ), config2_path)
  
  result <- diffview(config1_path, config2_path, ignore_whitespace = FALSE)
  
  expect_true(length(result) > 1)
  expect_true(any(grepl("centering.description", result)))
  
  # Clean up
  unlink(c(config1_path, config2_path))
})

test_that("diffview handles missing files gracefully", {
  # Test with non-existent file
  expect_error(diffview("nonexistent1.yaml", "nonexistent2.yaml"),
               "Configuration file not found")
})

test_that("diffview handles complex nested structures", {
  # Create configs with complex nested differences
  config1_path <- tempfile(fileext = ".yaml")
  config2_path <- tempfile(fileext = ".yaml")
  
  writeLines(c(
    "model_name: complex_model",
    "main_model:",
    "  transformations:",
    "    - name: age_centered",
    "      formula: center_variable(age, 30)",
    "    - name: weight_log",
    "      formula: log_transform(weight)",
    "  coefficients:",
    "    age: 0.1",
    "    weight: 0.05"
  ), config1_path)
  
  writeLines(c(
    "model_name: complex_model",
    "main_model:",
    "  transformations:",
    "    - name: age_centered",
    "      formula: center_variable(age, 35)",
    "    - name: height_log",
    "      formula: log_transform(height)",
    "  coefficients:",
    "    age: 0.15",
    "    height: 0.02"
  ), config2_path)
  
  result <- diffview(config1_path, config2_path)
  
  expect_true(length(result) > 1)
  expect_true(any(grepl("\\+", result)))  # Should have additions
  expect_true(any(grepl("\\-", result)))  # Should have removals  
  expect_true(any(grepl("~", result)))    # Should have changes
  
  # Clean up
  unlink(c(config1_path, config2_path))
})

test_that("diffview output format includes file paths", {
  # Create two different config files
  config1_path <- tempfile(fileext = ".yaml")
  config2_path <- tempfile(fileext = ".yaml")
  
  writeLines(c(
    "model_name: test1"
  ), config1_path)
  
  writeLines(c(
    "model_name: test2"
  ), config2_path)
  
  result <- diffview(config1_path, config2_path)
  
  expect_true(any(grepl("File 1:", result)))
  expect_true(any(grepl("File 2:", result)))
  expect_true(any(grepl(basename(config1_path), result)))
  expect_true(any(grepl(basename(config2_path), result)))
  
  # Clean up
  unlink(c(config1_path, config2_path))
})