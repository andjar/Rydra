test_that("Logging functionality in rydra_calculate", {
  library(testthat)
  library(Rydra) # Ensure Rydra functions are available
  library(jsonlite) # For reading JSON logs
  library(uuid)     # For checking UUID format if necessary

  # Helper function to create a temporary config file
  create_temp_config <- function(..., log_settings = NULL, dir = tempdir()) {
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    config_list <- list(...)
    # Ensure required top-level keys exist
    if (is.null(config_list$model_name)) config_list$model_name <- "test_log_model"
    if (!is.null(log_settings)) {
      config_list$logging <- log_settings
    }

    # Use a simpler model for logging tests to reduce complexity of main calc
    if (is.null(config_list$test_log_model)) {
        config_list$centering <- list(val = 10)
        config_list$test_log_model <- list(
          intercepts = list(baseline = 1.0),
          coefficients = list(val_centered = 0.5),
          transformations = list(
            list(name = "val_centered", formula = "center_variable(val, centering.val)")
          ),
          factors = list(),
          output_transformation = "add_value(result, 10)"
        )
    }

    temp_config_file <- tempfile(tmpdir = dir, fileext = ".yaml")
    yaml::write_yaml(config_list, temp_config_file)
    return(temp_config_file)
  }

  test_that("Logging is disabled by default and no logs are created", {
    temp_log_dir <- file.path(tempdir(), "default_log_test")
    # Ensure no pre-existing "rydra_logs" in CWD from other tests or runs
    if (dir.exists("rydra_logs")) unlink("rydra_logs", recursive = TRUE)
    # Also clean up temp_log_dir if it exists (it shouldn't for this test's purpose)
    if (dir.exists(temp_log_dir)) unlink(temp_log_dir, recursive = TRUE)

    # Create a config without any logging section
    config_file <- create_temp_config(dir = temp_log_dir)

    input_data <- list(val = 15)

    # Run calculation
    Rydra::rydra_calculate(config_path = config_file, data = input_data, model_name = "test_log_model")

    # Assert that the default "rydra_logs" directory is NOT created in CWD
    expect_false(dir.exists("rydra_logs"), "Default 'rydra_logs' directory created when logging should be disabled.")
    # Assert that no log directory was created in the temp_log_dir either (where config was)
    expect_false(dir.exists(file.path(temp_log_dir, "rydra_logs")), "Default 'rydra_logs' created in temp test dir.")

    # Clean up
    unlink(config_file, force = TRUE)
    unlink(temp_log_dir, recursive = TRUE, force = TRUE)
    if (dir.exists("rydra_logs")) unlink("rydra_logs", recursive = TRUE) # General cleanup
  })

  test_that("Logging enabled with default path creates logs in 'rydra_logs'", {
    # Test in a controlled temporary directory to avoid cluttering the project root with "rydra_logs"
    test_env_dir <- file.path(tempdir(), "test_env_default_log")
    if (dir.exists(test_env_dir)) unlink(test_env_dir, recursive = TRUE)
    dir.create(test_env_dir, showWarnings = FALSE, recursive = TRUE)

    original_wd <- getwd()
    setwd(test_env_dir) # Set WD to ensure 'rydra_logs' is created here if default path logic works as CWD-relative

    default_log_path <- "rydra_logs"
    if (dir.exists(default_log_path)) unlink(default_log_path, recursive = TRUE) # Clean up before test

    config_file <- create_temp_config(log_settings = list(enabled = TRUE), dir = test_env_dir) # Config in test_env_dir

    input_data <- list(val = 20)

    # Expected values for the simple model:
    # val_centered = 20 - 10 = 10
    # base_score = 1.0 (intercept) + (10 * 0.5) (val_centered * coeff) = 1.0 + 5.0 = 6.0
    # total_score (pre-output) = 6.0
    # final_result = 6.0 + 10 = 16.0
    expected_final_result <- 16.0

    result <- Rydra::rydra_calculate(config_path = config_file, data = input_data, model_name = "test_log_model")
    expect_equal(result, expected_final_result)

    expect_true(dir.exists(default_log_path), "Default 'rydra_logs' directory was not created in the current working directory.")

    log_files <- list.files(default_log_path, pattern = "\\.json$")
    expect_true(length(log_files) > 0, "No JSON log files found in the default log directory.")

    # Check content of one log file
    log_content <- jsonlite::read_json(file.path(default_log_path, log_files[1]))

    expect_true(!is.null(log_content$timestamp))
    expect_equal(log_content$invocation_params$config_path, config_file)
    expect_equal(log_content$invocation_params$model_name, "test_log_model")
    expect_equal(log_content$invocation_params$data$val, input_data$val)
    expect_true(!is.null(log_content$model_config_used))
    expect_equal(log_content$intermediate_values$transformed_data$val_centered, 10) # (20-10)
    expect_equal(log_content$final_result, expected_final_result)

    # Filename check: YYYYMMDDHHMMSSxxxxxx_uuid.json
    expect_true(grepl("^\\d{14}\\d{2,6}_[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}\\.json$", log_files[1]))

    # Timestamp has 'Z' suffix (UTC)
    expect_true(grepl("Z$", log_content$timestamp))

    # Clean up
    setwd(original_wd)
    unlink(test_env_dir, recursive = TRUE, force = TRUE)
  })


  test_that("Logging enabled with a custom path creates logs in that path", {
    custom_log_dir <- file.path(tempdir(), "custom_rydra_logs")
    if (dir.exists(custom_log_dir)) unlink(custom_log_dir, recursive = TRUE) # Clean before test

    config_file <- create_temp_config(log_settings = list(enabled = TRUE, path = custom_log_dir))

    input_data <- list(val = 25)
    expected_final_result <- ((25 - 10) * 0.5 + 1.0) + 10 # 7.5 + 1.0 + 10 = 18.5

    result <- Rydra::rydra_calculate(config_path = config_file, data = input_data, model_name = "test_log_model")
    expect_equal(result, expected_final_result)

    expect_true(dir.exists(custom_log_dir), "Custom log directory was not created.")

    log_files <- list.files(custom_log_dir, pattern = "\\.json$")
    expect_true(length(log_files) > 0, "No JSON log files found in the custom log directory.")

    log_content <- jsonlite::read_json(file.path(custom_log_dir, log_files[1]))
    expect_equal(log_content$final_result, expected_final_result)
    expect_equal(log_content$invocation_params$data$val, input_data$val)

    # Clean up
    unlink(config_file, force = TRUE)
    unlink(custom_log_dir, recursive = TRUE, force = TRUE)
  })

  test_that("Logging handles non-serializable data gracefully (logs what it can)", {
    # This test is more about ensuring the main calc doesn't fail if logging has an issue with complex data.
    # The current implementation converts factors to characters, which is good.
    # True non-serializable (like environments or connections) would error in jsonlite, caught by tryCatch.

    log_dir <- file.path(tempdir(), "complex_data_log_test")
    if (dir.exists(log_dir)) unlink(log_dir, recursive = TRUE)

    config_file <- create_temp_config(log_settings = list(enabled = TRUE, path = log_dir))

    # Input data with a factor, which our logger handles by converting to character
    input_data_complex <- list(val = 30, category = factor("A"))

    # Expected calculation for 'val' part: ((30-10)*0.5 + 1.0) + 10 = (20*0.5+1)+10 = (10+1)+10 = 11+10 = 21
    # 'category' is not used in the simple test model.
    expected_result_val_only <- 21.0

    # Expect a warning from rydra_calculate if logging fails, but not an error from the main function
    # However, our current logging of input data is quite robust for typical list/df structures.
    # The main concern would be if model_config or intermediates become non-serializable.

    # For this test, we'll mainly check that a log is produced and contains the factor as character.
    res <- Rydra::rydra_calculate(config_path = config_file, data = input_data_complex, model_name = "test_log_model")
    expect_equal(res, expected_result_val_only) # Calculation should succeed

    expect_true(dir.exists(log_dir))
    log_files <- list.files(log_dir, pattern = "\\.json$")
    expect_true(length(log_files) > 0)

    log_content <- jsonlite::read_json(file.path(log_dir, log_files[1]))
    expect_equal(log_content$invocation_params$data$val, 30)
    expect_equal(log_content$invocation_params$data$category, "A") # Factor converted to character

    unlink(config_file, force = TRUE)
    unlink(log_dir, recursive = TRUE, force = TRUE)
  })


  test_that("Logging continues with warning if log directory is not writable (simulated)", {
    # Simulating a non-writable directory is tricky and platform-dependent.
    # We can test the behavior of dir.create failing if showWarnings = FALSE is respected
    # and that the main calculation still proceeds. The actual logging write error is caught.

    # Let's use an existing file as path, which should make dir.create fail,
    # or jsonlite::write_json fail.
    temp_file_as_path <- tempfile()
    file.create(temp_file_as_path) # Create a file where a directory is expected

    config_file <- create_temp_config(log_settings = list(enabled = TRUE, path = temp_file_as_path))

    input_data <- list(val = 5)
    # Expected: ((5-10)*0.5 + 1.0) + 10 = (-5*0.5+1)+10 = (-2.5+1)+10 = -1.5+10 = 8.5
    expected_calc_result <- 8.5

    # Expect a warning from the logging system, but the calculation should complete.
    expect_warning(
      result <- Rydra::rydra_calculate(config_path = config_file, data = input_data, model_name = "test_log_model"),
      "Rydra logging failed"
    )

    expect_equal(result, expected_calc_result, info = "Calculation result should be correct even if logging fails.")

    # The file used as path should still be a file, not a directory with logs.
    expect_false(dir.exists(temp_file_as_path))
    expect_true(file.exists(temp_file_as_path)) # It should remain a file

    # Clean up
    unlink(config_file, force = TRUE)
    unlink(temp_file_as_path, force = TRUE)
  })

  # Final cleanup of any stray rydra_logs in CWD if tests failed mid-way
  if (dir.exists("rydra_logs")) unlink("rydra_logs", recursive = TRUE)
})
