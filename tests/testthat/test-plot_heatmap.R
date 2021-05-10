test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

df <- readRDS("df.rds")
temp_date <- readRDS("temp_date.rds")
temp_sites <- readRDS("temp_sites.rds")

# Test Error Messages

  # is data there and a dataframe?
  expect_error(plot_heatmap(
    data = NULL, x = "date", y = "sites", fill = "flow"
  ), "data missing, with no default")

  expect_error(plot_heatmap(
    data = "hello", x = "date", y = "sites", fill = "flow"
  ), "data input must be a dataframe")


  # x is not specified, missing from data, invalid format, or contains NAs
  expect_error(plot_heatmap(
    data = df, x = NULL, y = "sites", fill = "flow"
  ), "x is missing, please specify")

  expect_error(plot_heatmap(
    data = df, x = "hello", y = "sites", fill = "flow"
  ), "x cannot be found in input dataframe")

  #### invalid x format error here

  test_that("...", {

  expect_error(plot_heatmap(
    data = temp_date, x = "date", y = "sites", fill = "flow"
  ),"x contains NAs, NAs not a valid value of x")


  })


  # y is not specified, missing from data or invalid format
  expect_error(plot_heatmap(
    data = df, x = "date", y = NULL, fill = "flow"
  ), "y is missing, please specify")

  expect_error(plot_heatmap(
    data = df, x = "date", y = "hello", fill = "flow"
  ), "y cannot be found in input dataframe")

  #### invalid y format error here

  test_that("...", {


  expect_error(plot_heatmap(
    data = temp_sites, x = "date", y = "sites", fill = "flow"
  ),"y contains NAs, NAs not a valid value of y")


  })

  # fill is not specified, missing from data or invalid format
  expect_error(plot_heatmap(
    data = df, x = "date", y = "sites", fill = NULL
  ), "fill is missing, please specify")

  expect_error(plot_heatmap(
    data = df, x = "date", y = "sites", fill = "hello"
  ), "fill cannot be found in input dataframe")



  # Valid colour scheme
  expect_error(plot_heatmap(
    data = df, x = "date", y = "sites", fill = "flow", colour = "hello"
  ), "colour input not valid colour scheme")


  # Check if logical input settings have logical values
  expect_error(plot_heatmap(
    data = df, x = "date", y = "sites", fill = "flow", limits = "hello"
  ), "logical value of limits variable must be provided")

  expect_error(plot_heatmap(
    data = df, x = "date", y = "sites", fill = "flow", list_out = "hello"
  ), "logical value of list_out variable must be provided")

  expect_error(plot_heatmap(
    data = df, x = "date", y = "sites", fill = "flow", dual = "hello"
  ),"logical value of dual variable must be provided")


  # Test save results file errors:
  expect_error(plot_heatmap(
    data = df, x = "date", y = "sites", fill = "flow", save = TRUE, save_dir = "hello"
  ), "Specified save directory does not exist")

  expect_error(plot_heatmap(
    data = df, x = "date", y = "sites", fill = "flow", save = "hello"
  ), "Save is not logical")


  # Warnings
  # limit size of the data set to 30 x by 20 y if limit set to TRUE
  #expect_warning(plot_heatmap(
    #data = df, x = "date", y = "sites", fill = "flow", limits = TRUE
  #), "limits set to TRUE, dataset being trimmed a maximum of the first 30 unique x values and first 20 unique y values, no effect if less than 30 x and 20 y occur within the dataset. If trimming is not wanted set limits to FALSE")


