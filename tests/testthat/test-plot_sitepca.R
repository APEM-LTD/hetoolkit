test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# Test Error Messages

  # is data there and a dataframe?
test_that("1", {
  expect_error(plot_sitepca(data = NULL, vars = c("ALTITUDE", "SLOPE", "DIST_FROM_SOURCE", "WIDTH", "DEPTH")
  ), "data missing, with no default")

})

test_that("2", {
  expect_error(plot_sitepca(data = "hello", vars = c("ALTITUDE", "SLOPE", "DIST_FROM_SOURCE", "WIDTH", "DEPTH")
  ), "data input must be a dataframe")
})


  # vars is not specified, missing from data

test_that("3", {

  df_inv_open_site <- readRDS("df_inv_open_site.rds")

  expect_error(plot_sitepca(data =df_inv_open_site, vars = NULL
                            ), "vars is missing, please specify")
})

test_that("4", {

  df_inv_open_site <- readRDS("df_inv_open_site.rds")

expect_error(plot_sitepca(data =df_inv_open_site, vars = TRUE
                            ), "vars must be vector of character strings")
})

test_that("5", {

  df_inv_open_site <- readRDS("df_inv_open_site.rds")

expect_error(plot_sitepca(data =df_inv_open_site, vars = c(NA, "SLOPE", "DIST_FROM_SOURCE", "WIDTH", "DEPTH")
                            ), "vars contains NAs, not valid in list of vars")
})


#test_that("6", {
#  expect_error(plot_sitepca(data =df_inv_open_site, vars = c("hello", "SLOPE")
                            #), "missmatch between vars names and names in input dataframe")
#})

test_that("7", {

  df_inv_open_site <- readRDS("df_inv_open_site.rds")

  expect_error(plot_sitepca(data =df_inv_open_site, c("ALTITUDE", "SLOPE", "DIST_FROM_SOURCE", "WIDTH", "DEPTH"), label_by = TRUE
  ), "label_by must be character string")
})

test_that("8", {

  df_inv_open_site <- readRDS("df_inv_open_site.rds")

  expect_error(plot_sitepca(data =df_inv_open_site, vars = c("ALTITUDE", "SLOPE", "DIST_FROM_SOURCE", "WIDTH", "DEPTH"), label_by = c("SITE_ID","WATERBODY_TYPE")
                            ), "use only one variable as a label name")
})

test_that("9", {

  df_inv_open_site <- readRDS("df_inv_open_site.rds")

  expect_error(plot_sitepca(data =df_inv_open_site, vars = c("ALTITUDE", "SLOPE", "DIST_FROM_SOURCE", "WIDTH", "DEPTH"), label_by = c("hello")
                            ), "missmatch between label_by name and names in input dataframe")
})

#test_that("11", {
#  expect_error(plot_sitepca(data =df_inv_open_site, vars = c("ALTITUDE", "SLOPE", "DIST_FROM_SOURCE", "WIDTH", "DEPTH"), label_by = c("SITE_ID"),colour_by = "hello"
                            #), "missmatch between colour_by names and names in input dataframe")
#})

test_that("12", {

  df_inv_open_site <- readRDS("df_inv_open_site.rds")

  expect_error(plot_sitepca(data =df_inv_open_site, vars = c("ALTITUDE", "SLOPE", "DIST_FROM_SOURCE", "WIDTH", "DEPTH"), colour_by = TRUE
                            ),"colour_by must be character string or vector of character strings")
})

test_that("13", {

  df_inv_open_site <- readRDS("df_inv_open_site.rds")

 #check logical value for logical inputs
  expect_error(plot_sitepca(data =df_inv_open_site, vars = c("ALTITUDE", "SLOPE", "DIST_FROM_SOURCE", "WIDTH", "DEPTH"), eigenvectors = "hello"
                            ), "eigenvectors must be a logical statement")
})


test_that("14", {

  df_inv_open_site <- readRDS("df_inv_open_site.rds")

  expect_error(plot_sitepca(data =df_inv_open_site, vars = c("ALTITUDE", "SLOPE", "DIST_FROM_SOURCE", "WIDTH", "DEPTH"), plotly = "hello"
                            ), "plotly must be a logical statement")
})

test_that("15", {

  df_inv_open_site <- readRDS("df_inv_open_site.rds")

 #Test save results file errors:
  expect_error(plot_sitepca(data =df_inv_open_site, vars = c("ALTITUDE", "SLOPE", "DIST_FROM_SOURCE", "WIDTH", "DEPTH"), save = TRUE, save_dir = "hello"
                            ), "Specified save directory does not exist")
})

test_that("16", {

  df_inv_open_site <- readRDS("df_inv_open_site.rds")

  expect_error(plot_sitepca(data =df_inv_open_site, vars = c("ALTITUDE", "SLOPE", "DIST_FROM_SOURCE", "WIDTH", "DEPTH"), save = "hello"
                            ), "Save is not logical")
})

#test_that("17", {

#  expect_warning(plot_sitepca(data =df_inv_open_site, vars = c("ALTITUDE", "SLOPE", "DIST_FROM_SOURCE", "WIDTH", "DEPTH")
#                              ), paste(nrow(df_inv_open_site[!complete.cases(df_inv_open_site), ]),"sites omitted due to incomplete data"))
#})
