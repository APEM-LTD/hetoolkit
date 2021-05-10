
test_that("test data is valid...", {

  expect_error(calc_flowstats(data = "hello",
                              site_col = "site",
                              date_col = "Date",
                              flow_col = "Flow"),
                          "Data frame not found")

})

test_that("site_col is valid within data...", {

  testdata_flowstats <- readRDS("testdata_flowstats.rds")

  expect_error(calc_flowstats(data = testdata_flowstats,
                              site_col = "hello",
                              date_col = "Date",
                              flow_col = "Flow"),
               "Specified site column was not identified in data")

})


test_that("default site_col is valid within data...", {

  testdata_flowstats <- readRDS("testdata_flowstats.rds")

  expect_error(calc_flowstats(data = testdata_flowstats,
                              date_col = "Date",
                              flow_col = "Flow"),
               "Default 'flow_site_id' column was not identified in data")

})

test_that("date_col is valid within data...", {

  testdata_flowstats <- readRDS("testdata_flowstats.rds")

  expect_error(calc_flowstats(data = testdata_flowstats,
                              site_col = "site",
                              date_col = "hello",
                              flow_col = "Flow"),
               "Specified date column was not identified in data")

})


test_that("default date_col is valid within data...", {

  testdata_flowstats <- readRDS("testdata_flowstats.rds")

  expect_error(calc_flowstats(data = testdata_flowstats,
                              site_col = "site",
                              flow_col = "Flow"),
               "Default 'date' column was not identified in data")

})

test_that("flow_col is valid within data...", {

  testdata_flowstats <- readRDS("testdata_flowstats.rds")

  expect_error(calc_flowstats(data = testdata_flowstats,
                              site_col = "site",
                              date_col = "Date",
                              flow_col = "hello"),
               "Specified flow column was not identified in data")

})


test_that("default flow_col is valid within data...", {

  testdata_flowstats <- readRDS("testdata_flowstats.rds")

  expect_error(calc_flowstats(data = testdata_flowstats,
                              site_col = "site",
                              date_col = "Date"),
               "Default 'flow' column was not identified in data")

})

test_that("flow_col is numeric...", {

  testdata_flowstats <- readRDS("testdata_flowstats.rds")

  expect_error(calc_flowstats(data = testdata_flowstats,
                              site_col = "site",
                              date_col = "Date",
                              flow_col = "Date"),
               "Specified flow_col is not numeric")

})

test_that("ref_col is valid within data...", {

  testdata_flowstats <- readRDS("testdata_flowstats.rds")

  expect_error(calc_flowstats(data = testdata_flowstats,
                              site_col = "site",
                              date_col = "Date",
                              flow_col = "Flow",
                              ref_col = "hello"),
               "Specified ref_col was not identified in data")

})


test_that("duplicate dates...", {

  testdata_flowstats1 <- readxl::read_excel("flow.example_duplicate.xlsx")

  expect_error(calc_flowstats(data = testdata_flowstats1,
                              site_col = "site",
                              date_col = "Date",
                              flow_col = "Flow"),
                   "Duplicate dates identified")

})


#test_that("constructs expected output", {
#
#  flowdata_test <- readRDS("flow_data_test.rds")
#  flowstats_test <- readRDS("flowstats_test.rds")
#  calc_test <- readRDS("calc_test.rds")
#
#
#  result_1 <- hetoolkit::calc_flowstats(data = flowdata_test,
#                                      site_col = "flow_site_id",
#                                      date_col = "date",
#                                      flow_col = "flow")
#
#  result <- result[[1]]
#
#  compared <- calc_test
#  expect_equal(result, compared)
#})

