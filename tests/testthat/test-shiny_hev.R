# Test error messages

# Data frame is not valid/ does not exist

test_that("test data is valid...", {

  expect_error(shiny_hev(data = "hello",
                         sites_col = "Site_Code",
                         flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                         biol_metric = c("WHPT_ASPT_OE"),
                         date_col = "Year"),
               "Data frame 'data' not found")
})


# Sites column does not exist in data frame

test_that("Test sites column is valid...", {

  HEV_testdata <- readRDS("HEV_testdata.rds")

  expect_error(shiny_hev(data = HEV_testdata,
                         sites_col = "hello",
                         flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                         biol_metric = c("WHPT_ASPT_OE"),
                         date_col = "Year"),
               "Specified sites column was not identified in 'data'")

})


test_that("Test date column is valid...", {

  HEV_testdata <- readRDS("HEV_testdata.rds")

  # Date column does not exist in data frame
  expect_error(shiny_hev(data = HEV_testdata,
                         sites_col = "Site_Code",
                         flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                         biol_metric = c("WHPT_ASPT_OE"),
                         date_col = "hello"),
               "Specified date column was not identified in 'data'")

})


test_that("Test flow stats are valid...", {

  HEV_testdata <- readRDS("HEV_testdata.rds")

  # Flow statistics do not exist in data frame
  expect_error(shiny_hev(data = HEV_testdata,
                         sites_col = "SiteCode",
                         flow_stat = "hello",
                         biol_metric = c("WHPT_ASPT_OE"),
                         date_col = "Year"),
               "Specified flow statistics were not identified in 'data'")

})



test_that("Test biology metrics are valid...", {

  HEV_testdata <- readRDS("HEV_testdata.rds")

  # Flow statistics do not exist in data frame
  expect_error(shiny_hev(data = HEV_testdata,
                         sites_col = "SiteCode",
                         flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                         biol_metric = "hello",
                         date_col = "Year"),
               "Specified biology metrics were not identified in 'data'")

})
