# Test error messages

# Data frame is not valid/ does not exist

test_that("test data is valid...", {

  expect_error(plot_hev(data = "hello",
                        flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                        biol_metric = c("WHPT_ASPT_OE"),
                        date_col = "Year"),
                        "Data frame 'data' not found")
})


# Sites column does not exist in data frame

test_that("Test date column is valid...", {

  HEV_testdata <- readRDS("HEV_testdata.rds")

  # Date column does not exist in data frame
  expect_error(plot_hev(data = HEV_testdata,
                        flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                        biol_metric = c("WHPT_ASPT_OE"),
                        date_col = "hello"),
                    "Specified date column was not identified in 'data'")

})


test_that("Test that flow statistics are valid...", {

  HEV_testdata <- readRDS("HEV_testdata.rds")

  # Flow statistics do not exist in data frame
  expect_error(plot_hev(data = HEV_testdata,
                        flow_stat = "hello",
                        biol_metric = c("WHPT_ASPT_OE"),
                        date_col = "Year"),
               "Specified flow statistics were not identified in 'data'")

})



test_that("Test that biology metrics are valid...", {

  HEV_testdata <- readRDS("HEV_testdata.rds")

  # Flow statistics do not exist in data frame
  expect_error(plot_hev(data = HEV_testdata,
                        flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                        biol_metric = "hello",
                        date_col = "Year"),
               "Specified biology metrics were not identified in 'data'")

})


test_that("Test that multiplot is logical...", {

  HEV_testdata <- readRDS("HEV_testdata.rds")

  expect_error(plot_hev(data = HEV_testdata,
                        flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                        biol_metric = c("WHPT_ASPT_OE"),
                        date_col = "Year",
                        multiplot = "hello"),
               "multiplot is not logical")

})

# Test save_dir

# Check save is logical


test_that("Test that save is logical...", {

  HEV_testdata <- readRDS("HEV_testdata.rds")

expect_error(plot_hev(data = HEV_testdata,
                       flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                       biol_metric = c("WHPT_ASPT_OE"),
                       date_col = "Year",
                       save = "hello"),
                      "Save is not logical")

})

test_that("Test that save_dir exists...", {

  HEV_testdata <- readRDS("HEV_testdata.rds")

expect_error(plot_hev(data = HEV_testdata,
                       flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                       biol_metric = c("WHPT_ASPT_OE"),
                       date_col = "Year",
                       save_dir = "hello"),
                      "Specified save directory does not exist")

})


test_that("Test error if >4 biol_metric", {

  HEV_testdata <- readRDS("HEV_testdata.rds")

  expect_error(plot_hev(data = HEV_testdata,
                        flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                        biol_metric = c("WHPT_ASPT_OE", "WHPT_ASPT_OE", "WHPT_ASPT_OE", "WHPT_ASPT_OE", "WHPT_ASPT_OE"),
                        date_col = "Year"),
               "More then 4 biology metrics have been selected")

})

test_that("Test error if >2 flow_stat", {

  HEV_testdata <- readRDS("HEV_testdata.rds")

  expect_error(plot_hev(data = HEV_testdata,
                        flow_stat = c("RFRQ50.L1", "PFR50.L1", "PFR50.L1"),
                        biol_metric = c("WHPT_ASPT_OE"),
                        date_col = "Year"),
               "More then 2 flow statistics have been selected")

})


test_that("biol_metric is numeric", {

  HEV_testdata <- readRDS("HEV_testdata.rds")

  expect_error(plot_hev(data = HEV_testdata,
                        flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                        biol_metric = c("Season"),
                        date_col = "Year"),
               "Selected biol_metric is non-numeric")

})

test_that("flow_stat is numeric", {

  HEV_testdata <- readRDS("HEV_testdata.rds")

  expect_error(plot_hev(data = HEV_testdata,
                        flow_stat = c("Season"),
                        biol_metric = c("WHPT_ASPT_OE"),
                        date_col = "Year"),
               "Selected flow_stat is non-numeric")

})


test_that("clr_by exists", {

  HEV_testdata <- readRDS("HEV_testdata.rds")

  expect_error(plot_hev(data = HEV_testdata,
                        flow_stat = c("Season"),
                        biol_metric = c("WHPT_ASPT_OE"),
                        date_col = "Year",
                        clr_by = "hello"),
               "clr_by variable does not exist in data")

})


