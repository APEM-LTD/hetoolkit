# Data frame is not valid/ does not exist

test_that("test data is valid...", {

  expect_error(plot_predictions(data = "hello",
               biol_metric = "WHPT_ASPT_OE",
               time_col = "Date2",
               site_col = "Site_Code",
               flow_stat = c("RFRQ50.L1", "PFR50.L1"),
               pred_col = c("predictions", "lower", "upper"),
               save = TRUE),
               "Data frame 'data' not found")
})


# time_col is not valid within 'data'

test_that("time_col is not valid within 'data'", {

  data_hev_pred <- readRDS("data_hev_pred.rds")

  expect_error(plot_predictions(data = data_hev_pred,
                                biol_metric = "WHPT_ASPT_OE",
                                time_col = "hello",
                                site_col = "Site_Code",
                                flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                                pred_col = c("predictions", "lower", "upper"),
                                save = TRUE),
               "Specified time_col was not identified in 'data'")
})

# site_col is not valid within 'data'

test_that("site_col is not valid within 'data'", {

  data_hev_pred <- readRDS("data_hev_pred.rds")

  expect_error(plot_predictions(data = data_hev_pred,
                                biol_metric = "WHPT_ASPT_OE",
                                time_col = "Date2",
                                site_col = "hello",
                                flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                                pred_col = c("predictions", "lower", "upper"),
                                save = TRUE),
               "Specified site_col was not identified in 'data'")
})

# flow_stat is not valid within 'data'

test_that("flow_stat is not valid within 'data'", {

  data_hev_pred <- readRDS("data_hev_pred.rds")

  expect_error(plot_predictions(data = data_hev_pred,
                                biol_metric = "WHPT_ASPT_OE",
                                time_col = "Date2",
                                site_col = "Site_Code",
                                flow_stat = "hello",
                                pred_col = c("predictions", "lower", "upper"),
                                save = TRUE),
               "Specified flow statistics were not identified in 'data'")
})


# biol_metric is not valid within 'data'

test_that("biol_metric is not valid within 'data'", {

  data_hev_pred <- readRDS("data_hev_pred.rds")

  expect_error(plot_predictions(data = data_hev_pred,
                                biol_metric = "hello",
                                time_col = "Date2",
                                site_col = "Site_Code",
                                flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                                pred_col = c("predictions", "lower", "upper"),
                                save = TRUE),
               "Specified biology metric was not identified in 'data'")
})

# central predictions are not valid within 'data'

test_that("central predictions are not valid within 'data'", {

  data_hev_pred <- readRDS("data_hev_pred.rds")

  expect_error(plot_predictions(data = data_hev_pred,
                                biol_metric = "WHPT_ASPT_OE",
                                time_col = "Date2",
                                site_col = "Site_Code",
                                flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                                pred_col = c("hello"),
                                save = TRUE),
               "Specified central predictions were not identified in 'data'")
})

# central, lower or upper predictions are not valid within 'data'

test_that("central predictions are not valid within 'data'", {

  data_hev_pred <- readRDS("data_hev_pred.rds")

  expect_error(plot_predictions(data = data_hev_pred,
                                biol_metric = "WHPT_ASPT_OE",
                                time_col = "Date2",
                                site_col = "Site_Code",
                                flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                                pred_col = c("predictions", "lower", "hello"),
                                save = TRUE),
               "Specified central, lower or upper predictions were not identified in 'data'")
})

# pred_col is of length 2

test_that("if pred_col is of length 2", {

  data_hev_pred <- readRDS("data_hev_pred.rds")

  expect_error(plot_predictions(data = data_hev_pred,
                                biol_metric = "WHPT_ASPT_OE",
                                time_col = "Date2",
                                site_col = "Site_Code",
                                flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                                pred_col = c("predictions", "lower"),
                                save = TRUE),
               "pred_col is of length 2; pred_col must be of length 1 or 3")
})

# pred_col is of length >3

test_that("if pred_col is of length >3", {

  data_hev_pred <- readRDS("data_hev_pred.rds")

  expect_error(plot_predictions(data = data_hev_pred,
                                biol_metric = "WHPT_ASPT_OE",
                                time_col = "Date2",
                                site_col = "Site_Code",
                                flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                                pred_col = c("predictions", "lower", "upper", "WHPT_ASPT_OE"),
                                save = TRUE),
               "pred_col if of length >3; pred_col must be of length 1 or 3")
})


 # pred_col[2] > pred_col[3]

test_that("pred cols in correct order", {

  data_hev_pred <- readRDS("data_hev_pred.rds")

  expect_error(plot_predictions(data = data_hev_pred,
                                biol_metric = "WHPT_ASPT_OE",
                                time_col = "Date2",
                                site_col = "Site_Code",
                                flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                                pred_col = c("predictions", "upper", "lower"),
                                save = TRUE),
               "Values in lower prediction interval are greater than upper prediction interval")
  })


# save is not logical

test_that("save is not logical", {

  data_hev_pred <- readRDS("data_hev_pred.rds")

  expect_error(plot_predictions(data = data_hev_pred,
                                biol_metric = "WHPT_ASPT_OE",
                                time_col = "Date2",
                                site_col = "Site_Code",
                                flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                                pred_col = c("predictions", "lower", "upper"),
                                save = "hello"),
               "Save is not logical")
})

# save_dir is not valid

test_that("save_dir is valid", {

  data_hev_pred <- readRDS("data_hev_pred.rds")

  expect_error(plot_predictions(data = data_hev_pred,
                                biol_metric = "WHPT_ASPT_OE",
                                time_col = "Date2",
                                site_col = "Site_Code",
                                flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                                pred_col = c("predictions", "lower", "upper"),
                                save = FALSE,
                                save_dir = "hello"),
               "Specified save directory does not exist")
})


# more than 2 flow statistics specified

test_that(">2 flow statistics specified", {

  data_hev_pred <- readRDS("data_hev_pred.rds")

  expect_error(plot_predictions(data = data_hev_pred,
                                biol_metric = "WHPT_ASPT_OE",
                                time_col = "Date2",
                                site_col = "Site_Code",
                                flow_stat = c("RFRQ50.L1", "PFR50.L1", "RFRQ50.L1"),
                                pred_col = c("predictions", "lower", "upper"),
                                save = FALSE),
               "More than 2 flow statistics have been specified")
})

# biol_metric is not numeric

test_that("biol_metric is not numeric", {

  data_hev_pred <- readRDS("data_hev_pred.rds")

  expect_error(plot_predictions(data = data_hev_pred,
                                biol_metric = "Site_Code",
                                time_col = "Date2",
                                site_col = "Site_Code",
                                flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                                pred_col = c("predictions", "lower", "upper"),
                                save = FALSE),
               "biol_metric must be numeric")
})

# flow_stat is not numeric

test_that("flow_stat[1] is not numeric", {

  data_hev_pred <- readRDS("data_hev_pred.rds")

  expect_error(plot_predictions(data = data_hev_pred,
                                biol_metric = "WHPT_ASPT_OE",
                                time_col = "Date2",
                                site_col = "Site_Code",
                                flow_stat = c("Season", "PFR50.L1"),
                                pred_col = c("predictions", "lower", "upper"),
                                save = FALSE),
               "flow_stat must be numeric")
})

# flow_stat is not numeric

test_that("flow_stat[2] is not numeric", {

  data_hev_pred <- readRDS("data_hev_pred.rds")

  expect_error(plot_predictions(data = data_hev_pred,
                                biol_metric = "WHPT_ASPT_OE",
                                time_col = "Date2",
                                site_col = "Site_Code",
                                flow_stat = c("RFRQ50.L1", "Site_Code"),
                                pred_col = c("predictions", "lower", "upper"),
                                save = FALSE),
               "flow_stat must be numeric")
})


# pred_col is not numeric

test_that("pred_col[1] is not numeric", {

  data_hev_pred <- readRDS("data_hev_pred.rds")

  expect_error(plot_predictions(data = data_hev_pred,
                                biol_metric = "WHPT_ASPT_OE",
                                time_col = "Date2",
                                site_col = "Site_Code",
                                flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                                pred_col = c("Site_Code", "lower", "upper"),
                                save = FALSE),
               "pred_col must be numeric")
})


# pred_col is not numeric

test_that("pred_col[2] is not numeric", {

  data_hev_pred <- readRDS("data_hev_pred.rds")

  expect_error(plot_predictions(data = data_hev_pred,
                                biol_metric = "WHPT_ASPT_OE",
                                time_col = "Date2",
                                site_col = "Site_Code",
                                flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                                pred_col = c("predictions", "Site_Code", "upper"),
                                save = FALSE),
               "pred_col must be numeric")
})

# pred_col is not numeric

test_that("pred_col[3] is not numeric", {

  data_hev_pred <- readRDS("data_hev_pred.rds")

  expect_error(plot_predictions(data = data_hev_pred,
                                biol_metric = "WHPT_ASPT_OE",
                                time_col = "Date2",
                                site_col = "Site_Code",
                                flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                                pred_col = c("predictions", "lower", "Site_Code"),
                                save = FALSE),
               "pred_col must be numeric")
})


