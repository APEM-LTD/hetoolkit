# Test Error Messages
library(testthat, ggplot2)

test_that("data is specified", {
  expect_error(plot_rngflows(flow_stats = c("Q95z", "Q10z"), biol_metric = "LIFE_F_OE", label = "SAMPLE_DATE", plotly = FALSE),
             "The data input needs to be defined")
})

test_that("flow statistics are specified",{

  DU3_trimmed <- readRDS("DU3_trimmed.rds")

  expect_error(plot_rngflows(data = DU3_trimmed, biol_metric = "LIFE_F_OE", label = "SAMPLE_DATE", plotly = FALSE),
             "'flow_stats' is missing; please specify a vector of two flow statistics from 'data'")
})

test_that("biology metric is defined",{

  DU3_trimmed <- readRDS("DU3_trimmed.rds")

expect_error(plot_rngflows(data = DU3_trimmed, flow_stats = c("Q95z", "Q10z"), label = "SAMPLE_DATE", plotly = FALSE),
             "'biol_metric' is missing; please specify a column containing biology data")
})

test_that("only one biology metric is defined",{

  DU3_trimmed <- readRDS("DU3_trimmed.rds")

  expect_error(plot_rngflows(data = DU3_trimmed, flow_stats = c("Q95z", "Q10z"), biol_metric = c("LIFE_F_OE", "Q10z"),label = "SAMPLE_DATE", plotly = FALSE),
             "Only one biology metric can be defined")
})

test_that("two flow statistics are specificed",{

  DU3_trimmed <- readRDS("DU3_trimmed.rds")

  expect_error(plot_rngflows(data = DU3_trimmed, flow_stats = c("Q95z"), biol_metric = "LIFE_F_OE",label = "SAMPLE_DATE", plotly = FALSE),
               "Two flow statistics must be specified")
})

test_that("no more than two flow statistics are specified",{

  DU3_trimmed <- readRDS("DU3_trimmed.rds")

  expect_error(plot_rngflows(data = DU3_trimmed, flow_stats = c("Q95z", "Q10z", "Q95zLS1"), biol_metric = "LIFE_F_OE",label = "SAMPLE_DATE", plotly = FALSE),
            "No more than two flow statistics can be specified")
})

test_that ("only one wrap by variable is defined",{

  DU3_trimmed <- readRDS("DU3_trimmed.rds")

  expect_error(plot_rngflows(data = DU3_trimmed, flow_stats = c("Q95z", "Q10z"), biol_metric = "LIFE_F_OE", wrap_by = c("site.label", "Q10z"), label = "SAMPLE_DATE", plotly = FALSE),
             "only one 'wrap_by' variable can be defined")
})

test_that("two flow statistics are defined",{

  DU3_trimmed <- readRDS("DU3_trimmed.rds")

  expect_error(plot_rngflows(data = DU3_trimmed, flow_stats = c("Q10z"), biol_metric = "LIFE_F_OE", label = "SAMPLE_DATE", plotly = FALSE),
             "Two flow statistics must be specified")
})

test_that("error if more than two flow statistics defined",{
  expect_error(plot_rngflows(data = DU3_trimmed, flow_stats = c("Q95z","Q10z", "Q95z"), biol_metric = "LIFE_F_OE", label = "SAMPLE_DATE", plotly = FALSE),
               "No more than two flow statistics can be specified")
})

test_that("flow statistic 1 is present in the data",{

  DU3_trimmed <- readRDS("DU3_trimmed.rds")

  expect_error(plot_rngflows(data = DU3_trimmed, flow_stats = c("hello", "Q10z"), biol_metric = "LIFE_F_OE", label = "SAMPLE_DATE", plotly = FALSE),
             "Flow statistic 1 is not found in the data")
})

test_that("flow statistic 2 is present in the data",{

  DU3_trimmed <- readRDS("DU3_trimmed.rds")

  expect_error(plot_rngflows(data = DU3_trimmed, flow_stats = c("Q95z", "hello"), biol_metric = "LIFE_F_OE", label = "SAMPLE_DATE", plotly = FALSE),
             "Flow statistic 2 is not found in the data")
})

test_that("biology metric is present in the data",{

  DU3_trimmed <- readRDS("DU3_trimmed.rds")

  expect_error(plot_rngflows(data = DU3_trimmed, flow_stats = c("Q95z", "Q10z"), biol_metric = "IFE_F_OE", label = "SAMPLE_DATE", plotly = FALSE),
             "'biol_metric' is not found in the data")
})

test_that("label is present in the data",{

  DU3_trimmed <- readRDS("DU3_trimmed.rds")

  expect_error(plot_rngflows(data = DU3_trimmed, flow_stats = c("Q95z", "Q10z"), biol_metric = "LIFE_F_OE", label = "AMPLE_DATE", plotly = FALSE),
             "'label' is not found in the data")
})

test_that("wrap by variable is present in the data",{

  DU3_trimmed <- readRDS("DU3_trimmed.rds")

  expect_error(plot_rngflows(data = DU3_trimmed, flow_stats = c("Q95z", "Q10z"), biol_metric = "LIFE_F_OE", wrap_by = "site.able", label = "SAMPLE_DATE", plotly = FALSE),
             "'wrap_by' is not found in the data")
})

test_that("plotly is in correct format",{

  DU3_trimmed <- readRDS("DU3_trimmed.rds")

  expect_error(plot_rngflows(data = DU3_trimmed, flow_stats = c("Q95z", "Q10z"), biol_metric = "LIFE_F_OE", wrap_by = "site.label", label = "SAMPLE_DATE", plotly = "hello"),
             "'plotly' is not logical")
})

test_that("flow statistic 1 is in the correct format",{

  DU3_trimmed_1 <- readRDS("DU3_trimmed_1.rds")

  expect_error(plot_rngflows(data = DU3_trimmed_1, flow_stats = c("Q95z", "Q10z"), biol_metric = "LIFE_F_OE", wrap_by = "site.label", label = "SAMPLE_DATE", plotly = FALSE),
             "Flow statistic 1 is in an invalid format, should be numeric")
})

test_that("flow statistic 2 is in the correct format",{

  DU3_trimmed_2 <- readRDS("DU3_trimmed_2.rds")

  expect_error(plot_rngflows(data=DU3_trimmed_2, flow_stats = c("Q95z", "Q10z"), biol_metric = "LIFE_F_OE", wrap_by = "site.label", label = "SAMPLE_DATE", plotly = FALSE),
             "Flow statistic 2 is in an invalid format, should be numeric")
})

test_that("biology metric is in the correct format",{

  DU3_trimmed_3 <- readRDS("DU3_trimmed_3.rds")

  expect_error(plot_rngflows(data=DU3_trimmed_3, flow_stats = c("Q95z", "Q10z"), biol_metric = "LIFE_F_OE", wrap_by = "site.label", label = "SAMPLE_DATE", plotly = FALSE),
             "'biol_metric' is in an invalid format, should be numeric")
})


## Test outputs

#test_that("output is ggplot", {
#  result <- plot_rngflows(data = DU3_trimmed, flow_stats = c("Q95z", "Q10z"), biol_metric = "LIFE_F_OE", label = "SAMPLE_DATE", plotly = FALSE)
#  expect_is(result,"ggplot")
#})

#test_that("output is plotly", {
#  result <- plot_rngflows(data = DU3_trimmed, flow_stats = c("Q95z", "Q10z"), biol_metric = "LIFE_F_OE", label = "SAMPLE_DATE", plotly = TRUE)
#  expect_is(result,"plotly")
#})

