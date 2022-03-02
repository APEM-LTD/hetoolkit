test_that("biol_data is valid...", {

  biol_data <- readRDS("biol_data_jHE.rds")
  flow_stats <- readRDS("flow_stats_jHE.rds")

  expect_error(join_he(biol_data = "hello",
                       flow_stats = flow_stats),
               "biol_data is invalid format")

})

test_that("biol_site_id is valid within data...", {

  biol_data <- readRDS("biol_data_jHE.rds")
  flow_stats <- readRDS("flow_stats_jHE.rds")

  biol_data_2 <- biol_data %>% dplyr::rename(biol_site = biol_site_id)

  expect_error(join_he(biol_data = biol_data_2,
                      flow_stats = flow_stats),
                "biol_site_id column was not identified in biol_data")

})


test_that("date is valid within biol_data...", {

  biol_data <- readRDS("biol_data_jHE.rds")
  flow_stats <- readRDS("flow_stats_jHE.rds")

  biol_data_2 <- biol_data %>% dplyr::rename(hello = date)

  expect_error(join_he(biol_data = biol_data_2,
                       flow_stats = flow_stats),
               "date column was not identified in biol_data")

})


test_that("bio_data is not length 0", {

  flow_stats <- readRDS("flow_stats_1.rds")
  df <- data.frame(biol_site_id = as.Date(character()), date=character(), LIFE=character(), stringsAsFactors=FALSE)

  expect_error(join_he(biol_data = df,
                       flow_stats = flow_stats_1),
               "biol_data is of length 0")

})



test_that("flow_stats is valid...", {

  biol_data <- readRDS("biol_data_jHE.rds")

  expect_error(join_he(biol_data = biol_data,
                       flow_stats = "hello"),
               "flow_stats is invalid format")

})

test_that("flow_site_id is valid within flow_stats...", {

  biol_data <- readRDS("biol_data_jHE.rds")
  flow_stats <- readRDS("flow_stats_jHE.rds")

  flow_stats_2 <- flow_stats %>% dplyr::rename(flow_site = flow_site_id)

  expect_error(join_he(biol_data = biol_data,
                       flow_stats = flow_stats_2),
               "flow_site_id column was not identified in flow_stats")

})

test_that("flow_site_id is valid within flow_stats...", {

  biol_data <- readRDS("biol_data_jHE.rds")
  flow_stats <- readRDS("flow_stats_jHE.rds")

  flow_stats_2 <- flow_stats %>% dplyr::rename(flow_site = flow_site_id)

  expect_error(join_he(biol_data = biol_data,
                       flow_stats = flow_stats_2),
               "flow_site_id column was not identified in flow_stats")

})

test_that("start_date is valid within flow_stats...", {

  biol_data <- readRDS("biol_data_jHE.rds")
  flow_stats <- readRDS("flow_stats_jHE.rds")

  flow_stats_2 <- flow_stats %>% dplyr::rename(start = start_date)

  expect_error(join_he(biol_data = biol_data,
                       flow_stats = flow_stats_2),
               "start_date column was not identified in flow_stats")

})

test_that("end_date is valid within flow_stats...", {

  biol_data <- readRDS("biol_data_jHE.rds")
  flow_stats <- readRDS("flow_stats_jHE.rds")

  flow_stats_2 <- flow_stats %>% dplyr::rename(end = end_date)

  expect_error(join_he(biol_data = biol_data,
                       flow_stats = flow_stats_2),
               "end_date column was not identified in flow_stats")

})



test_that("mapping is valid...", {

  biol_data <- readRDS("biol_data_jHE.rds")
  flow_stats <- readRDS("flow_stats_jHE.rds")

  expect_error(join_he(biol_data = biol_data,
                       flow_stats = flow_stats,
                       mapping = "hello"),
               "Mapping data frame is invalid")

})


test_that("biol_site_id is contained within mapping...", {

  biol_data <- readRDS("biol_data_jHE.rds")
  flow_stats <- readRDS("flow_stats_jHE.rds")
  mapping <- readRDS("mapping_jHE.rds")

  mapping_2 <- mapping %>% dplyr::rename(biol_site = biol_site_id)

  expect_error(join_he(biol_data = biol_data,
                       flow_stats = flow_stats,
                       mapping = mapping_2),
               "biol_site_id column was not identified in mapping")

})

test_that("flow_site_id is contained within mapping...", {

  biol_data <- readRDS("biol_data_jHE.rds")
  flow_stats <- readRDS("flow_stats_jHE.rds")
  mapping <- readRDS("mapping_jHE.rds")

  mapping_2 <- mapping %>% dplyr::rename(flow_site = flow_site_id)

  expect_error(join_he(biol_data = biol_data,
                       flow_stats = flow_stats,
                       mapping = mapping_2),
               "flow_site_id column was not identified in mapping")

})

test_that("NAs are not contained within mapping...", {

  biol_data <- readRDS("biol_data_jHE.rds")
  flow_stats <- readRDS("flow_stats_jHE.rds")
  mapping <- readRDS("mapping_jHE.rds")

  mapping_2 <- mapping %>% dplyr::mutate(flow_site_id = NA)

  expect_error(join_he(biol_data = biol_data,
                       flow_stats = flow_stats,
                       mapping = mapping_2),
               "mapping contains NAs")

})


test_that("lags is an integer", {

  biol_data <- readRDS("biol_data_jHE.rds")
  flow_stats <- readRDS("flow_stats_jHE.rds")

  expect_error(join_he(biol_data = biol_data,
                       flow_stats = flow_stats,
                       lags = c(1.25, 1.5)),
               "lags must be an integer")

})

test_that("method is A or B", {

  biol_data <- readRDS("biol_data_jHE.rds")
  flow_stats <- readRDS("flow_stats_jHE.rds")

  expect_error(join_he(biol_data = biol_data,
                       flow_stats = flow_stats,
                       method = "hello"),
               "method must be specified using A or B")

})

test_that("join_type is...", {

  biol_data <- readRDS("biol_data_jHE.rds")
  flow_stats <- readRDS("flow_stats_jHE.rds")

  expect_error(join_he(biol_data = biol_data,
                       flow_stats = flow_stats,
                       join_type = "hello"),
               "join_type must be specified using add_flows or add_biol")

})


test_that("sample date precedes flow_start", {

  biol_data <- readRDS("biol_data_jHE.rds")
  flow_stats <- readRDS("flow_stats_jHE.rds")

  biol_data$date[1] <- "1990-01-01"

  expect_warning(join_he(biol_data = biol_data,
                       flow_stats = flow_stats),
               "biol_data: date precedes the the start_date of the earliest time window")

})

test_that("sample date is in the future", {

  biol_data <- readRDS("biol_data_jHE.rds")
  flow_stats <- readRDS("flow_stats_jHE.rds")

  biol_data$date[1] <- "2025-01-01"

  expect_warning(join_he(biol_data = biol_data,
                         flow_stats = flow_stats),
                 "biol_data: date is in the future")

})

test_that("biol_site_id repeated within mapping...", {

  biol_data <- readRDS("biol_data_jHE.rds")
  flow_stats <- readRDS("flow_stats_jHE.rds")
  mapping <- readRDS("mapping_jHE.rds")

  mapping_2 <- mapping %>% dplyr::mutate(biol_site_id = 27090)

  expect_error(join_he(biol_data = biol_data,
                       flow_stats = flow_stats,
                       mapping = mapping_2),
               "biol_site_id cannot be mapped to more than one flow_site_id")

})


test_that("biol_site_id not identified within mapping...", {

  biol_data <- readRDS("biol_data_jHE.rds")
  flow_stats <- readRDS("flow_stats_jHE.rds")
  mapping <- readRDS("mapping_jHE.rds")

  mapping$biol_site_id[1] <- "hello"

  expect_warning(join_he(biol_data = biol_data,
                       flow_stats = flow_stats,
                       mapping = mapping),
              "biol_site_id was not identified in mapping: 27090")

})


test_that("flow_site_id not identified within mapping...", {

  biol_data <- readRDS("biol_data_jHE.rds")
  flow_stats <- readRDS("flow_stats_jHE.rds")
  mapping <- readRDS("mapping_jHE.rds")

  mapping$flow_site_id[1] <- "hello"

  expect_warning(join_he(biol_data = biol_data,
                         flow_stats = flow_stats,
                         mapping = mapping),
                 "flow_site_id was not identified in mapping: 27090")

})

test_that("biol_site_id not identified within mapping...", {

  biol_data <- readRDS("biol_data_jHE.rds")
  flow_stats <- readRDS("flow_stats_jHE.rds")
  mapping <- readRDS("mapping_jHE.rds")

  mapping_2 <- mapping
  mapping_2$biol_site_id[1] <- "hello"
  mapping_2$biol_site_id[2] <- "hiya"

  expect_error(join_he(biol_data = biol_data,
                         flow_stats = flow_stats,
                         mapping = mapping_2),
                 "none of the biol_site_ids listed in biol_data are specified in mapping")

})

test_that("flow_site_id not identified within mapping...", {

  biol_data <- readRDS("biol_data_jHE.rds")
  flow_stats <- readRDS("flow_stats_jHE.rds")
  mapping <- readRDS("mapping_jHE.rds")

  mapping_2 <- mapping
  mapping_2$flow_site_id[1] <- "hello"
  mapping_2$flow_site_id[2] <- "hiya"

  expect_error(join_he(biol_data = biol_data,
                       flow_stats = flow_stats,
                       mapping = mapping_2),
               "none of the flow_site_ids listed in flow_stats are specified in mapping")

})

test_that("unavailable lags...", {

  biol_data <- readRDS("biol_data_jHE.rds")
  flow_stats <- readRDS("flow_stats_jHE.rds")
  mapping <- readRDS("mapping_jHE.rds")

  expect_warning(join_he(biol_data = biol_data,
                       flow_stats = flow_stats,
                       mapping = mapping,
                       lags = c(0, 5)),
               "lag5 may not be available for all flow periods. Flow data doesn’t extend that far back.")

})

#test_that("join_he constructs expected output", {
#
#  join_data_test <- readRDS("join_data_test.rds")
#  biol_all_test <- readRDS("biol_all_test.rds")
#  flowstats_test <- readRDS("flowstats_test.rds")
#  mapping_test <- readRDS("mapping_test.rds")
#
#  result <- join_he(biol_data = biol_all_test,
#                  flow_stats = flowstats_test,
#                  mapping = mapping_test,
#                 lag_vars = c("Q95z", "Q10z"),
#                  LS1 = TRUE,
#                  LS2 = TRUE)
#  compared <- join_data_test
#  expect_equal(result, compared)
#})
