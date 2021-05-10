test_that("biol_data is valid...", {

  flow_stats_1 <- readRDS("flow_stats_1.rds")

  expect_error(join_he(biol_data = "hello",
                       flow_stats = flow_stats_1,
                       mapping = NULL,
                       LS1 = FALSE,
                       LS2 = FALSE),
               "biol_data is invalid format")

})

test_that("biol_site_id is valid within data...", {

  biol_data_1 <- readRDS("biol_data_1.rds")
  flow_stats_1 <- readRDS("flow_stats_1.rds")

  expect_error(join_he(biol_data = biol_data_1,
                      flow_stats = flow_stats_1,
                      mapping = NULL,
                      LS1 = FALSE,
                      LS2 = FALSE),
                "biol_site_id column was not identified in biol_data")

})


test_that("Season is valid within data...", {

  biol_data_2 <- readRDS("biol_data_2.rds")
  flow_stats_1 <- readRDS("flow_stats_1.rds")

  expect_error(join_he(biol_data = biol_data_2,
                      flow_stats = flow_stats_1,
                      mapping = NULL,
                      LS1 = FALSE,
                      LS2 = FALSE),
               "Season column was not identified in biol_data")

})

test_that("Year is valid within data...", {

  biol_data_3 <- readRDS("biol_data_3.rds")
  flow_stats_1 <- readRDS("flow_stats_1.rds")

  expect_error(join_he(biol_data = biol_data_3,
                      flow_stats = flow_stats_1,
                      mapping = NULL,
                      LS1 = FALSE,
                      LS2 = FALSE),
               "Year column was not identified in biol_data")

})

test_that("bio_data is not length 0", {

  flow_stats_1 <- readRDS("flow_stats_1.rds")
  df <- data.frame(biol_site_id = as.Date(character()), Year=character(), Season=character(), stringsAsFactors=FALSE)

  expect_error(join_he(biol_data = df,
                       flow_stats = flow_stats_1,
                       mapping = NULL,
                       LS1 = FALSE,
                       LS2 = FALSE),
               "biol_data is of length 0")

})



test_that("flow_stats is valid...", {

  biol_data_A <- readRDS("biol_data_A.rds")

  expect_error(join_he(biol_data = biol_data_A,
                       flow_stats = "hello",
                       mapping = NULL,
                       LS1 = FALSE,
                       LS2 = FALSE),
               "flow_stats is invalid format")

})

test_that("flow_site_id is valid within flow_stats...", {

  biol_data_A <- readRDS("biol_data_A.rds")
  flow_stats_noflow <- readRDS("flow_stats_noflow.rds")

  expect_error(join_he(biol_data = biol_data_A,
                       flow_stats = flow_stats_noflow,
                       mapping = NULL,
                       LS1 = FALSE,
                       LS2 = FALSE),
               "flow_site_id column was not identified in flow_stats")

})


test_that("water_year is valid within flow_stats...", {

  biol_data_A <- readRDS("biol_data_A.rds")
  flow_stats_nowatery <- readRDS("flow_stats_nowatery.rds")

  expect_error(join_he(biol_data = biol_data_A,
                       flow_stats = flow_stats_nowatery,
                       mapping = NULL,
                       LS1 = FALSE,
                       LS2 = FALSE),
               "water_year column was not identified in flow_stats")

})

test_that("season is valid within flow_stats...", {

  biol_data_A <- readRDS("biol_data_A.rds")
  flow_stats_noseason <- readRDS("flow_stats_noseason.rds")

  expect_error(join_he(biol_data = biol_data_A,
                       flow_stats = flow_stats_noseason,
                       mapping = NULL,
                       LS1 = FALSE,
                       LS2 = FALSE),
               "season column was not identified in flow_stats")

})

test_that("mapping is valid...", {

  biol_data_A <- readRDS("biol_data_A.rds")
  flow_stats_1 <- readRDS("flow_stats_1.rds")

  expect_error(join_he(biol_data = biol_data_A,
                       flow_stats = flow_stats_1,
                       mapping = "hello",
                       LS1 = FALSE,
                       LS2 = FALSE),
               "Mapping data frame is invalid")

})

test_that("mapping is valid...", {

  biol_data_A <- readRDS("biol_data_A.rds")
  flow_stats_1 <- readRDS("flow_stats_1.rds")

  expect_error(join_he(biol_data = biol_data_A,
                       flow_stats = flow_stats_1,
                       mapping = "hello",
                       LS1 = FALSE,
                       LS2 = FALSE),
               "Mapping data frame is invalid")

})


test_that("biol_site_id is contained within mapping...", {

  biol_data_A <- readRDS("biol_data_A.rds")
  flow_stats_1 <- readRDS("flow_stats_1.rds")
  mapping_2 <- readRDS("mapping_2.rds")

  expect_error(join_he(biol_data = biol_data_A,
                       flow_stats = flow_stats_1,
                       mapping = mapping_2,
                       LS1 = FALSE,
                       LS2 = FALSE),
               "biol_site_id column was not identified in mapping")

})

test_that("flow_site_id is contained within mapping...", {

  biol_data_A <- readRDS("biol_data_A.rds")
  flow_stats_1 <- readRDS("flow_stats_1.rds")
  mapping_3 <- readRDS("mapping_3.rds")

  expect_error(join_he(biol_data = biol_data_A,
                       flow_stats = flow_stats_1,
                       mapping = mapping_3,
                       LS1 = FALSE,
                       LS2 = FALSE),
               "flow_site_id column was not identified in mapping")

})

test_that("LS1 is logical...", {

  biol_data_A <- readRDS("biol_data_A.rds")
  flow_stats_1 <- readRDS("flow_stats_1.rds")

  expect_error(join_he(biol_data = biol_data_A,
                       flow_stats = flow_stats_1,
                       mapping = NULL,
                       LS1 = "hello",
                       LS2 = FALSE),
               "LS1 is not logical")

})

test_that("LS2 is logical...", {

  biol_data_A <- readRDS("biol_data_A.rds")
  flow_stats_1 <- readRDS("flow_stats_1.rds")

  expect_error(join_he(biol_data = biol_data_A,
                       flow_stats = flow_stats_1,
                       mapping = NULL,
                       LS1 = FALSE,
                       LS2 = "hello"),
               "LS2 is not logical")

})



test_that("join_he constructs expected output", {

  join_data_test <- readRDS("join_data_test.rds")
  biol_all_test <- readRDS("biol_all_test.rds")
  flowstats_test <- readRDS("flowstats_test.rds")
  mapping_test <- readRDS("mapping_test.rds")

  result <- join_he(biol_data = biol_all_test,
                  flow_stats = flowstats_test,
                  mapping = mapping_test,
                  lag_vars = c("Q95z", "Q10z"),
                  LS1 = TRUE,
                  LS2 = TRUE)
  compared <- join_data_test
  expect_equal(result, compared)
})
