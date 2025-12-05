#Test error messages

expect_error(import_wq(source = "hello"),
             "If importing an existing file, it must be in .csv or .rds format")


expect_error(import_wq(source = 999,
                       sites = c("SW-60250424", "NE-45401461", "MD-28291180", "MD-44205010", "SE-99999999")),
             "Source must be either NULL to run download or a valid file path in string format")


expect_error(import_wq(source = "testdata_import_wq_format.xlsx"),
             "If importing an existing file, it must be in .csv or .rds format")


expect_error(import_wq(source = NULL),
             "A list of site IDs must be specified")


expect_error(import_wq(source = NULL, sites = c(1, 2, 3)),
             "Site must be a list of strings")


expect_error(import_wq(source = NULL,
                       sites = c("SW-60250424", "NE-45401461", "MD-28291180", "MD-44205010", "SE-99999999"),
                       dets = "test"),
             "dets must be set to default, all or a vector of determinand IDs. String IDs must be 4-digits long.")


expect_error(import_wq(source = NULL,
                       sites = c("SW-60250424", "NE-45401461", "MD-28291180", "MD-44205010", "SE-99999999"),
                       dets = c("1", "2", "3")),
             "dets must be set to default, all or a vector of determinand IDs. String IDs must be 4-digits long.")


expect_error(import_wq(source = NULL,
                       sites = c("SW-60250424", "NE-45401461", "MD-28291180", "MD-44205010", "SE-99999999"),
                       dets = "default",
                       start_date = "01/01/2022"),
             "Start date should be in YYYY-MM-DD format")


expect_error(import_wq(source = NULL,
                       sites = c("SW-60250424", "NE-45401461", "MD-28291180", "MD-44205010", "SE-99999999"),
                       dets = "default",
                       start_date = "2022-01-01",
                       end_date = "2023/01/01"),
             "End date should be in YYYY-MM-DD format")


expect_error(import_wq(source = NULL,
                       sites = c("SW-60250424", "NE-45401461", "MD-28291180", "MD-44205010", "SE-99999999"),
                       dets = "default",
                       start_date = "2026-01-01"),
             "Start data cannot be in the future")


expect_error(import_wq(source = NULL,
                       sites = c("SW-60250424", "NE-45401461", "MD-28291180", "MD-44205010", "SE-99999999"),
                       dets = "default",
                       start_date = "2022-01-01",
                       end_date = "2021-01-01"),
             "End date must be after start date")

expect_error(import_wq(source = NULL,
                       sites = c("SW-60250424", "NE-45401461", "MD-28291180", "MD-44205010", "SE-99999999"),
                       dets = "default",
                       start_date = "2021-01-01",
                       end_date = "2022-12-31",
                       save = "hello"),
             "Save must be logical")

expect_error(import_wq(source = "testdata_import_wq_colnames.csv",
                       sites = "SW-60250424",
                       save = FALSE),
             "Imported file is missing required column headers.")


expect_warning(import_wq(source = NULL,
                         sites = c("SW-60250424", NA, "MD-28291180", "MD-44205010", "SE-99999999"),
                         dets = "default",
                         start_date = "2021-01-01",
                         end_date = "2022-12-31",
                         save = FALSE),
               "Site list contains NAs")

expect_warning(import_wq(source = NULL,
                         sites = c("SW-60250424", "NE-45401461", "MD-28291180", "MD-44205010", "SE-99999999"),
                         dets = "default",
                         start_date = "2021-01-01",
                         end_date = "2026-12-31",
                         save = FALSE),
               "End date is in the future. End date has reverted to the current date")

expect_warning(import_wq(source = NULL,
                         sites = c("SW-60250424"),
                         dets = "default",
                         start_date = "1999-01-01",
                         end_date = "2000-12-31"),
               "Data not available from the WQA database before year 2000")

# Test filters
# Determinands

test_that("dets filter works", {
  result <- import_wq(source = NULL,
                      sites = c("SW-60250424", "NE-45401461", "MD-28291180", "MD-44205010", "SE-99999999"),
                      dets = c(180, 116),
                      start_date = "2022-01-01",
                      save = FALSE)

  result <- sort(unique(result$det_id))

  compared <- c('0116', '0180')

  expect_equal(result, compared)

})

# Start date

test_that("start_date filter works", {
  result <- import_wq(source = NULL,
                      sites = c("SW-60250424", "NE-45401461", "MD-28291180", "MD-44205010", "SE-99999999"),
                      start_date = "2022-01-01",
                      save = FALSE)

  result <- min(result$date)

  compared <- as.Date("2000-01-01")

  expect_gte(result, compared)
})

# End date

test_that("end_date filter works", {
  result <- import_wq(source = NULL,
                      sites = c("SW-60250424", "NE-45401461", "MD-28291180", "MD-44205010", "SE-99999999"),
                      start_date = "2022-01-01",
                      end_date = "2022-12-31",
                      save = FALSE)

  result <- max(result$date)

  compared <- as.Date("2023-01-01")

  expect_lte(result, compared)
})

# Test end date overwritten if set in future

test_that("End date over-written to Sys.Date if set to future date", {
  result <- import_wq(source = NULL,
                      sites = c("SW-60250424", "NE-45401461", "MD-28291180", "MD-44205010", "SE-99999999"),
                      dets = "default",
                      start_date = "2021-01-01",
                      end_date = "2026-12-31",
                      save = FALSE)

  res <- max(result$date)

  compared <- as.Date(Sys.Date())

  expect_lte(res, compared)
})

# Test output

test_that("import_wq constructs 'tibble' as expected", {
  result <- import_wq(source = NULL,
                      sites = "SW-60250424",
                      dets = "default",
                      start_date = "2022-01-01",
                      end_date = "2022-06-01",
                      save = TRUE)

  result <- result %>%
    dplyr::arrange(date_time, det_id)

  compared <- tibble::as_tibble(readRDS("testdata_import_wq_v2.rds")) %>%
    dplyr::arrange(date_time, det_id)
  expect_equal(result, compared, ignore_attr = TRUE)
})
