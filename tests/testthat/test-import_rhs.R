# Test Error Messages

expect_error(import_rhs(surveys = data), "If specified, surveys must be a vector")

expect_error(import_rhs(surveys = "39880",
                            save_dir = "hello"),
             "Specified save directory does not exist")

expect_error(import_rhs(surveys = "39880",
                            save = "hello"),
             "Save is not logical")

expect_error(import_rhs(save_dwnld = "hello"),
             "Save_dwnld is not logical")

# Test Output

test_that("import_rhs constructs 'tibble' as expected", {
  result <- import_rhs(surveys = "39880", save = TRUE)
  compared <- tibble::as_tibble(readRDS("RHS_survey_summary_F.rds"))
  expect_equivalent(result, compared)
})
