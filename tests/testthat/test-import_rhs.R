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

expect_error(import_rhs(source = "RHS_survey_summary_F.rds",
                        rhs_dir = "RHS_survey_summary_F.rds"),
             "Set source = NULL if using rhs_dir to specify file paths. Alternatively, file paths can be specified using source")

expect_warning(import_rhs(rhs_dir = "RHS_survey_summary_F.rds"),
               "In function import_rhs, rhs_dir argument deprecated. File paths can be specified using source.")

# Test Output

test_that("import_rhs constructs 'tibble' as expected", {
  result <- import_rhs(surveys = "39880", save = TRUE)
  compared <- tibble::as_tibble(readRDS("RHS_survey_summary_F.rds"))
  expect_equivalent(result, compared)
})
