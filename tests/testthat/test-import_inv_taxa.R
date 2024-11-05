## Test error messages

expect_error(import_inv_taxa(sites = test),
             "If specified, sites must be a character vector")

expect_error(import_inv_taxa(sites = c(1,2,3)),
             "If specified, sites must be a character vector")

expect_error(import_inv_taxa(sites = vector()),
             "No sites specified in input")

expect_error(import_inv_taxa(samples = data),
             "If specified, samples must be a character vector")

expect_error(import_inv_taxa(samples = c(1,2,3)),
             "If specified, samples must be a character vector")

expect_error(import_inv_taxa(samples = vector()),
             "No samples specified in input")

expect_error(import_inv_taxa(sites = "12345",
                         save = "test"),
             "save is not logical")

## Test warnings

expect_warning(import_inv_taxa(sites = c("34310", NA, "34343")),
               "sites contains NAs")

expect_warning(import_inv_taxa(samples = c("167406", NA, "167407")),
               "samples contains NAs")

expect_warning(import_inv_taxa(sites = c("999999999")),
               "No rows in output dataset")

# Test Output

test_that("import_inv_taxa constructs 'tibble' as expected", {
  result <- import_inv_taxa(sites = "34310")
  compared <- tibble::as_tibble(readRDS("testdata_import_taxa_format_v2.RDS"))
  expect_equal(result, compared, ignore_attr = TRUE)
})

