# Manual test needed vs RICT output

# Test Error Messages

test_that("Save directory error message is functioning", {

master.file <- readxl::read_excel("NDMN Ecology Master Network PROPOSED NEW FORMAT.xlsx", sheet = "Master NDMN")
biolsites <- dplyr::filter(master.file, Area_Code == "HNL")
biolsites <- biolsites$Biosys_Site_ID
env <- import_env(sites = biolsites)

expect_error(predict_indices(env_data = env,
                             save_dir = "hello"),
             "Specified save directory does not exist")

})

test_that("save = logical error message is functioning", {

  master.file <- readxl::read_excel("NDMN Ecology Master Network PROPOSED NEW FORMAT.xlsx", sheet = "Master NDMN")
  biolsites <- dplyr::filter(master.file, Area_Code == "HNL")
  biolsites <- biolsites$Biosys_Site_ID
  env <- import_env(sites = biolsites)

expect_error(predict_indices(env_data = env,
                        save = "hello"),
             "Save is not logical")

})
