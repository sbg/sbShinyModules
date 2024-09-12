testthat::test_that("Utility fucntion get_all_project_files() works as expected", { # nolint
  # Get mock directory for testing
  project_path <- testthat::test_path("sbgenomics_test/project-files/")
  files_df <- get_all_project_files(path = project_path)

  testthat::expect_true(inherits(files_df, "data.frame"))
  testthat::expect_equal(ncol(files_df), 12)
  testthat::expect_equal(nrow(files_df), 5)
  testthat::expect_true(all(c("name", "path", "size") %in% names(files_df)))
})

test_that("Utility fucntion get_all_project_files() throws error when expected", { # nolint
  # Test non-existing directory path
  project_path <- "non-existing-folder"
  testthat::expect_error(
    get_all_project_files(path = project_path),
    regexp = paste0("Directory not found on the provided path ", project_path, "."), # nolint
    fixed = TRUE
  )

  # Get mock empty directory for testing
  project_path <- testthat::test_path("empty_folder")
  testthat::expect_error(
    get_all_project_files(path = project_path),
    regexp = paste0("Empty directory. Files not found on the provided directory path ", project_path, "."), # nolint
    fixed = TRUE
  )
})
