testthat::test_that("Utility fucntion get_all_project_files() works as expected", { # nolint
  # Get mock directory for testing
  project_path <- testthat::test_path("test_files_folder")
  files_df <- get_all_project_files(path = project_path)

  testthat::expect_true(inherits(files_df, "data.frame"))
  testthat::expect_equal(ncol(files_df), 17)
  testthat::expect_equal(nrow(files_df), 5)
  testthat::expect_true(all(c("name", "path", "size") %in% names(files_df)))
})

test_that("Utility fucntion get_all_project_files() throws error when expected", { # nolint
  # Test non-existing directory path
  project_path <- "non-existing-folder"
  testthat::expect_error(
    get_all_project_files(path = project_path),
    regexp = "Directory not found on the provided path.",
    fixed = TRUE
  )

  # Get mock empty directory for testing
  project_path <- testthat::test_path("empty_folder")
  testthat::expect_error(
    get_all_project_files(path = project_path),
    regexp = "Empty directory. Files not found on the provided directory path.",
    fixed = TRUE
  )
})
