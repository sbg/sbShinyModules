# testthat::test_that("handle_file_export returns correct saves files of different types and extensions", { # nolint
#   extension <- ".txt"
#
#
#   FUN <- write.table
#   args <- list(x = iris, quote = FALSE, rownames = FALSE)
#   filename <- "test"
#   extension <- extension
#
#   testthat::expect_snapshot_output(
#     do.call(what = FUN, args = args))
#
#
#   extension <- ".csv"
#
#   extension <- ".json"
#
#   extension <- ".xml"
#
#   extension <- ".RDS"
#
#   extension <- ""
# })

testthat::test_that("check_file_existence works well", { # nolint

  existing_file <- "test_file_1.txt"
  non_existing_file <- "non_existing.txt"

  directory1 <- testthat::test_path("sbgenomics_test/project-files/")
  directory2 <- testthat::test_path("sbgenomics_test/output-files/")

  testthat::expect_true(
    sbShinyModules:::check_file_existence(existing_file, directory1, directory2)
  )
  testthat::expect_false(
    sbShinyModules:::check_file_existence(
      non_existing_file,
      directory1, directory2
    )
  )
})
