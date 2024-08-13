testthat::test_that("handle_file_export saves files of txt extensions", {
  # Test saving txt file
  arg_list <- list(
    extension = ".txt",
    FUN = write.table,
    args = list(x = iris, quote = FALSE, row.names = FALSE),
    filename = "test_txt",
    overwrite = TRUE,
    sbg_directory_path = testthat::test_path("sbgenomics_test")
  )

  export_status <- do.call(sbShinyModules:::handle_file_export, arg_list)

  testthat::expect_true(export_status$check)
  testthat::expect_equal(export_status$title, "File successfully saved!")
  testthat::expect_equal(
    export_status$text,
    "The file will be available in your project once you stop the app."
  )
  testthat::expect_true(
    file.exists(
      file.path(
        arg_list$sbg_directory_path,
        "output-files",
        paste0(arg_list$filename, arg_list$extension)
      )
    )
  )

  # Remove created txt file
  file.remove(file.path(
    arg_list$sbg_directory_path,
    "output-files",
    paste0(arg_list$filename, arg_list$extension)
  ))
})

testthat::test_that("handle_file_export saves files of csv extensions and reports when attempting to save the file with the same name ", { # nolint
  # Test saving csv file, with setting extension without a dot
  arg_list <- list(
    extension = "csv",
    FUN = write.table,
    args = list(x = iris, quote = FALSE, row.names = FALSE, sep = ";"),
    filename = "test_csv",
    overwrite = FALSE,
    sbg_directory_path = testthat::test_path("sbgenomics_test")
  )

  export_status <- do.call(sbShinyModules:::handle_file_export, arg_list)

  testthat::expect_true(export_status$check)
  testthat::expect_equal(export_status$title, "File successfully saved!")
  testthat::expect_equal(
    export_status$text,
    "The file will be available in your project once you stop the app."
  )
  testthat::expect_true(
    file.exists(
      file.path(
        arg_list$sbg_directory_path,
        "output-files",
        paste0(arg_list$filename, ".", arg_list$extension)
      )
    )
  )

  # Test when saving the file with the same name and extension, but overwrite
  # is set to FALSE
  arg_list <- list(
    extension = "csv",
    FUN = write.table,
    args = list(x = iris, quote = FALSE, row.names = FALSE, sep = ";"),
    filename = "test_csv",
    overwrite = FALSE,
    sbg_directory_path = testthat::test_path("sbgenomics_test")
  )

  export_status <- do.call(sbShinyModules:::handle_file_export, arg_list)

  testthat::expect_false(export_status$check)
  testthat::expect_equal(export_status$title, "Warning!")
  testthat::expect_equal(
    export_status$text,
    "The file with the same name already exists in the project. Please, change the file name or set the `overwrite` parameter to TRUE." # nolint
  )

  # Remove created csv file
  file.remove(file.path(
    arg_list$sbg_directory_path,
    "output-files",
    paste0(arg_list$filename, ".", arg_list$extension)
  ))
})

testthat::test_that("handle_file_export saves files of json extensions", {
  # Test saving json file
  arg_list <- list(
    extension = ".json",
    FUN = write,
    args = list(
      x = jsonlite::toJSON(x = iris, dataframe = "rows", pretty = TRUE)
    ),
    filename = "test_json",
    overwrite = TRUE,
    sbg_directory_path = testthat::test_path("sbgenomics_test")
  )

  export_status <- do.call(sbShinyModules:::handle_file_export, arg_list)

  testthat::expect_true(export_status$check)
  testthat::expect_equal(export_status$title, "File successfully saved!")
  testthat::expect_equal(
    export_status$text,
    "The file will be available in your project once you stop the app."
  )
  testthat::expect_true(
    file.exists(
      file.path(
        arg_list$sbg_directory_path,
        "output-files",
        paste0(arg_list$filename, arg_list$extension)
      )
    )
  )

  # Remove created json file
  file.remove(file.path(
    arg_list$sbg_directory_path,
    "output-files",
    paste0(arg_list$filename, arg_list$extension)
  ))
})

testthat::test_that("handle_file_export saves files of RDS extensions", {
  # Test saving rds file
  arg_list <- list(
    extension = "RDS",
    FUN = saveRDS,
    args = list(
      object = iris
    ),
    filename = "test_rds",
    overwrite = TRUE,
    sbg_directory_path = testthat::test_path("sbgenomics_test")
  )

  export_status <- do.call(sbShinyModules:::handle_file_export, arg_list)

  testthat::expect_true(export_status$check)
  testthat::expect_equal(export_status$title, "File successfully saved!")
  testthat::expect_equal(
    export_status$text,
    "The file will be available in your project once you stop the app."
  )
  testthat::expect_true(
    file.exists(
      file.path(
        arg_list$sbg_directory_path,
        "output-files",
        paste0(arg_list$filename, ".", arg_list$extension)
      )
    )
  )

  # Remove created RDS file
  file.remove(file.path(
    arg_list$sbg_directory_path,
    "output-files",
    paste0(arg_list$filename, ".", arg_list$extension)
  ))
})

testthat::test_that("handle_file_export reports error when FUN doesn't contain file, filename or path arguments", { # nolint
  # Test saving file
  arg_list <- list(
    extension = "",
    FUN = file.create,
    args = list(),
    filename = "test_blanc",
    overwrite = TRUE,
    sbg_directory_path = testthat::test_path("sbgenomics_test")
  )

  export_status <- do.call(sbShinyModules:::handle_file_export, arg_list)

  testthat::expect_false(export_status$check)
  testthat::expect_equal(export_status$title, "Error in FUN parameter")
  testthat::expect_equal(
    export_status$text,
    "The function doesn't contain arguments `filename`, `file` or `path` in order to set file name and its location." # nolint
  )
})

testthat::test_that("handle_file_export saves files without extensions", {
  # Test saving file
  arg_list <- list(
    extension = "",
    FUN = write.table,
    args = list(x = iris),
    filename = "test_blanc",
    overwrite = TRUE,
    sbg_directory_path = testthat::test_path("sbgenomics_test")
  )

  export_status <- do.call(sbShinyModules:::handle_file_export, arg_list)

  testthat::expect_true(export_status$check)
  testthat::expect_equal(export_status$title, "File successfully saved!")
  testthat::expect_equal(
    export_status$text,
    "The file will be available in your project once you stop the app."
  )
  # Remove created file
  file.remove(file.path(
    arg_list$sbg_directory_path,
    "output-files",
    arg_list$filename
  ))
})

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
