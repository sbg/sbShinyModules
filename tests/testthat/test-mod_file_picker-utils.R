testthat::test_that("create_col_def returns correct colDef for numeric data", {
  col_data <- c(1, 2, 3, 4, 5)
  result <- create_col_def(col_data)

  expect_s3_class(result, "colDef")
  expect_true(result$filterable)
  expect_true(result$resizable)
  expect_true(grepl("MaterialUI.Slider", result$filterInput))
})

testthat::test_that("create_col_def returns correct colDef for factor data", {
  col_data <- factor(c("A", "B", "A", "C"))
  result <- create_col_def(col_data)

  expect_s3_class(result, "colDef")
  expect_true(result$filterable)
  expect_true(result$resizable)
  # Check if filterInput inherits from JS_EVAL
  expect_s3_class(result$filterInput, "JS_EVAL")
})

testthat::test_that("create_col_def returns correct colDef for other data types", { # nolint
  col_data <- c("A", "B", "A", "C")
  result <- create_col_def(col_data)

  expect_s3_class(result, "colDef")
  expect_true(result$filterable)
  expect_false(is.null(result$resizable))
})

testthat::test_that("muiDependency returns correct list of dependencies", {
  deps <- muiDependency()

  # Check that the result is a list of length 2
  expect_length(deps, 2)

  # Check the first dependency is the React dependency from reactR
  react_dep <- deps[[1]]
  expect_s3_class(react_dep, "html_dependency")
  expect_equal(react_dep$name, "react")

  # Check the second dependency is the MUI dependency
  mui_dep <- deps[[2]]
  expect_s3_class(mui_dep, "html_dependency")
  expect_equal(mui_dep$name, "mui")
  expect_equal(mui_dep$version, "5.16.7")
  expect_equal(
    mui_dep$src$file,
    system.file("assets/material-ui", package = "sbShinyModules")
  )
  expect_equal(mui_dep$script, "material-ui.production.min.js")
})

testthat::test_that("The load_guide_content function processes a valid Markdown file correctly", { # nolint
  # Create a temporary .md file
  temp_md <- tempfile(fileext = ".md")
  writeLines(c(
    "### Test Heading",
    "",
    "This is a test markdown file."
  ), temp_md)

  # Call the load_guide_content function
  result <- load_guide_content(temp_md)

  # Check if result contains the correct HTML
  testthat::expect_true(grepl("<h3>Test Heading</h3>", result))
  testthat::expect_true(grepl("<p>This is a test markdown file.</p>", result))

  # Clean up
  unlink(temp_md)
})

testthat::test_that("load_guide_content function throws an error for the non-existent file", { # nolint
  non_existent_file <- "non_existent.md"

  testthat::expect_error(
    load_guide_content(non_existent_file),
    "The non_existent.md file does not exist."
  )
})

testthat::test_that("load_guide_content throws an error for non-markdown file extension", { # nolint
  temp_txt <- tempfile(fileext = ".txt")
  writeLines("This is a text file, not a markdown file.", temp_txt)

  testthat::expect_error(
    load_guide_content(temp_txt),
    paste("The provided file is not a Markdown (.md) file:", temp_txt),
    fixed = TRUE
  )

  # Clean up
  unlink(temp_txt)
})

testthat::test_that("load_guide_content function throws an error for unreadable (binary) file", { # nolint
  # Create a temporary directory with .md extension
  temp_dir_md <- tempfile(fileext = ".md")
  dir.create(temp_dir_md)

  # Expect error when trying to read the directory
  testthat::expect_error(
    suppressWarnings(load_guide_content(temp_dir_md)),
    "Failed to read the markdown file:",
    fixed = FALSE
  )

  # Clean up
  unlink(temp_dir_md, recursive = TRUE)
})

testthat::test_that("load_guide_content throws error when trying to read a file with no read permissions", { # nolint
  # The purpose of this test is to provoke the readLines() function within
  # the load_guide_content() function to fail by removing read permissions
  # from the file. We aim to verify that the error gets propagated correctly
  # through the tryCatch block and that the expected error message is produced.

  # Skip this test on Windows OS because Sys.chmod() function is Unix-specific.
  testthat::skip_if(.Platform$OS.type == "windows", "Skipping tests on Windows.") # nolint

  # Create a temporary file with .md extension
  temp_md <- tempfile(fileext = ".md")
  writeLines("This is a valid markdown file.", temp_md)

  # Remove read permissions for the file
  Sys.chmod(temp_md, mode = "000") # No permissions

  # Expect error when trying to read the file with no read permissions.
  # We use suppressWarnings() to ignore the permission-related warnings and
  # focus only on testing whether the correct error is thrown by the function.
  testthat::expect_error(
    suppressWarnings(load_guide_content(temp_md)),
    "Failed to read the markdown file: cannot open the connection"
  )

  # Restore permissions and clean up
  Sys.chmod(temp_md, mode = "600")
  unlink(temp_md)
})

testthat::test_that("load_guide_content handles markdown_html failure", {
  # Create a temporary file with .md extension
  temp_md <- tempfile(fileext = ".md")

  # Write valid markdown content to the file
  writeLines("# Valid Heading\n\nThis is a valid markdown file.", temp_md)

  # Temporarily override the markdown_html function to simulate a failure
  original_markdown_html <- commonmark::markdown_html
  assignInNamespace(
    "markdown_html",
    function(...) stop("Simulated conversion error"),
    ns = "commonmark"
  )

  # Expect error when the overridden markdown_html function is triggered
  testthat::expect_error(
    load_guide_content(temp_md),
    "Failed to convert markdown to HTML: Simulated conversion error"
  )

  # Restore the original function after the test
  assignInNamespace("markdown_html", original_markdown_html, ns = "commonmark")

  # Clean up
  unlink(temp_md)
})

testthat::test_that("sanitize_html removes risky tags", {
  html_content <- '<h1>Title</h1><script>alert("XSS Attack!");</script><iframe src="http://example.com"></iframe>' # nolint
  safe_html <- sbShinyModules:::sanitize_html(html_content)

  # Check that <script> and <iframe> tags are removed
  expect_false(grepl("<script>", safe_html))
  expect_false(grepl("<iframe>", safe_html))
  expect_true(grepl("<h1>Title</h1>", safe_html)) # Safe tag should remain
})

testthat::test_that("sanitize_html preserves safe href links", {
  html_content <- '<a href="http://example.com">Go to Example</a><a href="mailto:someone@example.com">Send Email</a>' # nolint
  safe_html <- sbShinyModules:::sanitize_html(html_content)

  # Check that safe href links are preserved
  expect_true(grepl('href="http://example.com"', safe_html))
  expect_true(grepl('href="mailto:someone@example.com"', safe_html))
})

testthat::test_that("sanitize_html neutralizes unsafe href links", {
  html_content <- '<a href="javascript:alert(\'XSS Attack!\')">Click me</a>'
  safe_html <- sbShinyModules:::sanitize_html(html_content)

  # Check that unsafe href links are replaced with "#"
  expect_false(grepl('href="javascript:', safe_html))
  expect_true(grepl('href="#"', safe_html))
})

testthat::test_that("sanitize_html removes other risky tags", {
  html_content <- '<object data="movie.mp4"></object><embed src="file.swf"><svg></svg><img src="image.png">' # nolint
  safe_html <- sbShinyModules:::sanitize_html(html_content)

  # Check that <object>, <embed>, <svg>, and <img> tags are removed
  expect_false(grepl("<object>", safe_html))
  expect_false(grepl("<embed>", safe_html))
  expect_false(grepl("<svg>", safe_html))
  expect_false(grepl("<img>", safe_html))
})
