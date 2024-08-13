testthat::test_that("create_col_def returns correct colDef for numeric data", {
  col_data <- c(1, 2, 3, 4, 5)
  result <- create_col_def(col_data)

  expect_s3_class(result, "colDef")
  expect_true(result$filterable)
  expect_true(result$resizable)
  expect_true(grepl("MaterialUI.Slider", result$filterInput))
})

test_that("create_col_def returns correct colDef for factor data", {
  col_data <- factor(c("A", "B", "A", "C"))
  result <- create_col_def(col_data)

  expect_s3_class(result, "colDef")
  expect_true(result$filterable)
  expect_true(result$resizable)
  # Check if filterInput is a function
  expect_true(is.function(result$filterInput))

  # Ensure that the function returns the expected HTML structure
  filter_input_html <- result$filterInput(levels(col_data), "test")
  expect_true(inherits(filter_input_html, "shiny.tag"))
  expect_true(grepl("select", as.character(filter_input_html)))
  expect_true(grepl("Reactable.setFilter", as.character(filter_input_html)))
})

testthat::test_that("create_col_def returns correct colDef for other data types", { # nolint
  col_data <- c("A", "B", "A", "C")
  result <- create_col_def(col_data)

  expect_s3_class(result, "colDef")
  expect_true(result$filterable)
  expect_false(is.null(result$resizable))
})

test_that("muiDependency returns correct list of dependencies", {
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
  expect_equal(mui_dep$version, "5.6.3")
  expect_equal(
    mui_dep$src$file,
    system.file("assets/material-ui", package = "sbShinyModules")
  )
  expect_equal(mui_dep$script, "material-ui.production.min.js")
})
