#' Get all project files
#'
#' @description
#'  Utility function for fetching all project files from the Seven Bridges File
#'  system within Data Studio accompanied by associated metadata if available.
#'  The expected output is the data.frame containing all available
#'  information about the files, ready to be used and displayed within file
#'  pickers.
#'
#'  **Note:** The `xattrs` package is required for extended attribute support,
#'  which includes retrieving additional metadata. This package works only on
#'  Unix-based operating systems (e.g., Linux, macOS) and cannot be installed
#'  or used on Windows systems due to the lack of support for extended
#'  attributes functions on Windows. Therefore, the `get_all_project_files()`
#'  function will not work on Windows systems. For Unix-based systems, please
#'  install the `xattrs` package to utilize this function.
#'
#' @param path Project files directory path.
#' @param ... Additional parameters that can be passed to the `list.files()`
#'  function that this function is relying on, like `pattern`,
#'  `include.dirs` etc. See more details on `?list.files`.
#'
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select left_join join_by mutate
#' @importFrom magrittr %>%
#' @importFrom checkmate assert_string
#' @importFrom rlang abort
#' @importFrom glue glue_col
#'
#' @return Data.frame containing all project files with their names, paths,
#'  sizes and associated metadata fields if available.
#'
#' @export
get_all_project_files <- function(path, ...) {
  # Check for OS type and abort if Windows
  if (.Platform$OS.type == "windows") {
    rlang::abort("The function `get_all_project_files` is not supported on Windows operating systems.") # nolint
  }

  if (!requireNamespace("xattrs", quietly = TRUE)) {
    rlang::abort("The xattrs package is required for this function, but it is not installed.") # nolint
  }

  checkmate::assert_string(path, null.ok = FALSE)
  if (!dir.exists(paths = path)) {
    rlang::abort(
      glue::glue_col("Directory not found on the provided path {red {path}}.")
    )
  }

  if (endsWith(path, "/")) {
    path <- substr(path, 1, nchar(path) - 1)
  }
  files <- list.files(path = path, recursive = TRUE, full.names = TRUE, ...)

  if (length(files) == 0) {
    rlang::abort(
      glue::glue_col(
        "Empty directory. Files not found on the provided directory path {red {path}}." # nolint
      )
    )
  }

  f_info <- file.info(files)
  rownames(f_info) <- NULL

  df <- data.frame(name = basename(files), path = files)

  files_info_df <- cbind.data.frame(df, f_info)

  metadata <- data.frame()

  for (file in files_info_df[["path"]]) {
    file_df <- tryCatch(
      xattrs::get_xattr_df(file),
      error = function(e) {
        rlang::abort(glue::glue_col("Error retrieving metadata for file {file}: {e$message}")) # nolint
      }
    )
    file_df$file_path <- file
    # In case of an error that 'nul' is present in the hex string, remove those 'nuls' # nolint
    file_df$raw <- sapply(file_df$contents, function(x) {
      hex <- subset(x, !x == "00")
      rawToChar(as.raw(strtoi(hex, 16L)))
    })
    metadata <- rbind(metadata, file_df)
  }

  # Transform data frame
  trans <- metadata %>%
    dplyr::select(-contents, -size) %>%
    dplyr::mutate(name = gsub(
      x = .data[["name"]],
      pattern = "sbg.",
      replacement = ""
    )) %>%
    tidyr::pivot_wider(.,
      names_from = "name",
      values_from = c("raw"),
      values_fill = NA
    )

  merged <- dplyr::left_join(files_info_df, trans,
    by = dplyr::join_by("path" == "file_path")
  )
  return(merged)
}
