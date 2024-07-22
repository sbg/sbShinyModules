#' Get all project's files
#'
#' @description
#'  Utility function for fetching all project files recursively from SB File
#'  system within Data Studio along side with their metadata associated if
#'  available. The expected output is the data.frame containing all files
#'  information available, ready to be used and displayed within file pickers.
#'
#' @param path Project files directory path.
#' @param ... Additional parameters that can be passed to a function
#'  `list.files()` that this function is relying on, like `pattern`,
#'  `include.dirs` etc. See more details on `?list.files`.
#'
#' @importFrom purrr map list_rbind
#' @importFrom xattrs get_xattr_df
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

  files_list <- purrr::map(
    .x = files_info_df[["path"]],
    .f = function(file) {
      file_df <- xattrs::get_xattr_df(file)
      file_df$file_path <- file
      # In case of error that nul is present in the hex string,
      # remove those nuls
      file_df$raw <- sapply(file_df$contents, function(x) {
        hex <- subset(x, !x == "00")
        rawToChar(as.raw(strtoi(hex, 16L)))
      })
      file_df
    }
  )
  metadata <- purrr::list_rbind(files_list)

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
