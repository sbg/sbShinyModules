#' save_file_generic UI Function
#'
#' @description A shiny Module.
#'
#' @param id Module ID.
#' @param save_button_title Button label.
#'
#' @return No value. Use in the UI & server of shiny application.
#'
#' @export
mod_save_file_generic_ui <- function(id, save_button_title = "Save file") {
  ns <- NS(id)

  tagList(
    actionButton(
      inputId = ns("save_file_btn"),
      label = save_button_title,
      icon = icon("file-export"),
      width = "100%"
    )
  )
}

#' Generic module server function for saving files that are exported to
#'  the Platform
#'
#' @description Server side of the shiny Module that allows users to save files
#'  generated within the Shiny app and export them to the project on the
#'  Seven Bridges Platform.
#'
#' @param id Module's ID.
#' @param reac_vals Reactive values list containing mandatory fields:
#' \itemize{
#'  \item `FUN`- Function for creating a file, i.e `write.table`,
#'     `save`, `write`, `write_json`, `write_xml`, `SaveH5Seurat` etc.
#'  \item `args`- List of function arguments for the provided FUN.
#'  \item `filename`- File name.
#'  \item `extension`- The expected file extension. Please provide the expected
#'     file extension if available, in order to properly validate the existence
#'     of the file with the same name and extension.
#'  \item `overwrite`- Boolean. Overwrite the existing file with the same name.
#' }
#' @param sbg_directory_path Path to the mounted `sbgenomics` directory
#'  containing `project-files`, `output-files` and `workspace` sub-directories
#'  on the instance.
#'  These directories are expected to exist on the instance where the app will
#'  run. For the purpose of testing your app locally, you can create a mock
#'  directory `sbgenomics` with the same structure - containing sub-directories
#'  `project-files`, `output-files` and `workspace` and populate them with test
#'  files mimicking the project file structure on the Platform.
#'
#' @return No value. Use in the UI & server of shiny application.
#'
#' @importFrom checkmate assert_function assert_list assert_string
#' @importFrom checkmate assert_true assert_logical
#' @importFrom shinyalert shinyalert
#'
#' @example inst/demos/generic_file_exporter_demo_app.R
#'
#' @export
mod_save_file_generic_server <- function(id,
                                         reac_vals,
                                         sbg_directory_path = "/sbgenomics") {
  checkmate::assert_true(isTruthy(reac_vals))
  checkmate::assert_string(sbg_directory_path, null.ok = FALSE)

  if (endsWith(x = sbg_directory_path, suffix = "/")) {
    sbg_directory_path <- substr(
      sbg_directory_path, 1,
      nchar(sbg_directory_path) - 1
    )
  }
  check_sbg_directory_path(sbg_directory_path)

  moduleServer(id, module = function(input, output, session) {
    ns <- session$ns

    observeEvent(input$save_file_btn, {
      if (!isTruthy(reac_vals$filename)) {
        shinyalert::shinyalert(
          title = "The file name is missing",
          text = "Please provide the file name.",
          type = "error"
        )
      }

      tryCatch(
        {
          checkmate::assert_function(reac_vals$FUN)
          checkmate::assert_list(reac_vals$args)
          checkmate::assert_string(reac_vals$extension, null.ok = FALSE)
          checkmate::assert_logical(reac_vals$overwrite)
        },
        error = function(e) {
          shinyalert::shinyalert(
            title = "An error occurred when passing module parameters",
            text = paste0(e$message),
            type = "error"
          )
          return(FALSE)
        }
      )

      req(reac_vals$FUN)
      req(reac_vals$args)
      req(reac_vals$filename)

      status <- handle_file_export(
        FUN = reac_vals$FUN,
        args = reac_vals$args,
        filename = reac_vals$filename,
        extension = reac_vals$extension,
        overwrite = reac_vals$overwrite,
        sbg_directory_path = sbg_directory_path
      )
      shinyalert::shinyalert(
        title = status$title,
        text = status$text,
        type = ifelse(status$check, yes = "success", no = "error")
      )
    })
  })
}


#' Handle the file export function
#'
#' @description This function handles the export process of almost any file,
#'  including checking for the existence of a file with the same name in
#'  specified directories, and saving the file if no such file exists.
#'
#' @param FUN Function for creating a file, i.e `write.table`,
#'  `save`, `write`, `write_json`, `write_xml`, `SaveH5Seurat` etc.
#' @param args List of function arguments for the provided FUN.
#' @param filename File name. Please, be aware if you provide the `file`,
#'  `path` or `filename` parameters within function's `args`, this `filename`
#'  value will overwrite it.
#' @param extension The expected file extension. Please provide the expected
#'  file extension in order to properly validate the existence of the file with
#'  the same name and extension.
#' @param overwrite Boolean. Overwrite the existing file with the same name.
#' @param sbg_directory_path The path to the mounted `sbgenomics` directory
#'  containing `project-files`, `output-files` and `workspace` sub-directories
#'  on the instance.
#'  These directories are expected on the instance where the app will
#'  run. For the purpose of testing your app locally, you can create a mock
#'  directory `sbgenomics` with the same structure - containing sub-directories
#'  `project-files`, `output-files`, `workspace` and populate them with test
#'  files mimicking the project's file structure on the Platform.
#'
#' @return A list containing the status for saving files with fields `check`,
#'  `title` and `text` ready to be used in the shinyalerts.
#'
#' @noRd
handle_file_export <- function(FUN, args, filename, extension,
                               overwrite, sbg_directory_path) {
  # Handle extension
  if (isTruthy(extension) && !startsWith(extension, ".")) {
    extension <- paste0(".", extension)
  }

  filename <- paste0(filename, extension)

  # Check if the file exists in either directory
  file_exists <- check_file_existence(
    file_name = filename,
    directory_1 = file.path(sbg_directory_path, "project-files"),
    directory_2 = file.path(sbg_directory_path, "output-files")
  )

  if (file_exists && isFALSE(overwrite)) {
    status <- list(
      check = FALSE,
      title = "Warning!",
      text = "The file with the same name already exists in the project. Please change the file name or set the `overwrite` parameter to TRUE." # nolint
    )
    return(status)
  }

  fun_args <- as.list(args(FUN))
  arg_idx <- as.numeric(
    unlist(
      sapply(
        get_golem_config("FILENAME_FUN_ARGS_EXPECTED"),
        function(x) {
          match(x, names(fun_args))
        }
      )
    )
  )
  if (all(is.na(arg_idx))) {
    status <- list(
      check = FALSE,
      title = "Error in FUN parameter",
      text = "The function does not contain the filename, file, or path arguments to set the file name and its location." # nolint
    )
    return(status)
  }

  tryCatch(
    {
      na_excluded <- na.exclude(arg_idx)
      filename_arg <- names(fun_args)[na_excluded[1]] # take first found
      args[[filename_arg]] <- file.path(
        sbg_directory_path,
        "output-files",
        filename
      )
      do.call(FUN, args)
      status <- list(
        check = TRUE,
        title = "File successfully saved!",
        text = "The file will be available in your project once you stop the app." # nolint
      )
      return(status)
    },
    error = function(e) {
      status <- list(
        check = FALSE,
        title = "Error during file saving",
        text = paste0(e)
      )
      return(status)
    }
  )
}

## To be copied in the UI
# mod_save_file_generic_ui("save_file_generic_1")

## To be copied in the server
# mod_save_file_generic_server("save_file_generic_1")
