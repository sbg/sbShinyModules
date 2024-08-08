#' save_file_generic UI Function
#'
#' @description A shiny Module.
#'
#' @param id Module ID.
#' @param save_button_title Button label.
#'
#' @return No value. Use in UI & server of shiny application.
#'
#' @export
mod_save_file_generic_ui <- function(id, save_button_title = "Save file") {
  ns <- NS(id)

  tagList(
    actionButton(
      inputId = ns("save_file_btn"),
      label = save_button_title,
      icon = icon("file-export")
    )
  )
}

#' Generic module server function for saving files for export to Platform
#'
#' @description A server side of a shiny Module that allows users to save files
#'  generated within the Shiny app and export them to the project on the
#'  Seven Bridges Platform.
#'
#' @param id Module's ID.
#' @param FUN Function for creating a file for saving, i.e `write.table`,
#'  `save`, `write`, `write_json`, `write_xml`, `SaveH5Seurat` etc.
#' @param args List of function arguments for the provided FUN.
#' @param filename File name.
#' @param extension Expected file extension. Please provide expected file
#'  extension in order to properly validate the existence of the file with the
#'  same name and extension.
#' @param overwrite Boolean. Overwrite existing file with the same name.
#' @param sbg_directory_path Path to the mounted `sbgenomics` directory
#'  containing `project-files`, `output-files` and `workspace` sub-directories
#'  on the instance.
#'  These directories are expected to exist on the instance where the app would
#'  run. For the purposes of testing your app locally, you can create a mock
#'  directory `sbgenomics` with the same structure - containing sub-directories
#'  `project-files`, `output-files` and `workspace` and populate with test files
#'  mimicking the project's file structure on the Platform.
#'
#' @return No value. Use in UI & server of shiny application.
#'
#' @importFrom checkmate assert_function assert_list assert_string
#' @importFrom checkmate assert_logical
#'
#' @example inst/demos/plot_exporter_demo_app.R
#'
#' @export
mod_save_file_geneeric_server <- function(id,
                                          FUN,
                                          args = list(),
                                          filename,
                                          extension = "",
                                          overwrite = TRUE,
                                          sbg_directory_path = "/sbgenomics") {
  checkmate::assert_function(FUN)
  checkmate::assert_list(args)
  checkmate::assert_string(filename, null.ok = FALSE)
  checkmate::assert_string(extension, null.ok = FALSE)
  checkmate::assert_logical(overwrite)
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
      handle_file_export(
        FUN = FUN,
        args = args,
        filename = filename,
        extension = extension,
        overwrite = overwrite,
        sbg_directory_path = sbg_directory_path
      )
    })
  })
}


#' Handle file export function
#'
#' @description This function handles the export process of almost any file,
#'  including checking for the existence of a file with the same name in
#'  specified directories, and saving the file if no such file exists.
#'
#' @param FUN Function for creating a file for saving, i.e `write.table`,
#'  `save`, `write`, `write_json`, `write_xml`, `SaveH5Seurat` etc.
#' @param args List of function arguments for the provided FUN.
#' @param filename File name.
#' @param extension Expected file extension. Please provide expected file
#'  extension in order to properly validate the existence of the file with the
#'  same name and extension.
#' @param overwrite Boolean. Overwrite existing file with the same name.
#' @param sbg_directory_path Path to the mounted `sbgenomics` directory
#'  containing `project-files`, `output-files` and `workspace` sub-directories
#'  on the instance.
#'  These directories are expected to exist on the instance where the app would
#'  run. For the purposes of testing your app locally, you can create a mock
#'  directory `sbgenomics` with the same structure - containing sub-directories
#'  `project-files`, `output-files` and `workspace` and populate with test files
#'  mimicking the project's file structure on the Platform.
#'
#' @return None or FALSE if error occurs.
#'
#' @importFrom shinyalert shinyalert
#'
#' @noRd
handle_file_export <- function(FUN, args, filename, extension,
                               overwrite, sbg_directory_path) {
  # Remove extension if provided in the filename input
  # Find the last dot in the filename
  dot_position <- regexpr("\\.[^\\.]*$", filename)

  # If dot_position is greater than 0, it means an extension was found
  if (dot_position > 0) {
    # Remove the extension by substring up to the position of the last dot
    # and extension based on the button that was clicked.
    filename <- paste0(substr(filename, 1, dot_position - 1), ".", extension)
  }

  # Check if the file exists in either directory
  file_exists <- check_file_existence(
    file_name = paste0(filename, extension),
    directory_1 = file.path(sbg_directory_path, "project-files"),
    directory_2 = file.path(sbg_directory_path, "output-files")
  )

  if (file_exists && !overwrite) {
    shinyalert::shinyalert(
      title = "Warning!",
      text = "The file with the same name already exists in the project. Please, change the file name or set the `overwrite` parameter to TRUE.", # nolint
      type = "error"
    )
    return(FALSE)
  }

  fun_args <- as.list(args(FUN))
  arg_idx <- as.numeric(
    unlist(
      sapply(
        c("file", "filename", "path"),
        function(x) {
          grep(x, names(fun_args))
        }
      )
    )
  )
  if (length(arg_idx) == 0) {
    shinyalert::shinyalert(
      title = "Warning!",
      text = "The function doesn't contain arguments `filename`, `file` or `path` in order to set file name and its location.", # nolint
      type = "error"
    )
    return(FALSE)
  }

  tryCatch(
    {
      filename_arg <- names(fun_args)[arg_idx[1]]
      fun_args[[filename_arg]] <- file.path(
        sbg_directory_path,
        "output-files"
      )
      do.call(FUN, fun_args)
    },
    error = function(e) {
      return(FALSE)
    }
  )
}
## To be copied in the UI
# mod_save_file_generic_ui("save_file_generic_1")

## To be copied in the server
# mod_save_file_generic_server("save_file_generic_1")
