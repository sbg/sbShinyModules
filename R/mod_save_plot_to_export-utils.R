# Utils plot export handlers -------------------------------------------------

#' Download a plot with specified parameters
#'
#' @description This function creates a download handler for a plot,
#'  allowing users to specify the filename, device, width, height,
#'  and destination folder.
#'
#' @param input Shiny input.
#' @param rv A reactive values list containing the plot to be downloaded.
#' @param device A character string specifying the file format for the plot
#'  (e.g., `png`, `pdf`).
#' @param folder A character string specifying the destination folder for the
#'  downloaded file. Defaults to the current directory.
#'
#' @return A download handler that saves the specified plot with the given
#'  parameters.
#'
#' @noRd
download_plot_rv <- function(input, rv, device, folder = ".") {
  downloadHandler(
    filename = function() {
      filename <- input$filename
      if (endsWith(filename, paste0("\\.", device))) {
        filename
      } else {
        paste0(filename, ".", device)
      }
    },
    content = function(file) {
      width <- input$width
      height <- input$height
      save_plot_file(
        cur_plot = rv$plot,
        filename = file,
        device = device,
        width = width,
        height = height,
        destination_folder = folder
      )
    }
  )
}


#' Create an HTML Dependency for the Moveable Library
#'
#' @description This function creates an HTML dependency for the Moveable
#'  library, including the necessary JavaScript files.
#'
#' @importFrom htmltools htmlDependency
#'
#' @noRd
html_dependency_moveable <- function() {
  htmltools::htmlDependency(
    name = "moveable",
    version = "0.23.0",
    src = system.file("assets", package = "sbShinyModules"),
    script = c("moveable.min.js", "resizer-handler.js"),
    all_files = FALSE
  )
}

#' Activate the Resizer for a Specified Element
#'
#' @description This function sends a custom message to activate the resizer for
#'  a specified HTML element, with optional parameters for the modal context and
#'  the container.
#'
#' @param id A character string specifying the ID of the HTML element to be
#'  resized.
#' @param ... Additional parameters to be passed to the resizer.
#' @param modal A logical value indicating if the element is in a modal
#'  dialog. The default value is `FALSE`.
#' @param container A character string specifying the container for the
#'  resizer. The default value is `"body"`.
#' @param session Shiny session object. Default: `getDefaultReactiveDomain()`.
#'
#' @return None.
#'
#' @noRd
activate_resizer <- function(id,
                             ...,
                             modal = FALSE,
                             container = "body",
                             session = getDefaultReactiveDomain()) {
  if (isTRUE(modal)) {
    container <- ".modal-body"
  }
  session$sendCustomMessage("resize", list(
    id = id,
    container = container,
    ...,
    modal = modal
  ))
}

#' Resize function
#'
#' @description This function sends a custom message to resize a specified HTML
#'  element to the given width and height.
#'
#' @param id A character string specifying the ID of the HTML element to be
#'  resized.
#' @param width The new width for the element.
#' @param height The new height for the element.
#' @param session Shiny session object. Default: `getDefaultReactiveDomain()`.
#'
#' @return None.
#'
#' @noRd
resize <- function(id,
                   width,
                   height,
                   session = getDefaultReactiveDomain()) {
  session$sendCustomMessage(paste0("resize-", id), list(
    width = width,
    height = height
  ))
}


#' Generate random ID
#'
#' @description This function generates a random ID of the specified number of
#'  bytes, formatted as a hexadecimal string.
#'
#' @param bytes An integer specifying the number of bytes for the ID.
#'  The default value is 12.
#'
#' @return A character string representing the generated ID in the hexadecimal
#'  format.
#'
#' @noRd
genId <- function(bytes = 12) {
  paste(format(as.hexmode(sample(256, bytes, replace = TRUE) - 1), width = 2),
    collapse = ""
  )
}

#' Check the filename for plot saving
#'
#' @description Check if the provided file name already contains an extension
#'  and remove it if it is supported by the file export: `png`, `pdf`, `svg`,
#'  `jpeg`, `bmp`, `tiff`. If not, keep everything as the file base
#'  name. Then add new extension based on the provided device parameter -
#'  depends on the button user clicked. If not, keep everything as the file
#'  base name.
#'
#' @details For example, if 'export-plot' is specified as the `filename`
#'  argument, the function returns 'export-plot.png' - file name as it is + real
#'  extension according to the button that was clicked.
#'  In case 'export-plot.png' is specified, it also returns 'export-plot.png'.
#'  On the other hand, when provided with 'export-plot.june', the
#'  function will keep the extension (the part after the last dot) since it is
#'  not one of the common extensions supported by the plot exporter. It will
#'  take the entire provided string 'export-plot.june' and treat it as a valid
#'  base name for a plot about to be saved. Depending on what the user selected,
#'  a plot type extension will be added to the base name.
#'  For example: 'export-plot.june.png'.
#'
#' @param filename Character. The name of the file (plot) that the function will
#'  check and potentially remove the extension if it matches
#'  one of the specified types ("png", "pdf", "svg", "jpeg", "bmp", "tiff").
#' @param device Character. The type of device to be used for the file
#'  export (e.g., "png", "pdf", "tiff", "jpeg", "svg", "bmp") - depends on the
#'  export button that was clicked.
#'
#' @noRd
check_filename <- function(filename, device) {
  # Common extensions
  allowed_extensions <- get_golem_config("PLOT_EXPORT_SUPPORTED_EXT")

  # Find the last dot in the filename
  dot_position <- regexpr("\\.[^\\.]*$", filename)

  # If the dot_position is greater than 0, that means an extension was found
  if (dot_position > 0) {
    # Extract the extension
    extension <- tolower(substr(filename, dot_position + 1, nchar(filename)))

    # Check if the extension is in the allowed_extensions list
    if (extension %in% allowed_extensions) {
      # Remove the extension by substring up to the position of the last dot
      # and extension based on the button that was clicked.
      output_filename <- paste0(substr(filename, 1, dot_position - 1), ".", device) # nolint
      return(output_filename)
    }
  }

  # If no valid extension was found, return the file name + the extension based
  # on the button that was clicked.
  return(paste0(filename, ".", device))
}

#' Check directory and its sub-directories existence
#'
#' @description The function checks if the directory exists on the provided
#'  path, along with its sub-directories with the expected names `project-files`
#'  and `output-files`. The expected path on the Data Studio instance if hosting
#'  the app as an on-demand app, should be `/sbgenomics` and it should contain
#'  folders `project-files` and `output-files` which are mounted to the
#'  instance.
#'
#' @param sbg_path Path to the parent directory containing `project-files` and
#'  `output-files` sub-directories. Should be set to `/sbgenomics` when hosting
#'  the app as on-demand Shiny app.
#'
#' @importFrom checkmate assert_character
#' @importFrom shinyalert shinyalert
#'
#' @noRd
check_sbg_directory_path <- function(sbg_path) {
  checkmate::assert_character(sbg_path, null.ok = FALSE)
  if (!dir.exists(sbg_path)) {
    rlang::abort("Directory not found. Provided directory path doesn't exist.")
  }

  if (!dir.exists(file.path(sbg_path, "project-files")) ||
    !dir.exists(file.path(sbg_path, "output-files"))) {
    rlang::abort("Subdirectories not found. The provided directory path does not contain project-files and/or output-files sub-directories.") # nolint
  }
}

#' Check the file existence
#'
#' @description This function checks if the file with the provided `file_name`
#'  already exists in at least one of the two provided directories (paths). For
#'  the on-demand DS version of the app, these two directories should be:
#'  - `/sbgenomics/project-files`
#'  - `/sbgenomics/output-files`
#'
#' @details We need to check file existence in both places because the file
#'  can be either in the `project-files` folder, which means that it is already
#'  visible in the platform project files tab, or it can exist in the analysis
#'  `output-files`, which means that it will be visible on the platform once the
#'  the analysis (app) is stopped (saved).
#'
#' @param file_name The name of the file for which the function should check
#'  existence.
#' @param directory_1 Path to the first directory where the function should
#'  look for the existence of the provided file, i.e `project-files`.
#' @param directory_2 Path to the second directory where the function should
#'  look for the existence of the provided file, i.e `output-files`.
#'
#' @return Logical. TRUE if the file exists in either directory, FALSE
#'  otherwise.
#'
#' @noRd
check_file_existence <- function(file_name, directory_1, directory_2) {
  file_path1 <- file.path(directory_1, file_name)
  file_path2 <- file.path(directory_2, file_name)

  # Check if the file exists in either directory
  file_exists <- file.exists(file_path1) || file.exists(file_path2)

  return(file_exists)
}

#' Handle plot export function
#'
#' @description This function handles the export process of a plot file,
#'  including checking for the existence of a file with the same name in
#'  specified directories, and saving the plot file if no such file exists.
#'
#' @param input Shiny input.
#' @param filename Character. The name of the file to be exported.
#' @param device Character. The type of device to be used for the file
#'  export (e.g., "png", "pdf", "tiff", "jpeg", "svg", "bmp").
#' @param width Numeric. The width of the plot to be exported.
#' @param height Numeric. The height of the plot to be exported.
#' @param plot_rv Reactive values. A reactive values object containing the
#'  plot to be exported.
#' @param sbg_directory_path Path to the `sbgenomics` directory containing
#'  `project-files`, `output-files` and `workspace` subdirectories on the
#'  instance. These directories are expected to exist on the instance where
#'  the app will run. For the purpose of testing your app locally, you
#'  can create a mock directory `sbgenomics` with the same structure -
#'  containing sub-directories `project-files`, `output-files`, and `workspace`.
#'
#' @return None.
#'
#' @importFrom shinyalert shinyalert
#' @importFrom shinyFeedback showFeedbackWarning
#'
#' @noRd
handle_plot_export <- function(input, filename, device, height, width,
                               plot_rv, sbg_directory_path) {
  # Remove the extension if provided in the filename input
  filename <- check_filename(filename = filename, device = device)

  if (isFALSE(input$overwrite_switch)) {
    # Check if the file exists in either directory
    file_exists <- check_file_existence(
      file_name = filename,
      directory_1 = file.path(sbg_directory_path, "project-files"),
      directory_2 = file.path(sbg_directory_path, "output-files")
    )

    if (file_exists) {
      status <- list(
        title = "Warning!",
        text = "The file with the same name already exists in the project. Please change the name and try again.", # nolint
        type = "warning",
        feedback = TRUE
      )
    } else {
      status <- save_plot_file(
        cur_plot = plot_rv$plot,
        filename = filename,
        device = device,
        width = width,
        height = height,
        destination_folder = file.path(sbg_directory_path, "output-files")
      )
    }
  } else {
    status <- save_plot_file(
      cur_plot = plot_rv$plot,
      filename = filename,
      device = device,
      width = width,
      height = height,
      destination_folder = file.path(sbg_directory_path, "output-files")
    )
  }
  check_plot_export_status(status)
}

#' Save the plot file function
#'
#' @description This function saves the provided plot as a file in the provided
#'  file format (png, bmp, tiff, pdf, svg or jpeg).
#'
#' @param cur_plot The plot that should be saved.
#' @param filename File name.
#' @param device The type of device to be used for the file
#'  export (e.g., "png", "pdf", "tiff", "jpeg", "svg", "bmp").
#' @param width The width of the device.
#' @param height The height of the device.
#' @param destination_folder A string representing the path to the folder where
#'  the plot file should be saved.
#'
#' @return This function returns the status list containing information about
#'  the success of saving the plot and is used for informing the user.
#'  Saves the file as a side effect.
#'
#' @importFrom grDevices png bmp tiff pdf svg jpeg
#'
#' @noRd
save_plot_file <- function(cur_plot, filename, device, width, height,
                           destination_folder) {
  # Add destination folder to filename
  if (destination_folder != ".") {
    filename <- file.path(destination_folder, filename)
  }

  # nolint start
  device_functions <- list(
    png = list(fun = grDevices::png, args = list(filename = filename, width = width, height = height)),
    bmp = list(fun = grDevices::bmp, args = list(filename = filename, width = width, height = height)),
    tiff = list(fun = grDevices::tiff, args = list(filename = filename, width = width, height = height)),
    pdf = list(fun = grDevices::pdf, args = list(file = filename, width = width / 72, height = height / 72)),
    svg = list(fun = grDevices::svg, args = list(filename = filename, width = width / 72, height = height / 72)),
    jpeg = list(fun = grDevices::jpeg, args = list(filename = filename, width = width, height = height))
  )
  # nolint end
  tryCatch(
    {
      do.call(device_functions[[device]]$fun, device_functions[[device]]$args)
      print(cur_plot)
      dev.off()

      status <- list(
        title = "Success!",
        text = "The plot has been successfully saved and will be available in the project once the analysis is stopped.", # nolint
        type = "success"
      )
      return(status)
    },
    error = function(e) {
      status <- list(
        title = "Error occurred during plot saving",
        text = paste0(e),
        type = "error"
      )
      return(status)
    }
  )
}

#' Check the plot export status function
#'
#' @description This function checks the status for attempted plot export and
#'  returns the appropriate notification.
#'
#' @param status Logical. A value returned by the `save_plot_file` function.
#'
#' @importFrom shinyalert shinyalert
#' @importFrom shinyFeedback showFeedbackWarning
#'
#' @noRd
check_plot_export_status <- function(status) {
  shinyalert::shinyalert(
    title = status$title,
    text = status$text,
    type = status$type
  )
  if (!is.null(status$feedback) && status$feedback) {
    shinyFeedback::showFeedbackWarning(
      inputId = "filename",
      text = "Please change the file name and try again."
    )
  }
}
