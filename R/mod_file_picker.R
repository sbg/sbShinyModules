#' file_picker UI Function
#'
#' @description UI function of the file picker Shiny module. The function adds
#'  an action button that triggers a file picker modal. This module allows
#'  users to select either a single file or multiple files to be used within
#'  the app. The UI function should be placed in the UI part of the Shiny app
#'  where the file picker button is required.
#'
#' @details To incorporate this module into your Shiny app, you need to include
#'  both the UI and server functions in the appropriate places in your app
#'  code:
# nolint start
#'  \itemize{
#'    \item **UI Function**: Add `mod_file_picker_ui("file_picker_1")` to the
#'     UI part of your Shiny app where you want the action button to appear.
#'    \item **Server Function**: Add `mod_file_picker_server("file_picker_1", files_df)`
#'     to the server part of your Shiny app. Make sure to replace `files_df`
#'     with your actual data frame containing file information.
#'  }
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param button_icon An optional icon to appear on the button. Defaults to `icon('circle-plus')`.
#' @param button_width The width of the button. Defaults to `100\%`.
# nolint end
#'
#' @seealso \code{\link{mod_file_picker_server}} for the corresponding server
#'  part of the module.
#'
#' @example inst/demos/file_pickers_demo_app.R
#'
#' @export
mod_file_picker_ui <- function(id,
                               button_icon = icon("circle-plus"),
                               button_width = "100%") {
  ns <- NS(id)
  actionButton(ns("select_file"),
    label = "Add files",
    icon = button_icon,
    width = button_width
  )
}

#' file_picker Server Functions
#'
#' @description
#'  Server function of a Shiny module for selecting either a single
#'  file or multiple files to be used within the app. This module displays a
#'  modal dialog with a preview table of files and allows users to select files
#'  based on the specified `selection` mode ('single' or 'multiple'). Table
#'  columns are filterable, and different types of filters are available
#'  depending on the type of data in each column:
#'  \itemize{
#'    \item For numeric columns: a range slider filter allows users to filter
#'     the table by selecting a range of values.
#'    \item For factor columns: a drop down filter allows users to filter the
#'     table by selecting specific factor levels.
#'     \item For other data types: a basic filter using a case-insensitive text
#'      match is available.
#'   }
#'
#' @details To incorporate this module into your Shiny app, you need to include
#'  both the UI and server functions in the appropriate places in your app
#'  code:
# nolint start
#'  \itemize{
#'    \item **UI Function**: Add `mod_file_picker_ui("file_picker_1")` to the
#'     UI part of your Shiny app where you want the action button to appear.
#'    \item **Server Function**: Add `mod_file_picker_server("file_picker_1", files_df)`
#'     to the server part of your Shiny app. Make sure to replace `files_df`
#'     with your actual data frame containing file information.
#'  }
# nolint end.
#'
#' @param id A unique identifier for the module instance.
#' @param files_df A data frame containing file information. This data frame
#'  should have a column for file paths and any other relevant metadata. You
#'  can use the \code{\link{get_all_project_files}} function to fetch all
#'  project files along with their metadata from the SB File system
#'  within Data Studio.This function returns a data frame containing
#'  comprehensive file information, making it an ideal input for the
#'  `mod_file_picker_server()` function.
#' @param selection A string specifying the selection mode. Can be either
#'  'single' for single file selection or 'multiple' for multiple file
#'   selection. Defaults to 'single'.
#' @param file_identifier_column A string specifying the column name in
#'  `files_df` from which the values of selected files will be returned.
#'   Defaults to `path`.
#' @param default_page_size Number of rows per page to display in the table.
#'  Defaults to 10.
#' @param use_bslib_theme A logical value indicating whether to generate the
#'  modal's UI using the \code{bslib} package. If \code{FALSE} (the default),
#'  the regular UI will be generated. If \code{TRUE}, the UI will be generated
#'  using the \code{bslib} package and its functions. Note that to use this
#'  option, the main UI of the app must include the line
#'  \code{theme = bslib::bs_theme()}. This requirement ensures the correct
#'   application of the \code{bslib} theme throughout the app.
#' @param ... Additional parameters to be passed to the `reactable()` function
#'  this module relies on.
#'
#' @return A reactive expression containing information about the selected
#'  files based on the specified `file_identifier_column`.
#'
#' @importFrom checkmate assert_data_frame assert_choice assert_character
#' @importFrom reactable getReactableState renderReactable reactable colDef
#' @importFrom htmlwidgets JS
#' @importFrom shinyalert shinyalert
#'
#' @seealso \code{\link{mod_file_picker_ui}} for the corresponding server
#'  part of the module.
#'
#' @example inst/demos/file_pickers_demo_app.R
#'
#' @export
mod_file_picker_server <- function(id,
                                   files_df,
                                   selection = "single",
                                   file_identifier_column = "path",
                                   default_page_size = 10,
                                   use_bslib_theme = FALSE,
                                   ...) {
  # Checks function arguments using checkmate
  checkmate::assert_data_frame(files_df, min.cols = 1)
  checkmate::assert_choice(selection, c("single", "multiple"))
  checkmate::assert_character(file_identifier_column)

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    module_output_files <- reactiveVal(NULL)

    observeEvent(input$select_file, {
      if (nrow(files_df) == 0) {
        shinyalert::shinyalert(
          title = "Empty project",
          text = "The project doesn't contain any files. Please add files to your project.", # nolint
          type = "warning"
        )
      } else {
        showModal(
          # Generate modal's UI
          ui = generate_file_picker_modal_ui(
            ns = ns,
            selection_type = selection,
            use_bslib_theme = use_bslib_theme
          )
        )
      }
    })

    selected <- reactive({
      selected_indices <- getReactableState("table", "selected")
      if (length(selected_indices) > 0) {
        selected_data <- files_df[selected_indices, file_identifier_column]
        return(selected_data)
      } else {
        return(NULL)
      }
    })

    # Create a named list of column definitions
    column_defs <- lapply(files_df, create_col_def)
    names(column_defs) <- names(files_df)

    output$table <- reactable::renderReactable({
      reactable::reactable(files_df,
        selection = selection,
        onClick = "select",
        filterable = TRUE,
        searchable = TRUE,
        resizable = TRUE,
        defaultPageSize = default_page_size,
        # Limit the number of displayed characters in a cell to 40
        defaultColDef = reactable::colDef(
          # nolint start
          cell = htmlwidgets::JS("function(cellInfo) {
                    var data = cellInfo.value;
                    return data != null && data.length > 40 ?
                      '<span title=\"' + data + '\">' + data.substr(0, 40) + '...</span>' : data;
                  }"),
          # nolint end
          html = TRUE,
          style = list(whiteSpace = "nowrap"),
          width = 400
        ),
        elementId = "file-picker-list",
        columns = c(
          list(
            .selection = reactable::colDef(
              width = 80,
              sticky = "left",
              style = list(cursor = "pointer"),
              headerStyle = list(cursor = "pointer")
            )
          ),
          # Add column definitions
          column_defs,
          ...
        )
      )
    })

    # Show paths for currently selected files
    output$selected_files <- renderPrint({
      validate(
        need(
          selected(),
          ifelse(selection == "single",
            "Please select a file from the table above",
            "Please select file(s) from the table above"
          )
        )
      )
      cat(selected(), sep = "\n")
    })

    # Add logic that will be triggered by clicking on the Submit button
    observeEvent(input$submit_selection, {
      req(selected())
      module_output_files(selected())
      removeModal()
    })

    # Close modal dialog by clicking the dismiss button
    observeEvent(input$dismiss, {
      removeModal()
    })

    return(module_output_files)
  })
}

## To be copied in the UI
# mod_file_picker_ui("file_picker_1")

## To be copied in the server
# mod_file_picker_server("file_picker_1")
