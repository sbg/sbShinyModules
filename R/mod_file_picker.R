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
# nolint end.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
#'
#' @seealso \code{\link{mod_file_picker_server}} for the corresponding server
#'  part of the module.
#'
#' @example inst/examples/file_pickers_demo_app.R
#'
#' @export
mod_file_picker_ui <- function(id) {
  ns <- NS(id)
  actionButton(ns("select_file"),
    label = "Add files"
  )
}

#' file_picker Server Functions
#'
#' @description Server function of a Shiny module for selecting either a single
#'  file or multiple files to be used within the app. This module displays a
#'  modal dialog with a preview table of files and allows users to select files
#'  based on the specified `selection` mode ('single' or 'multiple').
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
#'  should have a column for file paths and any other relevant metadata.
#' @param selection A string specifying the selection mode. Can be either
#'  'single' for single file selection or 'multiple' for multiple file
#'   selection. Defaults to 'single'.
#' @param file_identifier_column A string specifying the column name in
#'  `files_df` from which the values of selected files will be returned.
#'   Defaults to path'.
#' @param ... Additional parameters to be passed to the reactable function.
#'
#' @return A reactive expression containing information about the selected
#'  files based on the specified `file_identifier_column`.
#'
#' @importFrom checkmate assert_data_frame assert_choice assert_character
#' @importFrom shinyWidgets useShinydashboard
#' @importFrom shinydashboard box
#' @importFrom reactable.extras reactable_extras_dependency
#' @importFrom reactable reactableOutput getReactableState renderReactable
#'  reactable colDef
#' @importFrom bslib input_task_button
#'
#' @seealso \code{\link{mod_file_picker_ui}} for the corresponding server
#'  part of the module.
#'
#' @seealso \code{\link{get_all_project_files}} function that helps you to
#'  fetch all project files along with their metadata from the SB File system
#'  within Data Studio.This function returns a data frame containing
#'  comprehensive file information, making it an ideal input for the
#'  `mod_file_picker_server` function.
#'
#' @example inst/examples/file_pickers_demo_app.R
#'
#' @export
mod_file_picker_server <- function(id,
                                   files_df,
                                   selection = "single",
                                   file_identifier_column = "path",
                                   ...) {
  # Checks function arguments using checkmate
  checkmate::assert_data_frame(files_df, min.cols = 1)
  checkmate::assert_choice(selection, c("single", "multiple"))
  checkmate::assert_character(file_identifier_column)

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selected_files <- reactiveVal(NULL)

    observeEvent(input$select_file, {
      showModal(
        # nolint start
        modalDialog(
          title = "File Selection",
          size = "l",
          shinyWidgets::useShinydashboard(),
          fluidRow(
            div(
              shinydashboard::box(
                title = "File Selection Guides",
                width = 12,
                collapsible = TRUE,
                collapsed = TRUE,
                tags$div(
                  tags$p("To efficiently manage files, follow these steps:"),
                  if (selection == "single") {
                    tags$ol(
                      tags$li("Review file details."),
                      tags$li(HTML("To select a file, click the radio button in the leftmost column of the desired file's row.")),
                      tags$li(HTML("Use the search bar above the table to quickly locate a file by its name or metadata.")),
                      tags$li(HTML("Utilize the filter options to narrow down the file list based on specific criteria.")),
                      tags$li(HTML("Click on column headers to sort files in ascending or descending order.")),
                      tags$li(HTML("Use pagination controls at the bottom of the table to navigate through multiple pages of files.")),
                      tags$li(HTML("Once a file is selected, click <b>Submit</b> located below the table."))
                    )
                  } else if (selection == "multiple") {
                    tags$ol(
                      tags$li("Review file details."),
                      tags$li(HTML("Use the checkboxes in each row to select multiple files simultaneously.")),
                      tags$li(HTML("Use the search bar above the table to quickly locate files by their name or metadata.")),
                      tags$li(HTML("Utilize the filter options to narrow down the file list based on specific criteria.")),
                      tags$li(HTML("Click on column headers to sort files in ascending or descending order.")),
                      tags$li(HTML("Use pagination controls at the bottom of the table to navigate through multiple pages of files.")),
                      tags$li(HTML("Once files are selected, click <b>Submit</b> located below the table."))
                    )
                  }
                )
              )
            )
          ),
          # nolint end
          fluidRow(
            div(
              class = "file-selection-table",
              shinydashboard::box(
                title = "Files Table Preview",
                width = 12,
                collapsible = FALSE,
                tags$div(
                  id = ns("file_picker_selection"),
                  reactable.extras::reactable_extras_dependency(),
                  reactable::reactableOutput(ns("table"))
                ),
                br(),
                h5("Selected file"),
                verbatimTextOutput(ns("selected_files"),
                  placeholder = TRUE
                ),
                br()
              )
            )
          ),
          footer = tagList(
            actionButton(ns("dismiss"),
              label = "Dismiss",
              icon = icon("xmark")
            ),
            bslib::input_task_button(
              id = ns("submit_selection"),
              label = "Submit",
              icon = icon("check"),
              type = "default"
            )
          )
        )
      )
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

    output$table <- reactable::renderReactable({
      reactable::reactable(files_df,
        selection = selection,
        onClick = "select",
        filterable = TRUE,
        searchable = TRUE,
        defaultPageSize = 5,
        columns = list(
          .selection = reactable::colDef(
            width = 80,
            sticky = "left",
            style = list(cursor = "pointer"),
            headerStyle = list(cursor = "pointer")
          )
        ),
        ...
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
      selected_files(selected())
      removeModal()
    })

    # Close modal dialog by clicking the dismiss button
    observeEvent(input$dismiss, {
      removeModal()
    })

    return(selected_files)
  })
}

## To be copied in the UI
# mod_file_picker_ui("file_picker_1")

## To be copied in the server
# mod_file_picker_server("file_picker_1")
