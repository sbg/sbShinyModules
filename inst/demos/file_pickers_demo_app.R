library(shiny)
library(sbShinyModules)

# App's  UI
ui <- fluidPage(
  titlePanel("File Picker Module Examples"),
  sidebarLayout(
    sidebarPanel(
      # Single File Picker - UI
      fluidRow(
        h3("Single File Picker"),
        br(),
        sbShinyModules::mod_file_picker_ui("single_file_picker"),
        br(),
        h5("Selected File"),
        verbatimTextOutput("single_file_picker_selection")
      ),
      hr(),
      # Multiple Files Picker - UI
      fluidRow(
        h3("Multiple Files Picker"),
        br(),
        sbShinyModules::mod_file_picker_ui("multiple_files_picker"),
        br(),
        h5("Selected Files"),
        verbatimTextOutput("mult_files_picker_selection")
      )
    ),
    mainPanel(
      # Placeholder
    )
  )
)

# App's Server Logic
server <- function(input, output, session) {
  # ----------------------------- Load Files ----------------------------------
  # Load a built-in data frame for files
  files_df <- sbShinyModules::file_picker_example_data

  # Or fetch files from a provided directory (path) using the
  # get_all_project_files() utility function

  # files_df <- sbShinyModules::get_all_project_files(
  #   path = "/sbgenomics/project-files"
  # )

  # ---------------------------------------------------------------------------

  ## -------------------- Single File Picker - Server Code --------------------

  # Call the file picker module
  selected_files_single_picker <- sbShinyModules::mod_file_picker_server(
    id = "single_file_picker",
    files_df = files_df,
    selection = "single"
  )

  # Display selected files
  output$single_file_picker_selection <- renderPrint({
    validate(
      need(
        selected_files_single_picker(),
        "No file has been selected."
      )
    )
    cat(selected_files_single_picker(), sep = "\n")
  })
  # ---------------------------------------------------------------------------



  ## ----------------- Multiple Files Picker - Server Code --------------------

  # Call the file picker module
  selected_files_mult_picker <- sbShinyModules::mod_file_picker_server(
    id = "multiple_files_picker",
    files_df = files_df,
    selection = "multiple"
  )

  # Display selected files
  output$mult_files_picker_selection <- renderPrint({
    validate(
      need(
        selected_files_mult_picker(),
        "No files have been selected."
      )
    )
    cat(selected_files_mult_picker(), sep = "\n")
  })
  # ---------------------------------------------------------------------------
}

# Run the Shiny app
shinyApp(ui, server)
