library(shiny)
library(sbShinyModules)
# library(bslib) # uncomment if you want to use a Bootstrap theme

# App's  UI
ui <- fluidPage(
  titlePanel("File Picker Module Examples"),
  # theme = bslib::bs_theme(), # uncomment if you want to use a Bootstrap theme
  sidebarLayout(
    sidebarPanel(
      # Single File Picker - UI
      fluidRow(
        h3("Single File Picker"),
        br(),
        sbShinyModules::mod_file_picker_ui("single_file_picker"),
        br(),
        h5("Selected File"),
        verbatimTextOutput("single_file_picker_selection", placeholder = TRUE)
      ),
      hr(),
      # Multiple Files Picker - UI
      fluidRow(
        h3("Multiple Files Picker"),
        br(),
        sbShinyModules::mod_file_picker_ui("multiple_files_picker"),
        br(),
        h5("Selected Files"),
        verbatimTextOutput("mult_files_picker_selection", placeholder = TRUE)
      )
    ),
    mainPanel(
      # Placeholder
    )
  )
)

# App Server Logic
server <- function(input, output, session) {
  # ----------------------------- Load Files ----------------------------------
  # Load a built-in data frame for files
  files_df <- sbShinyModules::file_picker_example_data

  # Remove units (bytes) from the size column and make it numeric so that it has
  # a range filter
  files_df$size <- as.numeric(gsub(" bytes", "", files_df$size))

  # Alternatively, fetch files from a provided directory (path) using the
  # get_all_project_files() utility function. Note that this requires the
  # xattrs package, which is not available for Windows systems. Therefore,
  # this approach will only work on Unix-based systems.

  # files_df <- sbShinyModules::get_all_project_files(
  #   path = "/sbgenomics/project-files"
  # )

  # ---------------------------------------------------------------------------

  ## -------------------- Single File Picker - Server Code --------------------

  # Call the file picker module
  selected_files_single_picker <- sbShinyModules::mod_file_picker_server(
    id = "single_file_picker",
    files_df = files_df,
    # use_bslib_theme = TRUE, # uncomment if you want to use a Bootstrap theme
    selection = "single",
    default_page_size = 5
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
    # use_bslib_theme = TRUE, # uncomment if you want to use a Bootstrap theme
    selection = "multiple",
    default_page_size = 5
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

# Note: To use a Bootstrap theme, ensure you have the 'bslib' package
# installed and loaded in your app.
# 1. Uncomment `library(bslib)` at the beginning of the script.
# 2. Uncomment the `theme = bslib::bs_theme()` line in the UI section.
# 3. In the server logic, set `use_bslib_theme = TRUE` in the
# `mod_file_picker_server()` function calls.

# Run the Shiny app
shinyApp(ui, server)
