library(shiny)
library(magrittr)
# library(bslib)
library(sbShinyModules)

# App's  UI
ui <- fluidPage(
  titlePanel("Sprint Review 06/08/2024 - File picker and saving plot modules"),
  br(),
  # theme = bslib::bs_theme(), # uncomment if you want to use a Bootstrap theme
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        h3("Single File Picker"),
        br(),
        sbShinyModules::mod_file_picker_ui(id = "single_file_picker"),
        br(),
        h5("Selected File"),
        verbatimTextOutput(outputId = "single_file_picker_selection")
      ),
      hr(),
      # Multiple Files Picker - UI
      fluidRow(
        h3("Multiple Files Picker"),
        br(),
        sbShinyModules::mod_file_picker_ui(id = "multiple_files_picker"),
        br(),
        h5("Selected Files"),
        verbatimTextOutput(outputId = "mult_files_picker_selection")
      ),
      sliderInput("bins",
        "Number of bins:",
        min = 1,
        max = 50,
        value = 30
      )
    ),
    mainPanel(
      plotOutput("distPlot"),
      fluidRow(
        h3("Export Plot to Platform"),
        br(),
        column(
          width = 3,
          sbShinyModules::mod_save_plot_to_export_ui(id = "plot_exporter")
        )
      )
    )
  )
)

# App's Server Logic
server <- function(input, output, session) {
  # Create reactive values list with plot field
  helper_reactive <- reactiveValues(
    plot = NULL
  )

  input_files_df <- readRDS("inst/demos/files_df.RDS")

  # Call the file picker module
  selected_files_single_picker <- sbShinyModules::mod_file_picker_server(
    id = "single_file_picker",
    files_df = input_files_df,
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
    files_df = input_files_df,
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

  plot_output <- eventReactive(input$bins, {
    bins <- input$bins + 1

    # Draw the histogram with the specified number of bins
    faithful[, 2] %>%
      hist(
        breaks = seq(min(.),
          max(.),
          length.out = bins
        ),
        col = "darkgray",
        border = "white",
        main = "Geyser eruption duration"
      )
    helper_reactive$plot <- recordPlot()
  })

  output$distPlot <- renderPlot({
    plot_output()
  })

  # Call the plot exporter module
  sbShinyModules::mod_save_plot_to_export_server(
    id = "plot_exporter",
    plot_reactVals = helper_reactive,
    module_title = "Save plot to Platform",
    sbg_directory_path = system.file(
      "tests", "testthat", "sbgenomics_test",
      package = "sbShinyModules"
    ),
    btns_div_width = 12
  )
}

get_input_files <- function() {
  files_df <- sbShinyModules::get_all_project_files(
    path = "/sbgenomics/project-files"
  )

  files_df$extension <- sapply(files_df$name, extract_extension)
  files_df$extension <- as.factor(files_df$extension)
  files_df$size <- as.numeric(
    format(files_df$size / (1024^2), scientific = FALSE)
  )
  files_df$unit <- "Mb"

  colums_to_display <- c(
    "name", "path", "extension", "size", "unit",
    "reference_genome", "species", "md5_sum", "sample_id",
    "case_id", "investigation", "platform", "normal_id",
    "Quality scale", "experimental_strategy",
    "sample_type", "paired_end"
  )
  files_df <- files_df %>% dplyr::select(all_of(colums_to_display))
  return(files_df)
}

extract_extension <- function(file_name) {
  # Check if the file name ends with ".gz"
  if (endsWith(file_name, ".gz")) {
    # Extract the extension as .gz and whatever comes right before it
    extension <- sub(".+\\.(.+)\\.gz$", "\\1.gz", file_name)
  } else {
    # Extract regular extension (whatever comes after the last dot in the
    # file name)
    extension <- tools::file_ext(basename(file_name))
  }

  return(extension)
}


# Run the Shiny app
shinyApp(ui, server)
