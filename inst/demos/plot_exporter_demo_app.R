library(shiny)
library(magrittr)
library(sbShinyModules)

# App's  UI
ui <- fluidPage(
  titlePanel("Save plots for the export to the Platform - Module Demo"),
  sidebarLayout(
    sidebarPanel(
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
        h3("Export Plot to the Platform"),
        br(),
        sbShinyModules::mod_save_plot_to_export_ui(
          id = "plot_exporter"
        )
      )
    )
  )
)

# App Server Logic
server <- function(input, output, session) {
  # Create reactive values list with the plot field
  helper_reactive <- reactiveValues(
    plot = NULL
  )

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
    sbg_directory_path = system.file("demos/sbgenomics_test",
      package = "sbShinyModules"
    ),
    btns_div_width = 12
  )

  # Note: This way the plot will be saved in a specific directory within the
  # package installation path. To find this default location, you can use the
  # following R command to determine the installation path of the
  # sbShinyModules package:
  #
  # find.package("sbShinyModules")
  #
  # Within this directory, exported files will be placed under:
  #
  # demos/sbgenomics_test/output-files
  #
  # You can specify your own destination directory for exported files by
  # setting the sbg_directory_path parameter in the
  # mod_save_plot_to_export_server() function. Ensure that the custom directory
  # you choose follows the required organizational structure for proper
  # functionality. Refer to the sbg_directory_path parameter description in the
  # documentation for detailed requirements.
}

# Run the Shiny app
shinyApp(ui, server)
