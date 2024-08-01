library(shiny)
library(magrittr)
library(sbShinyModules)

# App's  UI
ui <- fluidPage(
  titlePanel("Save plots for export to Platform - Module Demo"),
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
        h3("Export Plot to Platform"),
        br(),
        sbShinyModules::mod_save_plot_to_export_ui(
          id = "plot_exporter"
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

# Run the Shiny app
shinyApp(ui, server)
