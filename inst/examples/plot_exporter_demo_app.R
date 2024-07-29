library(shiny)
library(magrittr)
library(sbShinyModules)

# App's  UI
ui <- fluidPage(
  titlePanel("Plot Exporter Module Example"),
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
        sbShinyModules::mod_save_plot_to_export_ui("plot_exporter")
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
    plot_rv = helper_reactive,
    sbg_directory_path = "tests/testthat/sbgenomics_test/"
  )
}

# Run the Shiny app
shinyApp(ui, server)
