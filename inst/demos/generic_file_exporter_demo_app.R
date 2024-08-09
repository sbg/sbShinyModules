library(shiny)
library(magrittr)
library(sbShinyModules)
library(reactable)

# App's  UI
ui <- fluidPage(
  titlePanel("Save data for export to Platform - Module Demo"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        h3("Export File to Platform"),
        br(),
        textInput("file_name", label = "Set file name", width = "100%"),
        fluidRow(
          column(
            width = 6,
            selectInput("separator",
              label = "Set separator",
              choices = c("comma", "tab", "new line", "semicolon")
            ) # nolint
          ),
          column(
            width = 6,
            selectInput("extension",
              label = "Set extension",
              choices = c("txt", "csv")
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            radioButtons("overwrite",
              label = "Overwrite existing file?",
              choiceNames = c("yes", "no"),
              choiceValues = c("TRUE", "FALSE")
            )
          )
        ),
        fluidRow(
          column = 4,
          sbShinyModules::mod_save_file_generic_ui(
            id = "file_exporter"
          )
        )
      )
    ),
    mainPanel(
      reactable::reactableOutput("table_data")
    )
  )
)

# App's Server Logic
server <- function(input, output, session) {
  output$table_data <- reactable::renderReactable({
    reactable::reactable(iris,
      onClick = "select",
      filterable = TRUE,
      searchable = TRUE,
      resizable = TRUE,
      defaultPageSize = 10
    )
  })

  helper_rv <- reactiveValues(
    FUN = write.table,
    args = list(
      x = iris, quote = FALSE, row.names = FALSE,
      col.names = TRUE
    ),
    filename = NULL,
    extension = NULL,
    overwrite = FALSE
  )

  observe(
    helper_rv$filename <- as.character(input$file_name)
  )
  observe(
    helper_rv$args[["sep"]] <- switch(input$separator,
      "comma" = ",",
      "tab" = "\t",
      "new line" = "\n",
      "semicolon" = ";"
    )
  )
  observe(
    helper_rv$extension <- as.character(input$extension)
  )
  observe(
    helper_rv$overwrite <- as.logical(input$overwrite)
  )

  # Call the plot exporter module
  sbShinyModules::mod_save_file_generic_server(
    id = "file_exporter",
    reac_vals = helper_rv,
    sbg_directory_path = system.file(
      "tests", "testthat", "sbgenomics_test",
      package = "sbShinyModules"
    )
  )
}

# Run the Shiny app
shinyApp(ui, server)
