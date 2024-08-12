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
        h3("Export txt/csv to Platform"),
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
          column(
            width = 4,
            sbShinyModules::mod_save_file_generic_ui(
              id = "file_exporter",
              save_button_title = "Save txt/csv"
            )
          )
        )
      ),
      fluidRow(
        h3("Export json to Platform"),
        br(),
        textInput("file_name_json", label = "Set file name", width = "100%"),
        fluidRow(
          column(
            width = 4,
            sbShinyModules::mod_save_file_generic_ui(
              id = "json_file_exporter",
              save_button_title = "Save json"
            )
          )
        )
      ),
      fluidRow(
        h3("Export RDS object to Platform"),
        br(),
        textInput("file_name_rds", label = "Set file name", width = "100%"),
        fluidRow(
          column(
            width = 4,
            sbShinyModules::mod_save_file_generic_ui(
              id = "rds_file_exporter",
              save_button_title = "Save RDS"
            )
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

  helper_rv_table <- reactiveValues(
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
    helper_rv_table$filename <- as.character(input$file_name)
  )
  observe(
    helper_rv_table$args[["sep"]] <- switch(input$separator,
      "comma" = ",",
      "tab" = "\t",
      "new line" = "\n",
      "semicolon" = ";"
    )
  )
  observe(
    helper_rv_table$extension <- as.character(input$extension)
  )
  observe(
    helper_rv_table$overwrite <- as.logical(input$overwrite)
  )

  # Call the file exporter module
  sbShinyModules::mod_save_file_generic_server(
    id = "file_exporter",
    reac_vals = helper_rv_table,
    sbg_directory_path = system.file(
      "tests", "testthat", "sbgenomics_test",
      package = "sbShinyModules"
    )
  )

  helper_rv_json <- reactiveValues(
    FUN = write,
    args = list(
      x = jsonlite::toJSON(x = iris, dataframe = "rows", pretty = TRUE),
      file = "myjson.json"
    ),
    filename = NULL,
    extension = "json",
    overwrite = TRUE
  )

  observe(
    helper_rv_json$filename <- as.character(input$file_name_json)
  )

  # Call the file exporter module
  sbShinyModules::mod_save_file_generic_server(
    id = "json_file_exporter",
    reac_vals = helper_rv_json,
    sbg_directory_path = system.file(
      "tests", "testthat", "sbgenomics_test",
      package = "sbShinyModules"
    )
  )

  helper_rv_rds <- reactiveValues(
    FUN = saveRDS,
    args = list(
      object = iris
    ),
    filename = NULL,
    extension = "RDS",
    overwrite = TRUE
  )

  observe(
    helper_rv_rds$filename <- as.character(input$file_name_rds)
  )

  # Call the file exporter module
  sbShinyModules::mod_save_file_generic_server(
    id = "rds_file_exporter",
    reac_vals = helper_rv_rds,
    sbg_directory_path = system.file(
      "tests", "testthat", "sbgenomics_test",
      package = "sbShinyModules"
    )
  )
}

# Run the Shiny app
shinyApp(ui, server)
