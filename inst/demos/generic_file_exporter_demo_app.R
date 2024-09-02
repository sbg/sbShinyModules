library(shiny)
library(magrittr)
library(sbShinyModules)
library(reactable)
library(jsonlite)


###### Test modules for demonstrating nested modules ######
test_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(
      inputId = ns("test_module_btn"),
      label = "Test nesting modules",
      width = "100%"
    )
  )
}

test_module_server <- function(id) {
  moduleServer(id, module = function(input, output, session) {
    ns <- session$ns

    observeEvent(input$test_module_btn, {
      showModal(
        ui = modalDialog(
          title = "Test nested modules",
          size = "l",
          tagList(
            reactable::reactableOutput(ns("nested_table_data")),
            fluidRow(
              column(
                width = 6,
                sbShinyModules::mod_save_file_generic_ui(ns("save_file_nested"))
              )
            )
          ),
          footer = tagList(
            actionButton(ns("dismiss"),
              label = "Dismiss",
              icon = icon("xmark")
            )
          )
        )
      )
    })

    output$nested_table_data <- reactable::renderReactable({
      reactable::reactable(iris,
        onClick = "select",
        filterable = TRUE,
        searchable = TRUE,
        resizable = TRUE,
        defaultPageSize = 10
      )
    })

    # Close the modal dialog by clicking the dismiss button
    observeEvent(input$dismiss, {
      removeModal()
    })

    # Create the reactive values list with mandatory fields to pass to the
    # module
    helper_rv_nested <- reactiveValues(
      FUN = write.table,
      args = list(x = iris, quote = FALSE, row.names = FALSE, col.names = TRUE),
      filename = "generic_file_name",
      extension = ".txt",
      overwrite = TRUE
    )
    # Call the file exporter module
    sbShinyModules::mod_save_file_generic_server(
      id = "save_file_nested",
      reac_vals = helper_rv_nested,
      sbg_directory_path = system.file(
        "tests", "testthat", "sbgenomics_test",
        package = "sbShinyModules"
      )
    )
  })
}
############## Test modules end ###########################

##### Demo App  UI #######
ui <- fluidPage(
  titlePanel("Save data for the export to the Platform - Module Demo"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        h3("Export txt/csv to the Platform"),
        br(),
        textInput("file_name", label = "Set file name", width = "100%"),
        fluidRow(
          column(
            width = 6,
            selectInput("separator",
              label = "Set separator",
              choices = c("comma", "tab", "new line", "semicolon")
            )
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
            width = 12,
            sbShinyModules::mod_save_file_generic_ui(
              id = "file_exporter",
              save_button_title = "Save txt/csv"
            )
          )
        )
      ),
      fluidRow(
        h3("Export json to the Platform"),
        br(),
        textInput("file_name_json", label = "Set file name", width = "100%"),
        fluidRow(
          column(
            width = 12,
            sbShinyModules::mod_save_file_generic_ui(
              id = "json_file_exporter",
              save_button_title = "Save json"
            )
          )
        )
      ),
      fluidRow(
        h3("Export RDS object to the Platform"),
        br(),
        textInput("file_name_rds", label = "Set file name", width = "100%"),
        fluidRow(
          column(
            width = 12,
            sbShinyModules::mod_save_file_generic_ui(
              id = "rds_file_exporter",
              save_button_title = "Save RDS"
            )
          )
        )
      ),
      fluidRow(
        h3("Test nested modules"),
        br(),
        fluidRow(
          column(
            width = 12,
            test_module_ui(id = "nested_modules")
          )
        )
      )
    ),
    mainPanel(
      reactable::reactableOutput("table_data")
    )
  )
)

###### Demo App Server Logic ######
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

  # Create the reactive values list with mandatory fields to pass to the module
  helper_rv_table <- reactiveValues(
    FUN = write.table,
    args = list(x = iris, quote = FALSE, row.names = FALSE, col.names = TRUE),
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
  # Create reactive values list for the json file export
  helper_rv_json <- reactiveValues(
    FUN = write,
    args = list(
      x = jsonlite::toJSON(x = iris, dataframe = "rows", pretty = TRUE)
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
  # Create reactive values list for the RDS file export
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

  # Call the test module
  test_module_server(id = "nested_modules")
}

###### Run the Shiny app #######
shinyApp(ui, server)
