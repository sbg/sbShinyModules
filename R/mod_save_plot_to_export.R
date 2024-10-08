#' `save_plot_to_export` module UI function
#'
#' @description UI function of the module that is saving plots for the export
#'  to the Seven Bridges Platform. The function adds an action button that
#'  triggers a plot export modal. This module allows users to adjust the plot
#'  they wish to save to the Seven Bridges Platform, by resizing it and then
#'  choosing the file format for the export: png, pdf, svg, jpeg, bmp, or tiff.
#'  The UI function should be placed in the UI part of the Shiny app
#'  where the file export button is required.
#'  Please, have in mind that this module only saves the plot, while the actual
#'  export of the saved plots occurs when the users stop the app.
#'
#' @param output_formats Output formats offered to the user. It can be a subset
#'  of: "png", "pdf", "svg", "jpeg", "bmp", or "tiff".
#' @param ns Namespace function for handling the input IDs.
#' @param btns_div_width Div width containing the block with buttons used for
#'  saving plots. Suitable for adjusting the width when only one or two buttons
#'  are enabled. The default value is 12.
#'
#' @return No value. Use in UI & server of shiny application
#'
#' @importFrom shinyFeedback useShinyFeedback
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets prettySwitch
#' @importFrom htmltools css
#'
#' @noRd
save_plot_modalDialog_ui <- function(output_formats,
                                     ns,
                                     btns_div_width = 12) {
  tagList(
    html_dependency_moveable(),
    tags$div(plotOutput(ns("plot"))),
    fluidRow(
      column(
        width = 4,
        numericInput(
          inputId = ns("width"),
          label = "Width:",
          value = 868,
          width = "100%"
        )
      ),
      column(
        width = 4,
        numericInput(
          inputId = ns("height"),
          label = "Height:",
          value = 400,
          width = "100%"
        )
      ),
      column(
        width = 4,
        actionButton(
          class = "update_preview_button",
          inputId = ns("update_preview"),
          label = "Update preview",
          icon = icon("eye"),
          width = "100%",
          style = "margin-top: 25px;"
        )
      )
    ),
    fluidRow(
      shinyFeedback::useShinyFeedback(),
      column(
        width = 4,
        textInput(
          inputId = ns("filename"),
          label = "Filename:",
          value = "export-plot",
          placeholder = "Filename",
          width = "100%"
        )
      ),
      column(
        width = 4,
        style = "margin-top: 35px;",
        shinyWidgets::prettySwitch(
          inputId = ns("overwrite_switch"),
          label = "Overwrite",
          status = "primary",
          fill = TRUE
        )
      )
    ),
    hr(),
    tags$label("Export file format"),
    tags$div(
      fluidRow(
        column(
          width = btns_div_width,
          style = htmltools::css(
            display = "grid",
            gridTemplateColumns = sprintf(
              "repeat(%s, 1fr)",
              length(output_formats)
            ),
            gridColumnGap = "10px"
          ),
          lapply(
            X = output_formats,
            FUN = function(x) {
              actionButton(
                class = "save_plot_to_project_btns",
                inputId = ns(x),
                label = toupper(x),
                icon = icon("file-export"),
                style = "width: 100%;"
              )
            }
          )
        )
      )
    ),
    tags$div(
      style = "display: none;",
      textInput(
        inputId = ns("hidden"),
        label = NULL,
        value = genId()
      )
    )
  )
}

#' Save `plot` for the export module UI function
#'
#' @description The UI part of the module that can be used to save the `plot`
#'  for the export to the Seven Bridges Platform. The UI contains a button that
#'  triggers a modal dialog with further settings.
#'
#' @param id Module ID.
#' @param save_button_title Button label.
#'
#' @return No value. Use in UI & server of shiny application.
#'
#' @importFrom htmltools tagList tags
#'
#' @example inst/demos/plot_exporter_demo_app.R
#'
#' @export
mod_save_plot_to_export_ui <- function(id, save_button_title = "Save plot") {
  ns <- NS(id)

  tagList(
    actionButton(
      inputId = ns("save_button"),
      label = save_button_title,
      icon = icon("file-export")
    )
  )
}

#' Save `plot` for the export module server function
#'
#' @description The server side of the shiny module that allows users to save
#'  plots generated within the Shiny app and export them to the project on the
#'  Seven Bridges Platform.
#'
#' @param id Module's ID.
#' @param plot_reactVals A `reactiveValues` variable with a slot `plot`
#'  containing an object created with the `recordPlot()` function you would
#'  like to save. Please, check out our example app in the inst/demos folder -
#'  `plot_exporter_demo_app.R` for a demo on how this module is used. \cr
#'  Example:
#'  ```{r}
#'  helper_reactive <- reactiveValues(
#'   plot = NULL
#'  )
#'
#'  # Some observer or eventReactive where you create your plot
#'  # and then recordPlot() at the end:
#'  plot_output <- eventReactive(input$bins, {
#'  bins <- input$bins + 1
#'
#'  # Draw the histogram with the specified number of bins
#'  faithful[, 2] %>%
#'    hist(
#'      breaks = seq(min(.),
#'                   max(.),
#'                   length.out = bins
#'      ),
#'      col = "darkgray",
#'      border = "white",
#'      main = "Geyser eruption duration"
#'    )
#'  helper_reactive$plot <- recordPlot()
#'  })
#'
#'  output$distPlot <- renderPlot({
#'    plot_output()
#'  })
#'  ```
#' @param output_formats The supported output formats. It can be a subset of:
#'  "png", "pdf", "svg", "jpeg", "bmp", or "tiff".
#' @param module_title Title (top left corner) of a modal (popup window) with
#'  the settings.
#' @param sbg_directory_path Path to the mounted `sbgenomics` directory
#'  containing `project-files`, `output-files` and `workspace` sub-directories
#'  on the instance.
#'  These directories are expected to exist on the instance where the app will
#'  run. For the purpose of testing your app locally, you can create a mock
#'  directory `sbgenomics` with the same structure - containing sub-directories
#'  `project-files`, `output-files`, `workspace`, and populate it with test
#'  files mimicking the project file structure on the Platform.
#' @param btns_div_width Width of the `div()` containing the block with the
#'  buttons for saving plots in the pop-up modal dialogue. Suitable to update
#'  when having one or two buttons out of the supported set. The default value
#'  is 12.
#'
#' @return No value. Use in the UI & server of shiny application.
#'
#' @importFrom shinyFeedback hideFeedback
#' @importFrom checkmate assert_true assert_subset assert_character
#' @importFrom checkmate assert_logical
#' @importFrom shinyalert shinyalert
#'
#' @example inst/demos/plot_exporter_demo_app.R
#'
#' @export
mod_save_plot_to_export_server <- function(id,
                                           plot_reactVals,
                                           output_formats = get_golem_config("PLOT_EXPORT_SUPPORTED_EXT"), # nolint
                                           module_title = "Save plot for export", # nolint
                                           sbg_directory_path = "/sbgenomics",
                                           btns_div_width = 12) {
  checkmate::assert_true(isTruthy(plot_reactVals))
  checkmate::assert_subset(
    x = output_formats,
    choices = get_golem_config("PLOT_EXPORT_SUPPORTED_EXT"),
    empty.ok = FALSE
  )
  checkmate::assert_character(module_title, null.ok = TRUE)
  if (endsWith(x = sbg_directory_path, suffix = "/")) {
    sbg_directory_path <- substr(
      sbg_directory_path, 1,
      nchar(sbg_directory_path) - 1
    )
  }
  check_sbg_directory_path(sbg_directory_path)

  moduleServer(id, module = function(input, output, session) {
    ns <- session$ns

    observeEvent(input$save_button, {
      showModal(modalDialog(
        # Overwrite --bs-modal-width that is setting modal width in case the bslib theme is used. # nolint
        tags$head(
          tags$style(HTML(paste0(".modal-lg { --bs-modal-width: auto !important;}"))) # nolint
        ),
        title = module_title,
        footer = modalButton("Close"),
        easyClose = TRUE,
        size = "l",
        fade = FALSE,
        shinyjs::useShinyjs(),
        save_plot_modalDialog_ui(
          output_formats = output_formats,
          ns = ns,
          btns_div_width = btns_div_width
        ),
        tags$div(
          style = "display: none;",
          checkboxInput(inputId = ns("modal"), label = NULL, value = TRUE)
        )
      ))
    })


    plot_width <- paste0(
      "output_",
      ns("plot"),
      "_width"
    )
    plot_height <- paste0(
      "output_",
      ns("plot"),
      "_height"
    )

    observeEvent(input$hidden, {
      activate_resizer(
        id = ns("plot"),
        modal = isTRUE(input$modal)
      )
    })

    observeEvent(input$update_preview, {
      if (isTruthy(input$width) & isTruthy(input$height)) {
        resize(
          id = ns("plot"),
          width = input$width,
          height = input$height
        )
      }
    })
    observeEvent(session$clientData[[plot_width]], {
      updateNumericInput(
        session = session,
        inputId = "width",
        value = session$clientData[[plot_width]]
      )
    })
    observeEvent(session$clientData[[plot_height]], {
      updateNumericInput(
        session = session,
        inputId = "height",
        value = session$clientData[[plot_height]]
      )
    })

    output$plot <- renderPlot({
      req(plot_reactVals$plot)
      plot_reactVals$plot
    })

    # Hide feedback on the file name input
    observeEvent(
      c(
        input$save_button,
        input$filename,
        input$overwrite_switch
      ),
      {
        shinyFeedback::hideFeedback("filename")
      },
      ignoreInit = TRUE
    )

    observeEvent(input$png, {
      shinyFeedback::hideFeedback("filename")
      handle_plot_export(input,
        filename = input$filename,
        device = "png",
        width = input$width,
        height = input$height,
        plot_rv = plot_reactVals,
        sbg_directory_path = sbg_directory_path
      )
    })

    observeEvent(input$pdf, {
      shinyFeedback::hideFeedback("filename")
      handle_plot_export(input,
        filename = input$filename,
        device = "pdf",
        width = input$width,
        height = input$height,
        plot_rv = plot_reactVals,
        sbg_directory_path = sbg_directory_path
      )
    })

    observeEvent(input$svg, {
      shinyFeedback::hideFeedback("filename")
      handle_plot_export(input,
        filename = input$filename,
        device = "svg",
        width = input$width,
        height = input$height,
        plot_rv = plot_reactVals,
        sbg_directory_path = sbg_directory_path
      )
    })

    observeEvent(input$jpeg, {
      shinyFeedback::hideFeedback("filename")
      handle_plot_export(input,
        filename = input$filename,
        device = "jpeg",
        width = input$width,
        height = input$height,
        plot_rv = plot_reactVals,
        sbg_directory_path = sbg_directory_path
      )
    })

    observeEvent(input$bmp, {
      shinyFeedback::hideFeedback("filename")
      handle_plot_export(input,
        filename = input$filename,
        device = "bmp",
        width = input$width,
        height = input$height,
        plot_rv = plot_reactVals,
        sbg_directory_path = sbg_directory_path
      )
    })

    observeEvent(input$tiff, {
      shinyFeedback::hideFeedback("filename")
      handle_plot_export(input,
        filename = input$filename,
        device = "tiff",
        width = input$width,
        height = input$height,
        plot_rv = plot_reactVals,
        sbg_directory_path = sbg_directory_path
      )
    })

    return(NULL)
  })
}
