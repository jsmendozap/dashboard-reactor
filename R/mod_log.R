#' Log Module UI
#'
#' @description Creates the UI components for the log module in the dashboard. This module displays event summaries, reactor inflow data, and an experiment overview with interactive plots.
#'
#' @param id A string. The namespace ID for the module, used to create unique input/output IDs.
#'
#' @return A `tabItem` containing the UI elements for the log module.
#'
#' @importFrom shiny NS tagList fluidRow textOutput uiOutput reactive reactiveValues observeEvent req
#' @importFrom bs4Dash tabItem box column valueBoxOutput renderValueBox
#' @importFrom reactable reactableOutput renderReactable getReactableState
#' @importFrom shinyWidgets multiInput
#' @importFrom dygraphs dygraphOutput renderDygraph
#' @importFrom dplyr filter mutate select slice_head pull rowwise ungroup rename n
#' @importFrom htmltools tags
#' @importFrom stringr str_glue
#' @importFrom utils stack
#' @importFrom shinycssloaders withSpinner
#' @noRd

choices <- c(
  "Air flow" = "fi_110",
  "CO2 flow" = "fi_120",
  "Argon/Propane flow" = "fi_130",
  "Nitrogen flow" = "fi_140",
  "Setted pressure" = "p_set",
  "Measured temperature" = "tic_300_pv",
  "Setted temperature" = "tic_300_sp"
)

mod_log_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = 'log',
    fluidRow(
      column(
        width = 8,
        style = "padding-right: 0px",
        box(
          title = "Events summary",
          status = 'info',
          solidHeader = TRUE,
          width = 12,
          withSpinner(
            reactableOutput(ns('log')),
            color = "#607282",
            caption = "Loading data",
            size = 0.5
          ),
          style = 'padding: 10px'
        )
      ),
      column(
        width = 4,
        box(
          title = "Leak test",
          width = 12,
          status = 'info',
          solidHeader = TRUE,
          uiOutput(ns('leak')),
          style = "padding: 10px"
        ),
        box(
          title = "Reactor's inflow",
          status = 'info',
          solidHeader = TRUE,
          width = 12,
          textOutput(ns('valve')),
          br(),
          fluidRow(
            column(
              width = 6,
              valueBoxOutput(ns("fi_110"), width = 12),
              valueBoxOutput(ns("fi_120"), width = 12),
              style = 'padding: 0px'
            ),
            column(
              width = 6,
              valueBoxOutput(ns("fi_130"), width = 12),
              valueBoxOutput(ns("fi_140"), width = 12),
              style = 'padding: 0px'
            )
          ),
          style = 'padding-right: 8px'
        )
      )
    ),
    box(
      status = "info",
      title = "Experiment overview",
      solidHeader = TRUE,
      width = 12,
      fluidRow(
        column(
          width = 4,
          multiInput(
            inputId = ns('var'),
            label = "Variables to plot",
            choices = choices,
            selected = choices[1],
            width = "100%",
            options = list(limit = 2)
          ),
          div(
            id = 'legend',
            style = "margin-left: 5px; margin-bottom: 20px"
          )
        ),
        column(
          width = 8,
          dygraphOutput(ns("dygraph"), height = '45vh')
        )
      )
    )
  )
}

#' Log Module Server
#'
#' @description Defines the server-side logic for the log module in the dashboard. This module handles event summaries, reactor inflow data, and interactive plots.
#'
#' @param id A string. The namespace ID for the module, used to create unique input/output IDs.
#' @param app_state A reactiveValues object containing the application state, including the experimental database (`bd`).
#'
#' @return A reactive object containing user comments for events.
#'
#' @noRd

mod_log_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selected <- reactive({
      input$var
    })

    output$dygraph <- renderDygraph({
      req(app_state$bd())
      renderdy(app_state$bd(), selected(), 'legend')
    })

    output$log <- renderReactable({
      req(app_state$bd())

      app_state$bd() %>%
        filter(name == mode(name), .by = event) %>%
        slice_head(n = 1, by = event) %>%
        select(event, date_time, name, time_duration) %>%
        mutate(obs = "") %>%
        custom_reactable(
          columns = list(
            event = colDef(name = 'Event', width = 80),
            date_time = colDef(name = 'Start time', width = 200),
            name = colDef(name = 'Event name', minWidth = 100),
            time_duration = colDef(name = 'Duration', width = 100),
            obs = colDef(
              name = 'Observations',
              width = 150,
              cell = function(value, index) {
                tags$input(
                  type = "text",
                  class = "obs",
                  id = paste0(id, "-comment", index),
                  value = value,
                  onchange = sprintf(
                    "Shiny.setInputValue('%s', {row: %d, value: this.value})",
                    session$ns("comment"),
                    index
                  )
                )
              }
            )
          ),
          selection = 'single'
        )
    })

    obs <- reactiveValues()

    observeEvent(input$comment, {
      row <- unique(app_state$bd()$event)[input$comment$row] %>% as.character()
      obs[[row]] <- input$comment$value
      app_state$comment <- obs
    })

    output$leak <- renderUI({
      req(app_state$df)

      test <- app_state$df() %>%
        rowwise() %>%
        mutate(sum = sum(fic_110, fic_120, fic_130, fic_140)) %>%
        ungroup() %>%
        filter(n > 5 & p_set != 0 & sum == 0) %>%
        select(event, n, p_set, sum, pt_310) %>%
        filter(event == max(event)) %>%
        slice(c(1, n())) %>%
        {
          c(event = unique(.$event), dif = .$pt_310[1] - .$pt_310[2])
        }

      renderPrint({
        if (!is.na(test[1])) {
          str_glue(
            "Event: {test[1]} \nPressure difference: {round(test[2], 2)}"
          )
        } else {
          cat("No Leak test found in data")
        }
      })
    })

    output$valve <- renderText({
      req(getReactableState('log', 'selected'))

      sel <- getReactableState('log', 'selected')
      pos <- app_state$bd() %>%
        slice_head(n = 1, by = event) %>%
        pull(rswitch_val)

      paste('Valve position:', pos[sel])
    })

    output$fi_110 <- renderValueBox({
      reactor_values(app_state$bd(), 'fi_110', 'Air')
    })

    output$fi_120 <- renderValueBox({
      reactor_values(app_state$bd(), 'fi_120', 'Carbon dioxide')
    })

    output$fi_130 <- renderValueBox({
      reactor_values(app_state$bd(), 'fi_130', 'Argon / Propane')
    })

    output$fi_140 <- renderValueBox({
      reactor_values(app_state$bd(), 'fi_140', 'Nitrogen')
    })

    return(reactive({
      reactiveValuesToList(obs) %>%
        {
          if (length(.) > 0) {
            stack(.) %>%
              rename(comment = values, event = ind) %>%
              mutate(event = as.double(as.character(event)))
          }
        }
    }))
  })
}
