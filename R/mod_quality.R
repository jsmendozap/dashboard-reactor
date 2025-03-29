#' Quality Module UI
#'
#' @description Creates the UI components for the quality module in the dashboard. This module displays flow, temperature, and pressure data with interactive tables and plots.
#'
#' @param id A string. The namespace ID for the module, used to create unique input/output IDs.
#'
#' @return A `tabItem` containing the UI elements for the quality module.
#'
#' @importFrom shiny NS tagList fluidRow tabPanel
#' @importFrom bs4Dash tabItem tabBox box
#' @importFrom reactable reactableOutput
#' @importFrom plotly plotlyOutput
#' @importFrom dygraphs dygraphOutput
#' @noRd

mod_quality_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = 'quality',
    tabBox(
      id = "tabset1",
      height = "100%",
      width = 12,
      tabPanel(
        title = "Flow",
        fluidRow(
          tabBox(
            width = 7,
            tabPanel(
              title = "Rate Flow",
              reactableOutput(ns('flow'))
            ),
            tabPanel(
              title = "Correlations",
              reactableOutput(ns('corr'))
            )
          ),
          box(
            title = "Normalized millimeters plot",
            status = 'info',
            solidHeader = TRUE,
            width = 5,
            plotlyOutput(ns('norm'))
          )
        )
      ),
      tabPanel(
        title = "Temperature",
        fluidRow(
          box(
            title = 'Summary',
            width = 12,
            status = 'info',
            solidHeader = TRUE,
            reactableOutput(ns('temp'))
          ),
          plotlyOutput(ns('diffTemp'), height = '100vh')
        )
      ),
      tabPanel(
        title = "Pressure",
        fluidRow(
          box(
            width = 7,
            status = 'info',
            solidHeader = TRUE,
            reactableOutput(ns('press'))
          ),
          box(
            width = 5,
            status = 'info',
            solidHeader = TRUE,
            dygraphOutput(ns('plotPress')),
            title = 'Pressure difference per event plot'
          )
        )
      )
    )
  )
}

#' Quality Module Server
#'
#' @description Defines the server-side logic for the quality module in the dashboard. This module handles flow, temperature, and pressure data processing and visualization.
#'
#' @param id A string. The namespace ID for the module, used to create unique input/output IDs.
#' @param app_state A reactiveValues object containing the application state, including the experimental database (`bd`).
#'
#' @return None. This function is called internally by `moduleServer`.
#'
#' @importFrom shiny moduleServer reactive req renderUI
#' @importFrom reactable renderReactable colDef
#' @importFrom plotly renderPlotly ggplotly
#' @importFrom dygraphs renderDygraph dygraph dySeries dyRangeSelector dyOptions dyLegend
#' @importFrom dplyr summarise mutate transmute filter rowwise ungroup across pull slice_head
#' @importFrom ggplot2 theme element_text
#' @noRd

mod_quality_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render flow table
    output$flow <- renderReactable({
      req(app_state$bd())

      app_state$bd() %>%
        summarise(
          across(
            .cols = c(2, 4, 6, 8),
            .fn = \(x) round(sum(x) / n(), 1)
          ),
          .by = event
        ) %>%
        rowwise() %>%
        mutate(total = sum(fi_110, fi_120, fi_130, fi_140)) %>%
        custom_reactable(
          columns = list(
            event = colDef(name = 'Event', minWidth = 80),
            fi_110 = colDef(name = 'Air (mL/min)'),
            fi_120 = colDef(name = 'Carbon dioxide (mL/min)'),
            fi_130 = colDef(name = 'Argon / Propane (mL/min)'),
            fi_140 = colDef(name = 'Nitrogen (mL/min)'),
            total = colDef(name = 'Total flow (mL/min)')
          )
        )
    })

    # Render correlation table
    output$corr <- renderReactable({
      req(app_state$bd())

      app_state$bd() %>%
        summarise(
          cor_air = mean(((fi_110 + 1) / (1 + fic_110))) %>% round(1),
          cor_co2 = mean(((fi_120 + 1) / (fic_120 + 1))) %>% round(1),
          cor_ar = mean(((fi_130 + 1) / (fic_130 + 1))) %>% round(1),
          cor_n2 = mean(((fi_140 + 1) / (fic_140 + 1))) %>% round(1),
          .by = event
        ) %>%
        custom_reactable(
          columns = list(
            event = colDef(name = 'Event', minWidth = 80),
            cor_air = colDef(name = 'Air', style = conditional_color),
            cor_co2 = colDef(
              name = 'Carbon dioxide',
              style = conditional_color
            ),
            cor_ar = colDef(
              name = 'Argon / Propane',
              style = conditional_color
            ),
            cor_n2 = colDef(name = 'Nitrogen', style = conditional_color)
          )
        )
    })

    # Render normalized plot
    output$norm <- renderPlotly({
      req(app_state$bd())

      plot <- tryCatch(
        expr = {
          app_state$bd() %>%
            transmute(
              date_time = date_time,
              deriv = norm_deriv(normoliter_out)
            ) %>%
            group_by(
              group = rep(
                row_number(),
                each = 5,
                length.out = n()
              )
            ) %>%
            summarise(
              time = mean(date_time),
              value = mean(deriv, na.rm = TRUE) %>% round(3)
            ) %>%
            plot(
              x = time,
              y = value,
              lines = TRUE,
              xlab = 'Time',
              ylab = "Normalized millimeters rate",
              args = list(lines = list(linewidth = 0.2))
            )
        },
        error = \(e) {
          plot(
            data = app_state$bd(),
            x = date_time,
            y = normoliter_out,
            xlab = "Time",
            ylab = "Normalized millimeters",
            title = "Flow rate not possible"
          )
        }
      )

      ggplotly(plot)
    })

    # Render temperature table
    output$temp <- renderReactable({
      req(app_state$bd())

      suppressWarnings({
        app_state$bd() %>%
          summarise(
            name = name[1],
            rate = diff(tic_300_pv) %>% mean(na.rm = TRUE),
            mean_measure = mean(tic_300_pv),
            mean_set = mean(tic_300_sp),
            mean_r1 = mean(te_310),
            mean_r2 = mean(te_320),
            .by = event
          ) %>%
          mutate(across(.cols = 3:7, .fns = ~ round(.x, 1))) %>%
          custom_reactable(
            columns = list(
              event = colDef(name = 'Event', minWidth = 80),
              name = colDef(name = 'Name', minWidth = 200),
              rate = colDef(name = 'Rate of change'),
              mean_measure = colDef(name = 'Avg. measured'),
              mean_set = colDef(name = 'Avg. setted'),
              mean_r1 = colDef(name = 'Avg. Reactor 1'),
              mean_r2 = colDef(name = 'Avg. Reactor 2')
            ),
            rowStyle = reactable::JS(
              "function(rowInfo) { return { height: '50px' }}"
            )
          )
      })
    })

    # Render temperature difference plot
    output$diffTemp <- renderPlotly({
      req(app_state$bd())

      plot <- app_state$bd() %>%
        mutate(difference = tic_300_pv - te_320) %>%
        plot(
          x = te_310,
          y = difference,
          lines = TRUE,
          facet = "event",
          xlab = "Temperature (°C)",
          ylab = "Temperature differences (°C)",
          args = list(lines = list(linewidth = 0.2))
        ) +
        ggplot2::theme(
          plot.title = element_text(hjust = 0.5, face = 'bold')
        )

      ggplotly(plot)
    })

    # Render pressure table
    output$press <- renderReactable({
      req(app_state$bd)

      app_state$bd() %>%
        summarise(
          name = name[1],
          mean_set = mean(p_set),
          mean_r1 = mean(pt_310),
          mean_r2 = mean(pt_320),
          delta = mean_r1 - mean_r2,
          .by = event
        ) %>%
        mutate(across(.cols = 3:6, .fns = ~ round(.x, 1))) %>%
        custom_reactable(
          columns = list(
            event = colDef(name = 'Event', minWidth = 80),
            name = colDef(name = 'Name', minWidth = 120),
            mean_set = colDef(name = 'Avg. setted', minWidth = 80),
            mean_r1 = colDef(name = 'Avg. Reactor 1', minWidth = 80),
            mean_r2 = colDef(name = 'Avg. Reactor 2', minWidth = 80),
            delta = colDef(name = 'Delta of pressure', minWidth = 90)
          ),
          selection = 'single'
        )
    })

    # Render pressure plot
    output$plotPress <- renderDygraph({
      req(reactable::getReactableState('press', 'selected'))

      selected <- slice_head(app_state$bd(), n = 1, by = event) %>%
        filter(
          row_number() == reactable::getReactableState('press', 'selected')
        ) %>%
        pull(event)

      app_state$bd() %>%
        filter(event == selected) %>%
        rowwise() %>%
        transmute(
          time = date_time,
          delta = mean(pt_310) - mean(pt_320)
        ) %>%
        dygraph() %>%
        dySeries("delta") %>%
        dyRangeSelector() %>%
        dyOptions(useDataTimezone = TRUE) %>%
        dyLegend(labelsDiv = 'legend')
    })
  })
}

# Helper functions
conditional_color <- function(value) {
  if (value < 0.9 || value >= 1.1) {
    color <- "red"
  } else {
    color <- "lightgreen"
  }

  list(color = color, fontWeight = 'bold')
}

norm_deriv <- function(col) {
  index <- which(diff(col) < -1)
  new <- col[1:index[1]]

  for (i in 2:(length(index) + 1)) {
    value <- sum(col[index[1:i - 1]])
    if (i == (length(index) + 1)) {
      aux <- col[(index[i - 1] + 1):length(col)] + value
    } else {
      aux <- col[(index[i - 1] + 1):index[i]] + value
    }
    new <- c(new, aux)
  }
  c(diff(new, lag = 5) / 5, rep(NA, 5))
}
