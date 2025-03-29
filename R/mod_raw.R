#' Raw Data Module UI
#'
#' @description Creates the UI components for the raw data module in the dashboard. This module allows users to visualize and interact with GC and MS raw data.
#'
#' @param id A string. The namespace ID for the module, used to create unique input/output IDs.
#'
#' @return A `tabItem` containing the UI elements for the raw data module.
#'
#' @importFrom shiny NS fluidRow tabPanel uiOutput checkboxInput sliderInput plotOutput
#' @importFrom bs4Dash tabItem tabBox box column
#' @importFrom shinycssloaders withSpinner
#' @importFrom plotly plotlyOutput
#' @importFrom dygraphs dygraphOutput
#' @noRd

mod_raw_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "raw-data",
    tabBox(
      id = "tabset2",
      height = "100%",
      width = 12,
      tabPanel(
        title = "GC composition",
        fluidRow(
          column(
            width = 3,
            uiOutput(ns('UIcompunds')),
            uiOutput(ns('gc_events')),
            uiOutput(ns('xgc'))
          ),
          column(
            width = 9,
            withSpinner(
              plotlyOutput(ns('composition'), height = '100%'),
              color = "#607282",
              caption = "Generating plot",
              size = 0.5
            )
          )
        )
      ),
      tabPanel(
        title = "MS signals",
        fluidRow(
          column(
            width = 3,
            uiOutput(ns('xms')),
            uiOutput(ns('yms')),
            uiOutput(ns('ms_events')),
            checkboxInput(ns('scale'), 'Same scale', TRUE),
            sliderInput(
              ns('smooth'),
              'Smoothing level:',
              min = 0,
              max = 1,
              value = 0
            ),
            uiOutput(ns("ms_int")),
            plotOutput(ns('int_plot'), height = '250px')
          ),
          tabBox(
            id = "tabset3",
            width = 9,
            tabPanel(
              title = "General",
              br(),
              fluidRow(
                column(
                  width = 12,
                  dygraphOutput(
                    ns('msplot_gen'),
                    height = '400px'
                  )
                )
              )
            ),
            tabPanel(
              title = "Events",
              fluidRow(
                column(
                  width = 12,
                  br(),
                  withSpinner(
                    plotlyOutput(
                      ns('msplot'),
                      height = '100%'
                    ),
                    color = "#607282",
                    caption = "Generating plot",
                    size = 0.5
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

#' Raw Data Module Server
#'
#' @description Defines the server-side logic for the raw data module in the dashboard. This module processes and visualizes GC and MS raw data.
#'
#' @param id A string. The namespace ID for the module, used to create unique input/output IDs.
#' @param app_state A reactiveValues object containing the application state, including GC and MS data.
#'
#' @return None. This function is called internally by `moduleServer`.
#'
#' @importFrom shiny moduleServer reactive req renderUI renderPlot
#' @importFrom shinyWidgets pickerInput
#' @importFrom dplyr summarise mutate filter select rename_with pull contains rowwise ungroup across n
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom stringr str_replace_all str_c
#' @importFrom plotly renderPlotly ggplotly
#' @importFrom dygraphs renderDygraph dygraph dySeries dyRangeSelector dyOptions dySeriesData
#' @importFrom lubridate ymd_hms
#' @importFrom prettyunits pretty_num
#' @importFrom ggplot2 theme element_text unit element_rect as_labeller
#' @noRd

mod_raw_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    names <- reactive({
      app_state$bd() %>%
        summarise(name = unique(name), .by = event) %>%
        mutate(name = str_c("Event: ", event, " - ", name)) %>%
        {
          setNames(.$name, .$event)
        }
    })

    output$UIcompunds <- renderUI({
      req(app_state$gc())

      comp <- colnames(app_state$gc())[-c(1:5)] %>% change_str("_", " ")

      pickerInput(
        inputId = ns("compounds"),
        label = "Select compounds to plot",
        choices = comp,
        multiple = TRUE,
        selected = comp,
        options = list(`actions-box` = TRUE, `live-search` = TRUE),
      )
    })

    output$xgc <- renderUI({
      selectInput(
        inputId = ns('gc_xaxis'),
        label = 'Select x axis',
        choices = c("Time" = "time", "Temperature" = "tic_300_pv")
      )
    })

    output$gc_events <- renderUI({
      req(app_state$gc)

      selected <- app_state$gc() %>%
        filter(n() > 1, .by = event) %>%
        pull(event) %>%
        unique()

      pickerInput(
        inputId = ns("gc_event"),
        label = "Select events to plot",
        choices = unique(app_state$gc()$event),
        multiple = TRUE,
        selected = selected,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      )
    })

    output$composition <- renderPlotly({
      req(input$gc_event, input$compounds)

      comp_plot <- app_state$gc() %>%
        select(-injection) %>%
        pivot_longer(
          cols = 5:ncol(.),
          names_to = 'Compound',
          values_to = 'value'
        ) %>%
        mutate(Compound = change_str(Compound, "_", " ")) %>%
        filter(
          Compound %in% input$compounds & event %in% input$gc_event
        ) %>%
        plot(
          x = .data[[input$gc_xaxis]],
          y = value,
          fill = Compound,
          facet = "event",
          ylab = "Composition",
          area = T,
          hline = T,
          custom_color = T,
          xlab = ifelse(
            input$gc_xaxis == 'tic_300_pv',
            'Temperature (°C)',
            'Reaction time (min)'
          ),
          args = list(
            area = list(alpha = 0.6, color = 'black', linewidth = 0.2),
            hline = list(yintercept = 100, linetype = 'dashed'),
            facet = list(
              ncol = 2,
              labeller = as_labeller(names()),
              scales = 'free_x'
            )
          )
        ) +
        theme(
          axis.text = element_text(color = 'black', size = 10),
          axis.title = element_text(size = 12),
          panel.spacing = unit(1, "lines"),
          plot.margin = unit(c(0, 0, 2, 0), 'cm'),
          panel.background = element_rect(colour = 'black')
        )

      total_height <- 180 + 180 * length(input$gc_event)

      ggplotly(
        comp_plot,
        height = total_height,
        dynamicTicks = T,
        tooltip = "fill"
      )
    })

    ## ms file -------------------------------------------------------------------------------------

    output$xms <- renderUI({
      conditionalPanel(
        "input.tabset3 == 'Events'",
        selectInput(
          inputId = ns('ms_xaxis'),
          label = 'Select x axis:',
          choices = c("Time" = "time", "Temperature" = "tic_300_pv")
        )
      )
    })

    output$yms <- renderUI({
      req(app_state$ms())

      choices <- app_state$ms() %>%
        select(contains('_amu_')) %>%
        colnames() %>%
        str_replace_all('_', ' ')

      pickerInput(
        inputId = ns("ms_yaxis"),
        label = "Select compound:",
        multiple = T,
        choices = choices,
        selected = choices[1],
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      )
    })

    output$ms_events <- renderUI({
      req(app_state$ms())

      selected <- app_state$ms() %>%
        filter(n() > 1, .by = event) %>%
        pull(event) %>%
        unique()

      conditionalPanel(
        "input.tabset3 == 'Events'",
        pickerInput(
          inputId = ns("ms_event"),
          label = "Select events to plot",
          choices = unique(app_state$ms()$event),
          multiple = TRUE,
          selected = selected,
          options = list(`actions-box` = TRUE, `live-search` = TRUE)
        )
      )
    })

    output$msplot_gen <- renderDygraph({
      req(input$ms_yaxis)

      data <- app_state$ms() %>%
        rename_with(
          .fn = ~ str_replace_all(.x, '_', ' '),
          .cols = contains('_amu_')
        ) %>%
        select(
          time_absolute_date_time,
          contains(input$ms_yaxis)
        ) %>%
        mutate(across(
          .cols = contains('amu'),
          .fns = ~ smooth.spline(.x, spar = input$smooth)$y
        )) %>%
        dygraph(
          xlab = ifelse(
            input$ms_xaxis == 'tic_300_pv',
            'Temperature (°C)',
            'Reaction Time (min)'
          )
        )

      dySeriesData(data, 'plot', input$ms_yaxis) %>%
        dyRangeSelector() %>%
        dyOptions(useDataTimezone = TRUE) %>%
        dyOptions(strokeWidth = 2) %>%
        dyLegend(width = 450)
    })

    output$msplot <- renderPlotly({
      req(input$ms_yaxis, input$compounds, input$ms_event)
      dates <- ymd_hms(input$msplot_gen_date_window)

      ms_plot <- app_state$ms() %>%
        filter(
          between(time_absolute_date_time, dates[1], dates[2]) &
            event %in% input$ms_event
        ) %>%
        pivot_longer(
          cols = 5:ncol(.),
          names_to = 'Compound',
          values_to = 'value'
        ) %>%
        mutate(
          value = smooth.spline(value, spar = input$smooth)$y,
          .by = Compound
        ) %>%
        mutate(
          Compound = stringr::str_replace_all(Compound, '_', ' ')
        ) %>%
        filter(Compound %in% input$ms_yaxis) %>%
        drop_na(event) %>%
        plot(
          x = .data[[input$ms_xaxis]],
          y = value,
          color = Compound,
          lines = T,
          xlab = ifelse(
            input$ms_xaxis == 'tic_300_pv',
            'Temperature (°C)',
            'Reaction time (min)'
          ),
          ylab = "Arbitrary unit (a.u)",
          facet = 'event',
          args = list(
            facet = list(
              scales = ifelse(input$scale == TRUE, 'fixed', 'free_x'),
              ncol = 2,
              labeller = as_labeller(names())
            )
          )
        ) +
        theme(
          axis.text = element_text(color = 'black', size = 10),
          axis.title = element_text(size = 12),
          panel.spacing = unit(1, "lines"),
          panel.background = element_rect(colour = 'black'),
          plot.margin = unit(c(0, 0, 2, 2), units = 'cm')
        )

      total_height <- 180 + 180 * length(input$ms_event)
      ggplotly(
        ms_plot,
        height = total_height,
        dynamicTicks = T,
        tooltip = c('color', 'x', 'y')
      )
    })

    fit <- reactive({
      req(input$msplot_gen_date_window, input$ms_yaxis)

      range <- input$msplot_gen_date_window

      app_state$ms() %>%
        filter(between(
          time_absolute_date_time,
          ymd_hms(range[1]),
          ymd_hms(range[2])
        )) %>%
        rename_with(
          .fn = ~ stringr::str_replace_all(.x, '_', ' '),
          .cols = contains('_amu_')
        ) %>%
        pull(.data[[input$ms_yaxis[1]]]) %>%
        smooth.spline(spar = input$smooth)
    })

    output$ms_int <- renderUI({
      req(input$msplot_gen_date_window)

      smooth <- \(x) predict(fit(), x)$y
      value <- integrate(smooth, 1, length(fit()$x))$value

      div(
        span('Integral value:', style = 'font-weight: bold'),
        span(pretty_num(value), 'u.m.a')
      )
    })

    output$int_plot <- renderPlot({
      req(input$msplot_gen_date_window)

      par(mar = c(5, 4, 1.5, 2))
      base::plot(
        x = fit()$x,
        y = fit()$y,
        type = 'l',
        bty = 'n',
        xlab = '',
        ylab = ''
      )
    })
  })
}
