#' Chemometrics Module
#'
#' @description This module provides a Shiny interface for chemometric calculations,
#' including molar flows, conversions, mass balances, and box plots. It integrates
#' with the `ChemCalculations` R6 class to perform backend computations.
#'
#' @details The module consists of a UI function (`mod_chemometrics_ui`) and a server
#' function (`mod_chemometrics_server`). The UI includes interactive elements for
#' selecting compounds and events, and displays results in tables and plots. The server
#' handles user inputs, performs calculations, and renders outputs dynamically.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param app_state A reactive object containing the application state, including
#'   data and settings required for chemometric calculations.
#'
#' @importFrom shiny NS tagList fluidRow br moduleServer reactiveValues reactiveValuesToList
#'   observeEvent reactive req renderUI
#' @importFrom bs4Dash tabItem column tabBox actionButton
#' @importFrom reactable reactableOutput renderReactable colDef
#' @importFrom shinyWidgets pickerInput
#' @importFrom plotly plotlyOutput renderPlotly ggplotly
#' @importFrom dplyr slice_head filter mutate pull select distinct left_join rowwise ungroup
#'   across any_of contains relocate summarise
#' @importFrom tidyr drop_na pivot_longer
#' @importFrom ggplot2 theme element_text element_rect unit as_labeller stat_summary element_blank
#' @importFrom reactable.extras dropdown_extra
#'
#' @return A list of reactive outputs, including chemometric values, molar flows,
#'   conversions, mass balances, and box plot data.
#'
#' @noRd

# UI Function
mod_chemometrics_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "chemometric",
    fluidRow(reactableOutput(ns('std'), width = '100%')),
    br(),
    fluidRow(
      column(
        width = 3,
        uiOutput(ns('flow_compounds')),
        uiOutput(ns('flow_events')),
        actionButton(
          ns('btn_flow'),
          label = 'Calculate flows'
        )
      ),
      tabBox(
        width = 9,
        tabPanel(
          title = "Molar rate",
          fluidRow(
            column(
              width = 12,
              plotlyOutput(ns('molar_flow'), height = '50%')
            )
          )
        ),
        tabPanel(
          title = "Conversion",
          fluidRow(
            column(
              width = 12,
              plotlyOutput(ns('conversion'), height = '30%')
            )
          )
        ),
        tabPanel(
          title = "Mass balance",
          fluidRow(
            column(
              width = 6,
              plotlyOutput(ns('mass_balance'), height = '30%')
            ),
            column(
              width = 6,
              reactableOutput(ns('mass_summary'))
            )
          )
        ),
        tabPanel(
          title = "Box plot",
          fluidRow(
            column(
              width = 12,
              plotlyOutput(ns('boxplot'), height = '30%')
            )
          )
        )
      )
    )
  )
}

#' Server function

mod_chemometrics_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render the standard table
    output$std <- renderReactable({
      req(app_state$bd())

      tecq <- c('', 'Regeneration', 'By Pass', 'TOS')

      app_state$bd() %>%
        slice_head(n = 1, by = event) %>%
        select(event, name, qis = fic_140) %>%
        mutate(
          is = app_state$setting() %>%
            drop_na(internal_standar) %>%
            pull(compound),
          technique = list(tecq)
        ) %>%
        custom_reactable(
          columns = list(
            event = colDef(name = 'Event'),
            name = colDef(name = 'Event name', minWidth = 300),
            is = colDef(
              name = 'Internal Standard',
              minWidth = 150
            ),
            qis = colDef(
              name = 'QIS (NmL/min)',
              minWidth = 100
            ),
            technique = colDef(
              name = 'Technique/Reaction',
              minWidth = 150,
              cell = dropdown_extra(
                id = ns('dropdown'),
                choices = tecq,
                class = 'dropdown-extra'
              )
            )
          ),
          style = "border-radius: '3px'"
        )
    })

    # Reactive values for techniques
    tech <- reactiveValues()

    observeEvent(input$dropdown, {
      name <- app_state$bd() %>%
        slice_head(n = 1, by = event) %>%
        filter(row_number() == as.numeric(input$dropdown$row)) %>%
        pull(event) %>%
        as.character()

      tech[[name]] <- input$dropdown$value
    })

    # Create ChemCalculations object
    chem <- reactive({
      req(tech)

      ChemCalculations$new(
        setting = isolate(app_state$setting()),
        tech = reactiveValuesToList(tech),
        path = isolate(app_state$path()),
        bd = isolate(app_state$bd()),
        gc = isolate(app_state$gc()),
        ms = isolate(app_state$ms())
      )
    }) %>%
      bindEvent(input$btn_flow)

    ## Molar flow --------------------------------------------------------------------------------------------------------

    output$flow_compounds <- renderUI({
      req(app_state$gc())

      comp <- colnames(app_state$gc())[-c(1:5)] %>%
        {
          .[which(!. %in% c('nitrogen', 'argon'))]
        } %>%
        change_str('_', ' ')

      pickerInput(
        inputId = ns("graph_compounds"),
        label = "Select compounds to plot",
        choices = comp,
        multiple = TRUE,
        selected = comp,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      )
    })

    output$flow_events <- renderUI({
      req(chem())
      selected <- chem()$chem_values %>% pull(event) %>% unique()

      pickerInput(
        inputId = ns("graph_event"),
        label = "Select events to plot",
        choices = selected,
        multiple = TRUE,
        selected = selected,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      )
    })

    mf <- reactive({
      req(chem(), input$graph_compounds, input$graph_event)

      chem()$molar_flows(
        comps = input$graph_compounds,
        events = input$graph_event
      )
    })

    output$molar_flow <- renderPlotly({
      req(chem())
      molar_plot <- mf() %>%
        plot(
          x = time,
          y = value,
          fill = Compound,
          area = T,
          xlab = 'Time (min)',
          ylab = 'Molar flow (mol/h)',
          facet = "event",
          custom_color = T,
          args = list(
            area = list(alpha = 0.6, color = 'black', linewidth = 0.2),
            facet = list(
              scales = 'fixed',
              ncol = 2,
              labeller = as_labeller(chem()$event_names(filter = F))
            )
          )
        ) +
        theme(
          axis.text = element_text(color = 'black', size = 10),
          axis.title = element_text(size = 12),
          panel.spacing = unit(0.5, "cm"),
          plot.margin = unit(c(0, 0, 2, 2), 'cm'),
          panel.background = element_rect(colour = 'black')
        )

      total_height <- 180 * length(unique(chem()$chem_values$event))
      ggplotly(
        molar_plot,
        height = total_height,
        dynamicTicks = T,
        tooltip = "fill"
      )
    })

    ### Conversion --------------------------------------------------------------------------------------------

    conversion <- reactive({
      req(chem())
      chem()$conversion()
    })

    output$conversion <- renderPlotly({
      req(conversion())

      conversion_plot <- conversion() %>%
        plot(
          x = time,
          y = value,
          fill = Compound,
          shape = Compound,
          lines = T,
          points = T,
          xlab = 'Time (min)',
          ylab = 'Conversion (%)',
          facet = 'event',
          scale_y = list(n.breaks = 10),
          args = list(
            facet = list(
              ncol = 2,
              labeller = as_labeller(chem()$event_names())
            )
          )
        ) +
        theme(
          axis.text.y = element_text(color = 'black', size = 10),
          axis.title.y = element_text(size = 12),
          panel.spacing = unit(0.5, "cm"),
          plot.margin = unit(c(0, 0, 2, 2), 'cm'),
          panel.background = element_rect(colour = 'black')
        )

      total_height <- 180 + 180 * length(input$graph_event) / 2
      ggplotly(conversion_plot, height = total_height, dynamicTicks = T)
    })

    ### Mass balance -------------------------------------------------------------------------------------------

    mb <- reactive({
      req(chem())
      chem()$mass_balance(events = input$graph_event)
    })

    output$mass_balance <- renderPlotly({
      req(mb())

      mass_plot <- mb()$data %>%
        plot(
          x = time,
          y = value,
          fill = Compound,
          lines = T,
          points = T,
          xlab = "Time (min)",
          ylab = "Mass balance",
          facet = 'event',
          scale_y = list(n.breaks = 10),
          args = list(
            facet = list(
              ncol = 1,
              labeller = as_labeller(chem()$event_names())
            )
          )
        ) +
        theme(
          axis.text.y = element_text(color = 'black', size = 10),
          axis.title.y = element_text(size = 12),
          panel.spacing = unit(0.5, "cm"),
          plot.margin = unit(c(0, 0, 2, 2), 'cm'),
          panel.background = element_rect(colour = 'black')
        )

      total_height <- 180 + 180 * length(input$graph_event) / 2
      ggplotly(mass_plot, height = total_height, dynamicTicks = T)
    })

    output$mass_summary <- renderReactable({
      mb()$summary %>%
        custom_reactable(
          columns = list(
            event = colDef(name = 'Event'),
            Compound = colDef(name = 'Compound'),
            avg = colDef(name = 'Average'),
            sd = colDef(name = 'Standard deviation')
          ),
          style = "border-radius: '3px'; margin-bottom: 10px"
        )
    })

    ### Boxplot ------------------------------------------------------------------------------------------------

    boxplot_data <- reactive({
      chem()$boxplot(
        compounds = input$graph_compounds,
        events = input$graph_event
      )
    })

    output$boxplot <- renderPlotly({
      p <- boxplot_data() %>%
        plot(
          x = Compound,
          y = value,
          fill = Compound,
          boxp = T,
          xlab = "",
          ylab = "Molar flow (mol/h)",
          facet = 'event',
          args = list(
            facet = list(
              scales = "free",
              ncol = 2,
              labeller = as_labeller(chem()$event_names())
            ),
            boxp = list(show.legend = F)
          ),
          custom_color = T
        ) +
        stat_summary(
          fun = mean,
          geom = "point",
          shape = 23,
          fill = "white",
          size = 2.5
        ) +
        stat_summary(
          fun = min,
          geom = "point",
          shape = 25,
          size = 2.5
        ) +
        stat_summary(
          fun = max,
          geom = "point",
          shape = 19,
          size = 2.5
        ) +
        theme(
          axis.text.y = element_text(color = 'black', size = 10),
          axis.title.y = element_text(size = 12),
          panel.spacing = unit(0.5, "cm"),
          plot.margin = unit(c(0, 0, 2, 2), 'cm'),
          panel.background = element_rect(colour = 'black'),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )

      total_height <- 180 + 180 * length(input$graph_event) / 2
      boxp <- ggplotly(p, height = total_height)

      boxp$x$data <- lapply(boxp$x$data, function(x) {
        if (x$type == "box") {
          x$q1 <- min(x$y)
          x$q3 <- max(x$y)
          x$boxpoints <- FALSE
          x$whiskerwidth <- 0
        }
        return(x)
      })

      boxp
    })

    return(list(
      chem_values = reactive({
        chem()$chem_values
      }),
      molar_flow = mf,
      conversion = conversion,
      mass_balance = mb,
      boxplot = boxplot_data
    ))
  })
}
