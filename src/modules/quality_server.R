quality_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$flow <- reactable::renderReactable({
      shiny::req(app_state$bd)

      app_state$bd() %>%
        dplyr::summarise(
          across(
            .cols = c(2, 4, 6, 8),
            .fn = \(x) round(sum(x) / dplyr::n(), 1)
          ),
          .by = event
        ) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(total = sum(fi_110, fi_120, fi_130, fi_140)) %>%
        custom_reactable(
          columns = list(
            event = reactable::colDef(name = 'Event', minWidth = 80),
            fi_110 = reactable::colDef(name = 'Air (mL/min)'),
            fi_120 = reactable::colDef(name = 'Carbon dioxide (mL/min)'),
            fi_130 = reactable::colDef(name = 'Argon / Propane (mL/min)'),
            fi_140 = reactable::colDef(name = 'Nitrogen (mL/min)'),
            total = reactable::colDef(name = 'Total flow (mL/min)')
          ),
        )
    })

    output$corr <- reactable::renderReactable({
      shiny::req(app_state$bd)

      app_state$bd() %>%
        dplyr::summarise(
          cor_air = mean(((fi_110 + 1) / (1 + fic_110))) %>% round(1),
          cor_co2 = mean(((fi_120 + 1) / (fic_120 + 1))) %>% round(1),
          cor_ar = mean(((fi_130 + 1) / (fic_130 + 1))) %>% round(1),
          cor_n2 = mean(((fi_140 + 1) / (fic_140 + 1))) %>% round(1),
          .by = event
        ) %>%
        custom_reactable(
          columns = list(
            event = reactable::colDef(name = 'Event', minWidth = 80),
            cor_air = reactable::colDef(
              name = 'Air',
              style = conditional_color
            ),
            cor_co2 = reactable::colDef(
              name = 'Carbon dioxide',
              style = conditional_color
            ),
            cor_ar = reactable::colDef(
              name = 'Argon / Propane',
              style = conditional_color
            ),
            cor_n2 = reactable::colDef(
              name = 'Nitrogen',
              style = conditional_color
            )
          )
        )
    })

    output$norm <- plotly::renderPlotly({
      shiny::req(app_state$bd)
      plot <- tryCatch(
        expr = {
          app_state$bd() %>%
            dplyr::transmute(date_time, deriv = norm_deriv(normoliter_out)) %>%
            dplyr::group_by(
              group = rep(
                dplyr::row_number(),
                each = 5,
                length.out = dplyr::n()
              )
            ) %>%
            dplyr::summarise(
              time = mean(date_time),
              value = mean(deriv, na.rm = T) %>% round(3)
            ) %>%
            plot(
              x = time,
              y = value,
              lines = T,
              xlab = 'Time',
              ylab = "Normalized milimeters rate",
              args = list(lines = list(linewidth = 0.2))
            )
        },
        error = \(e) {
          plot(
            data = app_state$bd(),
            x = date_time,
            y = normoliter_out,
            xlab = "Time",
            ylab = "Normolized milimeters",
            title = "Flow rate not possible"
          )
        }
      )

      plotly::ggplotly(plot)
    })

    output$temp <- reactable::renderReactable({
      shiny::req(app_state$bd)
      suppressWarnings({
        app_state$bd() %>%
          dplyr::summarise(
            name = name[1],
            rate = diff(tic_300_pv) %>% mean(na.rm = T),
            mean_measure = mean(tic_300_pv),
            mean_set = mean(tic_300_sp),
            mean_r1 = mean(te_310),
            mean_r2 = mean(te_320),
            .by = event
          ) %>%
          dplyr::mutate(dplyr::across(.cols = 3:7, .fns = ~ round(.x, 1))) %>%
          custom_reactable(
            columns = list(
              event = reactable::colDef(name = 'Event', minWidth = 80),
              name = reactable::colDef(name = 'Name', minWidth = 200),
              rate = reactable::colDef(name = 'Rate of change'),
              mean_measure = reactable::colDef(name = 'Avg. measured'),
              mean_set = reactable::colDef(name = 'Avg. setted'),
              mean_r1 = reactable::colDef(name = 'Avg. Reactor 1'),
              mean_r2 = reactable::colDef(name = 'Avg. Reactor 2')
            ),
            rowStyle = reactable::JS(
              "function(rowInfo) { return { height: '50px' }}"
            )
          )
      })
    })

    output$diffTemp <- plotly::renderPlotly({
      shiny::req(app_state$bd)

      plot <- app_state$bd() %>%
        dplyr::mutate(difference = tic_300_pv - te_320) %>%
        plot(
          x = te_310,
          y = difference,
          lines = T,
          facet = "event",
          xlab = "Temperature (°C)",
          ylab = "Temperature differences (°C)",
          args = list(lines = list(linewidth = 0.2))
        ) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, face = 'bold')
        )

      plotly::ggplotly(plot)
    })

    output$press <- reactable::renderReactable({
      shiny::req(app_state$bd)

      app_state$bd() %>%
        dplyr::summarise(
          name = name[1],
          mean_set = mean(p_set),
          mean_r1 = mean(pt_310),
          mean_r2 = mean(pt_320),
          delta = mean_r1 - mean_r2,
          .by = event
        ) %>%
        dplyr::mutate(dplyr::across(.cols = 3:6, .fns = ~ round(.x, 1))) %>%
        custom_reactable(
          columns = list(
            event = reactable::colDef(name = 'Event', minWidth = 80),
            name = reactable::colDef(name = 'Name', minWidth = 120),
            mean_set = reactable::colDef(name = 'Avg. setted', minWidth = 80),
            mean_r1 = reactable::colDef(name = 'Avg. Reactor 1', minWidth = 80),
            mean_r2 = reactable::colDef(name = 'Avg. Reactor 2', minWidth = 80),
            delta = reactable::colDef(name = 'Delta of pressure', minWidth = 90)
          ),
          selection = 'single'
        )
    })

    output$plotPress <- dygraphs::renderDygraph({
      shiny::req(reactable::getReactableState('press', 'selected'))

      selected <- dplyr::slice_head(app_state$bd(), n = 1, by = event) %>%
        dplyr::filter(
          dplyr::row_number() ==
            reactable::getReactableState('press', 'selected')
        ) %>%
        dplyr::pull(event)

      app_state$bd() %>%
        dplyr::filter(event == selected) %>%
        dplyr::rowwise() %>%
        dplyr::transmute(
          time = date_time,
          delta = mean(pt_310) - mean(pt_320)
        ) %>%
        dygraphs::dygraph() %>%
        dygraphs::dySeries("delta") %>%
        dygraphs::dyRangeSelector() %>%
        dygraphs::dyOptions(useDataTimezone = TRUE) %>%
        dygraphs::dyLegend(labelsDiv = 'legend')
    })
  })
}
