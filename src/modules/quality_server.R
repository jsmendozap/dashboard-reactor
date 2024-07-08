quality_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    
    output$flow <- renderReactable({
      req(app_state$bd)

      app_state$bd() %>% 
        summarise(across(.cols = c(2, 4, 6, 8),
                         .fn = \(x) round(sum(x)/n(), 1)),
                  .by = event) %>%
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
          ), 
        )
    }) 
    
    output$corr <- renderReactable({
      req(app_state$bd)

      app_state$bd() %>% 
        summarise(cor_air = mean(((fi_110 + 1)/(1 + fic_110))) %>% round(1),
                  cor_co2 = mean(((fi_120 + 1)/(fic_120 + 1))) %>% round(1),
                  cor_ar = mean(((fi_130 + 1)/(fic_130 +1))) %>% round(1),
                  cor_n2 = mean(((fi_140 + 1)/(fic_140 + 1))) %>% round(1),
                  .by = event) %>%
        custom_reactable(
          columns = list(
            event = colDef(name = 'Event', minWidth = 80),
            cor_air = colDef(name = 'Air', style = conditional_color),
            cor_co2 = colDef(name = 'Carbon dioxide', style = conditional_color),
            cor_ar = colDef(name = 'Argon / Propane', style = conditional_color),
            cor_n2 = colDef(name = 'Nitrogen', style = conditional_color)
          )
        )
    })
    
    output$norm <- renderPlotly({
      req(app_state$bd)
      plot <- tryCatch(expr = {
        
        app_state$bd() %>% 
          transmute(date_time, deriv = norm_deriv(normoliter_out)) %>% 
          group_by(group = rep(row_number(), each = 5, length.out = n())) %>%
          summarise(time = mean(date_time), 
                    value = mean(deriv, na.rm = T) %>% round(3)) %>% 
          ggplot() +
          geom_line(aes(x = time, y = value), linewidth = 0.2) +
          labs(x = "Time", y = "Normalized milimeters rate") +
          theme_bw()
        
      }, error = \(e) {
        ggplot(app_state$bd()) +
          geom_line(aes(x = date_time, y = normoliter_out)) +
          labs(x = "Time", y = "Normalized milimeters",
               title = 'Flow rate not possible') +
          theme_bw()
      })
        
      ggplotly(plot)
    })
    
    output$temp <- renderReactable({
      req(app_state$bd)
      suppressWarnings({
        
        app_state$bd() %>%
        summarise(name = name[1],
                  rate = diff(tic_300_pv) %>% mean(na.rm = T) ,
                  mean_measure = mean(tic_300_pv),
                  mean_set = mean(tic_300_sp),
                  mean_r1 = mean(te_310),
                  mean_r2 = mean(te_320),
                  .by = event)  %>%
        mutate(across(.cols = 3:7, .fns = ~round(.x, 1))) %>%
        custom_reactable(
          columns = list(
            event = colDef(name = 'Event', minWidth = 80),
            name = colDef(name = 'Name', minWidth = 200),
            rate = colDef(name = 'Rate of change'),
            mean_measure = colDef(name = 'Avg. measured'),
            mean_set = colDef(name = 'Avg. setted'),
            mean_r1 = colDef(name = 'Avg. Reactor 1'),
            mean_r2 = colDef(name = 'Avg. Reactor 2')),
          rowStyle = JS("function(rowInfo) { return { height: '50px' }}")
        )
      })
    })
    
    output$diffTemp <- renderPlotly({
      req(app_state$bd)

      plot <- app_state$bd() %>%
        mutate(difference = tic_300_pv - te_320) %>%
        ggplot() +
        geom_line(aes(x = te_310, y = difference), linewidth = 0.2) +
        facet_wrap(~event, scales = 'fixed') +
        labs(x = "Temperature (°C)", y = "Temperature differences (°C)") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
      
      ggplotly(plot)
    })
    
    output$press <- renderReactable({
      req(app_state$bd)

      app_state$bd() %>% 
        summarise(name = name[1],
                  mean_set = mean(p_set),
                  mean_r1 = mean(pt_310),
                  mean_r2 = mean(pt_320),
                  delta = mean_r1 - mean_r2,
                  .by = event) %>% 
        mutate(across(.cols = 3:6, .fns = ~round(.x, 1))) %>%
        custom_reactable(
          columns = list(
            event = colDef(name = 'Event', minWidth = 80),
            name = colDef(name = 'Name', minWidth = 120),
            mean_set = colDef(name = 'Avg. setted', minWidth = 80),
            mean_r1 = colDef(name = 'Avg. Reactor 1', minWidth = 80),
            mean_r2 = colDef(name = 'Avg. Reactor 2', minWidth = 80),
            delta = colDef(name = 'Delta of pressure', minWidth = 90)
          ), selection = 'single'
        )
    })
    
    output$plotPress <- renderDygraph({
      req(getReactableState('press', 'selected'))
      
      selected <- slice_head(app_state$bd(), n = 1, by = event) %>%
        filter(row_number() == getReactableState('press', 'selected')) %>%
        pull(event)
      
      app_state$bd() %>%
        filter(event == selected) %>%
        rowwise() %>%
        transmute(time = date_time, delta = mean(pt_310) - mean(pt_320)) %>%
        dygraph() %>%
        dySeries("delta") %>%
        dyRangeSelector() %>%
        dyOptions(useDataTimezone = TRUE) %>%
        dyLegend(labelsDiv = 'legend')
    })
  })
}