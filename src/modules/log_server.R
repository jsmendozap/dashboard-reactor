log_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session){

    selected1 <- reactive({ input$var })
    selected2 <- reactive({ input$var2 })
    
    output$dygraph <- renderDygraph({ 
      req(app_state$bd)
      renderdy(app_state$bd(), selected1(), input, 'legend') 
    })
    
    output$dygraph2 <- renderDygraph({
      req(app_state$bd)
      renderdy(app_state$bd(), selected2(), input, 'legend2') 
    })
    
    output$log <- renderReactable({
      req(app_state$bd)

      app_state$bd() %>%
        slice_head(n = 1, by = event) %>%
        select(event, date_time, name, time_duration) %>%
        custom_reactable(
          columns = list(
            event = colDef(name = 'Event', width = 100),
            date_time = colDef(name = 'Start time', width = 200),
            name = colDef(name = 'Event name', minWidth = 200),
            time_duration = colDef(name = 'Duration', width = 100)
          ), selection = 'single'
        )
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
        {c(event = unique(.$event), dif = .$pt_310[1] - .$pt_310[2])}
  
      renderPrint({ 
        if(!is.na(test[1])){
          str_glue("Event: {test[1]} \nPressure difference: {round(test[2], 2)}")
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
  })
}