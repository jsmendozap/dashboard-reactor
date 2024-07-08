chem_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {

    ns <- NS(id)

    output$std <- renderReactable({
      req(app_state$bd)

      tecq <- c('None', 'CO2-O2/TPO', 'Regeneration', 'CO2-TPD', 'Propane-TPD', 'H2-TPR', 'Propane-TPR', 'By Pass', 'TOS', 'Ramp')
      is <- c('None', 'Nitrogen', 'Argon')
      
      app_state$bd() %>%
        slice_head(n = 1, by = event) %>%
        select(event, name) %>%
        mutate(technique = list(tecq), is = list(is), qis = NA) %>%
        custom_reactable(columns = list(
                           event = colDef(name = 'Event'),
                           name = colDef(name = 'Event name', minWidth = 300),
                           technique = colDef(name = 'Technique/Reaction', minWidth = 150,
                                              cell = dropdown_extra(id = ns('dropdown'),
                                                                choices = tecq,
                                                                class = 'dropdown-extra')),
                           is = colDef(name = 'Internal Standard', minWidth = 150,
                                       cell = dropdown_extra(id = ns('is'), choices = is,
                                                             class = 'dropdown-extra')),
                           qis = colDef(name = 'QIS (NmL/min)', minWidth = 100,  
                                        cell = text_extra(ns('qis'), class = 'text-input'))
                         ), style = "border-radius: '3px'"
                        )
    })
    
    tech <- reactiveValues()
    is <- reactiveValues()
    qis <- reactiveValues()
    
    observeEvent(input$dropdown, {
      name <- as.character(input$dropdown$row)
      tech[[name]] <- input$dropdown$value
    })
    
    observeEvent(input$is, {
      name <- as.character(input$is$row)
      is[[name]] <- input$is$value
    })
    
    observeEvent(input$qis, {
      name <- as.character(input$qis$row)
      qis[[name]] <- input$qis$value
    })
    
    chem_values <- eventReactive(input$btn_flow, {
      
      sel_events <- function(x, qis) {
        events <- x %>% unique
        filter <- reactiveValuesToList(qis) %>% unlist %>% discard(\(x) nchar(x) == 0) %>% names %>% as.numeric
        events[filter]
      }
      
      mass <- app_state$path() %>% as.character %>% strsplit(" ") %>%
        unlist %>% {.[length(.) - 1]} %>% as.numeric(.)/1000000
      
      data.frame(
        event = sel_events(app_state$bd()$event, qis),
        name = sel_events(app_state$bd()$name, qis),
        technique = reactiveValuesToList(tech) %>% unlist %>% discard(~.x == "None"),
        is = reactiveValuesToList(is) %>% unlist %>% discard(~.x == "None") %>% tolower,
        qis = reactiveValuesToList(qis) %>% unlist %>% as.numeric %>% discard(is.na)) %>%
        left_join(app_state$gc(), by = join_by('event')) %>%
        rowwise() %>%
        mutate(across(10:ncol(.), ~ qis * (. / get(is)) * (60 / (22.4 * mass * 1000)))) %>%
        ungroup()
    })
    
    output$flow_compounds <- renderUI({
      req(app_state$gc)

      comp <- colnames(app_state$gc())[-c(1:5)] %>%
        {.[which(!. %in% c('nitrogen', 'argon'))]} %>%
        str_replace_all('_', ' ') %>% 
        str_to_title()
      
      pickerInput(
        inputId = ns("graph_compounds"), label = "Select compounds to plot", 
        choices = comp, multiple = TRUE, selected = comp,
        options = list(`actions-box` = TRUE, `live-search` = TRUE),
      )
    })
    
    output$flow_events <- renderUI({
      
      selected <- chem_values() %>% pull(event) %>% unique()
      
      pickerInput(
        inputId = ns("graph_event"), label = "Select events to plot", 
        choices = selected, multiple = TRUE, selected = selected,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      )
    })
    
    output$molar_flow <- renderPlotly({
      req(chem_values())
      req(input$graph_compounds)
      req(input$graph_event)
      
      names <- chem_values() %>%
        summarise(name = unique(name), .by = event) %>%
        mutate(name = str_c("Event: ", event, " - ", name)) %>%
        {setNames(.$name, .$event)}

      plot <- chem_values() %>%
        select(event, time, 9:ncol(.)) %>%
        pivot_longer(cols = 3:ncol(.), names_to = 'Compound', values_to = 'value') %>%
        filter(!Compound %in% c('argon', 'nitrogen')) %>%
        mutate(Compound = str_replace_all(Compound, '_', ' ') %>% str_to_title()) %>%
        filter(Compound %in% input$graph_compounds & event %in% input$graph_event) %>%
        ggplot(aes(x = time, y = value, fill = Compound)) +
        geom_area(alpha = 0.6, color = 'black', linewidth = 0.2) +
        labs(x = 'Time (min)', y = "Molar flow (mol/h)") +
        facet_wrap(~event, scales = 'free', ncol = 2,
                   labeller = as_labeller(names)) +
        theme_bw() +
        theme(axis.text = element_text(color = 'black', size = 10),
              axis.title = element_text(size = 12),
              panel.spacing = unit(0.5, "cm"),
              plot.margin = unit(c(0, 0, 2, 2), 'cm'),
              panel.background = element_rect(colour = 'black'))
      
      total_height <- 180 * length(unique(chem_values()$event))
      ggplotly(plot, height = total_height, dynamicTicks = T, tooltip = "fill")
    })

  })
}
