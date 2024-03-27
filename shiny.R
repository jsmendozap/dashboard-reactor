# app.R

pacman::p_load(tidyverse, readxl, janitor, DT, dygraphs, bs4Dash, shiny, prettyunits)

assign_event <- function(df){
  df <- df %>% 
    select(fic_140, rswitch_val) %>%
    mutate(event = NA)
  
  event <- 0
  first_count <- T
  second_count <- T
  
  for (i in 1:dim(df)[1]) {
    
    if (df[i, "fic_140"] == 100) {
      if (first_count) {
        event <- event + 1
        first_count <- F
        second_count <- T
      }
      
      df[i, "event"] = event
      
    } else if (df[i, "fic_140"] - df[i - 1, "fic_140"] != 0 |
               df[i, "rswitch_val"] != df[i - 1, "rswitch_val"]) {
      
      if(df[i, "rswitch_val"] != df[i - 1, "rswitch_val"]) {second_count <- T}
      
      if (second_count) {
        event <- event + 1
        second_count <- F
        first_count <- T
      }
      
      df[i, "event"] = event
      
    }
  }
  
  df %>% fill(event) %>% pull(event)
  
}

bd <- read_excel('RB003Test.xlsx') %>%
  janitor::clean_names() %>% 
  select(1, 2, 4, 6, 8, 9, 13:15, 26) %>% 
  mutate(across(.cols = 2:5, .fns = ~ifelse(.x < 0, 0, .x))) %>%
  filter(row_number() >= detect_index(fic_140, \(x) x == 100)) %>%
  rename('p_set' = 7) %>% 
  group_by(event = assign_event(.)) %>%
  mutate(n = pretty_sec(n() * 60)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(temp = case_when(
                  near(tic_300_sp, tic_300_pv, 10) ~ "Isotherm",
                  tic_300_sp > tic_300_pv ~ "Heating",
                  tic_300_sp < tic_300_pv ~ "Cooling down"),
         r1 = if_else(rswitch_val == 1, sum(fi_120, fi_130, fi_140), fi_110),
         r2 = if_else(rswitch_val == 1, fi_110, sum(fi_120, fi_130, fi_140)),
         name = str_glue("R1-{round(r1)} R2-{round(r2)} T-{temp} P-{p_set}")) %>%
  ungroup()

ui <- dashboardPage(
  dashboardHeader(title = "Experiment control"),
  dashboardSidebar(minified = F,
    sidebarMenu(
      menuItem("Log", tabName = 'log', icon = icon("clipboard-list", style = "margin-right: 5px"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'log',
              fluidRow(
                box(title = "Log summary",
                    status = 'primary',
                    solidHeader = T,
                    width = 7,
                    dataTableOutput('log')
                ),
                box(title = "Variable plot",
                    status = "info",
                    solidHeader = T,
                    width = 5,
                    selectInput(inputId = 'var', label = 'Select variable to print',
                                choices = c("Air flow" = "fi_110",
                                            "CO2 flow" = "fi_120",
                                            "Argon/Propane flow" = "fi_130",
                                            "Nitrogen flow" = "fi_140",
                                            "Setted pressure" = "p_set",
                                            "Measured temp" = "tic_300_pv",
                                            "Setted temp" = "tic_300_sp")),
                    div(id = 'legend', style = "margin-left: 5px"),
                    dygraphOutput("dygraph", height = '55vh')
                )
              )            
              )
    )
    
  )
)

server <- function(input, output) {
  selected <- reactive({
    input$var
  })
  
  output$dygraph <- renderDygraph({
    
    plot <- dygraph(bd %>% select(1, selected())) %>%
      dySeries(selected()) %>%
      dyRangeSelector() %>%
      dyOptions(useDataTimezone = TRUE) %>%
      dyCSS("dygraph.css") %>%
      dyLegend(labelsDiv = 'legend')
    
    if(!is.null(input$log_rows_selected)){
      time <- bd %>% 
        filter(event == input$log_rows_selected) %>%
        slice(1, n()) %>%
        pull(date_time)
      
      plot %>% dyShading(from = time[1], to = time[2], color = "#C1BEE1") 
      
    } else {
      plot
    }
  })
  
  output$log <- renderDataTable(bd %>%
                                  slice(1, .by = event) %>%
                                  select(1, 16, 12) %>%
                                  rename('Date' = 1, 'Event' = 2, 'Duration' = 3))
}

shinyApp(ui, server)#, options = list(launch.browser = T))