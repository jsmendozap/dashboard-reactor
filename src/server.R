options(shiny.maxRequestSize=30*1024^2) 

server <- function(input, output) {
  
  volumes <- c(Home = fs::path_home(),  getVolumes()())
  shinyDirChoose(input, "directory", roots = volumes)

  path <- eventReactive(input$directory, {
      parseDirPath(volumes, input$directory)
  })

  output$dir_header <- renderText({ 
    path() %>% as.character %>% strsplit('/') %>%
      unlist %>% {paste("Folder:", .[length(.)])}
  })
  
  df <- reactive({
    req(path())
    grep("*.xlsx", dir(path(), full.names = T), value = TRUE) %>% load_file
  })
  
  bd <- reactive({
    df() %>% filter(n >= input$log_events)
  })

  gc <- reactive({
    req(path())
    tryCatch({
      grep("*.txt", dir(path(), full.names = T), value = TRUE) %>% load_gc(bd = bd())
    }, error = \(e) { NULL })
  })
  
  ms <- reactive({
    req(path())
    tryCatch({
      grep("*.dat", dir(path(), full.names = T), value = TRUE) %>% load_ms(bd = bd())
    }, error = \(e) { NULL })
  })
  
  ## Log -----------------------------------------------------------------------
  
  observeEvent(ignoreInit = T, list(input$log_events, input$directory), {
    log_server('log', bd = bd(), df = df())
  })
  
  ## Quality control -----------------------------------------------------------
  
  observeEvent(ignoreInit = T, list(input$log_events, input$directory), {
    qualiity_server('quality', bd())
  }) 

  ## Raw data --------------------------------------------------------------------

  observeEvent(ignoreInit = T, list(input$log_events, input$directory), {
    raw_server('raw', bd(), gc(), ms())
  })
  
  ## Chemometric -----------------------------------------------------------------

  observeEvent(ignoreInit = T, list(input$log_events, input$directory), {
    chem_server('chem', bd(), gc(), path())
  })

  ### Report -------------------------------------------------------------------
  
  output$report <- downloadHandler(

    filename = "report.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.qmd")
      file.copy("report.qmd", tempReport, overwrite = TRUE)
      
      quarto_render(input = tempReport, 
                    execute_params = list(bd = bd()))
      
      file.copy(file.path(tempdir(), 'report.html'), file)
    }
  )
  
  }
