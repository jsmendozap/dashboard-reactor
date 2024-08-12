options(shiny.maxRequestSize=30*1024^2)

server <- function(input, output) {

  app_state <- shiny::reactiveValues()

  shiny::observe({
    app_state$df <- df
    app_state$bd <- bd
    app_state$gc <- gc
    app_state$ms <- ms
    app_state$path <- path
  }) %>% shiny::bindEvent(c(df(), bd(), gc(), ms(), path()))

  volumes <- c(Home = fs::path_home(),  shinyFiles::getVolumes()())
  shinyFiles::shinyDirChoose(input, "directory", roots = volumes)

  path <- shiny::eventReactive(input$directory, {
      shinyFiles::parseDirPath(volumes, input$directory)
  })

  output$dir_header <- shiny::renderText({
    path() %>% as.character %>% strsplit('/') %>%
      unlist %>% {paste("Folder:", .[length(.)])}
  })

  df <- shiny::reactive({
    shiny::req(path())
    tryCatch({
      grep("*.xlsx", dir(path(), full.names = T), value = TRUE) %>% load_file
    }, error = \(e) { NULL })
  })

  bd <- shiny::reactive({
    tryCatch({
      df() %>% dplyr::filter(n >= input$log_events)
    }, error = \(e) { NULL })
  })

  gc <- shiny::reactive({
    shiny::req(path())
    tryCatch({
      grep("*.txt", dir(path(), full.names = T), value = TRUE) %>% load_gc(bd = bd())
    }, error = \(e) { NULL })
  })

  ms <- shiny::reactive({
    shiny::req(path())
    tryCatch({
      grep("*.dat", dir(path(), full.names = T), value = TRUE) %>% load_ms(bd = bd())
    }, error = \(e) { NULL })
  })

  ## Pages -----------------------------------------------------------------------

  reaction_server('reaction')
  log_server('log', app_state)
  quality_server('quality', app_state)
  raw_server('raw', app_state)
  chem_server('chem', app_state)

  ### Report -------------------------------------------------------------------

  output$report <- shiny::downloadHandler(

    filename = "report.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.qmd")
      file.copy("report.qmd", tempReport, overwrite = TRUE)

      quarto::quarto_render(input = tempReport,
                    execute_params = list(df = df(), bd = bd(), gc = gc(),
                                          ms = ms() %>% dplyr::mutate(time_absolute_date_time = as.character(time_absolute_date_time)),
                                          path = path()))

      file.copy(file.path(tempdir(), 'report.html'), file)
    }
  )
}
