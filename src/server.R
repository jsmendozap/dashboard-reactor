options(shiny.maxRequestSize=30*1024^2)

server <- function(input, output) {

  app_state <- shiny::reactiveValues()

  shiny::observe({
    app_state$df <- df
    app_state$bd <- bd
    app_state$gc <- gc
    app_state$ms <- ms
    app_state$path <- path
    app_state$setting <- setting
  }) %>% shiny::bindEvent(c(df(), bd(), gc(), ms(), path(), setting()))

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
      tools::list_files_with_exts(dir = path(), exts = "xlsx") %>% load_file
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
      tools::list_files_with_exts(dir = path(), exts = "txt") %>% load_gc(bd = bd())
    }, error = \(e) { NULL })
  })

  ms <- shiny::reactive({
    shiny::req(path())
    tryCatch({
      tools::list_files_with_exts(dir = path(), exts = "dat") %>% load_ms(bd = bd())
    }, error = \(e) { NULL })
  })

  ## Pages -----------------------------------------------------------------------

  setting <- reaction_server('reaction')
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
                                                  ms = tryCatch({
                                                    ms() %>%
                                                      dplyr::mutate(time_absolute_date_time = as.character(time_absolute_date_time))
                                                  }, error = \(e) NULL),
                                                  path = path()))

      file.copy(from = file.path(tempdir(), 'report.html'),
                to = file.path(path(), 'report.html'))
    }
  )

  ### timeout -------------------------------------------------------------------

  timeout <- shiny::reactiveTimer(10000)
  shiny::observeEvent(timeout, "App session active\n")

}
