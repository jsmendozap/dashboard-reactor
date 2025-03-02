options(shiny.maxRequestSize = 30 * 1024^2)

server <- function(input, output) {
  app_state <- shiny::reactiveValues()

  shiny::observe({
    app_state$df <- df
    app_state$bd <- bd
    app_state$gc <- gc
    app_state$ms <- ms
    app_state$path <- path
    app_state$setting <- setting
    app_state$comment <- comment
  }) %>%
    shiny::bindEvent(c(df(), bd(), gc(), ms(), path(), setting(), comment()))

  volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())
  shinyFiles::shinyDirChoose(input, "directory", roots = volumes)

  path <- shiny::eventReactive(input$directory, {
    shinyFiles::parseDirPath(volumes, input$directory)
  })

  output$dir_header <- shiny::renderText({
    path() %>%
      as.character %>%
      strsplit('/') %>%
      unlist %>%
      {
        paste("Folder:", .[length(.)])
      }
  })

  df <- shiny::reactive({
    shiny::req(path())
    tryCatch(
      {
        fs::path(path(), "R.xlsx") %>% load_file
      },
      error = \(e) {
        NULL
      }
    )
  })

  bd <- shiny::reactive({
    tryCatch(
      {
        df() %>% dplyr::filter(n >= input$log_events)
      },
      error = \(e) {
        NULL
      }
    )
  })

  gc <- shiny::reactive({
    shiny::req(path())
    tryCatch(
      {
        fs::path(path(), "GC.txt") %>% load_gc(bd = bd())
      },
      error = \(e) {
        NULL
      }
    )
  })

  ms <- shiny::reactive({
    shiny::req(path())
    tryCatch(
      {
        fs::path(path(), "MS.dat") %>% load_ms(bd = bd())
      },
      error = \(e) {
        NULL
      }
    )
  })

  ## Pages -----------------------------------------------------------------------

  setting <- reaction_server('reaction')
  catalyst_char <- catalyst_server('catalyst')
  comment <- log_server('log', app_state)
  chemometric <- chem_server('chem', app_state)
  quality_server('quality', app_state)
  raw_server('raw', app_state)

  shiny::observe({
    app_state$chem_values <- chemometric$chem_values
    app_state$molar_flow <- chemometric$molar_flow
    app_state$conversion <- chemometric$conversion
    app_state$mass_balance <- chemometric$mass_balance
    app_state$boxplot <- chemometric$boxplot
    app_state$material <- catalyst_char$material
    app_state$preparation <- catalyst_char$preparation
    app_state$cc <- catalyst_char$cc
    app_state$fcc <- catalyst_char$fcc
    app_state$scc <- catalyst_char$scc
  })

  ### Report -------------------------------------------------------------------

  output$report <- shiny::downloadHandler(
    filename = "report.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.qmd")
      file.copy("report.qmd", tempReport, overwrite = TRUE)

      quarto::quarto_render(
        input = tempReport,
        execute_params = list(
          bd = if (is.null(app_state$comment())) {
            bd() %>% dplyr::mutate(comment = "")
          } else {
            bd() %>% dplyr::left_join(app_state$comment())
          },
          df = df(),
          gc = gc(),
          ms = tryCatch(
            {
              ms() %>%
                dplyr::mutate(
                  time_absolute_date_time = as.character(
                    time_absolute_date_time
                  )
                )
            },
            error = \(e) NULL
          ),
          path = path(),
          material = app_state$material(),
          preparation = app_state$preparation(),
          cc = app_state$cc(),
          fcc = app_state$fcc(),
          scc = app_state$scc(),
          chem_values = app_state$chem_values(),
          molar_flow = app_state$molar_flow(),
          conversion = app_state$conversion(),
          mass_balance = app_state$mass_balance(),
          boxplot = app_state$boxplot()
        )
      )

      filename <- path() %>%
        as.character %>%
        strsplit('/') %>%
        unlist %>%
        .[length(.)]
      file.copy(
        from = file.path(tempdir(), 'report.html'),
        to = file.path(path(), paste0(filename, '.html'))
      )
    }
  )

  ### timeout -------------------------------------------------------------------

  timeout <- shiny::reactiveTimer(10000)
  shiny::observeEvent(timeout, "App session active\n")
}
