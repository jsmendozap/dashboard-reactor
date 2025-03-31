#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    bs4Dash::dashboardPage(
      help = NULL,
      dark = NULL,
      title = 'Experiment control',
      header = header_ui(),
      sidebar = sidebar_ui(),
      body = body_ui()
    )
  )
}

#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom reactable.extras reactable_extras_dependency
#' @noRd

golem_add_external_resources <- function() {
  addResourcePath("www", app_sys("app/www"))

  tags$head(
    golem::activate_js(),
    golem::favicon(),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css"),
    reactable_extras_dependency()
  )
}

tags$head(
  favicon(),
  bundle_resources(
    path = app_sys("app/www"),
    app_title = "dashboardReactor"
  )
  # Add here other external resources
  # for example, you can add shinyalert::useShinyalert()
)
