#' Reaction Module UI
#'
#' @description Creates the UI components for the reaction module in the dashboard. This module allows users to view available reactions and their details.
#'
#' @param id A string. The namespace ID for the module, used to create unique input/output IDs.
#'
#' @return A `tabItem` containing the UI elements for the reaction module.
#'
#' @importFrom shiny NS fluidRow uiOutput tags
#' @importFrom bs4Dash tabItem box
#' @importFrom reactable reactableOutput
#' @noRd

mod_reaction_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = 'reaction',
    fluidRow(
      box(
        title = "Available reactions",
        width = 12,
        status = 'info',
        solidHeader = TRUE,
        reactableOutput(ns("setting")),
        tags$style(HTML(
          "
          .reactable-details {
            padding: 15px;
            background-color: #f8f9fa;
            border-top: 2px solid #dee2e6;
          }
          "
        ))
      )
    )
  )
}

#' Reaction Module Server
#'
#' @description Defines the server-side logic for the reaction module. This module processes and displays reaction data from Excel files.
#'
#' @param id A string. The namespace ID for the module.
#'
#' @return A reactive object containing the selected reaction's details.
#'
#' @importFrom shiny moduleServer reactive req observeEvent
#' @importFrom reactable reactable renderReactable colDef getReactableState
#' @importFrom janitor clean_names
#' @importFrom dplyr filter select rename row_number
#' @importFrom tidyr drop_na
#' @importFrom purrr map_df
#' @importFrom tools file_path_sans_ext
#' @importFrom openxlsx readWorkbook
#' @importFrom tibble tibble
#' @noRd

mod_reaction_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive to load and process reaction data
    reaction_data <- reactive({
      files <- list.files(
        path = "~/Reactor Dashboard/Reactions",
        full.names = TRUE,
        pattern = "\\.xlsx$"
      )

      lapply(files, function(file) {
        res <- readWorkbook(file)

        details <- res %>%
          filter(row_number() > 7) %>%
          select(2:16) %>%
          setNames(res[7, 2:16]) %>%
          rename("Compound" = 1) %>%
          drop_na(Compound)

        list(
          name = file_path_sans_ext(basename(file)),
          description = res[1, 3],
          details = details
        )
      })
    })

    # Function to create a details table for nested rows
    create_details_table <- function(data) {
      reactable(
        data[, c(1, 7:14)],
        bordered = TRUE,
        highlight = TRUE
      )
    }

    # Render the main reactable table
    output$setting <- renderReactable({
      req(reaction_data())

      main_data <- map_df(
        reaction_data(),
        ~ tibble(
          name = .x$name,
          description = .x$description
        )
      )

      reactable(
        main_data,
        columns = list(
          name = colDef(name = "Reaction name"),
          description = colDef(name = "Reaction description")
        ),
        details = function(index) {
          create_details_table(reaction_data()[[index]]$details)
        },
        rowStyle = list(cursor = "pointer"),
        searchable = TRUE,
        selection = 'single',
        onClick = "select"
      )
    })

    # Reactive value to store the selected reaction's details
    selected_reaction <- reactiveVal(NULL)
    observeEvent(getReactableState("setting", "selected"), {
      sel <- getReactableState("setting", "selected")
      req(sel)

      selected_reaction(reaction_data()[[sel]]$details %>% clean_names())
    })

    return(selected_reaction)
  })
}
