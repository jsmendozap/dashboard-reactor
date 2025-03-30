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
#' @importFrom dplyr filter select rename row_number left_join rename_with across
#' @importFrom tidyr drop_na fill
#' @importFrom purrr map_df
#' @importFrom tools file_path_sans_ext
#' @importFrom openxlsx readWorkbook
#' @importFrom tibble tibble
#' @noRd

compounds <- c(
  Hydrogen = "#FFA500",
  "Carbon monoxide" = "#90EE90",
  "Carbon dioxide" = "#FF0000",
  Methane = "#0000FF",
  Ethane = "#800080",
  Ethylene = "#FFFF00",
  Propane = "#808080",
  Propylene = "#4169E1",
  "n-butane" = "#FF1493",
  "i-butane" = "#3CB371",
  "Cis-2-butene" = "#9400D3",
  "t-2-butene" = "#7FFF00",
  Nitrogen = "#1E90FF",
  Argon = "#40E0D0",
  Water = "#808000"
) %>%
  data.frame(Compound = names(.), color = ., row.names = NULL)

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
          rename_with(
            .cols = 4:6,
            .fn = ~ sapply(strsplit(.x, " "), `[`, 1)
          ) %>%
          mutate(across(.cols = 4:6, .fn = ~ as.numeric(.x))) %>%
          drop_na(Compound) %>%
          left_join(compounds)

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
        data[, c(1, 7:14, 16)],
        bordered = TRUE,
        highlight = TRUE,
        columns = list(
          color = colDef(
            name = "Color",
            cell = function(value, index) {
              tags$input(
                type = "color",
                value = value,
                id = paste0("color_picker_", index),
                onchange = sprintf(
                  "Shiny.setInputValue('color_changed', {index: %d, color: this.value})",
                  index
                )
              )
            }
          )
        )
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
