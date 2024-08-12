reaction_ui <- function(id){

  ns <- shiny::NS(id)
  choices <- c("Reactant", "Product", "IS")

  compounds <- tibble::tribble(
    ~name,              ~formula,             ~weight,       ~carbon,        ~hydrogen,      ~oxygen,
    "Hydrogen",            "H2",                 2,              0L,              2L,            0L,
    "Carbon monoxide",     "CO",                 28.01,          1L,              0L,            1L,
    "Carbon dioxide",      "CO2",                44.01,          1L,              0L,            2L,
    "Methane",             "CH4",                16.04,          1L,              4L,            0L,
    "Ethane",              "C2H6",               30.07,          2L,              2L,            0L,
    "Ethylene",            "C2H4",               28.05,          2L,              2L,            0L,
    "Propane",             "C3H8",               44.097,         3L,              3L,            0L,
    "Propylene",           "C3H6",               42.081,         3L,              3L,            0L,
    "n-butane",            "n-C4H10",            58.012,         4L,              4L,            0L,
    "i-butane",            "iso-C4H10",          58.012,         4L,              4L,            0L,
    "Cis-2-butene",        "c-C4H8",             56.1,           4L,              4L,            0L,
    "t-2-butene",          "t-C4H8",             56.1,           4L,              4L,            0L,
    "Nitrogen",            "N2",                 28.0134,        0L,              0L,            0L,
    "Argon",               "Ar",                 39.948,         0L,              0L,            0L,
    "Water",               "H2O",                18.015,         0L,              0L,            1L
  )

  bs4Dash::tabItem(tabName = 'reaction',
          bs4Dash::tabBox(width = 12,
                shiny::tabPanel(title = "Create reaction",
                        shiny::fluidRow(
                          shiny::div(
                            bs4Dash::column(width = 4, shinyWidgets::pickerInput(ns('compound'), 'Compound', compounds$name,
                                                                                 options = list(`actions-box` = TRUE, `live-search` = TRUE))),
                            bs4Dash::column(width = 2, shinyWidgets::pickerInput(ns('c_type'), 'Type', choices)),
                            bs4Dash::column(width = 1, shiny::numericInput(ns("mfc1"), "MFC 1", value = NA)),
                            bs4Dash::column(width = 1, shiny::numericInput(ns("mfc2"), "MFC 2", value = NA)),
                            bs4Dash::column(width = 1, shiny::numericInput(ns("mfc3"), "MFC 3", value = NA)),
                            bs4Dash::column(width = 1, shiny::numericInput(ns("mfc4"), "MFC 4", value = NA)),
                            bs4Dash::column(width = 2, bs4Dash::actionButton(ns("new"), "Add row")),
                            style = "display: flex; align-items: center"
                          ),
                          reactable::reactableOutput(ns('compounds_table')),
                        ),
                        shiny::br(),
                        shinyWidgets::searchInput(
                          inputId = ns("save"),
                          label = "Save reaction",
                          placeholder = "Reaction name",
                          btnSearch = shiny::icon("floppy-disk"),
                          width = "450px"
                        )
                ),
                shiny::tabPanel(title = "Load reaction",
                        shiny::fluidRow(
                          bs4Dash::column(width = 3, 
                                          shiny::uiOutput(ns("reactions_table"))
                          ),
                          bs4Dash::column(width = 9, 
                                          reactable::reactableOutput(ns('setting'))
                          )
                        )        
                )
          )
        )
}
