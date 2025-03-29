#' Catalyst Module UI
#'
#' @description Creates the UI components for the catalyst characterization module in the dashboard.
#'
#' @param id A string. The namespace ID for the module, used to create unique input/output IDs.
#'
#' @return A `tabItem` containing the UI elements for catalyst characterization.
#'
#' @importFrom shiny NS tagList fluidRow textAreaInput reactive reactiveVal observeEvent
#' @importFrom bs4Dash tabItem box column
#' @importFrom excelR excelOutput
#' @noRd

mod_catalyst_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = 'catalyst',
    fluidRow(
      box(
        title = "Catalyst characterization",
        width = 12,
        status = 'info',
        solidHeader = TRUE,
        fluidRow(
          column(
            width = 6,
            textAreaInput(
              inputId = ns("md"),
              label = "Description of the material"
            )
          ),
          column(
            width = 6,
            textAreaInput(
              inputId = ns("pd"),
              label = "Preparation description"
            )
          )
        ),
        excelOutput(ns('cc'), height = "100%")
      ),
      box(
        title = "Fresh catalyst characterization",
        width = 12,
        status = 'info',
        solidHeader = TRUE,
        collapsed = TRUE,
        excelOutput(ns('fcc'), width = "100%", height = "100%")
      ),
      box(
        title = "Spent catalyst characterization",
        width = 12,
        status = 'info',
        solidHeader = TRUE,
        collapsed = TRUE,
        excelOutput(ns('scc'), width = "100%", height = "100%")
      )
    )
  )
}

#' Catalyst Module Server
#'
#' @description Defines the server-side logic for the catalyst characterization module in the dashboard.
#'
#' @param id A string. The namespace ID for the module, used to create unique input/output IDs.
#'
#' @return A list of reactive values containing the material description, preparation description,
#' and data from the catalyst characterization tables.
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent
#' @importFrom excelR renderExcel excelTable excel_to_R
#' @importFrom purrr iwalk
#' @noRd

mod_catalyst_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Catalyst characterization table
    output$cc <- renderExcel({
      col <- data.frame(
        title = c(
          "--",
          "IUPAC name",
          "Nominal load (mmol*g-1)",
          "Nominal load (%w/w)",
          "Experimental load (mmol*g-1)",
          "Experimental load (%w/w)"
        ),
        width = c(400, 800, 800, 800, 800, 1200),
        type = rep("text", 6)
      )

      data <- data.frame(
        a = c("Metal 1", "Metal 2", "Metal 3"),
        b = NA,
        c = NA,
        d = NA,
        e = NA,
        f = NA
      )

      excelTable(data, columns = col)
    })

    # Fresh catalyst characterization table
    output$fcc <- renderExcel({
      columns <- data.frame(
        title = c("Property", "Value", "Unit", "Technique", "Observation"),
        width = c(300, 150, 150, 200, 250),
        type = rep("text", 5)
      )

      data <- data.frame(
        Property = c(
          "Humedity (%)",
          "Particle size",
          "Surface Area",
          "Micropore Volume",
          "Mesopore Volume",
          "Pore volume",
          "Acidity 1",
          "Acidity 2",
          "Acidity 3",
          "Acidity 4",
          "Redox 1",
          "Redox 2",
          "Redox 3",
          "Redox 4",
          "Other 1",
          "Other 2",
          "Other 3",
          "Other 4",
          "Other 5"
        ),
        Value = rep("", 19),
        Unit = c("%", "", "m2/g", "cm3/g", "cm3/g", "cm3/g", rep("", 13)),
        Technique = rep("", 19),
        Observation = rep("", 19)
      )

      excelTable(data = data, columns = columns)
    })

    # Spent catalyst characterization table
    output$scc <- renderExcel({
      columns <- data.frame(
        title = c("Property", "Value", "Unit", "Technique", "Observation"),
        width = c(300, 150, 150, 200, 250),
        type = rep("text", 5)
      )

      data <- data.frame(
        Property = c(
          "Humedity (%)",
          "Particle size",
          "Surface Area",
          "Micropore Volume",
          "Mesopore Volume",
          "Pore volume",
          "Acidity 1",
          "Acidity 2",
          "Acidity 3",
          "Acidity 4",
          "Redox 1",
          "Redox 2",
          "Redox 3",
          "Redox 4",
          "Coke",
          "Soft coke",
          "Hard coke",
          "Other 1",
          "Other 2"
        ),
        Value = rep("", 19),
        Unit = c("%", "", "m2/g", "cm3/g", "cm3/g", "cm3/g", rep("", 13)),
        Technique = rep("", 19),
        Observation = rep("", 19)
      )

      excelTable(data = data, columns = columns)
    })

    # Reactive values for catalyst data
    cc <- reactiveVal(NULL)
    fcc <- reactiveVal(NULL)
    scc <- reactiveVal(NULL)

    # Observe changes in Excel inputs
    iwalk(
      .x = list(cc = cc, fcc = fcc, scc = scc),
      .f = \(reactive_var, name) {
        observeEvent(input[[name]], {
          reactive_var(excel_to_R(input[[name]]))
        })
      }
    )

    return(
      list(
        material = reactive(input$md),
        preparation = reactive(input$pd),
        cc = cc,
        fcc = fcc,
        scc = scc
      )
    )
  })
}
