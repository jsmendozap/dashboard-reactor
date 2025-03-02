catalyst_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- shiny::NS(id)

    output$cc <- excelR::renderExcel({
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

      excelR::excelTable(data, columns = col)
    })

    output$fcc <- excelR::renderExcel({
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

      excelR::excelTable(
        data = data,
        columns = columns
      )
    })

    output$scc <- excelR::renderExcel({
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

      excelR::excelTable(
        data = data,
        columns = columns
      )
    })

    cc <- shiny::reactiveVal(NULL)
    fcc <- shiny::reactiveVal(NULL)
    scc <- shiny::reactiveVal(NULL)

    purrr::iwalk(
      .x = list(cc = cc, fcc = fcc, scc = scc),
      .f = \(reactive_var, name) {
        shiny::observeEvent(input[[name]], {
          reactive_var(excelR::excel_to_R(input[[name]]))
        })
      }
    )

    return(
      list(
        material = shiny::reactive(input$md),
        preparation = shiny::reactive(input$pd),
        cc = cc,
        fcc = fcc,
        scc = scc
      )
    )
  })
}
