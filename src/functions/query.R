query <- function(operation, name = NULL, value = NULL){
  con <- duckdb::dbConnect(drv = duckdb::duckdb(),
                        dbdir = "~/reactor.duckdb",
                        read_only = FALSE
                      )

  switch (operation,
    append = duckdb::dbAppendTable(con, name, value),
    list = duckdb::dbListTables(con) %>% print(),
    head = dplyr::tbl(con, name) %>% head() %>% print()
  )

  duckdb::dbDisconnect(conn = con)
}
