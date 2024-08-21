query <- function(operation, name = NULL, value = NULL){
  con <- duckdb::dbConnect(drv = duckdb::duckdb(),
                        dbdir = "~/reactor.duckdb",
                        read_only = FALSE
                      )

  res <- switch (operation,
    insert = duckdb::dbAppendTable(con, name, value),
    list = {
      statement <- stringr::str_glue('SELECT "{value}" FROM {name}')
      DBI::dbGetQuery(con, statement) %>% as.vector %>% unlist %>% unname
    },
    exist = duckdb::dbExistsTable(con, name),
    head = {
      statement <- stringr::str_glue('SELECT * FROM {name} WHERE ("reaction" = \'{value}\')')
      DBI::dbGetQuery(con, statement) 
    },
    tables = duckdb::dbListTables(con) %>% print()
  )
  
  duckdb::dbDisconnect(conn = con)
  
  return(res)
}
