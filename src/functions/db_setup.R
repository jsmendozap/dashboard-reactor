db_setup <- function(){
  con <- duckdb::dbConnect(drv = duckdb::duckdb(),
                        dbdir = "~/reactor.duckdb",
                        read_only = FALSE
                      )

  if(!duckdb::dbExistsTable(con, "settings")) {
    DBI::dbCreateTable(conn = con,
                       name = "settings",
                       fields = c(ID = "numeric",
                                  name = "character"))
  }

  duckdb::dbDisconnect(conn = con)
}
