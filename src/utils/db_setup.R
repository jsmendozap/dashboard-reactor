db_setup <- function(){
  con <- duckdb::dbConnect(drv = duckdb::duckdb(),
                        dbdir = "~/reactor.duckdb",
                        read_only = FALSE
                      )

  if(!duckdb::dbExistsTable(con, "reactions")) {

    DBI:::dbExecute(con, "
      CREATE SEQUENCE reactions_id START 1;
    ")

    DBI::dbExecute(con, "
      CREATE TABLE reactions (
        ID INTEGER PRIMARY KEY DEFAULT NEXTVAL('reactions_id'),
        name VARCHAR
      )
    ")
  }

  if(!duckdb::dbExistsTable(con, "settings")) {

    fields <- c("ID" = "INTEGER", "Compound" = "VARCHAR", "Type" = "VARCHAR",
                "MFC_1" = "INTEGER", "MFC_2" = "INTEGER", "MFC_3" = "INTEGER",
                "MFC_4" = "INTEGER", "reaction" = "VARCHAR")

    DBI::dbCreateTable(conn = con,
                       name = "settings",
                       fields = fields)
  }

  duckdb::dbDisconnect(conn = con)
}
