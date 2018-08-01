overviewUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = "", width = 12, status = "primary",
        valueBoxOutput("NomDB2"),
        valueBoxOutput("NbTableDB"),
        valueBoxOutput("TailleDB")),
    h3("Tables présentes dans la base de données"),
    uiOutput(ns("tables"))
  )
}

overview <- function(input, output, session, pool) {
  output$tables <- renderUI({
    all_tables <- tbls()
    bullets <- list()
    for (i in seq_len(length(all_tables))) {
      tblName <- all_tables[[i]]
      fieldNames <- db_query_fields(pool, tblName)
      nRows <- db_query_rows(pool, tblName)
      bullets[[i]] <- tags$li(paste0(
        all_tables[[i]], ": ", nRows, " enregistrements. Noms des champs: ", 
        paste(fieldNames, collapse = ", "), "."
      ))
    }
    tags$ul(bullets)
  })
}

