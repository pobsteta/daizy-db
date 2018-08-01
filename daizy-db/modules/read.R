readUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = "", width = 6, status = "primary",
        valueBoxOutput("NomDB1"),
        box(title = "Table et colonnes", width = 12, solidHeader = TRUE, status = "primary",
          selectInput(ns("tableName"), "Choisir une table", character(0)),
          checkboxGroupInput(ns("select"), "Choisir les colonnes à afficher")
        ),
        box(title = "Enregistrements (optionel)", width = 12, solidHeader = TRUE, status = "info",
            selectInput(ns("filter"), "Choisir une colonne à filtrer", NULL),
            selectInput(ns("vals"), "Choisir les valeurs à afficher", multiple = TRUE, choices = character(0))
        )
    ),
    busyIndicator(),
    box(rHandsontableOutput(ns("res")), width = 6, status = "success")
  )
}

read <- function(input, output, session, pool, reqTable, reqColInTable) {
  
  observeEvent(tbls(), {
    updateSelectInput(session, "tableName", choices = tbls())
  })
  
  observe({
    reqTable(input$tableName)
    cols <- db_query_fields(pool, input$tableName)
    updateCheckboxGroupInput(session, "select", 
      choices = cols, selected = cols, inline = TRUE)
  })
  
  observe({
    reqTable(input$tableName)
    req(input$select)
    updateSelectInput(session, "filter", choices = input$select)
  })
  
  observe({
    reqColInTable(input$tableName, input$filter)
    df <- as_data_frame(pool %>% tbl(input$tableName) %>% select(input$filter))
    allUniqueVals <- unique(df[[input$filter]])
    updateSelectInput(session, "vals", selected = allUniqueVals, choices = allUniqueVals)
  })
  
  output$res <- renderRHandsontable({
      reqColInTable(input$tableName, input$filter)
      filterVar <- sym(input$filter)
      vals <- input$vals
      res <- pool %>% tbl(input$tableName) %>% select(input$select) %>%
        filter(filterVar %in% vals) %>% collect()
      rhandsontable(res, readOnly = TRUE, width = 880, height =800) %>%
          hot_cols(rowHeaderWidth = 100) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
}
