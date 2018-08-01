server <- function(input, output, session) {
  goHome <- function() updateTabItems(session, "tabs", "overview")
  
  output$menu <- renderMenu({
    if (!is.null(input$datadb)) {
      my_list = list(
        menuItem("Vue générale", tabName = "overview"), 
        menuItem("Lecture des tables", tabName = "read"),
        menuItem("Création de tiges", tabName = "createtige")
        
        )
      sidebarMenu(my_list)
    }
  })
  
  # fichier des data de la BDD
  filedata <- reactive({
    datafile <- input$datadb
    if (is.null(datafile)) return(NULL)
    pathdb <<- datafile$datapath
    nomdb <<- datafile$name
    output$NomDB1 <<- renderValueBox({
      valueBox(value = "Nom de la BDD :", subtitle = nomdb , icon = NULL, color = "green", width = 6)
    })
    output$NomDB2 <<- renderValueBox({
      valueBox(value = "Nom de la BDD :", subtitle = nomdb , icon = NULL, color = "green", width = 4)
    })
    output$NomDB3 <<- renderValueBox({
      valueBox(value = "Nom de la BDD :", subtitle = nomdb , icon = NULL, color = "green", width = 4)
    })
    tailledb <<- gdata::humanReadable(file.size(datafile$datapath), standard = "SI", units = "MB")
    output$TailleDB <<- renderValueBox({
      valueBox(value = "Taille de la BDD :", subtitle = tailledb , icon = NULL, color = "red", width = 4)
    })
    pool <<- dbPool(RSQLite::SQLite(), dbname = pathdb)
    con$cc <<- pool$minSize
    tbls <<- reactiveFileReader(500, NULL, pathdb,
                               function(x) db_list_tables(pool)
    )
    all_tables <- tbls()
    output$NbTableDB <<- renderValueBox({
      valueBox(value = "Nombre total de table :", subtitle = paste(length(all_tables), " tables présentes en base de données") , icon = NULL, color = "blue", width = 4)
    })
    reqTable <- function(tableName) {
      tbls()
      req(tableName)
      req(tableName %in% db_list_tables(pool))
    }
    
    reqColInTable <- function(tableName, colName) {
      reqTable(tableName)
      req(colName)
      req(colName %in% db_query_fields(pool, tableName))
    }
    
    callModule(overview, "overview-module", pool)
    callModule(read, "read-module", pool, reqTable, reqColInTable)
    callModule(createtige, "createtige-module", pool, reqTable, reqColInTable)
    datafile
  })
  
  observeEvent(input$datadb, {
    filedata()
  })
}
