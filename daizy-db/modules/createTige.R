createtigeUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = "", width = 5, status = "primary",
        valueBoxOutput("NomDB3"),
        box(title = "Critères de création des tiges", width = 12, solidHeader = TRUE, status = "info",
            numericInput(ns("nbtige"), "Nombre de tiges à créer :", 10, min = 1, max = 1000000),
            selectInput(ns("essences"), "Choisir les essences à créer", multiple = TRUE, choices = c('Choisir les essences' = '')),
            selectInput(ns("diametre"), "Choisir les classes de diamètre min et max à créer", multiple = TRUE, choices = c('Choisir les diametres' = '')),
            selectInput(ns("hauteur"), "Choisir les hauteurs min et max à créer", multiple = TRUE, choices = c('Choisir les hauteurs' = '')),
            numericInput(ns("nombre"), "Choisir le nombre de tige maximum à créer par enregistrement", 1, min = 1, max = 1000),
            selectInput(ns("quala"), "Choisir les qualités arbre à créer", multiple = TRUE, choices = c('Choisir les qualités arbre' = '')),
            selectInput(ns("qualb"), "Choisir les qualités bois à créer", multiple = TRUE, choices = c('Choisir les qualités bois' = '')),
            actionButton(ns("createtige"), "1-Création des tiges", icon("refresh"), class = "btn btn-primary"),
            actionButton(ns("sauvetige"), "2-Sauvegarde des tiges", icon("refresh"), class = "btn btn-primary"),
            downloadButton(ns("sauvebdd"), "3-Enregistrement de la base de données modifiée", class = "btn btn-primary")
        )
    ),
    busyIndicator(),
    box(title = "", width = 7, status = "success",
        box(title = "FDP", width = 12, solidHeader = TRUE, status = "info",
            selectInput(ns("tableFDP"), "Choisir la FDP à modifier", choices = c('Choisir la FDP à modifier' = '')),
            box(title = "FDP sélectionnée", status = "primary", solidHeader = TRUE, width = 12, tableOutput(ns("currentfdp"))),
            box(title = "Lot(s) présent(s) dans la FDP sélectionnée - Cliquez sur le lot désiré", status = "primary", solidHeader = TRUE, width = 12, DT::dataTableOutput(ns("currentfdplot"))),
            verbatimTextOutput(ns("curlot")),
        box(title = "Table Tige", solidHeader = TRUE, rHandsontableOutput(ns("restige")), width = 12, status = "success")
        )
    )
  )
}

createtige <- function(input, output, session, pool, reqTable, reqColInTable) {
  
  observeEvent(tbls(), {
    dffiche <<- as_data_frame(pool %>% tbl("Fiche"))
    allUniqueValsFiche <- unique(dffiche[["IdNatEA"]])
    updateSelectInput(session, "tableFDP", choices = c('Choisir la FDP à modifier' = '', allUniqueValsFiche))
  })
  
  observe({
    dfess <<- as_data_frame(pool %>% tbl("Essence"))
    allUniqueValsEss <- unique(dfess[["LibelleCourt"]])
    updateSelectInput(session, "essences", choices = c('Choisir les essences' = '', allUniqueValsEss), selected = 'CHP')
    dfdiam <<- as_data_frame(pool %>% tbl("ElementRef")) %>% dplyr::filter(grepl('CD',IdNatElement))
    allUniqueValsDiam <- unique(dfdiam[["LibelleCourt"]])
    updateSelectInput(session, "diametre", choices = c('Choisir les diametres' = '', allUniqueValsDiam), selected = '10')
    dfhaut <<- as_data_frame(pool %>% tbl("ElementRef")) %>% dplyr::filter(grepl('CL',IdNatElement))
    allUniqueValsHaut <- unique(dfhaut[["LibelleCourt"]])
    updateSelectInput(session, "hauteur", choices = c('Choisir les hauteurs' = '', allUniqueValsHaut), selected = '4')
    dfquala <<- as_data_frame(pool %>% tbl("ElementRef")) %>% dplyr::filter(grepl('QA',IdNatElement))
    allUniqueValsQualA <- unique(dfquala[["LibelleCourt"]])
    updateSelectInput(session, "quala", choices = c('Choisir les qualités arbre' = '', allUniqueValsQualA))
    dfqualb <<- as_data_frame(pool %>% tbl("ElementRef")) %>% dplyr::filter(grepl('QB',IdNatElement))
    allUniqueValsQualB <- unique(dfqualb[["LibelleCourt"]])
    updateSelectInput(session, "qualb", choices = c('Choisir les qualités bois' = '', allUniqueValsQualB))
    dfea <<- as_data_frame(pool %>% tbl("EA"))
    dftige <<- as_data_frame(pool %>% tbl("Tige"))
  })
  
  observeEvent(input$createtige, {
    if (input$tableFDP == '') {showModal(
      modalDialog(
        title = "!!! CREATION DES TIGES IMPOSSIBLE !!!",
        "Aucune FDP n'a été sélectionnée ! Choisissez une FDP dans la liste déroulante." ,
        footer = modalButton("OK")
    ))} else if (is.null(input$currentfdplot_rows_selected)) {showModal(
      modalDialog(
        title = "!!! CREATION DES TIGES IMPOSSIBLE !!!",
        "Aucun Lot n'a été sélectionné ! Choisissez un Lot dans la liste des lots." ,
        footer = modalButton("OK")
      ))}
  })
  
  resulttige <- eventReactive(input$createtige, {
    res <- data_frame(
      IdTige=seq(1:input$nbtige),
      IdLot=clot,
      IdPlacette=as.integer(1),
      ModeCompteur=as.integer(0),
      StatutDistribution=as.integer(0),
      IdNatEssence=sample(dfess %>% filter(LibelleCourt %in% input$essences) %>% select(IdNatEss) %>% pull(), input$nbtige, rep=TRUE),
      IdNatDiametre=sample(dfdiam %>% filter(LibelleCourt %in% input$diametre) %>% select(IdNatElement) %>% pull(), input$nbtige, rep=TRUE),
      IdNatHauteur=sample(dfhaut %>% filter(LibelleCourt %in% input$hauteur) %>% select(IdNatElement) %>% pull(), input$nbtige, rep=TRUE),
      IdNatQualiteArbre=sample(c(dfquala %>% filter(LibelleCourt %in% input$quala) %>% select(IdNatElement) %>% pull(), ''), input$nbtige, rep=TRUE),
      IdNatHauteurBois='',
      IdNatQualiteBois='',
      NbTiges=sample(1:input$nombre,input$nbtige, rep=TRUE),
      Observation='',
      IdNatPredestination='DE0121Q',
      IdNatModeMarquage='MM0001A' ,
      IdNatPredestinationHouppier='DE0121Q',
      IdNatPredestinationMenuBois='DE0121Q',
      IdNatUG=sample(dfea %>% filter(IdNatEA %in% input$tableFDP) %>% select(IdNatUG) %>% pull(), input$nbtige, rep=TRUE),
      IdNatParcelle=sample(dfea %>% filter(IdNatEA %in% input$tableFDP) %>% select(IdNatParcelle) %>% pull(), input$nbtige, rep=TRUE),
      Reserve=as.logical(0),
      Biologique=as.logical(0),
      IdNatReserveBio='',
      IdNatReserveNonBio='',
      OrdreSaisie='',
      IdTDS='9b710a4703dda91d',
      IdNatTarifRetenu='',
      HauteurRetenue='',
      CoeffHouppierRetenu='',
      CoeffMenuBoisRetenu='',
      DateCreation=toString(as.numeric(difftime(Sys.time(), as.Date("0001-01-01"), units = "secs"))*10^7)
    )
  })
  
  output$restige <- renderRHandsontable({
    rhandsontable(resulttige(), readOnly = TRUE, width = 780, height =300) %>%
      hot_cols(rowHeaderWidth = 100) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
  
  output$currentfdp <- renderTable({
    res <- pool %>% tbl("Fiche")  %>% dplyr::filter(IdNatEA == input$tableFDP) %>% select(IdFiche, IdNatEA, Principale, StatutFiche, StatutDistribution, DateCreation, DateTransfert)
    currentfdp <<- as_data_frame(res)
    res
  })

  output$currentfdplot <- DT::renderDataTable({
    idfiche <- pool %>% tbl("Fiche") %>% dplyr::filter(IdNatEA == input$tableFDP) %>% select(IdFiche) %>% pull()
    if (length(idfiche) >= 1) {
      res <- pool %>% tbl("Lot") %>% 
        dplyr::filter(IdFiche == idfiche) %>% select(IdLot, IdLotEA, IdFiche, Ordre, Nom, StatutLot, Surface, PlacetteRef, SurfacePlacette)
      currentfdplot <<- as.data.frame(res)
    } else {
      NULL
    }
  },
  selection=list(mode="single"))
  
  output$curlot = renderPrint({
    s <- input$currentfdplot_rows_selected
    clot <<- currentfdplot[s, 1] 
    if (length(s)) {
      cat('Vous avez sélectionné le lot d\'idLot: ')
      cat(clot, sep = ', ')
    }
  })
  
  observeEvent(input$sauvetige, {
    if (!is.null(input$restige)) {
      # liste les noms des champs de la table Tige
      fields <- pool %>% tbl("Tige") %>% head %>% collect %>% lapply(type_sum) %>% unlist
      fields <- fields[2:length(fields)]
      # liste les valeurs affectees à chaque attribut dans le dataframe res et les affecte les nouvelles valeurs
      entryValues <- data.frame(stringsAsFactors = FALSE, lapply(fields, type.convert))
      for (i in 1:input$nbtige) {
        for (name in names(entryValues)) {
          entryValues[name] <- resulttige()[i, name]
        }
        if (!is.na(entryValues[[1]])) dbWriteTable(pool,"Tige", entryValues, append = TRUE)
      }
      
      
      # dbWriteTable(pool, "Tige", hot_to_r(input$restige), overwrite = TRUE, row.names = FALSE)
      showModal(
        modalDialog(
          title = "!!! TIGES INSÉRÉES !!!",
          paste("Les tiges ont été enregistrées dans la base de données", pathdb, "dans la table 'Tige'."),
          easyClose = TRUE,
          footer = NULL
        ))
    } else {
      showModal(
        modalDialog(
          title = "!!! TIGES PAS INSÉRÉES !!!",
          "Les tiges n'ont pas été enregistrées dans la base de données." ,
          easyClose = TRUE,
          footer = NULL
        ))
    }
  })
  
  # save data
  output$sauvebdd <- downloadHandler( 
    # Nom par défaut :
    filename = function() {
      paste0(tools::file_path_sans_ext(nomdb), "_new.db3")
    },
    content = function(file) {
      file.copy(from = pathdb, to = file, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
    }
  )
  
}
