ui <- dashboardPage(
  dashboardHeader(title = "DAIZY DB ONF"),
  dashboardSidebar(
    fileInput(
         "datadb",
         "Choisir la Base De Données",
         accept = c(".db3")
    ),
    sidebarMenuOutput("menu")
  ),
  dashboardBody(
    tabItems(
      tabItem("overview", overviewUI("overview-module")),
      tabItem("read", readUI("read-module")),
      tabItem("createtige", createtigeUI("createtige-module"))
    )
  )
)
