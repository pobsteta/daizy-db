library(shiny)
library(shinydashboard)
library(tibble)
library(pool)
library(rlang)
library(DT)
library(dplyr)
library(rhandsontable)
library(shinysky)
library(gdata)
library(tools)
library(RSQLite)

# augmente les capacités de la taille de la base de données chargées à 200MB
mega <- 200
options(shiny.maxRequestSize=mega*1024^2)
options(encoding = "UTF-8")

source("modules/overview.R", local = TRUE)
source("modules/read.R", local = TRUE)
source("modules/createTige.R", local = TRUE)

con <- list(cc=NULL)


