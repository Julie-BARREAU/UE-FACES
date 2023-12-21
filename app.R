#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/



library(shiny)
library(shinythemes)
library(shinyjs)
library(ggplot2)
library(base64enc)


# Charger le fichier UI
source("ui.R")

# Charger le fichier server
source("server.R")


# Lancer l'application Shiny
shinyApp(ui = ui, server = server)

