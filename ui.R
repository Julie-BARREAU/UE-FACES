#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyjs)
library(ggplot2)
library(base64enc)
library(png)


ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f8f9fa;  /* Couleur de fond de l'application */
      }
      .navbar-default {
        background-color: #ADD8E6;  /* Couleur de fond de la barre de navigation */
      }
      .navbar-default .navbar-nav > li > a {
        font-weight: bold;  /* Mettre le nom des onglets en gras */
        text-align: justify;  /* Justifier le texte dans les onglets */
        text-decoration: underline; /* Souligner en cas de survol */
      }
      .full-width {
        width: 100%;  /* Texte occupe toute la largeur */
      }
      .justified-text {
        text-align: justify; /* Justifier le texte */
      }
      .center-text {
        text-align: center; /*Texte au milieu*/
      }
    "))
  ),
  
  
  titlePanel("Fishery Model"),
  
  navbarPage(
    " ",  # Titre de la barre de navigation
    tabPanel("Introduction",
             tabsetPanel(
               tabPanel("Presentation",
                        br(),
                        h3(class = "center-text",
                           style = "font-size: 40px; color: #96b9d1;",
                           "Bienvenue dans notre application de simulation d'un modele de production de biomasse ! "
                        )
               ),
               
               tabPanel("Contexte",
                        h3(" "),
                        p(class = "justified-text",
                          "En raison de la valeur economique substantielle des pecheries, la dynamique des populations de poissons est l'un des domaines les plus anciens de l'ecologie quantitative des populations."),
                        p(class = "justified-text",
                          "Comprendre la surpeche et reguler les quotas de peche est un enjeu mondial majeur pour le 21eme siecle, tant en termes de nourriture pour l'humanite que pour preserver les ecosystemes des oceans."),
                        p(class = "justified-text",
                          "En effet, les captures de peche jouent un role fondamental dans la gestion des pecheries. Elles influent directement sur la taille et la structure")
                        
               ),
               tabPanel("Notre application",
                        br(),
                        p(class = "justified-text",
                          "Notre application vise a fournir une plateforme interactive pour experimenter un modele de biomasse avec divers scenarios, aidant ainsi les gestionnaires des pecheries a prendre des decisions eclairees pour la preservation des ressources halieutiques."),
                        p(class = "justified-text",
                          "Notre modele integre divers parametres modulables tels que : "),
                        tags$ul(
                          tags$li(class = "justified-text",
                                  "Des parametres sur la dynamique de la population etudiee avec le choix du taux de croissance de l'espece et de la capacite d'accueil du milieu, ainsi que l'incertitude sur ces deux mesures."),
                          tags$li(class = "justified-text",
                                  "Un coefficient de peche correspondant au pourcentage de biomasse que l'on capture par rapport a ce qu'il y a dans la mer."),
                          tags$li(class = "justified-text",
                                  "La variabilite environnementale.")
                        ),
                        p(class = "justified-text",
                          "A partir de ces parametres, nous calculons des trajectoires representant l'evolution de la biomasse."),
                        br(),
                        p(class = "justified-text",
                          "Le Rendement Maximal Durable (RMD), appele en anglais Maximum Sustainable Yield (MSY), est la plus grande quantite de biomasse que l'on peut extraire en moyenne et a long terme d'un stock halieutique dans les conditions environnementales existantes sans affecter le processus de reproduction ."),
                        p(class="justified-text",
                          "Le niveau de biomasse au RMD peut etre calcule de la facon suivante :"
                        ),
                        p(class = "center-text",
                          "K/2"
                        ),
                        br(),
                        p(class="justified-text",
                          "Ainsi, par les trajectoires de biomasse traces selon les differents parametres choisis, on peut decouvrir les strategies optimales pour une peche durable et assurer la perennite des ressources halieutiques."
                        ),
                        
                        br(),
                        p(class = "justified-text",
                          "Utilisez ces outils de simulation dans l'onglet simulation pour comprendre comment les parametres influent sur la dynamique de votre population."),
                        
               )
             )
    )
    
    ,
    
    tabPanel("Simulation",
             fluidPage(
               titlePanel("Simulation"),
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("r", "Taux de croissance intrinseque :", min = -2, max = 2, value = 0.5, step = 0.01),
                   sliderInput("K", "Capacite d'acceuil du milieu :", min = 1, max = 1000, value = 100, step = 1),
                   sliderInput("h", "Coefficient de peche :", min = 0, max = 1, value = 0.2, step = 0.01),
                   sliderInput("sd_r", "Incertitude sur le parametre r :", min = 0, max = 0.2, value = 0.04, step = 0.001),
                   sliderInput("sd_K", "Incertitude sur le parametre K :", min = 0, max = 100, value = 10, step = 1),
                   sliderInput("sd_noise", "Stochasticite environnementale :", min = 0, max = 1, value = 0.1, step = 0.01),
                   sliderInput("n", "Nombre d'annees pour lesquelles on simule :", min = 1, max = 500, value = 100, step = 1),
                   sliderInput("n_traj", "Nombre de trajectoires :", min = 1, max = 500, value = 10, step = 1)
                 ),
                 mainPanel(
                   plotOutput("wormPlot"),
                   plotOutput("boxPlot"),
                   plotOutput("riskPlot")
                 )
               )
             )
    ),
    
    
    tabPanel("Utilisation",
             fluidPage(
               titlePanel("Utilisation de l'application"),
               
               sidebarLayout(
                 sidebarPanel(
                   # Widget pour choisir le type de poisson
                   radioButtons("fishType", "Choisissez l'espèce de poisson que vous souhaitez pecher :",
                                choices = c("Sardines", "Maquereau"),
                                selected = "Sardines"),
                   
                   # Widget pour entrer la valeur de h
                   numericInput("hValue", "Taux de capture de peche (h) :", value = 0.20, min = 0, max = 1, step = 0.001),
                   
                   # Bouton pour lancer la simulation
                   actionButton("runSimulation", "Lancer la simulation")
                 ),
                 
                 mainPanel(
                   # Resultat de la simulation
                   textOutput("simulationResult")
                 )
               )
             )
    ),
    
    tabPanel("Conclusion",
             
             br(),
             p(class="justified-text",
               "Grace aux differents scenarios que vous avez pu realiser, vous avez pu voir que la dynamique d'un stock (sa productivite et son renouvellement) depend des modalites d'exploitation (proportion de capture et effort de peche), mais aussi des parametres biologiques de l'espece et de la variabilite environnementale."
             ),
             p(class="justified-text",
               p("Afin de parvenir a l'objectif consistant a retablir progressivement et a maintenir les populations des stocks halieutiques au-dessus des niveaux de biomasse qui permettent d'obtenir le rendement maximal durable. Nous esperons que des outils, comme cette application simple d'utilisation, pourront aider les decisionnaires et les guider dans l'elaboration des quotas de peche pour assurer un avenir durable pour la peche mondiale.")
             ),
             
             
             
    )
  )
)



