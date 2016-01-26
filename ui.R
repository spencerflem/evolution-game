library(shinydashboard)
library(shinyjs)

header <- dashboardHeader(title = "Evolution!")

sidebar <- dashboardSidebar(
  #MAIN GAME
  textOutput("creature"),
  sidebarMenu(
    id = "selectedEvo",
    sidebarMenuOutput("evos")
  ),
  actionButton("confirmed","Confirm Evolution"),
  
  #STARTUP
  h3(id = "heading", "CREATE YOUR CREATURE!"),
  selectInput("class","Class",choices = c("carnivore","omnivore","herbivore")),
  selectInput("race","Race",choices = c("big","average","small")),
  textInput("name","Name", value = "test"),
  actionButton("joined","Create Creatrue"),
  
  #HIDDEN
  numericInput("ID","unset", value = 0)
)

body <- dashboardBody(
  
  useShinyjs(),
  
  tabBox(
    width = 12,
    tabPanel(
      "Your Creature",
      textOutput("yourCreature")
    ),
    tabPanel(
      "Other Creatures",
      dataTableOutput("otherCreatures")
    ),
    tabPanel(
      "Food Web",
      plotOutput("foodWeb")
    ),
    tabPanel(
      "Populations",
      plotOutput("populations")
    ),
    tabPanel(
      "Statistics",
      plotOutput("statistics")
    )
  )
)

dashboardPage(
  header,
  sidebar,
  body
)