library(shinydashboard)

header <- dashboardHeader(title = "Evolution!")

sidebar <- dashboardSidebar(
  #CREATURE SUMMARY
  sidebarMenu(
    id = "selectedEvo",
    sidebarMenuOutput("evos")
  ),
  actionButton("confirmed","Confirm Evolution"),
  
  #INPUT CLASS RACE NAME
  
  selectInput("class","Class",choices = c("carnivore","herbivore","omnivore")),
  selectInput("race","Race",choices = c("big","dinky","average")),
  textInput("name","Name"),
  numericInput("ID","unset", value = 0),
  actionButton("joined","Create Creatrue")
  
  #renderUI to distinguish the two?
  #if not, shinyJS :/
)

body <- dashboardBody(
  tabBox(
    width = 12,
    
    tabPanel(
      "Your Creature",
      textOutput("yourCreature")
    ),
    tabPanel(
      "Other Creatures",
      htmlOutput("otherCreatures")
    ),
    tabPanel(
      "Food Web",
      plotOutput("foodWeb")
    ),
    tabPanel(
      "Evolutionary Tree",
      plotOutput("evoTree")
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