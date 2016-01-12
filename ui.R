library(shinydashboard)
library(shinyjs)

header <- dashboardHeader(title = "Evolution!")

sidebar <- dashboardSidebar(
  #CREATURE SUMMARY
  textOutput("creature"),
  sidebarMenu(
    id = "selectedEvo",
    sidebarMenuOutput("evos")
  ),
  actionButton("confirmed","Confirm Evolution"),
  
  #INPUT CLASS RACE NAME
  
  h3("CREATE YOUR CREATURE!"),
  selectInput("class","Class",choices = c("carnivore","herbivore","omnivore")),
  selectInput("race","Race",choices = c("big","dinky","average")),
  textInput("name","Name"),
  numericInput("ID","unset", value = 0),
  actionButton("joined","Create Creatrue")
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