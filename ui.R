library(shinydashboard)

header <- dashboardHeader(title = "Evolution!")

sidebar <- dashboardSidebar(
  #CREATURE SUMMARY
  sidebarMenu(
    id = "selectedEvo",
    sidebarMenuOutput("evos")
  ),
  actionButton("confirmed","Confirm Evolution")
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
      textOutput("otherCreatures")
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