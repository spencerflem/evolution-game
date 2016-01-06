library(shinydashboard)

header <- dashboardHeader(title = "Evolution!")

sidebar <- dashboardSidebar(
  #CREATURE SUMMARY
  sliderInput("bins",
              "Number of bins:",
              min = 1,
              max = 50,
              value = 30),
  sidebarMenu(
    id = "selectedEvo",
    menuItem("arms",tabName = "Arms",
              menuSubItem("good arms",tabName = "good arms"),
              menuSubItem("bad arms",tabName = "bad arms")),
    menuItem("legs",tabName = "Legs",
              menuSubItem("good legs",tabName = "good legs"),
              menuSubItem("bad legs",tabName = "bad legs")),
    menuItem("body",tabName = "Body",
              menuSubItem("good body",tabName = "good body"),
              menuSubItem("bad body",tabName = "bad body")),
    menuItem("brain",tabName = "Brain",
              menuSubItem("good brain",tabName = "good brain"),
              menuSubItem("bad brain",tabName = "bad brain"))
  ),
  "yourEvo",
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
      plotOutput("otherCreatures")
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