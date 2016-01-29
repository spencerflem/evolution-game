library(shinydashboard)
library(shinyjs)

header <- dashboardHeader(title = "Darwin or Lose")

sidebar <- dashboardSidebar(
  #MAIN GAME
  textOutput("creature"),
  sidebarMenu(
    id = "selectedEvo",
    sidebarMenuOutput("evos")
  ),
  actionButton("confirmed","Confirm Evolution"),
  
  #STARTUP
  h3(id = "heading", "CREATE YOUR SPECIES!"),
  selectInput("class","Diet",choices = c("carnivore","omnivore","herbivore")),
  selectInput("race","Size",choices = c("big","average","small")),
  textInput("name","Name", value = "Nomenus Examplaris"),
  actionButton("joined","Create Species"),
  
  #HIDDEN
  numericInput("ID","unset", value = 0)
)

body <- dashboardBody(
  
  useShinyjs(),
  
  plotOutput("populations")
  
)

dashboardPage(
  header,
  sidebar,
  body
)