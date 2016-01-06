
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(Rgraphviz)
library(ape)

evolutions <- c("LOLOL")

addEvolution <- function(evolution)
{
  evolutions <<- c(evolutions, evolution)
  return(evolutions)
}

creatures <- matrix(
  c(-0.2, 1, 800, 1, 50, 1, 0, #predator
    -0.2, 1.2, 550, 1, 30, 0, 0, #prey
    0.2, 2, 50, 5, 95, 0, 1), #grass
  nrow = 3, ncol = 7, byrow = TRUE
)
#dataframe? dimnames?
stepSim <- function(creatures) 
{
  r <- creatures[,1] #r-value, + if plant, - if creatrue
  starting <- creatures[,2] #initial population size
  a <- calculateInteractionMatrix(creatures) #interaction matrix
  
  steps <- 7
  N0 <- starting
  final <- starting
  
  while (steps > 0) {
    steps <- steps - 1
    parms <- list(r,a)
    t=seq(1,1000, by=1)
    library(deSolve)
    lvout <- ode(N0, t, function (t, n, parms) 
    {
      r <- parms[[1]]
      a <- parms[[2]]
      dns.dt <- n * (r + (a %*% n)[,1])
      return(list(c(dns.dt)))
    }, parms)
    parms2 <- list(r,a,lvout)
    lvout2 <- ode(N0, t, function (t, n, parms2) 
    {
      r <- parms2[[1]]
      a <- parms2[[2]]
      n2 <- parms2[[3]]
      n3 <- n2[t,-1]
      dns.dt <- n3 * (r + (a %*% n3)[,1])  * (2^(-0.02 * t))
      return(list(c(dns.dt)))
    }, parms2)
    lastrow <- lvout2[999,-1]
    changes <- lastrow - N0
    N0 <- N0 + changes
    final <- N0
  }
  return(final)
}

calculateInteractionMatrix <- function(creatures) {
  samplespace <- computeSampleSpace(creatures)
  return(matrix(c(0,0.2,0,-0.2,0,0.2,0,-0.2,0), nrow=3, ncol=3, byrow=TRUE))
}

computeSampleSpace <- function(creatures) {
  ncreatures <- nrow(creatures)
  ssones <- rep(1,2^ncreatures)
  ssdims <- rep(2,ncreatures)
  samplespace <- array(ssones, dim = ssdims)
  for(i in 1:ncreatures) {
    index <- rep(c(1,2),ncreatures)
    index[i] <- 1
    index[i+1] <- 1
    #[] or [[]]
    samplespace[index]
  }
}



shinyServer(function(input, output) {

  output$mainMenu <- renderMenu({
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
    )
  })
  
  output$foodWeb <- renderPlot({
    V <- letters[1:10]
    M <- 1:4
    set.seed(input$bins)
    g1 <- randomGraph(V, M, 0.2)
    plot(g1)
  })
  
  output$evoTree <- renderPlot({
    tree <- rtree(n = 20)
    plot(tree, edge.width = 2)
  })
  
  output$populations <- renderPlot({
    toplot <- matrix(c(1:5,10:15),nrow = 2, ncol = 5)
    matplot(toplot, type = "l", log = "y")
  })

  output$yourCreature <- renderText(getEvolutions())
  
  output$otherCreatures <- renderText(input$selectedEvo)
  
  getEvolutions <- eventReactive(input$confirmed, {addEvolution(input$selectedEvo)}, ignoreNULL = FALSE)
  
  output$evos <- renderMenu({
    sidebarMenu(
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
    )})
  
})
