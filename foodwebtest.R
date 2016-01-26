library(DiagrammeR)
library(Rgraphviz)
library(graph)
library(magrittr)
library(diagram)
library(igraph)

#DIAGRAM METHOD
names <- c("PHYTO", "NH3", "ZOO", "DETRITUS", "BotDET", "FISH")
M <- matrix(nrow = 6, ncol = 6, byrow = TRUE, data = c(
  #   p  n  z   d   b   f
  0, 1, 0,  0,  0,  0, #p
  0, 0, 4, 10, 11,  0, #n
  2, 0, 0,  0,  0,  0, #z
  8, 0, 13, 0,  0, 12, #d
  9, 0, 0,  7,  0,  0, #b
  0, 0, 5,  0,  0,  0  #f
))

pp <- plotmat(eatenMatrix, pos = c(1, 2, 1, 2), curve = 0, name = names,
                  lwd = 1, box.lwd = 2, cex.txt = 0.8,
                  box.type = "square", box.prop = 0.5, arr.type = "triangle",
                  arr.pos = 0.4, shadow.size = 0.01, prefix = "f",
                  main = "NPZZDD model")

phyto    <- pp$comp[names=="PHYTO"]
zoo      <- pp$comp[names=="ZOO"]
nh3      <- pp$comp[names=="NH3"]
detritus <- pp$comp[names=="DETRITUS"]
fish     <- pp$comp[names=="FISH"]


t(eatenMatrix)

plotweb(t(eatenMatrix), main = "Gulf of Riga food web", sub = "mgC/m3/d", val = TRUE) #BACKUP FOOD WEB

am.graph<-new("graphAM", adjMat=eatenMatrix, edgemode="directed")
am.graph
plot(am.graph)







nodes <- c( LETTERS[1:3] )
edgesL <- list( A=c("B", "C"), B=c("A", "C"), C=c("B", "A" ) )
graph <- new( "graphNEL", nodes= nodes, edgemode="undirected", edgeL=edgesL )
rag <- agopen( am.graph, "" )
rag@AgEdge[[2]]@lwd <- 5
plot(rag)

rag@AgEdge


am.graph
$edge$weight




edgeRenderInfo(am.graph) <- list(lwd = c("n8~n4" = 6))
edgeRenderInfo(am.graph)
log <- layoutGraph(am.graph)
renderGraph(log)

set.seed(120)
V <- LETTERS[1:4]
edL <- vector("list", length=4)
names(edL) <- V
for(i in 1:4)
  edL[[i]] <- list(edges=5-i, weights=runif(1))
gR <- new("graphNEL", nodes=V, edgeL=edL)
edges(gR)
edgeWeights(gR)
plot(gR)



df <- data.frame(from = c("SEA", "SFO", "SEA", "LAX", "SEA"),
                to = c("SFO", "LAX", "LAX", "SEA", "DEN"),
                 weight = c( 9000, 90006, 124, 115, 259))
g <- graphBAM(df, edgemode = "directed")
plot(g)







adjm <- beatenMatrix
g2 <- graph.adjacency(adjm, weighted=TRUE)
egan <- (log(E(g2)$weight) + .4)/max(log(E(g2)$weight)+.4)
E(g2)$width <- egan
plot(g2)






beatenMatrix <- 4 * eatenMatrix
beatenMatrix
g <- graph.adjacency(beatenMatrix, mode = "directed", weighted = TRUE)
E(g)$width <- E(g)$weight + min(E(g)$weight + 1)
plot(g)
