library("muxViz")
library(igraph)
library(Matrix)
library(readxl)
mEdges <- read_excel("D:/NDSU/PhD Work/Research/IME Research/extended file.xlsx")

mEdges
mgraph <- BuildSupraAdjacencyMatrixFromExtendedEdgelist(mEdges,2,max(mEdges[,c(1,3)]))