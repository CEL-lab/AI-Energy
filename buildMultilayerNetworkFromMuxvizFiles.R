library("muxViz")
library("igraph")

#This dataset is provided with the "standalone" library

config.file <- "D:/NDSU/PhD Work/Research/IME Research multinet_course/config_file/config.txt"

#This flag is important to understand if input networks are directed or not

isDirected <- F

#This flag is important to understand if input networks are weighted or not

isWeighted <- T

#Network of layers (ordinal or categorical) and coupling strength
networkOfLayersType <- "categorical"
LayerCouplingStrength <- 10

mux <- buildMultilayerNetworkFromMuxvizFiles(config.file=config.file,  
                                             isDirected=isDirected,
                                             isWeighted=isWeighted,
                                             MultisliceType=networkOfLayersType, 
                                             LayerCouplingStrength=LayerCouplingStrength, 
                                             format="muxviz edge-colored",
                                             verbose=T
                                          )

mux
