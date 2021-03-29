#importation des biblioth??ques____________________________________________________________________________

library(igraph)
library(bnlearn)
library(pcalg)
library("grid")
library(RWeka)
library(Rgraphviz)
library(DataExplorer)
library(miic)

#lecture des donn??es________________________________________________________________________________________

data <- read.arff("C:/Users/tkdbo/Downloads/ThoraricSurgery.arff")
sum(is.na(data))
dim(data)
summary(data)
plot_histogram(data)
#DataExplorer::create_report(data)

#Network reconstruction with the hill-climbing approach________________________________________________________

h<-hc(data)
h
class(h)

#c. Get the adjacency matrix (bnlearn::amat).
adjmatrixhc<-amat(h)

#d. Build a directed igraph network from the adjacency matrix and propose a (nice!)
#plot.
g<-graph_from_adjacency_matrix(adjmatrixhc, diag = FALSE,mode = "undirected")
plot(g)

#Network reconstruction with the PC approach____________________________________________________________________

data$AGE = as.factor(data$AGE)
data$PRE4 = as.factor(data$PRE4)
data$PRE5 = as.factor(data$PRE5)

data_pc = data.matrix(data) 
#Make the categories start from 0.
data_pc= data_pc - 1 
data_pc = subset(data_pc, select=-c(PRE4,PRE5,AGE))
#Compute the number of levels for each variable
nlevels = apply(data_pc, 2, function(x) length(unique(x)))
#Prepare the suffStat object (see pc help)
suffStat = list(dm=data_pc, nlev=nlevels, adaptDF=FALSE)

pcl <- pc(suffStat, disCItest, labels = colnames(data_pc), alpha=0.05,  verbose = FALSE)
pcl
# Get the adjacency matrix (bnlearn::amat).
class(pcl)
pc2<-as.bn(pcl)
amat(pc2)
# c. Build a directed igraph network from the adjacency matrix and propose a (nice!)
#plot.
g<-graph_from_adjacency_matrix(amat(pc2), diag = FALSE,mode = "directed")

plot(g)
#si on supprime l'age ona des lien sinon on a pas
#si on met PRE4 PRE5 AGE as factor on a un seul lien 

#Network reconstruction with the aracne approach _____________________________________________________________________________

a<-aracne(data)
a
class(a)

#b. Get the adjacency matrix (bnlearn::amat).
adjmatrixaracne<-amat(a)
#c. Build a directed igraph network from the adjacency matrix and propose a (nice!)
#plot.
g<-graph_from_adjacency_matrix(adjmatrixaracne, diag = FALSE,mode = "undirected")
plot(g)

#Network reconstruction with the MIIC approach__________________________________________________________________________

miic.res <- miic(
  input_data = data, latent = "yes",
  n_shuffles = 100, conf_threshold = 3
)

# plot graph
if(require(igraph)) {
  plot(miic.res)
}

