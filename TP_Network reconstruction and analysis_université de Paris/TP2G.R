#1. Preliminaries

#a. Installation  et chargement des packages R : bnlearn, igraph et pcalg

install.packages("bnlearn")
install.packages("igraph")*
install.packages("pcalg")
install.packages("BiocManager")
BiocManager::install("Rgraphviz")


if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("graph")
BiocManager::install("RBGL")
library(igraph)
library(bnlearn)
library(pcalg)
library("grid")

#b. Cr??ation du  mod??le v??rit?? terrain de l'assurance ?? partir du mod??le  String 
# load the data.
D<-data(insurance)
head(D)
dim(D)

# create and plot the network structure.
modelstring = paste0("[Age][Mileage][SocioEcon|Age][GoodStudent|Age:SocioEcon]",
                     "[RiskAversion|Age:SocioEcon][OtherCar|SocioEcon][VehicleYear|SocioEcon:RiskAversion]",
                     "[MakeModel|SocioEcon:RiskAversion][SeniorTrain|Age:RiskAversion]",
                     "[HomeBase|SocioEcon:RiskAversion][AntiTheft|SocioEcon:RiskAversion]",
                     "[RuggedAuto|VehicleYear:MakeModel][Antilock|VehicleYear:MakeModel]",
                     "[DrivingSkill|Age:SeniorTrain][CarValue|VehicleYear:MakeModel:Mileage]",
                     "[Airbag|VehicleYear:MakeModel][DrivQuality|RiskAversion:DrivingSkill]",
                     "[Theft|CarValue:HomeBase:AntiTheft][Cushioning|RuggedAuto:Airbag]",
                     "[DrivHist|RiskAversion:DrivingSkill][Accident|DrivQuality:Mileage:Antilock]",
                     "[ThisCarDam|RuggedAuto:Accident][OtherCarCost|RuggedAuto:Accident]",
                     "[MedCost|Age:Accident:Cushioning][ILiCost|Accident]",
                     "[ThisCarCost|ThisCarDam:Theft:CarValue][PropCost|ThisCarCost:OtherCarCost]")
dag = model2network(modelstring)
dag

#c. Check the class of the returned object and see the content.
c1<-class(dag)
c1

#graphviz.plot(dag, shape = "ellipse")

#d. la matrice d'adjacente  (bnlearn :: amat).

adjmatrix=amat(dag)

#e.Construire un r??seau igraph dirig?? ?? partir de la matrice d'adjacente 

g<-graph_from_adjacency_matrix(adjmatrix, diag = FALSE,mode = "directed")
plot(g)

#2. Score-based method (hill-climbing)___________________________________________________________

#a. Load the insurance data from the bnlearn package.
data(insurance)

#b. Reconstruct the insurance network using the hill-climbing approach (bnlearn::hc).
#Check the class of the returned object and see the content.
h<-hc(insurance)
h
class(h)

#c. Get the adjacency matrix (bnlearn::amat).
adjmatrixhc<-amat(h)

#d. Build a directed igraph network from the adjacency matrix and propose a (nice!)
#plot.
g<-graph_from_adjacency_matrix(adjmatrixhc, diag = FALSE,mode = "directed")
plot(g)

#e. Count the number of true positive (TP), false positive (FP) and false negative (FN)
#(for the graph skeleton only). Compute Precision, Recall and Fscore.
res<-bnlearn::compare(bnlearn::skeleton(dag),bnlearn::skeleton(h))
res
#Precision [TP/(TP+FP]; Recall [TP/(TP+FN)]; F-score [2.Prec.Rec./(Prec.+Rec.)]
precision = res$tp/ (res$tp+res$fp)
precision
Recall= res$tp/ (res$tp+res$fn)
Recall
Fscore= (2*precision*Recall)/(precision+Recall)
Fscore

#f. Highlight the FP edges in your reconstructed network.
diff.args = list(show.first=FALSE)
graphviz.compare(bnlearn::skeleton(dag), bnlearn::skeleton(h),
                 diff.args=diff.args, shape = "ellipse", 
                 sub=c("", paste("Prediction of real graph using HC with: \n - red",
                                 "for false positive edges \n - blue" ,
                                 "for false negative edges \n PS:",
                                 "orientations aren't taken into account")))

#g. Propose a method to take the orientation into account.
diff.args = list(show.first=FALSE)
graphviz.compare(dag, h,
                 diff.args=diff.args, shape = "ellipse", 
                 sub=c("", paste("Prediction of real graph using HC with: \n - red",
                                 "for false positive edges \n - blue" ,
                                 "for false negative edges \n PS:",
                                 "orientations aren't taken into account")))


#3. Constraint-based method (PC)______________________________________________________________________________
#a. Reconstruct the insurance network using the PC approach (pcalg::pc) using the
#disCItest conditional independence test. You will need to perform the following
#transformations:
#  { Convert your dataset to numeric using data.matrix
data = insurance
D3 = data.matrix(data) 
#    { Make the categories start from 0.
D3=D3 - 1 
#      { Compute the number of levels for each variable
nlevels = apply(D3, 2, function(x) length(unique(x)))
#        { Prepare the suffStat object (see pc help)

suffStat = list(dm=D3, nlev=nlevels, adaptDF=FALSE)
pcl <- pc(suffStat, disCItest, labels = colnames(D3), alpha=0.05,  verbose = FALSE)
pcl
# b. Get the adjacency matrix (bnlearn::amat).
class(pcl)
pc2<-as.bn(pcl)
amat(pc2)
# c. Build a directed igraph network from the adjacency matrix and propose a (nice!)
#plot.
g<-graph_from_adjacency_matrix(amat(pc2), diag = FALSE,mode = "directed")
plot(g)
#d. Count the number of true positive (TP), false positive (FP) and false negative (FN)
#(for the graph skeleton only). Compute Precision, Recall and Fscore.
res<-bnlearn::compare(bnlearn::skeleton(dag),bnlearn::skeleton(pc2))
res
#Precision [TP/(TP+FP]; Recall [TP/(TP+FN)]; F-score [2.Prec.Rec./(Prec.+Rec.)]
precision = res$tp/ (res$tp+res$fp)
precision
Recall= res$tp/ (res$tp+res$fn)
Recall
Fscore= (2*precision*Recall)/(precision+Recall)
#e. Highlight the FP edges in your reconstructed network.
graphviz.compare(bnlearn::skeleton(dag), bnlearn::skeleton(pc2),
                 diff.args=diff.args, shape = "ellipse", 
                 sub=c("", paste("Prediction of real graph using PC with: \n - red",
                                 "for false positive edges \n - blue" ,
                                 "for false negative edges \n PS:",
                                 "orientations aren't taken into account")))

#4. Local search method (aracne)____________________________________________________________________________
#a. Reconstruct the insurance network using the PC approach (bnlearn::aracne).
data(insurance)
a<-aracne(insurance)
a
class(a)

#b. Get the adjacency matrix (bnlearn::amat).
adjmatrixaracne<-amat(a)
#c. Build a directed igraph network from the adjacency matrix and propose a (nice!)
#plot.
g<-graph_from_adjacency_matrix(adjmatrixaracne, diag = FALSE,mode = "directed")
plot(g)

#d. Count the number of true positive (TP), false positive (FP) and false negative (FN)
#(for the graph skeleton only). Compute Precision, Recall and Fscore.
res<-bnlearn::compare(bnlearn::skeleton(dag),bnlearn::skeleton(a))
res
#Precision [TP/(TP+FP]; Recall [TP/(TP+FN)]; F-score [2.Prec.Rec./(Prec.+Rec.)]
precision = res$tp/ (res$tp+res$fp)
precision
Recall= res$tp/ (res$tp+res$fn)
Recall
Fscore= (2*precision*Recall)/(precision+Recall)
Fscore
#e. Highlight the FP edges in your reconstructed network.
graphviz.compare(bnlearn::skeleton(dag), bnlearn::skeleton(a),
                 diff.args=diff.args, shape = "ellipse", 
                 sub=c("", paste("Prediction of real graph using PC with: \n - red",
                                 "for false positive edges \n - blue" ,
                                 "for false negative edges \n PS:",
                                 "orientations aren't taken into account")))

