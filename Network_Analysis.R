
knitr::opts_chunk$set(
  # fig.width = 6, fig.height = 6, fig.align = 'center'
)

rm(list = ls())

files_path <- "/Volumes/Roland/Notes/Network_Analysis/Import/"


#| message: false
#| warning: false
library(sna)
library(xUCINET)
library(igraph)
library(FactoMineR)
library(ergm)



Friends <- read.csv(
  paste0(
    files_path, 
    "Friends.csv"
    ),
  header = T, 
  sep = ";", 
  row.names = 1) |> 
  as.matrix()

Friends



SmallProject <- 
  xCreateProject(GeneralDescription = "A small 5 people study",    #description of the project
                 NetworkName = "Friends",    #name of the network
                 References = "",    #needed to put citations or references
                 NETFILE = Friends,   #name of the data file
                 FileType = "Robject",    #type of the file
                 InFormatType = "AdjMat",    #format of the file. In this case, adjacency matrix
                 NetworkDescription="Friendship networks among 5 people with 3=Good friend, 2=Friend and 1=Acquaintance",
                 Mode = c("People"),   #type of nodes; cells, persons, firms, ...
                 Directed = T,
                 Loops = F,
                 Values = "Ordinal",
                 Class = "matrix"
                 )



SmallProject$Friends



Attributes <- read.csv(
  paste0(
    files_path,
    "Attributes.csv"
  ),
  header = T,
  sep = ";",
  row.names = 1
  )

SmallProject <- 
  xAddAttributesToProject(
    ProjectName = SmallProject,
    ATTFILE1 = Attributes,
    FileType = "Robject",
    Mode = c("People"),
    AttributesDescription = c(
      'Age in years', 'Gender (1 = Male, 2 = Female")', "Preferred Music"
    )
  )



SmallProject$Attributes



SmallProject



Communication <- read.csv(
  paste0(files_path, "communication.csv"), 
  header = T,
  sep = ";",
  row.names = 1
  ) |>
  as.matrix()

SmallProject<-xAddToProject(ProjectName=SmallProject,
                             NetworkName="Communication",
                             NETFILE1=Communication,
                             FileType="Robject",
                             NetworkDescription="Communication network",
                             Mode=c("People"),
                             Directed=FALSE,
                             Loops=FALSE,
                             Values="Binary",
                             Class="matrix")



dichotomized_friend_network <- xDichotomize(
  SmallProject$Friends,
  Value=1,
  Type="GT"
  )

dichotomized_friend_network



density_of_communication<-xDensity(dichotomized_friend_network)



IsFriendOf<-xTranspose(SmallProject$Friends)
IsFriendOf



Baker_Journals



xSymmetrize(Baker_Journals$CoCitations, Type = "Max")



xSymmetrize(Baker_Journals$CoCitations, Type = "Min")



cor(c(SmallProject$Friends),c(SmallProject$Communication))



T1 <- SmallProject$Friends
diag(T1) <- NA
T2 <- SmallProject$Communication
diag(T2) <- NA
cor(c(T1),c(T2),use="complete.obs")



agediff <- xAttributeToNetwork(SmallProject$Attributes[,2], Type = "AbsDiff")*1
xNormalize(agediff, Type = "Max")



xGeodesicDistance(SmallProject$Communication)



save(SmallProject, file = paste0(files_path, "ClassProject.RData"))



snafile.n <- as.network(SmallProject$Friends, directed = TRUE)
snafile.n



igraph_file.i <- graph_from_adjacency_matrix(SmallProject$Friends,
                                   mode = c("directed"), diag = FALSE)
igraph_file.i



hp5edges <- read.table(
  paste0(
    files_path, 'Friends_edgelist.csv'
    ), 
  sep = ';', 
  header = F
  )
hp5edges |> head()



colnames(hp5edges) <- c("from","to","w")
hp5edgesmat <- as.matrix(hp5edges) #igraph wants our data in matrix format
hp5net <- graph_from_edgelist(hp5edgesmat[,1:2], directed=TRUE)

MyAdj <- as_adjacency_matrix(
  hp5net,
  type = c("both"),
  attr = NULL,
  names = TRUE,
  sparse = FALSE
)



yeast_igraph <- read_graph(
  paste0(
    files_path, 'YeastS.net'
    ),
  format = c("pajek")
) |> 
  as_adjacency_matrix(type= 'both', attr = NULL, names = T, sparse = F)



par(mar= c(0, 0, 0, 0))
gplot(yeast_igraph, gmode = 'graph')



Krackhardt_HighTech$ProjectInfo



Krackhardt_HighTech$Friendship



#method 1
Krackhardt_HighTech$Friendship_SymMin<-(Krackhardt_HighTech$Friendship)*t(Krackhardt_HighTech$Friendship)
#method 2
Krackhardt_HighTech$Friendship_SymMin2<-xSymmetrize(Krackhardt_HighTech$Friendship,Type="Min")



sum(Krackhardt_HighTech$Friendship_SymMin - Krackhardt_HighTech$Friendship_SymMin2)



names(Krackhardt_HighTech)



par(mar = c(0, 0, 0, 0))
gplot(Krackhardt_HighTech$Friendship_SymMin, mode="circle")



par(mar = c(0, 0, 0, 0))
gplot(Krackhardt_HighTech$Friendship_SymMin, mode="circle", gmode="graph")



gplot(Krackhardt_HighTech$Friendship_SymMin, mode="circle", gmode="graph",vertex.cex=.8)



par(mar = c(0, 0, 0, 0))
gplot(Krackhardt_HighTech$Friendship_SymMin,
      gmode="graph",           # type of network: undirected
      mode="circle",           # how the nodes are positioned
      vertex.cex=.8,           # the size of the node shapes
      displaylabels=TRUE,      # to add the node labels
      label.pos=1,             # to position the labels below the node shapes
      label.cex=.8,            # to decrease the size of the node labels
      edge.col="grey70")       # to make the colour of the ties/edges 30% black and 70% white 



par(mar = c(0, 0, 0, 0))
Wolfe_Primates$ProjectInfo



Wolfe_Primates$Attributes



Wolfe_Primates$Attributes<-cbind(Wolfe_Primates$Attributes,Rank_Rev=max(Wolfe_Primates$Attributes[,3])+1-Wolfe_Primates$Attributes$Rank)
Wolfe_Primates$Attributes



Wolfe_Primates$JointPresence



gplot(Wolfe_Primates$JointPresence)



table(Wolfe_Primates$JointPresence)



par(mar = c(0, 0, 0, 0))
gplot(Wolfe_Primates$JointPresence>6,                     # NOTE: This is an an alternative way of dichotomizing "on the fly"
      gmode="graph",                                      # type of network: undirected
      coord=Wolfe_Primates$Attributes[,c(2,4)],           # coordinates to be use
      vertex.cex=.8,                                      # the size of the node shapes
      vertex.col=(Wolfe_Primates$Attributes$Gender==1)*8, # a vector which is used to define the colour of each node (0=white and 8=grey)
      displaylabels=TRUE,                                 # to add the node labels
      label.pos=1,                                        # to position the labels below the node shapes
      label.cex=.7,                                       # to decrease the size of the node labels to 50% of the default
      edge.col="grey70")                                  # to make the colour of the ties/edges 30% black and 70% white 

arrows(.2, .2, (max(Wolfe_Primates$Attributes$Age)+min(Wolfe_Primates$Attributes$Age))+.2, .2, length = 0.1)
arrows(.2, .2, .2, (max(Wolfe_Primates$Attributes[,2])+min(Wolfe_Primates$Attributes[,2])), length = 0.1)
text(0, (max(Wolfe_Primates$Attributes$Rank)+min(Wolfe_Primates$Attributes$Rank))-.5, labels="Rank",
     cex=0.8, pos=4)
text((max(Wolfe_Primates$Attributes$Age)+min(Wolfe_Primates$Attributes$Age)),.2, labels="Age",
     cex=0.8, pos=3)



#First, create a matrix with a list of departments
#and copy them in columns.
MATRIX_A1<-matrix(
  Krackhardt_HighTech$Attributes$Department,
  dim(
    Krackhardt_HighTech$Friendship
    )[1],
  dim(
    Krackhardt_HighTech$Friendship
    )[1]
  )
#if the two values are equal, 
#it means that two people are in the same department.
MATRIX_A2<-(MATRIX_A1==t(MATRIX_A1))*1 
MATRIX_A2



COORD_A2 <- gplot(MATRIX_A2)



par(mar = c(0, 0, 0, 0))
gplot((Krackhardt_HighTech$Friendship)*t(Krackhardt_HighTech$Friendship), 
      gmode="graph",
      coord=COORD_A2,
      vertex.cex=.8,
      displaylabels=TRUE, 
      label.pos=1,
      label.cex=.7,
      edge.col="grey70")



par(mar = c(0, 0, 0, 0))
gplot((Krackhardt_HighTech$Advice)*t(Krackhardt_HighTech$Advice),
      gmode="graph",
      coord=COORD_A2,
      vertex.cex=.8,
      displaylabels=TRUE, 
      label.pos=1,
      label.cex=.7,
      edge.col="grey70")



par(mar = c(0, 0, 0, 0))
gplot(ASNR_Fig07x11,
      gmode="graph",
      vertex.cex=.8,
      displaylabels=TRUE, 
      label.pos=1,
      label.cex=.9,
      edge.col="grey70")



par(mar = c(0, 0, 0, 0))
gplot(ASNR_Fig07x11,
      mode="kamadakawai",
      gmode="graph",
      vertex.cex=.8,
      displaylabels=TRUE, 
      label.pos=1,
      label.cex=.9,
      edge.col="grey70")



par(mar = c(0, 0, 0, 0))
gplot(Krackhardt_HighTech$Friendship*t(Krackhardt_HighTech$Friendship),
      gmode="graph",
      vertex.cex=.8,
      displaylabels=TRUE, 
      label.pos=1,
      label.cex=.9,
      edge.col="grey70")



par(mar = c(0, 0, 0, 0))
gplot(Krackhardt_HighTech$Advice*t(Krackhardt_HighTech$Advice),
      gmode="graph",
      vertex.cex=.8,
      displaylabels=TRUE, 
      label.pos=1,
      label.cex=.9,
      edge.col="grey70")



par(mar = c(0, 0, 0, 0))
gplot(Borgatti_Scientists504$Collaboration>3,
      mode="kamadakawai",
      gmode="graph",
      vertex.cex=.8,
      edge.col="grey70")



par(mar=c(0,0,0,0))
gplot(Krackhardt_HighTech$Friendship_SymMin, 
      gmode="graph",
      edge.col="grey30",
      edge.lwd=.4,
      vertex.cex=Krackhardt_HighTech$Attributes$Tenure/20+1, # node dimension is dependent on an attribute
      vertex.col="white",
      displaylabels=TRUE,
      label.cex=.8)



par(mar=c(0,0,0,0))
gplot(Krackhardt_HighTech$Friendship_SymMin, 
      gmode="graph",
      mode="fruchtermanreingold",
      jitter=F,
      edge.col="grey30",
      edge.lwd=.4,
      vertex.cex=Krackhardt_HighTech$Attributes$Tenure/20+1,
      vertex.col=1+(Krackhardt_HighTech$Attributes$Level==2)+(Krackhardt_HighTech$Attributes$Level==3)*-1,
      vertex.sides=3+(Krackhardt_HighTech$Attributes$Level==2)+(Krackhardt_HighTech$Attributes$Level==3)*50,
      displaylabels=TRUE,
      label.cex=.8)



Tenure <- (Borgatti_Scientists504$Attributes$Years)
# I normalize and discretize the tenure levels between 0 and 10
TenureA <- (round(10*(Tenure)/max((Tenure))))
# return functions that interpolate a set of given colours to create new colour palettes
colfunc <- colorRampPalette(c("yellow", "red")) 
COLWB <- colfunc(7)



TenureA2 <- TenureA
TenureA2[TenureA==0] <- COLWB[1]
TenureA2[TenureA==1] <- COLWB[2]
TenureA2[TenureA==2] <- COLWB[3]
TenureA2[TenureA==3] <- COLWB[4]
TenureA2[TenureA==4] <- COLWB[5]
TenureA2[TenureA==5] <- COLWB[6]
TenureA2[TenureA==6] <- COLWB[6]
TenureA2[TenureA==7] <- COLWB[7]
TenureA2[TenureA==8] <- COLWB[7]
TenureA2[TenureA==9] <- COLWB[7]
TenureA2[TenureA==10] <- COLWB[7]



par(mar=c(0,0,0,0))
gplot(Borgatti_Scientists504$Collaboration>3,
      mode="kamadakawai",
      gmode="graph",
      edge.col="grey80",
      edge.lwd=.2, 
      vertex.col=TenureA2,
      vertex.cex=1+.7*(2-Borgatti_Scientists504$Attributes$Sex), 
      vertex.sides=(Borgatti_Scientists504$Attributes$Sex-1)*47+4)



par(mar=c(0,0,0,0))
PFM_Degree<-sna::degree(Padgett_FlorentineFamilies$Marriage)
gplot(Padgett_FlorentineFamilies$Marriage, 
      gmode="graph",
      edge.col="grey80",
      edge.lwd=1.2,
      vertex.cex=0,
      vertex.col="white",
      displaylabels=TRUE,
      label.pos=5,
      label.cex=PFM_Degree/14+.45)



par(mar=c(0,0,0,0))
gplot(Newcomb_Fraternity$PreferenceT00<4,
      gmode="digraph", # not that we changed the mode
      #layout
      jitter=F,
      #ties
      edge.col="grey30",
      edge.lwd=1,
      arrowhead.cex=.6,
      #nodes
      vertex.col="gold", 
      vertex.cex=1.4,
      #labels
      displaylabels=TRUE, 
      label.pos=5,
      label.cex=.7)



par(mar=c(0,0,0,0))
gplot(Newcomb_Fraternity$PreferenceT00<4,
      gmode="digraph",
      #layout
      jitter = F,
      #ties
      edge.col="grey30",
      edge.lwd=1,
      usecurve=T, # Adding this parameter
      edge.curve=.09, 
      arrowhead.cex=.6,
      #nodes
      vertex.col="gold", 
      vertex.cex=1.4,
      #labels
      displaylabels=TRUE, 
      label.pos=5,
      label.cex=.7)



HBG <- Hawthorne_BankWiring$Game # Positive ties
HBA <- Hawthorne_BankWiring$Antagonistic # Negative ties



HB <- (2*HBG+HBA)
HBc <- (HBA==1)*(HBG==1)*1+(HBA==0)*(HBG==1)*3+(HBA==1)*(HBG==0)*2
#Change tie width so it is also visible in black & white
HBl <- (HBA==1)*(HBG==1)*5+(HBA==0)*(HBG==1)*3+(HBA==1)*(HBG==0)*1



par(mar=c(0,0,0,0))
gplot(HB, jitter = F, gmode="graph", displaylabels=TRUE, label.pos=5, 
      vertex.col="grey90", edge.lwd=HBl,
      label.cex=.7, vertex.cex=1.4, edge.col=HBc)



xDegreeCentrality(Krackhardt_HighTech$Advice)



xDegreeCentrality(t(Krackhardt_HighTech$Advice))



Krackhardt_HighTech$NetworkInfo



Krackhardt_HighTech$Friendship



gplot(Krackhardt_HighTech$Friendship)



xDensity(Krackhardt_HighTech$Friendship)



xGeodesicDistance(Krackhardt_HighTech$Friendship)



xComponents(Krackhardt_HighTech$Friendship)



xComponents(Krackhardt_HighTech$Friendship, Type = 'weak')



xConnectedness(Krackhardt_HighTech$Friendship)



xReciprocity(Krackhardt_HighTech$Friendship)



SymNetwork<-xSymmetrize(Krackhardt_HighTech$Friendship,Type = "Min")



Clus<-xTransitivity(Krackhardt_HighTech$Friendship)
Clus



DC<-xDegreeCentrality(Krackhardt_HighTech$Friendship)
DC



xClosenessCentrality(Krackhardt_HighTech$Friendship)



CC<-xClosenessCentrality(Krackhardt_HighTech$Friendship)[,6]



BC<-xBetweennessCentrality(Krackhardt_HighTech$Friendship)[,2]



DCOrder<- order(DC)
BCOrder<- order(BC)
CCOrder<- order(CC)



plot(BCOrder,DCOrder)
title('Betweeness vs Degree Centrality')
abline(0,1,col=2,lty=2)



plot(CCOrder,DCOrder)
title('Closeness vs Degree Centrality')
abline(0,1,col=2,lty=2)



plot(BCOrder,CCOrder, pch = )
title('Betwenness vs Closeness Centrality')
abline(0,1,col=2,lty=2)



Friends<-graph_from_adjacency_matrix(
  Krackhardt_HighTech$Friendship,
  mode = c("directed"), # "undirected", "max", "min", "upper", "lower", "plus"
  weighted = NULL,
  diag = FALSE,
  add.colnames = names(Krackhardt_HighTech$Friendship),
  add.rownames = NA
)
str(Friends)



components(Friends, mode = c("strong"))



PR<- page_rank(
  Friends,
  algo = c("prpack"),
  vids = V(Friends),
  directed = TRUE,
  damping = 0.85,
  personalized = NULL,
  weights = NULL,
  options = NULL
)
PROrder <- order(PR$vector)
PROrder



plot(DCOrder,PROrder)
title('Degree vs PageRank Centrality')
abline(0,1,col=2,lty=2)




plot(CCOrder,PROrder)
title('Closeness vs PageRank Centrality')
abline(0,1,col=2,lty=2)



plot(BCOrder,PROrder)
title('Betwenness vs PageRank Centrality')
abline(0,1,col=2,lty=2)



transitivity(
  Friends,
  type = c("undirected"),
  vids = NULL, # null if you want to compute the global index or the local for all nodes
  weights = NULL,
  isolates = c("NaN")
)



transitivity(
  Friends,
  type = c("local"),
  vids = NULL, # null if you want to compute the global index or the local for all nodes
  weights = NULL,
  isolates = c("NaN")
)



diameter(Friends, directed = TRUE, unconnected = TRUE, weights = NULL)



assortativity_degree(Friends, directed = TRUE)



DC <- xDegreeCentrality(Krackhardt_HighTech$Friendship)[,1]
plot(table((DC)))


#| fig-width: 5
#| fig-height: 5
#| fig-align: "center"
hist(DC, col = 'gold')


#| fig-width: 5
#| fig-height: 5
#| fig-align: "center"
Indegrees <- Krackhardt_HighTech$Friendship |> t() |> xDegreeCentrality()
hist(Indegrees, col = 'dodgerblue')



# Several algorithms do not work on directed networks, so let's symmetrize
SymFriends <- xSymmetrize(Krackhardt_HighTech$Friendship,Type = "Max")

# I create the igraph network of my adjacency matrix.
Friends <- graph_from_adjacency_matrix(
  SymFriends,
  mode = c("undirected"), # "undirected", "max", "min", "upper", "lower", "plus"
  weighted = NULL,
  diag = FALSE,
  add.colnames = names(SymFriends),
  add.rownames = NA
)

# This command optimise the graphical representation of a network by applying FR algo
coord <- layout_with_fr(Friends)



CluLouvain<-cluster_louvain(Friends, weights = NULL, resolution = 1)
CluLouvain



CluLouvain$membership



CluLouvain$modularity



plot(CluLouvain,Friends,layout=coord) 



CluFastGreedy<-cluster_fast_greedy(
  Friends,
  merges = TRUE,
  modularity = TRUE,
  membership = TRUE,
  weights = NULL
)



CluFastGreedy$membership



plot(CluFastGreedy,Friends,layout=coord)



plot_dendrogram(CluFastGreedy)



#Bridge Detection
#Girvan Newman (edge betweenness)
CLuGirvan<-cluster_edge_betweenness(
  Friends,
  weights = NULL,
  directed = FALSE, # Note that this works also fro directed networks
  edge.betweenness = TRUE,
  merges = TRUE,
  bridges = TRUE,
  modularity = TRUE,
  membership = TRUE
)



CLuGirvan$membership



plot(CLuGirvan,Friends,layout=coord)



CluLabelProp<-cluster_label_prop(Friends, weights = NULL, initial = NULL, fixed = NULL)
plot(CluLabelProp,Friends,layout=coord)



M18Centrz<-c(0.59, 0.83, 0.60, 0.45, 0.52, 0.62, 0.64, 0.00, 0.43, 0.44, 
             0.76, 0.50, 0.65, 0.68, 0.48, 0.52, 0.80, 0.10)
M18Perf<-c(4, 1, 5, 4, 3, 8, 2, 5, 5, 4, 6, 7, 6, 4, 7, 4, 6, 1)



cor(M18Centrz,M18Perf)
cor.test(M18Centrz,M18Perf)



par(mfrow = c(1,2))
xCorrelation(M18Centrz,M18Perf, NPERM=5000)



par(mfrow = c(1, 3))

Age15<-c(29, 72, 23, 51, 49, 61, 48, 68, 38, 46, 31, 63, 62, 59, 26)
hist(Age15)

Consc15<-c(0.1, 5.5, 3.1, 2.3, 3.8, 9.3, 7.0, 0.7, 5.5, 0.8, 6.2, 3.1, 1.5, 5.6, 2.7)
hist(Consc15)

Burnout15<- c(2.55, 6.80, 5.00, 6.19, 3.00, 3.84, 3.42, 8.31, 5.41, 6.58, 4.74, 7.14, 6.43, 3.18, 2.83)
hist(Burnout15)

Comm15<-matrix(c(0,1,0,1,0, 1,1,0,0,0, 0,0,0,0,0, 
                 1,0,0,1,0, 1,0,1,0,0, 0,0,0,0,0,
                 0,0,0,1,0, 0,0,1,1,0, 0,0,0,0,0,
                 1,1,1,0,1, 1,0,0,0,0, 0,0,0,0,0,
                 0,0,0,1,0, 0,0,1,0,0, 0,0,0,0,0,
                 
                 1,1,0,1,0, 0,0,0,0,0, 0,0,0,0,0,
                 1,0,0,0,0, 0,0,0,0,0, 0,0,0,0,1,
                 0,1,1,0,1, 0,0,0,0,0, 0,0,0,0,1,
                 0,0,1,0,0, 0,0,0,0,0, 1,0,1,0,0,
                 0,0,0,0,0, 0,0,0,0,0, 0,0,1,1,0,
                 
                 0,0,0,0,0, 0,0,0,1,0, 0,1,1,0,0,
                 0,0,0,0,0, 0,0,0,0,0, 1,0,0,1,0,
                 0,0,0,0,0, 0,0,0,1,1, 1,0,0,0,0,
                 0,0,0,0,0, 0,0,0,0,1, 0,1,0,0,0,
                 0,0,0,0,0, 0,1,1,0,0, 0,0,0,0,0),15,15)



gplot(Comm15, gmode="graph")



Degr15<-rowSums(Comm15)



X15<-data.frame(Age15,Consc15,Degr15)
xRegression(Burnout15,X15,NPERM=5000) # NPERM>1000 is advisable.



xQAPCorrelation(Padgett_FlorentineFamilies$Marriage,Padgett_FlorentineFamilies$Business,NPERM = 10000)



MATRIX1<-matrix(c(0,4,2,3, 3,0,4,1, 4,8,0,2, 6,2,6,0),4,4)
MATRIX2<-matrix(c(0,1,2,1, 3,0,3,7, 4,7,0,2, 5,2,2,0),4,4)
MATRIX3<-matrix(c(0,4,5,3, 2,0,2,8, 4,4,0,2, 3,5,3,0),4,4)

LIST2<-list(MATRIX2,MATRIX3)
RESULTS1<-xQAPRegression(MATRIX1,LIST2,NPERM=1000)
RESULTS1



ASNR_Fig15x1



E14A <- as.network(ASNR_Fig15x1$`Network A`,directed=F)
E14_ATTR <- ASNR_Fig15x1$Attributes
E14_ATTR



E14A %v% "Gender" <- E14_ATTR$Gender



gplot(
  E14A,
  gmode="graph",
  displaylabels=T,
  vertex.col=E14A %v% "Gender"*2
  )



set.seed(516)
M.E14A.1<-ergm(E14A ~ edges + kstar(2) + triangle)
  
summary(M.E14A.1)



set.seed(738)
M.E14A.2<-ergm(E14A ~ edges + nodefactor("Gender",levels=2) + nodematch("Gender"))
summary(M.E14A.2)



set.seed(343)
M.E14A.3<-ergm(
  E14A ~ edges + kstar(2) + triangle + nodefactor("Gender",levels=2) + nodematch("Gender"))
summary(M.E14A.3)

