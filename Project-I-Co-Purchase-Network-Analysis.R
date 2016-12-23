##############################################################################################################################
##############################################################################################################################
#Step 0  
#load all necessary packages and setup the path and remove all data present in r environment
############################ ##################################################################################################
##############################################################################################################################
install.packages("igraph")
install.packages("dplyr")
library(igraph)
library(dplyr)

#Set the directory path to the path where file is located
path ="C:/Users/Mihir Sanghvi/Desktop/monsteRs"
setwd(path)

#remove all the data present in the r environment  
rm(list=ls())

#supress warning messages 
options(warn=0)

##############################################################################################################################
# IMPORTANT NOTE:         
##############################################################################################################################

# Code to create Communities takes 2-4.8 hours, we have exported workspace after creating communities
# You can simply load workspace using following code and directly go to Step-2B

# Following is the code to load the workspace

# load("Project-I-Workspace.RData")

##############################################################################################################################
##############################################################################################################################
#Step 1: Create and Diagnose Graph Network 
# 1) Read Node Metadata and EdgeList Files and create network graph 
# 2) Run Network Statistics 
##############################################################################################################################
##############################################################################################################################

coPurchaseEdgeList <- read.csv("coPurchaseEdgeList.csv")
coPurchaseMeta <- read.csv("coPurchaseMeta.csv")



coPurchaseNetworkGraph <- graph.data.frame(d = coPurchaseEdgeList, vertices = coPurchaseMeta,directed = T) 

# Remove unnecessary objects from workspace
remove(coPurchaseEdgeList)
remove(coPurchaseMeta)
help("graph.data.frame")



########################################################################################################
###descriptive stats
########################################################################################################
summary(coPurchaseNetworkGraph)

#Is the network globally connected ?
is.connected(coPurchaseNetworkGraph)    

help("is.connected")

#Checking for connected components
cl <- clusters(coPurchaseNetworkGraph)  
str(cl)
help("clusters")
#size of connected components
cl$csize      

#total(in+out) degree
Degree <- degree(coPurchaseNetworkGraph, mode = "total")   

#sorting descending order
Degree <- sort(Degree, decreasing = T)   
head(as.data.frame(Degree))

# eigenvector centrality
EVcentrality <- evcent(coPurchaseNetworkGraph)    
EVcentrality <- EVcentrality$vector   # convert to array
EVcentrality <- sort(EVcentrality, decreasing = T)
head(as.data.frame(EVcentrality))


avgcc <- transitivity(coPurchaseNetworkGraph, type = "average", isolates = "zero") #average clustering coeff
avgcc



##############################################################################################################################
##############################################################################################################################
#Step 2:
# A) Create Communities using InfoMaps Algorithm
# B) Attach communityId to the Network Graph Node Attribute 
##############################################################################################################################
##############################################################################################################################

# Step2A
# Create Communities 
coPurchaseCommunity <- infomap.community(coPurchaseNetworkGraph) 

# length(coPurchaseCommunity)  # This function shows number of communities created
# algorithm(coPurchaseCommunity) # This function shows name of the algorithm applied for community creation

# Step2B
# Attach community Membership as a vertex attribute 
coPurchaseNetworkGraph <-  set.vertex.attribute(coPurchaseNetworkGraph, "Community", value = coPurchaseCommunity$membership)  

##############################################################################################################################
##############################################################################################################################
#Step 3: 
# A) Identify top communities based on average degree of the nodes
# B) Visualizing Communities:
##############################################################################################################################
##############################################################################################################################

# Step3A Created a function which returns top communities in a dataframe 

topCommunities = function(networkGraph, topCommunityCount = 5){
  
  # Create a new graph where we contract all vertices in a single community to one vertice, and find the average degree
  coPurchaseCommunityGraph <-  contract.vertices(networkGraph, V(networkGraph)$Community, vertex.attr.comb = list(Degree ="mean", Community=toString))
  
  # save average degree associated with communities within a vector 
  communityMeanDegreeVector <- V(coPurchaseCommunityGraph)$Degree
  
  # get community list 
  communityList <- V(coPurchaseCommunityGraph)$Community
  
  # Save community number in a list 
  communityNumericVectorList <- lapply(communityList, function(x){as.vector(as.numeric(strsplit(x, ",")[[1]]))})
  
  # Save community membership into a vector 
  communityMembershipVector <-  sapply(communityNumericVectorList, function(x){(x)[1]})
  
  # Compute number of vertices within a community and save it to a vector  
  communityNodeCountVector <- sapply(communityNumericVectorList, function(x){length(x)})
  
  # Create dataframe
  communityDetailsDataFrame <- data.frame(communityMembershipVector,communityMeanDegreeVector,communityNodeCountVector)
  
  # Order rows based on communityMeanSalesRankVector
  communityDetailsDataFrame <- arrange(communityDetailsDataFrame,desc(communityMeanDegreeVector))
  
  # Change column names
  colnames(communityDetailsDataFrame) <- c("communityMembership","communityMeanDegree","communityNodeCount")
  
  # Return Top Communities 
  return(head(communityDetailsDataFrame,topCommunityCount))
  
}


topCommunities(coPurchaseNetworkGraph)


# Step3B Visualizing Communities

communityGraph <-
  induced.subgraph(coPurchaseNetworkGraph, which(V(coPurchaseNetworkGraph)$Community == 222))

unique(V(coPurchaseNetworkGraph)$Group)

V(communityGraph)$color=V(communityGraph)$Group #assign Group attribute as the vertex color
V(communityGraph)$color=gsub("Others","red",V(communityGraph)$color) #Others will be red
V(communityGraph)$color=gsub("Book","violet",V(communityGraph)$color) #Book will be violet
V(communityGraph)$color=gsub("Music","green",V(communityGraph)$color) #Music will be green
V(communityGraph)$color=gsub("DVD","orange",V(communityGraph)$color) #DVD will be orange
V(communityGraph)$color=gsub("Video","yellow",V(communityGraph)$color) #Video will be yellow


tkplot(
  communityGraph,
  vertex.size = 9,
  vertex.label = V(communityGraph)$Title,
  vertex.label.dist = -2,
  edge.arrow.size = 0.5,
  edge.color = "black",
  canvas.width = 450,
  canvas.height = 500,
  layout= layout.circle,
  vertex.color=V(communityGraph)$color
)


##############################################################################################################################
##############################################################################################################################
#Step 4:  
# Identify Top Products based on salesRank
##############################################################################################################################
##############################################################################################################################


topCommunityProducts = function(networkGraph,communityMembership,topCommunityProductCount = 5){
  
  # Create a subgraph for the given community from the main network graph 
  communityGraph <-induced.subgraph(networkGraph, which(V(networkGraph)$Community==communityMembership))
  
  # extract nodeId, salesRank, title, group and averageRating from the community graph 
  nodes <- as.vector(as.numeric(V(communityGraph)$name))
  salesRank <- as.vector(V(communityGraph)$SalesRank)
  title <- as.vector(V(communityGraph)$Title)
  group <- as.vector(V(communityGraph)$Group)
  averageRating <- as.vector(V(communityGraph)$AverageRating)
  
  
  # Create a dataframe
  nodesDataFrame <- data.frame(nodes,title,group,salesRank,averageRating)
  
  # Filter DataFrame (Remove all rows with salesRank less than 0 )
  nodesDataFrame <- nodesDataFrame[nodesDataFrame$salesRank>0,]
  
  # Sort DataFrame rows based on salesRank 
  nodesDataFrame <- arrange(nodesDataFrame,salesRank)
  
  # Rename DataFrame Columns
  colnames(nodesDataFrame) <- c("NodeId","Title","Group","SalesRank","AverageRating")
  
  # Return top n products within community based on salesRank (n = topCommunityProductCount)
  return(head(nodesDataFrame,topCommunityProductCount))
  
}

topCommunityProducts(coPurchaseNetworkGraph,3)

##############################################################################################################################
##############################################################################################################################
#Step 5: Identify cliques within the top community which includes top product of the community
# I)   Create Subgraph for the given community 
# II)  Identify cliques of size 4 within the subgraph 
# III) Filter out cliques based on the given productId 
# IV)  Export cliques 
##############################################################################################################################
##############################################################################################################################

exportCliques = function(networkGraph,communityMembership,productNodeId){
  
  # Create a subgraph for the given community from the main network graph 
  communityGraph <-induced.subgraph(networkGraph, which(V(networkGraph)$Community==communityMembership))
  
  
  # Identify cliques from the community graph 
  # The output of the following funciton is a list object consisting of vectors of vertices of cliques
  CliqueVertexList = cliques(communityGraph,min = 4,max = 4)
  
  # convert the vector of vertices for a given clique to graph object and save it to list object 
  CliqueList <- lapply(CliqueVertexList, function (x) { induced_subgraph(communityGraph, x) })

  # check if the given productNodeId is present within clique graph objects 
  ProductBasedCliqueFlagList = sapply(CliqueList, function(x){as.vector(table(V(x)$name==productNodeId))[2]})
  ProductBasedCliqueFlagList <- ifelse(is.na(ProductBasedCliqueFlagList),0,1)
  
  # Filter out all clique graph objects where productNodeId is not present 
  ProductBasedCliqueList <- CliqueList[ProductBasedCliqueFlagList==1]

  cliquesDataFrameList <- lapply(ProductBasedCliqueList, 
                                 function(y) {as.data.frame(list(NodeId=as.numeric(V(y)$name),
                                                                 Title = as.character(V(y)$Title),Group=as.vector(V(y)$Group),
                                                                 SalesRank=as.numeric(V(y)$SalesRank),AverageRating=as.numeric(V(y)$AverageRating),
                                                                 CliqueId = 1) , stringsAsFactors=FALSE)})
  cliqueDataFrame <- data.frame() # Prepare 
  tempCliqueFrame <- data.frame()
    
  for(i in 1:length(cliquesDataFrameList)){
    tempCliqueFrame <- cliquesDataFrameList[[i]]
    tempCliqueFrame$CliqueId = i
    cliqueDataFrame = rbind(cliqueDataFrame,tempCliqueFrame)
  }
  cliqueDataFrame$CommunityMembership = communityMembership
  cliqueDataFrame$PopularProductId = productNodeId
  
  write.csv(cliqueDataFrame,"cliquesData.csv",row.names = F)
  View(cliqueDataFrame)
}

exportCliques(coPurchaseNetworkGraph,3,600)



