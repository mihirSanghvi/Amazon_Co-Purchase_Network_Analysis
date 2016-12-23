##############################################################################################################################
# Title: " Project-I Data Preperation"
# Team Name: "monsteRs"
##############################################################################################################################
#Converting a flat file similar to json file into copurchasemetadata file
##############################################################################################################################
##############################################################################################################################
#Step 1 :- 
#Upload the data into the r environment
#remove the unnecessary rows based on the attribute values 
#Attributes needed are Id,title, review,salesrank and group
#Divide the rows into two columns based on the : seperator
#trim the row values and upload data to excel with name as 
##############################################################################################################################
##############################################################################################################################
  
#remove all the data present in the r environment  
rm(list=ls())
#install and load the packages used in step 1  
install.packages("data.table")
install.packages("sqldf")
install.packages("tidyr")
install.packages("igraph")

library("igraph")
library("data.table") # to use read.table function which is faster than write.csv
library("sqldf") # to write sql queries in R using sqldf function
library("tidyr") # Used a method 'seperate' to divide the column into two columns using delimiter

#Set the directory path to the path where file is located

path ="C:/Users/Mihir Sanghvi/Desktop/monsteRs"
setwd(path)

#assign filename to the filename variable
filename="amazon-meta.txt"

#load the text file with every new line is new row 
system.time(amazonmeta <- read.table(filename,sep = ",",quote = ""))

#'Id','title','categories','reviews','group','salesrank'
#remove the columns which doesnt start with the above mentioned values
filteredRawData <- sqldf("select * from amazonmeta where 
                          trim(V1)  like 'Id%'  or 
                          trim(V1)  like 'title%'  or
                          trim(V1)  like 'reviews%'  or
                          trim(V1)  like 'group%'  or
                          trim(V1)  like 'salesrank%' ")

rm(amazonmeta)


#divide every line to two columns based on the first occurence of the colon
filteredRawData <- separate(data=filteredRawData,col=V1,
                      into=c("attribute","value"),sep="\\:",extra = "merge")


#remove the spaces in the rows 
filteredRawData <- sqldf("select  trim(attribute) key ,trim(value) value from filteredRawData ")


#Save the Data to the excel file 
write.csv(x=filteredRawData,"FilteredRawData.csv",row.names = FALSE)
#remove the dataframe from the environment once loaded into the excel
rm(filteredRawData)

##################################################################################################################################
##################################################################################################################################
#Step 2 :- 
#Upload the Step1 Output Data i.e., FilteredRawData.csv into the r environment
#Convert the key value pair to the column and rows using loop 
#Conver the Data frame to matrix for the faster value accessing
#loop the data based on the chunk size
#loop the data as chunks and convert the key value to column,values
#after reaching 20000 rows the data is then uploaded to the 
##################################################################################################################################
##################################################################################################################################
#remove all the data present in the workspace
rm(list=ls())

#packages needed
install.packages("foreach")
library(foreach) #used to loop faster compared to default for loop

#Access the file output in the Step 1 which has key & value as columns
#key are the column name and the values are column values
processdata <- read.csv("FilteredRawData.csv")

#convert the data frame into excel for faster loop
processdatamatrix <- as.matrix(processdata)
#remove the data frame after converting the 
rm(processdata)

#created data frames to use in for loop
target <- data.frame()
existingData<-data.frame()

#assign total number of rows present in the matrix to "rows" variable 
rows <- nrow(processdatamatrix) #nrow(processdata)         
#chunk size to access the subset instead of accessing the total matrix to make the loop faster
chunkSize <-20000

#Calculate the number of data chunks present in the data
totalChunks <- round(rows/chunkSize,0)
#if the data present is less than 20000 then assign chunks  1
if(totalChunks==0)
{
  totalChunks=1
  chunkSize=rows
}

#rowincrement
i=1
#variable to change the row number if reviews data is not present
check=TRUE
#to check already the existing data 
j=1


#for loop to convert the data into columns
#foreach is similar to the for loop but faster
system.time(foreach(p=1:totalChunks) %do% {
  #Start and End of the subset 
  start=(p-1)*chunkSize+1
  end=p*chunkSize
  #if the last rows are left then add them in the last loop
  if(p==totalChunks)
  {
    end=end +(rows%%chunkSize)
  }
  
  #subset the data based on the chunk size
  samplekeyvalue= processdatamatrix[start:end,]
  rows <- nrow(samplekeyvalue)   
  foreach(q=1:rows) %do% {
    #key is column name
    key <- samplekeyvalue[q,1]
    #value is to assign value to the column
    value <- samplekeyvalue[q,2]
    
    #if the product has no reviews then when next Id is reached then the row increment will be done
    if(check==FALSE & key=="Id")
    {
      i=i+1
    }
    if(key=="Id"){
      #To display multiples of 1000 rows are processed
      if(i%%1000==0){
        print((j-1)*20000 +i)
      }
      check=FALSE
    }
    #Access the existing data and merge the new data into the existing data
    #Upload the data into excel every time the loop reaches 20000 rows  
    if(i>10 & i%%20001==1 & key=='Id')
    {
      print("file write operation")
      #for the first time file has to be created so else is for the first time
      if(j>1)
      {
        existingData <- read.csv("CumulativeLoopedData.csv")
        write.csv(x=rbind(target,existingData[,c("Id","title","group","salesrank","reviews")]),file="CumulativeLoopedData.csv",row.names = FALSE)  
      }
      else
      {
        write.csv(x=rbind(target),file="CumulativeLoopedData.csv",row.names = FALSE)  
      }
      #empty the data present in the 
      target <- data.frame()
      existingData<- data.frame()
      i=1
      j=j+1
      check=T
    }
    #Create only specific columns mentioned in the if condition
    if(key=='Id' | key=='title' | key=='reviews' | key=='group' | key=='salesrank'){
      target[i,key]=value 
    }
    #increase the row count if the key value is reviews
    if(key=="reviews"){
      i=i+1
      check=TRUE
    }
  }
})

#access the exising data  
existingData <- read.csv("CumulativeLoopedData.csv")
#accumulate the last rows left after the for loop which is less than 20000
copurchasedata <- rbind(target,existingData)  
#remove the objects to release the memory 
rm(processdatamatrix)
rm(samplekeyvalue)
rm(existingData)
rm(target)

###############################################################################################################################################
##############################################################################################################################################
#Step 3:-
#review column has three sub columns total, downloaded and average rating 
#Extract the average rating from the data
##############################################################################################################################################
###############################################################################################################################################

#divide the review column data to extract the averagerating
tempdata <- separate(data=copurchasedata,col=reviews,
                     into=c("a1","total","downloaded","averagerating"),sep="\\:",extra = "merge")

#add the average rating to the copurchaseproduct
copurchasedata$averagerating <- tempdata$averagerating
#remove the reviews column
copurchasedata$reviews <- NULL
rm(tempdata)

coPurchaseMeta <- copurchasedata

###############################################################################################################################################
##############################################################################################################################################
#Step 4:-
#Data Cleaning & Missing Value Treatment  
#DiscontinuedProducts and Collapse additional levels in product group 
##############################################################################################################################################
###############################################################################################################################################

# Rename columns 
colnames(coPurchaseMeta) <- c("NodeId","Title","Group","SalesRank","AverageRating")


# Discontinued products do not have information other than NodeId
# Create new column that indicates discontinued products
coPurchaseMeta$DiscontinuedProduct <- factor(ifelse(is.na(coPurchaseMeta$Title),1,0))

# create table summary of Discontinued Products
table(coPurchaseMeta$DiscontinuedProduct)
# There are total 5868 Discontinued Products

# Replace NA in title and Group columns with Discontinued Product
# Replace NA in SalesRank and AverageRating columns with 0
coPurchaseMeta$Title[is.na(coPurchaseMeta$Title)] <- "Discontinued Product"
coPurchaseMeta$Group[is.na(coPurchaseMeta$Group)] <- "Discontinued Product"
coPurchaseMeta$SalesRank[is.na(coPurchaseMeta$SalesRank)] <- 0
coPurchaseMeta$AverageRating[is.na(coPurchaseMeta$AverageRating)] <- 0 


# Create table summary of product groups
table(coPurchaseMeta$Group)
# In the dataset there are 4 main categories -> Music, Video, Book, DVD 


# Make Group Column as Factor 
coPurchaseMeta$Group <- factor(coPurchaseMeta$Group)

# Collapse Levels other than main 4 categories
levels(coPurchaseMeta$Group) = list(Music = c("Music"),DVD =c("DVD"),Video = c("Video"),Book=c("Book"), Others = c("Baby Product","CE","Software","Sports","Toy","Video Games","Discontinued Product"))
table(coPurchaseMeta$Group)




###############################################################################################################################################
##############################################################################################################################################
#Step 5:-
#Load Edgelist and extract metadata about the nodes present in edgelist 
##############################################################################################################################################
###############################################################################################################################################


# Load Downloaded EdgeList 
originalEdgeList <- read.delim("Amazon0601.txt", header=FALSE, comment.char="#")
colnames(originalEdgeList) <- c("FromNodeId","ToNodeId")


##########################################################################################
# Extract metadata only about nodes present in edgelist 
# Step-A
# i)   Extract nodes from the edgelist data and compute degree and Eigenvector
# ii)  Save nodeId, degree and Eigenvector into a NodesDataFrame 
# iii) Merge the NodesDataFrame with coPurchaseMeta datafeame
##########################################################################################

# Create Graph from Original Edge List 
graphFromOriginalEdgeList <- graph.data.frame(originalEdgeList, directed = TRUE)

# Get name of nodes
Nodes = V(graphFromOriginalEdgeList)$name   

# Calculate degree of nodes
Degree = degree(graphFromOriginalEdgeList, mode='total', loops=FALSE)  

# Calculate Eigenvector centrality of nodes (list format)
ECent =evcent(graphFromOriginalEdgeList) 

# Get Eigenvector centrality of nodes (convert to array)
ECent=ECent$vector 

# Bind all data for node in a dataframe 
NodesDataFrame = cbind.data.frame(Nodes, Degree, ECent) 

# Set Col Names 
colnames(NodesDataFrame) <- c("NodeId","Degree","EigenValueCentrality")


# Merge coPurchaseMeta and Nodes data from original edgelist 
coPurchaseMetaNew <- merge(x = coPurchaseMeta,y = NodesDataFrame)


##########################################################################################
# Step-B, Merge the edgelist dataframe with metadata dataframe and create new EdgeList 
##########################################################################################

# There are some nodes present in originalEdgeList that are not present in metadata, so we are merging 
# originalEdgeList with MetaData and create new edgeList 

coPurchaseData <- merge(x = originalEdgeList[c("FromNodeId","ToNodeId")], y = coPurchaseMetaNew[c("NodeId")], by.x = "FromNodeId", by.y = "NodeId" )
coPurchaseData <- merge(x = coPurchaseData[c("FromNodeId","ToNodeId")], y = coPurchaseMetaNew[c("NodeId")], by.x = "ToNodeId", by.y = "NodeId" )
coPurchaseEdgeList <- data.frame(coPurchaseData[c("FromNodeId","ToNodeId")])



##########################################################################################
# Step-C, Write CSV Files
# i)  Write MetaData File
# ii) Write edgelist File
##########################################################################################

write.csv(coPurchaseEdgeList,"coPurchaseEdgeList.csv",row.names = F)
View(coPurchaseEdgeList)

write.csv(coPurchaseMetaNew,"coPurchaseMeta.csv",row.names = F)
View(coPurchaseMetaNew)

