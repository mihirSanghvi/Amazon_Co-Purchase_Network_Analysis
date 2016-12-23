# Amazon Co-Purchase Network Analysis

Raw Data Files:

* amazon-meta.txt - Amazon Products Metadata
* Amazon0601.txt  - Amazon Co-Purchase EdgeList Data
* amazon-meta.txt - Amazon Products Metadata Sample File

There are mainly 2 R code files.
1. Project-I-Data-Preperation.R 
2. Project-I-Co-Purchase-Network-Analysis.R

-> Project-I-Data-Preperation.R takes long time to preprocess the data (75-90 minutes), 
-> You can instead run Project-I-Data-Preperation-Sample-Data.R (it executes faster as it preprocesses a sample data file)

-> Project-I-Co-Purchase-Network-Analysis.R takes almost 2-5 hours depending on the configuration of the machine
There is one function "infomap.community" which takes 95% of the time (2-4.8 hours)

-> We have created the workspace after creating communities using "infomap.community" function

-> You can run step-0 and then load the workspace the following function 

load("Project-I-Workspace.RData")

and directly go to step 2B and run the rest of the code...
