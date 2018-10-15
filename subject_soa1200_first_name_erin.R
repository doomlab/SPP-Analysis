#fancy code to set the working directory to the current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#use the right file either LDT or N
spp.data = read.csv("subjectdataN.csv") 

#pull only the information you are interested in
spp.data = subset(spp.data, 
                  )