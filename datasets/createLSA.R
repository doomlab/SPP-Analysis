setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/SPP-Analysis/datasets")

##this dataset is from the LSAfun website
##https://sites.google.com/site/fritzgntr/software-resources
load(file = "EN_100k_lsa.rda")

library(lsa)
library(readxl)

##example of how to find the rows
cosine(EN_100k_lsa["is", ], EN_100k_lsa["was", ])

##use the double words files 
doublewords = read_excel("double_word_spp.xlsx")

##fix up the double words files
doublewords$prime = tolower(as.character(doublewords$prime))
doublewords$TARGET = tolower(as.character(doublewords$TARGET))

doublewords$prime = gsub("'", "", doublewords$prime)
doublewords$TARGET = gsub("'", "", doublewords$TARGET)

doublewords$prime = gsub(" ", "", doublewords$prime)
doublewords$TARGET = gsub(" ", "", doublewords$TARGET)

doublewords$LSA = NA

##loop and get the cosines
for (i in 1:nrow(doublewords)){
  if (doublewords$prime[i] %in% rownames(EN_100k_lsa)) { #if prime word is in 
    if (doublewords$TARGET[i] %in% rownames(EN_100k_lsa)) { #if target word is in 
      doublewords$LSA[i] = cosine(EN_100k_lsa[doublewords$prime[i], ], EN_100k_lsa[doublewords$TARGET[i], ])
    }
  }
}
write.csv(doublewords, "doublewords.csv", row.names = F)
