####let's make a dataset####
##set the working directory to the datasets folder
library(readxl)
setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/SPP-Analysis/datasets")

####import the files to combine####
##spp specific priming
firstassocLDT = read.csv("assoc_relatedLDT.csv")
firstassocN = read.csv("assoc_relatedN.csv")
otherassocLDT = read.csv("other_assoc_relatedLDT.csv")
otherassocN = read.csv("other_assoc_relatedN.csv")
allLDT = read.csv("all ldt subs_all trials3.csv")
allname = read_excel("all naming subjects.xlsx")

##first associate is 1, other associate is 2 in the primecond
allLDT = subset(allLDT, rel == "rel")
allname = subset(allname, primecond == 1 | primecond == 2)

####figure out the information we want####
##length, frequency, orthoN, phonoN, POS
##all can be found in the single words file created from the item spreadsheet of SPP
##phonoN added from ELP manually to avoid duplicates.
singlewords = read_excel("single_word_spp.xlsx")

##associations
##FSG/BSG fan in and out
#doublewords = read_excel("double_word_spp.xlsx")
doublewords = read.csv("doublewords.csv")
##SWOW
swow = read.delim("strength.SWOW-EN.R123.csv")

##semantics
cosine_data = read.table("all averaged cosine.txt", quote= "\"", comment.char = "")
colnames(cosine_data) = c("first", "second", "root", "raw", "affix", "old")
cosinefs = read_excel("feature set size.xlsx")
##make cosine set size
cosiness = as.data.frame(table(cosine_data$first))
cosinessSMALL = read.csv("cos set size.csv")
cbow = read.csv("cbow mandera.csv")
jcn = read.csv("jcn_data.csv")

##thematics
##lsa was added to the double words file in a separate script

####clean up the cue target columns for everything####

##lower case
firstassocLDT$TargetWord = tolower(as.character(firstassocLDT$TargetWord))
firstassocLDT$Prime = tolower(as.character(firstassocLDT$Prime))
firstassocN$TargetWord = tolower(as.character(firstassocN$TargetWord))
firstassocN$Prime = tolower(as.character(firstassocN$Prime))
otherassocLDT$TargetWord = tolower(as.character(otherassocLDT$TargetWord))
otherassocLDT$Prime = tolower(as.character(otherassocLDT$Prime))
otherassocN$TargetWord = tolower(as.character(otherassocN$TargetWord))
otherassocN$Prime = tolower(as.character(otherassocN$Prime))
allLDT$prime = tolower(as.character(allLDT$prime))
allLDT$target = tolower(as.character(allLDT$target))
allname$prime = tolower(as.character(allname$prime))
allname$target = tolower(as.character(allname$target))
singlewords$word = tolower(as.character(singlewords$word))
doublewords$prime = tolower(as.character(doublewords$prime))
doublewords$TARGET = tolower(as.character(doublewords$TARGET))
swow$cue = tolower(as.character(swow$cue))
swow$response = tolower(as.character(swow$response))
cosine_data$first = tolower(as.character(cosine_data$first))
cosine_data$second = tolower(as.character(cosine_data$second))
cbow$word_1 = tolower(as.character(cbow$word_1))
cbow$word_2 =  tolower(as.character(cbow$word_2))
cosiness$Var1 = tolower(as.character(cosiness$Var1))
cosinessSMALL$Var1 = tolower(as.character(cosinessSMALL$Var1))
cosinefs$cue = tolower(as.character(cosinefs$cue))

##take out ' symbols
firstassocLDT$TargetWord = gsub("'", "", firstassocLDT$TargetWord)
firstassocLDT$Prime = gsub("'", "", firstassocLDT$Prime)
firstassocN$TargetWord = gsub("'", "", firstassocN$TargetWord)
firstassocN$Prime = gsub("'", "", firstassocN$Prime)
otherassocLDT$TargetWord = gsub("'", "", otherassocLDT$TargetWord)
otherassocLDT$Prime = gsub("'", "", otherassocLDT$Prime)
otherassocN$TargetWord = gsub("'", "", otherassocN$TargetWord)
otherassocN$Prime = gsub("'", "", otherassocN$Prime)
allLDT$prime = gsub("'", "", allLDT$prime)
allLDT$target = gsub("'", "", allLDT$target)
allname$prime = gsub("'", "", allname$prime)
allname$target = gsub("'", "", allname$target)
singlewords$word = gsub("'", "", singlewords$word)
doublewords$prime = gsub("'", "", doublewords$prime)
doublewords$TARGET = gsub("'", "", doublewords$TARGET)
swow$cue = gsub("'", "", swow$cue)
swow$response = gsub("'", "", swow$response)
cosine_data$first = gsub("'", "", cosine_data$first)
cosine_data$second = gsub("'", "", cosine_data$second)
cbow$word_1 = gsub("'", "", cbow$word_1)
cbow$word_2 =  gsub("'", "", cbow$word_2)
cosiness$Var1 = gsub("'", "", cosiness$Var1)
cosinessSMALL$Var1 = gsub("'", "", cosinessSMALL$Var1)
cosinefs$cue = gsub("'", "", cosinefs$cue)

##take out spaces
firstassocLDT$TargetWord = gsub(" ", "", firstassocLDT$TargetWord)
firstassocLDT$Prime = gsub(" ", "", firstassocLDT$Prime)
firstassocN$TargetWord = gsub(" ", "", firstassocN$TargetWord)
firstassocN$Prime = gsub(" ", "", firstassocN$Prime)
otherassocLDT$TargetWord = gsub(" ", "", otherassocLDT$TargetWord)
otherassocLDT$Prime = gsub(" ", "", otherassocLDT$Prime)
otherassocN$TargetWord = gsub(" ", "", otherassocN$TargetWord)
otherassocN$Prime = gsub(" ", "", otherassocN$Prime)
allLDT$prime = gsub(" ", "", allLDT$prime)
allLDT$target = gsub(" ", "", allLDT$target)
allname$prime = gsub(" ", "", allname$prime)
allname$target = gsub(" ", "", allname$target)
singlewords$word = gsub(" ", "", singlewords$word)
doublewords$prime = gsub(" ", "", doublewords$prime)
doublewords$TARGET = gsub(" ", "", doublewords$TARGET)
swow$cue = gsub(" ", "", swow$cue)
swow$response = gsub(" ", "", swow$response)
cosine_data$first = gsub(" ", "", cosine_data$first)
cosine_data$second = gsub(" ", "", cosine_data$second)
cbow$word_1 = gsub(" ", "", cbow$word_1)
cbow$word_2 =  gsub(" ", "", cbow$word_2)
cosiness$Var1 = gsub(" ", "", cosiness$Var1)
cosinessSMALL$Var1 = gsub(" ", "", cosinessSMALL$Var1)
cosinefs$cue = gsub(" ", "", cosinefs$cue)

##make index column
firstassocLDT$index = paste(firstassocLDT$Prime,firstassocLDT$TargetWord, sep = ".")
firstassocN$index = paste(firstassocN$Prime, firstassocN$TargetWord, sep = ".")
otherassocLDT$index = paste(otherassocLDT$Prime,otherassocLDT$TargetWord, sep = ".")
otherassocN$index = paste(otherassocN$Prime, otherassocN$TargetWord, sep = ".")
allLDT$index = paste(allLDT$prime, allLDT$target, sep = ".")
allname$index = paste(allname$prime, allname$target, sep = ".")
doublewords$index = paste(doublewords$prime, doublewords$TARGET, sep = ".")
swow$index = paste(swow$cue, swow$response, sep = ".")
cosine_data$index = paste(cosine_data$first, cosine_data$second, sep = ".")
cbow$index = paste(cbow$word_1, cbow$word_2, sep = ".")

##deal with the forward backward problem, cosine, jcn
cosine_data2 = cosine_data
cosine_data2$index = paste(cosine_data2$second, cosine_data2$first, sep = ".")
cosine_combined = rbind(cosine_data, cosine_data2)
cosine_combined = unique(cosine_combined) #get rid of word-word
cbow2  = cbow
cbow2$index = paste(cbow2$word_2, cbow2$word_1, sep = ".")
cbow_combined = rbind(cbow, cbow2)
cbow_combined = cbow_combined[ !duplicated(cbow_combined$index), ]
#already dealt with the forward backwards JCN as well as the index for the variable

####create item data####
library(expss)
itemdata = as.data.frame(matrix(NA, nrow = nrow(firstassocLDT)*4, ncol = 35))
colnames(itemdata) = c("prime", "target", "p.length", "index", "t.length",
                       "p.orthoN", "t.orthoN", "p.freq", "t.freq",
                       "p.phonoN", "t.phonoN", "p.POS", "t.POS",
                       "swowfsg", "fsg", "bsg", "p.fsg_ss", "t.fsg_ss", "p.css", "t.css",
                       "p.fanss", "t.fanss", "p.fss", "t.fss", "JCN", "root", "affix", "distance", "beagle",
                       "LSA", "LSA2", "relation", "task", "SOA200", "SOA1200")

itemdata$task = c(rep("LDT", nrow(firstassocLDT)*2), rep("NAME", nrow(firstassocLDT)*2))
itemdata$relation = c(rep("first", nrow(firstassocLDT)), 
                      rep("other", nrow(firstassocLDT)),
                      rep("first", nrow(firstassocLDT)),
                      rep("other",nrow(firstassocLDT)))
itemdata$prime = c(firstassocLDT$Prime, otherassocLDT$Prime, firstassocN$Prime, otherassocN$Prime)
itemdata$target = c(firstassocLDT$TargetWord, otherassocLDT$TargetWord, firstassocN$TargetWord, otherassocN$TargetWord)
itemdata$index = paste(itemdata$prime, itemdata$target, sep = ".")
itemdata$p.length = vlookup(itemdata$prime, singlewords, "length", "word")
itemdata$t.length = vlookup(itemdata$target, singlewords, "length", "word")
itemdata$p.orthoN = vlookup(itemdata$prime, singlewords, "orthoN", "word")
itemdata$t.orthoN = vlookup(itemdata$target, singlewords, "orthoN", "word")
itemdata$p.phonoN = vlookup(itemdata$prime, singlewords, "phonoN", "word")
itemdata$t.phonoN = vlookup(itemdata$target, singlewords, "phonoN", "word")
itemdata$p.freq = vlookup(itemdata$prime, singlewords, "log_subtlex", "word")
itemdata$t.freq = vlookup(itemdata$target, singlewords, "log_subtlex", "word")
itemdata$p.POS = vlookup(itemdata$prime, singlewords, "POS", "word")
itemdata$t.POS = vlookup(itemdata$target, singlewords, "POS", "word")
itemdata$swowfsg = vlookup(itemdata$index, swow, "R123.Strength", "index")
itemdata$fsg = c(firstassocLDT$FAS, otherassocLDT$FAS, firstassocN$FAS, otherassocN$FAS)
itemdata$bsg = c(firstassocLDT$BAS, otherassocLDT$BAS, firstassocN$BAS, otherassocN$BAS)
itemdata$p.fsg_ss = vlookup(itemdata$prime, singlewords, "setsize", "word")
itemdata$t.fsg_ss = vlookup(itemdata$target, singlewords, "setsize", "word")
itemdata$p.css = vlookup(itemdata$prime, cosiness, "Freq", "Var1")
itemdata$t.css = vlookup(itemdata$target, cosiness, "Freq", "Var1")
itemdata$p.fss = vlookup(itemdata$prime, cosinefs, "rootset", "cue")
itemdata$t.fss = vlookup(itemdata$target, cosinefs, "rootset", "cue")
itemdata$JCN = vlookup(itemdata$index, jcn, "JCNmaki", "index")
itemdata$root = vlookup(itemdata$index, cosine_combined, "root", "index")
itemdata$affix = vlookup(itemdata$index, cosine_combined, "affix", "index")
itemdata$distance = vlookup(itemdata$index, cbow_combined, "distance", "index")
itemdata$LSA = vlookup(itemdata$index, doublewords, "LSA_f.t", "index")
itemdata$LSA2 = vlookup(itemdata$index, doublewords, "LSA", "index")
itemdata$beagle = vlookup(itemdata$index, doublewords, "BEAGLE_PMI", "index")
itemdata$p.fanss = vlookup(itemdata$prime, singlewords, "fanin", "word")
itemdata$t.fanss = vlookup(itemdata$target, singlewords, "fanin", "word")
itemdata$SOA200 = c(firstassocLDT$LDT.200ms.Z.Priming, otherassocLDT$LDT.200ms.Z.Priming, 
                    firstassocN$NT.200ms.Z.Priming, otherassocN$NT.200ms.Z.Priming)
itemdata$SOA1200 = c(firstassocLDT$LDT.1200ms.Z.Priming, otherassocLDT$LDT.1200ms.Z.Priming, 
                     firstassocN$NT.1200ms.Z.Priming, otherassocN$NT.1200ms.Z.Priming)
itemdata$root[is.na(itemdata$root)] = 0
itemdata$affix[is.na(itemdata$affix)] = 0
itemdata$p.phonoN = as.numeric(itemdata$p.phonoN)
itemdata$t.phonoN = as.numeric(itemdata$t.phonoN)
itemdata$p.fsg_ss = as.numeric(itemdata$p.fsg_ss)
itemdata$t.fsg_ss = as.numeric(itemdata$t.fsg_ss)
itemdata$p.fanss = as.numeric(itemdata$p.fanss)
itemdata$t.fanss = as.numeric(itemdata$t.fanss)
itemdata$root[itemdata$index == "word.letter"] = 0.229909167
itemdata$affix[itemdata$affix == "word.letter"] = 0.2655702
itemdata$p.fss[itemdata$prime == "word"] = 5
itemdata$p.css[itemdata$prime == "word"] = 75

##clean up part of speech
itemdata$p.POSr = substr(itemdata$p.POS, 0, 2)
itemdata$t.POSr = substr(itemdata$t.POS, 0, 2)
itemdata$p.POSr = gsub("JJ", "other", itemdata$p.POSr)
itemdata$p.POSr = gsub("RB", "other", itemdata$p.POSr)
itemdata$p.POSr = gsub("mi", "other", itemdata$p.POSr)
itemdata$t.POSr = gsub("JJ", "other", itemdata$t.POSr)
itemdata$t.POSr = gsub("RB", "other", itemdata$t.POSr)
itemdata$t.POSr = gsub("mi", "other", itemdata$t.POSr)

#write.csv(itemdata[ , c("prime", "target", "p.POSr", "t.POSr")], "jcn_data.csv", row.names = F)
write.csv(itemdata, "itemdata.csv", row.names = F)
##here we will need to apply the same basic structure into creating the overall LDT/naming datasets

subjectdataLDT = matrix(NA, nrow(allLDT), ncol = 33)

colnames(subjectdataLDT) = c("prime", "target", "p.length", "t.length",
                       "p.orthoN", "t.orthoN", "p.freq", "t.freq",
                       "p.phonoN", "t.phonoN", "p.POS", "t.POS",
                       "swowfsg", "swowbsg", "p.fan", "t.fan", "p.css", "t.css",
                       "p.fss", "t.fss", "JCN", "root", "affix", "distance", "beagle", 
                       "LSA", "relation", "SOA", "ACC", "RT", "ZRT",
                       "subject", "trial")

subjectdataN = matrix(NA, nrow(allname), ncol = 33)

colnames(subjectdataN) = c("prime", "target", "p.length", "t.length",
                             "p.orthoN", "t.orthoN", "p.freq", "t.freq",
                             "p.phonoN", "t.phonoN", "p.POS", "t.POS",
                             "swowfsg", "swowbsg", "p.fan", "t.fan", "p.css", "t.css",
                             "p.fss", "t.fss", "JCN", "root", "affix", "distance", "beagle",
                             "LSA", "relation", "SOA", "ACC", "RT", 
                             "subject", "trial")





