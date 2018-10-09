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
#allLDT = read.csv("all ldt subs_all trials3.csv")
#allname = read_excel("all naming subjects.xlsx")

##first associate is 1, other associate is 2 ian the primecond
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
swow = read.csv("strength.SWOW-EN.R123.csv")

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

##create swow statistics
swowfsgss = as.data.frame(table(swow$cue))
swowfanss = as.data.frame(table(swow$response))

####create item data####
library(expss)
itemdata = as.data.frame(matrix(NA, nrow = nrow(firstassocLDT)*4, ncol = 2))
colnames(itemdata) = c("prime", "target")

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
itemdata$swow.t.fsg_ss = vlookup(itemdata$target, swowfsgss, "Freq", "Var1")
itemdata$swow.p.fsg_ss = vlookup(itemdata$prime, swowfsgss, "Freq", "Var1")
itemdata$swow.t.fan_ss = vlookup(itemdata$target, swowfanss, "Freq", "Var1")
itemdata$swow.p.fan_ss = vlookup(itemdata$prime, swowfanss, "Freq", "Var1")
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
itemdata$p.fan_ss = vlookup(itemdata$prime, singlewords, "fanin", "word")
itemdata$t.fan_ss = vlookup(itemdata$target, singlewords, "fanin", "word")
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
itemdata$p.fan_ss = as.numeric(itemdata$p.fan_ss)
itemdata$t.fan_ss = as.numeric(itemdata$t.fan_ss)

##deal with spelling stuff
itemdata$root[itemdata$index == "word.letter"] = 0.229909167
itemdata$affix[itemdata$affix == "word.letter"] = 0.2655702
itemdata$p.fss[itemdata$prime == "word"] = 5
itemdata$p.css[itemdata$prime == "word"] = 75

#hatchet - ax
#wed - thursday

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

####make subject level data here####
##here we will need to apply the same basic structure into creating the overall LDT/naming datasets

#pull in completed item data
itemdata = read.csv("../itemdata.csv")

#read in the participant files
library(readxl)
allLDT = read.csv("all ldt subs_all trials3.csv")
allname = read_excel("all naming subjects.xlsx")

#create file structure
subjectdataLDT = matrix(NA, nrow(allLDT), ncol = ncol(itemdata))
colnames(subjectdataLDT) = colnames(itemdata)
subjectdataLDT = cbind(allLDT, subjectdataLDT[ , -c(1:4)])

subjectdataN = matrix(NA, nrow(allname), ncol = ncol(itemdata))
colnames(subjectdataN) = colnames(itemdata)
subjectdataN = cbind(allname, subjectdataN[ , -c(1:4)])

#now merge with other information we are using
library(expss)
subjectdataLDT$prime = tolower(as.character(subjectdataLDT$prime))
subjectdataLDT$index = paste(subjectdataLDT$prime, subjectdataLDT$target, sep = ".")
subjectdataLDT$p.length = vlookup(subjectdataLDT$prime, itemdata, "p.length", "prime")
subjectdataLDT$t.length = vlookup(subjectdataLDT$target, itemdata, "t.length", "target")


##keep going editing these - figure out how to analyze this nonsense. 
subjectdataLDT$p.orthoN = vlookup(subjectdataLDT$prime, itemdata, "orthoN", "word")
subjectdataLDT$t.orthoN = vlookup(subjectdataLDT$target, itemdata, "orthoN", "word")
subjectdataLDT$p.phonoN = vlookup(subjectdataLDT$prime, itemdata, "phonoN", "word")
subjectdataLDT$t.phonoN = vlookup(subjectdataLDT$target, itemdata, "phonoN", "word")
subjectdataLDT$p.freq = vlookup(subjectdataLDT$prime, itemdata, "log_subtlex", "word")
subjectdataLDT$t.freq = vlookup(subjectdataLDT$target, itemdata, "log_subtlex", "word")
subjectdataLDT$p.POS = vlookup(subjectdataLDT$prime, itemdata, "POS", "word")
subjectdataLDT$t.POS = vlookup(subjectdataLDT$target, itemdata, "POS", "word")
subjectdataLDT$swowfsg = vlookup(subjectdataLDT$index, swow, "R123.Strength", "index")
subjectdataLDT$swow.t.fsg_ss = vlookup(subjectdataLDT$target, swowfsgss, "Freq", "Var1")
subjectdataLDT$swow.p.fsg_ss = vlookup(subjectdataLDT$prime, swowfsgss, "Freq", "Var1")
subjectdataLDT$swow.t.fan_ss = vlookup(subjectdataLDT$target, swowfanss, "Freq", "Var1")
subjectdataLDT$swow.p.fan_ss = vlookup(subjectdataLDT$prime, swowfanss, "Freq", "Var1")
subjectdataLDT$fsg = c(firstassocLDT$FAS, otherassocLDT$FAS, firstassocN$FAS, otherassocN$FAS)
subjectdataLDT$bsg = c(firstassocLDT$BAS, otherassocLDT$BAS, firstassocN$BAS, otherassocN$BAS)
subjectdataLDT$p.fsg_ss = vlookup(subjectdataLDT$prime, singlewords, "setsize", "word")
subjectdataLDT$t.fsg_ss = vlookup(subjectdataLDT$target, singlewords, "setsize", "word")
subjectdataLDT$p.css = vlookup(subjectdataLDT$prime, cosiness, "Freq", "Var1")
subjectdataLDT$t.css = vlookup(subjectdataLDT$target, cosiness, "Freq", "Var1")
subjectdataLDT$p.fss = vlookup(subjectdataLDT$prime, cosinefs, "rootset", "cue")
subjectdataLDT$t.fss = vlookup(subjectdataLDT$target, cosinefs, "rootset", "cue")
subjectdataLDT$JCN = vlookup(subjectdataLDT$index, jcn, "JCNmaki", "index")
subjectdataLDT$root = vlookup(subjectdataLDT$index, cosine_combined, "root", "index")
subjectdataLDT$affix = vlookup(subjectdataLDT$index, cosine_combined, "affix", "index")
subjectdataLDT$distance = vlookup(subjectdataLDT$index, cbow_combined, "distance", "index")
subjectdataLDT$LSA = vlookup(subjectdataLDT$index, doublewords, "LSA_f.t", "index")
subjectdataLDT$LSA2 = vlookup(subjectdataLDT$index, doublewords, "LSA", "index")
subjectdataLDT$beagle = vlookup(subjectdataLDT$index, doublewords, "BEAGLE_PMI", "index")
subjectdataLDT$p.fan_ss = vlookup(subjectdataLDT$prime, singlewords, "fanin", "word")
subjectdataLDT$t.fan_ss = vlookup(subjectdataLDT$target, singlewords, "fanin", "word")
subjectdataLDT$SOA200 = c(firstassocLDT$LDT.200ms.Z.Priming, otherassocLDT$LDT.200ms.Z.Priming, 
                    firstassocN$NT.200ms.Z.Priming, otherassocN$NT.200ms.Z.Priming)
subjectdataLDT$SOA1200 = c(firstassocLDT$LDT.1200ms.Z.Priming, otherassocLDT$LDT.1200ms.Z.Priming, 
                     firstassocN$NT.1200ms.Z.Priming, otherassocN$NT.1200ms.Z.Priming)
subjectdataLDT$root[is.na(subjectdataLDT$root)] = 0
subjectdataLDT$affix[is.na(subjectdataLDT$affix)] = 0
subjectdataLDT$p.phonoN = as.numeric(subjectdataLDT$p.phonoN)
subjectdataLDT$t.phonoN = as.numeric(subjectdataLDT$t.phonoN)
subjectdataLDT$p.fsg_ss = as.numeric(subjectdataLDT$p.fsg_ss)
subjectdataLDT$t.fsg_ss = as.numeric(subjectdataLDT$t.fsg_ss)
subjectdataLDT$p.fan_ss = as.numeric(subjectdataLDT$p.fan_ss)
subjectdataLDT$t.fan_ss = as.numeric(subjectdataLDT$t.fan_ss)

##deal with spelling stuff
subjectdataLDT$root[subjectdataLDT$index == "word.letter"] = 0.229909167
subjectdataLDT$affix[subjectdataLDT$affix == "word.letter"] = 0.2655702
subjectdataLDT$p.fss[subjectdataLDT$prime == "word"] = 5
subjectdataLDT$p.css[subjectdataLDT$prime == "word"] = 75

#hatchet - ax
#wed - thursday

##clean up part of speech
subjectdataLDT$p.POSr = substr(subjectdataLDT$p.POS, 0, 2)
subjectdataLDT$t.POSr = substr(subjectdataLDT$t.POS, 0, 2)
subjectdataLDT$p.POSr = gsub("JJ", "other", subjectdataLDT$p.POSr)
subjectdataLDT$p.POSr = gsub("RB", "other", subjectdataLDT$p.POSr)
subjectdataLDT$p.POSr = gsub("mi", "other", subjectdataLDT$p.POSr)
subjectdataLDT$t.POSr = gsub("JJ", "other", subjectdataLDT$t.POSr)
subjectdataLDT$t.POSr = gsub("RB", "other", subjectdataLDT$t.POSr)
subjectdataLDT$t.POSr = gsub("mi", "other", subjectdataLDT$t.POSr)




