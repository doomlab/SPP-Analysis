##set working directory to datasets
setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/SPP-Analysis/datasets")

##open the item data to add to 
itemdata = read.csv("itemdata.csv")

##pull in the cosine information
alldata = read.table("all averaged cosine.txt", quote = "\"", comment.char = "")
colnames(alldata) = c("cue", "target", "root", "raw", "affix", "old")

#root cosine only
rootcos = alldata[, c("cue", "target", "root")]

library(reshape2)
#fix into wide dataset
wideroot = dcast(rootcos, cue ~ target, value.var = "root")
rownames(wideroot) = wideroot$cue #set up row names
wideroot = wideroot[ , -1] #drop cue so it's cues by target only
wideroot[is.na(wideroot)] = 0 #fix all connections to zero
#diag(wideroot) = 0 #set diagonal to zero for normalization
sum_cosine = rowSums(wideroot)
wideroot = wideroot/sum_cosine #normalize 

is.nan.data.frame = function (x) {
  do.call(cbind, lapply(x, is.nan))
}

wideroot[is.nan(wideroot)] = 0 #take out the NaN since we divided by zero
wideroot = as.table(as.matrix(wideroot)) #coerce into a table

##try pmi
library(svs)
pmi_matrix = pmi(wideroot, 
                 base = 2, 
                 normalize = T)
#write.csv(pmi_matrix, "pmi_matrix.csv")

##try random walk
#identity matrix minus alpha times inverse pmi
alpha = .25
rw_matrix = diag(nrow(pmi_matrix)) - alpha * solve(pmi_matrix)
#write.csv(rw_matrix, "rw_matrix_cos.csv")

#make data frame to merge
rw_matrix2 = as.data.frame(as.table(rw_matrix))
rw_matrix2$index = apply( rw_matrix2[ , 1:2 ] , 1 , paste , collapse = ".")

##merge with item data 
library(expss)
itemdata$rw_cosine = vlookup(itemdata$index, rw_matrix2, "Freq", "index")

rootcos$index = apply(rootcos[ , 1:2], 1, paste, collapse = ".")
rootcos$index2 = apply(rootcos[ , 2:1], 1, paste, collapse = ".")
itemdata$full_cosine = vlookup(itemdata$index, rootcos, "root", "index")
itemdata$full_cosine2 = vlookup(itemdata$index, rootcos, "root", "index2")
itemdata$full_cos_final = ifelse(!is.na(itemdata$full_cosine), 
                                 itemdata$full_cosine, 
                                 itemdata$full_cosine2)
#remember to drop these extra columns at the end

#add pmi to the data
pmi_matrix2 = as.data.frame(as.table(pmi_matrix))
pmi_matrix2$index = apply( pmi_matrix2[ , 1:2 ] , 1 , paste , collapse = ".")
itemdata$pmi_cosine = vlookup(itemdata$index, pmi_matrix2, "Freq", "index")

#drop full_cosine and full_cosine2
itemdata = itemdata[ , -c(41,42)]
itemdata$full_cos_final[is.na(itemdata$full_cos_final)] = 0

#fill in word letter NA values
View(rw_matrix2[rw_matrix2$Var1 == "words", ])
itemdata$rw_cosine[is.na(itemdata$rw_cosine)] = -0.006338416	#words.letter

View(pmi_matrix2[pmi_matrix2$Var1 == "words", ])
itemdata$pmi_cosine[is.na(itemdata$pmi_cosine)] = 


# create pmi and rw for association ---------------------------------------

swow = read.csv("strength.SWOW-EN.R123.csv")
swow$cue = tolower(as.character(swow$cue))
swow$response = tolower(as.character(swow$response))
swow$cue = gsub(" ", "", swow$cue)
swow$response = gsub(" ", "", swow$response)
swow$cue = gsub("'", "", swow$cue)
swow$response = gsub("'", "", swow$response)
swow$index = paste(swow$cue, swow$response, sep = ".")
swow = swow[swow$R123 > 1 , ]
swow = swow[ , c("cue", "response", "R123.Strength")]

#testing to see what the duplicates are
swow$dupe=duplicated(swow)
swow.dupe=swow[swow$dupe=="TRUE",]
View(swow.dupe)
new.swow=merge(swow.dupe, swow, by=c("cue", "response"))
#this shows that all duplicates are identical when it comes to R123.Strength, so we can just remove duplicates
swow=swow[swow$dupe=="FALSE",]
swow = swow[ , c("cue", "response", "R123.Strength")]

#fix into wide dataset
wideswow = dcast(swow, cue ~ response, value.var = "R123.Strength")
rownames(wideswow) = wideswow$cue #set up row names
wideswow = wideswow[ , -1] #drop cue column 
wideswow[is.na(wideswow)] = 0 #fix all connections to zero
#should already be normalized

is.nan.data.frame = function (x) {
  do.call(cbind, lapply(x, is.nan))
}

wideswow[is.nan(wideswow)] = 0 #take out the NaN since we divided by zero
wideswow = as.table(as.matrix(wideswow)) #coerce into a table

##try pmi
library(svs)
pmi_matrix = pmi(wideswow, 
                 base = 2, 
                 normalize = T)

##try random walk
#identity matrix minus alpha times inverse pmi
alpha = .25
rw_matrix = diag(nrow(pmi_matrix)) - alpha * solve(pmi_matrix)

#make data frame to merge
rw_matrix2 = as.data.frame(as.table(rw_matrix))
rw_matrix2$index = apply( rw_matrix2[ , 1:2 ] , 1 , paste , collapse = ".")

##merge with item data 
library(expss)
itemdata$rw_swow = vlookup(itemdata$index, rw_matrix2, "Freq", "index")

#add pmi to the data
pmi_matrix2 = as.data.frame(as.table(pmi_matrix))
pmi_matrix2$index = apply( pmi_matrix2[ , 1:2 ] , 1 , paste , collapse = ".")
itemdata$pmi_swow = vlookup(itemdata$index, pmi_matrix2, "Freq", "index")