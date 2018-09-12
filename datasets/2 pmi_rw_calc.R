##set working directory to datasets
setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/SPP-Analysis/datasets")

##pull in the cosine information
alldata = read.table("all averaged cosine.txt", quote = "\"", comment.char = "")
colnames(alldata) = c("cue", "target", "root", "raw", "affix", "old")

#root cosine only
rootcos = alldata[, c("cue", "target", "root")]

library(reshape2)
#fix into wide dataset
wideroot = dcast(rootcos, cue ~ target)
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
rw_matrix2$index2 = apply( rw_matrix2[ , 2:1 ] , 1 , paste , collapse = ".")

##merge with item data (should open from spp folder)
library(expss)
itemdata$rw_cosine = vlookup(itemdata$index, rw_matrix2, "Freq", "index")
itemdata$rw_cosine2 = vlookup(itemdata$index, rw_matrix2, "Freq", "index2")
itemdata$rw_cosine_final = ifelse(!is.na(itemdata$rw_cosine), 
                                 itemdata$rw_cosine, 
                                 itemdata$rw_cosine2)

rootcos$index = apply(rootcos[ , 1:2], 1, paste, collapse = ".")
rootcos$index2 = apply(rootcos[ , 2:1], 1, paste, collapse = ".")
itemdata$full_cosine = vlookup(itemdata$index, rootcos, "root", "index")
itemdata$full_cosine2 = vlookup(itemdata$index, rootcos, "root", "index2")
itemdata$full_cos_final = ifelse(!is.na(itemdata$full_cosine), 
                                 itemdata$full_cosine, 
                                 itemdata$full_cosine2)
itemdata$full_cos_final[is.na(itemdata$full_cos_final)] = 0
#itemdata = itemdata[ , -c(39:40)]

#add pmi to the data
pmi_matrix2 = as.data.frame(as.table(pmi_matrix))
pmi_matrix2$index = apply( pmi_matrix2[ , 1:2 ] , 1 , paste , collapse = ".")
pmi_matrix2$index2 = apply( pmi_matrix2[ , 2:1 ] , 1 , paste , collapse = ".")
itemdata$pmi_cosine = vlookup(itemdata$index, pmi_matrix2, "Freq", "index")
itemdata$pmi_cosine2 = vlookup(itemdata$index, pmi_matrix2, "Freq", "index2")
itemdata$pmi_cosine_final = ifelse(!is.na(itemdata$pmi_cosine), 
                                  itemdata$pmi_cosine, 
                                  itemdata$pmi_cosine2)