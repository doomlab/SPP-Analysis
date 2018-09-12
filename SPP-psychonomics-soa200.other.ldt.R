#given prime type= other , task type= LDT , and DV=SOA200
#bring in data from github
install.packages(readr) #install pacakge that lets us read in the .csv file
library(readr) #call up that package
spp.data=read.csv("/Users/KDValentine/Documents/GitHub/SPP-Analysis/itemdata.csv") #bring in data--you will need to change this path name

#next, we need to split the data up so that we are only analyzing for our prime and task type
#we have prime type OTHER
spp.data.other=spp.data[spp.data$relation=="other",] #this keeps only rows where relation=other and keeps all columns

#we have task type LDT
spp.data.other.ldt=spp.data.other[spp.data.other$task=="LDT",] #this keeps only rows where task=LDT and keeps all columns

#now we can run our regressions
#our first step is to use these lexical variables to predict our DV
#remember there are 2 possible DV columns, since we have DV=SOA200, we will predict that variable
model.1=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr, 
           data=spp.data.other.ldt) #make sure this data name is correct for your data
#this tells us that soa200 is the variable we are trying to predict, and the variables on the other
#side of the "~" are the variables we are using to predict soa200.

summary(model.1) #this will give us information about our model. 

#For step 2 we want to compare these two models below to see which variable is a better predictor
#2a swowfsg added now
model.2a=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+swowfsg, 
           data=spp.data.other.ldt)


#2b rw_swow added now
model.2b=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+rw_swow, 
           data=spp.data.other.ldt)
#compare r^2
#now compare the two models. Here, you can see that model 2a has an r^2 of __, while model 2b has an r^2 of ___
#becuase ___  model wins, we will _____


#2.2 add fsg_ss and fanss

#IF swowfsg is the better predictor, OR swowfsg and rw_swow are both equal models use this model
model.2.2a=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                swowfsg+p.fsg_ss+p.fanss+t.fsg_ss+t.fanss, 
            data=spp.data.other.ldt)

#IFF rw_swow is the better predictor, use this model
model.2.2b=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                rw_swow+p.fsg_ss+p.fanss+t.fsg_ss+t.fanss, 
            data=spp.data.other.ldt)



#3rd-semantics. For this, it will be like the second step; 
#run one model adding full_cos_final, get output. Then, remove full_cos_final, 
#add rw_cos_full, get output, then compare r2.

#IF model swowfsg won above, OR both models tied, use these two models
model.3a.c=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                swowfsg+p.fsg_ss+p.fanss+t.fsg_ss+t.fanss+full_cos_final, 
              data=spp.data.other.ldt)
model.3a.d=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                swowfsg+p.fsg_ss+p.fanss+t.fsg_ss+t.fanss+rw_cos_final, 
              data=spp.data.other.ldt)
#compare r^2

#IF model rw_swow won above, use these two models
model.3b.c=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                rw_swow+p.fsg_ss+p.fanss+t.fsg_ss+t.fanss+full_cos_final, 
              data=spp.data.other.ldt)
model.3b.d=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                rw_swow+p.fsg_ss+p.fanss+t.fsg_ss+t.fanss+rw_cos_final, 
              data=spp.data.other.ldt)
#compare r^2

#3.2-Cosine set size
#IF model 3a.c won above, use this model
model.3.2a.c=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                swowfsg+p.fsg_ss+p.fanss+t.fsg_ss+t.fanss+full_cos_final+css, 
              data=spp.data.other.ldt)
#IF model 3a.c won above, use this model
model.3.2a.d=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                swowfsg+p.fsg_ss+p.fanss+t.fsg_ss+t.fanss+rw_cos_final+css, 
              data=spp.data.other.ldt)
#IF model 3b.c won above, use this model
model.3.2b.c=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                rw_swow+p.fsg_ss+p.fanss+t.fsg_ss+t.fanss+full_cos_final, 
              data=spp.data.other.ldt)
#IF model 3b.c won above, use this model
model.3.2b.d=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                rw_swow+p.fsg_ss+p.fanss+t.fsg_ss+t.fanss+rw_cos_final, 
              data=spp.data.other.ldt)

#4th-thematics; association and semantic relationships take together.
#IF model 3a.c won above, use this model
model.3.2a.c=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                  swowfsg+p.fsg_ss+p.fanss+t.fsg_ss+t.fanss+full_cos_final+css+distance+beagle+LSA, 
                data=spp.data.other.ldt)
#IF model 3a.c won above, use this model
model.3.2a.d=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                  swowfsg+p.fsg_ss+p.fanss+t.fsg_ss+t.fanss+rw_cos_final+css+distance+beagle+LSA, 
                data=spp.data.other.ldt)
#IF model 3b.c won above, use this model
model.3.2b.c=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                  rw_swow+p.fsg_ss+p.fanss+t.fsg_ss+t.fanss+full_cos_final+css+distance+beagle+LSA, 
                data=spp.data.other.ldt)
#IF model 3b.c won above, use this model
model.3.2b.d=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                  rw_swow+p.fsg_ss+p.fanss+t.fsg_ss+t.fanss+rw_cos_final+css+distance+beagle+LSA, 
                data=spp.data.other.ldt)

