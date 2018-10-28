#given prime type= other , task type= LDT , and DV=SOA200
#fancy code to set the working directory to the current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
spp.data=read.csv("itemdata.csv") 

#next, we need to split the data up so that we are only analyzing for our prime and task type
#we have prime type OTHER
spp.data.other=spp.data[spp.data$relation=="other",] #this keeps only rows where relation=other and keeps all columns

#we have task type LDT
spp.data.other.ldt=spp.data.other[spp.data.other$task=="LDT",] #this keeps only rows where task=LDT and keeps all columns

#now we can run our regressions
#our first step is to use these lexical variables to predict our DV
model.1=lm(SOA1200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr, 
           data=spp.data.other.ldt) #make sure this data name is correct for your data


summary(model.1) #this will give us information about our model. 
###for me, t.freq, intc, and p.length are only sign. 
##R^2 = .01941



#For step 2 we want to compare these three models below to see which variable is a better predictor
#Erin added both after discovering the correlation between them is not too high. 
#R2 values should be >.01 for you to consider they are "better". 
#If they are all equal (so to speak), then use the computationally easier one (swow or cosine, not pmi).

#2a swowfsg added now
model.2a=lm(SOA1200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+swowfsg, 
           data=spp.data.other.ldt)
summary(model.2a)
###gives us sign pred for swowfsg, t.freq, p.length, and intcpt
###Multiple R^2=0.03226

#2b pmi_swow added now
model.2b=lm(SOA1200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+pmi_swow, 
           data=spp.data.other.ldt)
summary(model.2b)
###gives us sign. pred for pmi_swow, t.POSrVB, t.freq, p.length
###Multiple R^2=0.03358

#2c swowfsg and pmi_swow together
model.2c=lm(SOA1200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+swowfsg+pmi_swow, 
            data=spp.data.other.ldt)
summary(model.2c)
###gives us sing for t.freq, p.length, t.POSrVB
###Multiple R^2=0.03366
#compare r^2
#now compare the  models. Here, you can see that all models are quite similar. 
#given this, we will choose the simplest computational model--swowfsg


##ADDIES NOTE: Model 2a


######################### Keep in mind that from here on out, only models I needed are kept
######################### All other models are included in this code, but commented out
######################### Use ONLY those models that make sense given your R^2 above!

#2.2 add fsg_ss and fanss



#IF swowfsg is the better predictor, OR all models equal, use this model
model.2.2a=lm(SOA1200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss, 
              data=spp.data.other.ldt)
summary(model.2.2a)
#sign. for intercept, t.freq, p.length, snowfsg
#R^2 = 0.03469




#3rd-semantics. For this, it will be like the second step; 
#run one model adding full_cos_final, get output. Then, remove full_cos_final, 
#addpmi_cosine, get output. Then try model with both in there, then compare r2.

#IF model swowfsg won above, OR  models tied, use these two models
model.3a.a=lm(SOA1200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+full_cos_final, 
              data=spp.data.other.ldt)
model.3a.b=lm(SOA1200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+pmi_cosine, 
              data=spp.data.other.ldt)
model.3a.c=lm(SOA1200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+full_cos_final+pmi_cosine, 
              data=spp.data.other.ldt)
summary(model.3a.a)
#sign. intercept, t.freq, p.length, swowfsg
#R^2 = 0.03663
summary(model.3a.b)
#sign. intercept, t.freq, p.length, swowfsg
#R^2 = 0.03491
summary(model.3a.c)
#sign. intercept, t.freq, p.length, swowfsg
#R^2 = 0.03667


model.3.2a.a=lm(SOA1200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                  swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ 
                  swow.t.fan_ss+ swow.p.fan_ss+full_cos_final+
                  p.css+t.css+p.fss+t.fss, 
                data=spp.data.other.ldt)
summary(model.3.2a.a)

#IF model 3a.a won above, use this model
model.4a.a=lm(SOA1200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ 
                swow.p.fan_ss+full_cos_final+
                p.css+t.css+p.fss+t.fss+distance+LSA+beagle, 
              data=spp.data.other.ldt)
summary(model.4a.a)
#sign are int, t.freq, p.length, t.POSrother, snowfsg
#R^2=0.04121


