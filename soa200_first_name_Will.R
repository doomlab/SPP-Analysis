#given prime type= first , task type= NAME , and DV=SOA200
#fancy code to set the working directory to the current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
spp.data=read.csv("itemdata.csv") 

#next, we need to split the data up so that we are only analyzing for our prime and task type
#we have prime type OTHER
spp.data.first=spp.data[spp.data$relation=="first",] #this keeps only rows where relation=first and keeps all columns

#we have task type NAME
spp.data.first.name=spp.data.first[spp.data.first$task=="NAME",] #this keeps only rows where task=LDT and keeps all columns

#now we can run our regressions
#our first step is to use these lexical variables to predict our DV
#remember there are 2 possible DV columns, since we have DV=SOA200, we will predict that variable
model.1=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr, 
           data=spp.data.first.name) #make sure this data name is correct for your data
#this tells us that soa200 is the variable we are trying to predict, and the variables on the other
#side of the "~" are the variables we are using to predict soa200.

summary(model.1) #this will give us information about our model. 
###for Will, p.freq, t.freq, and p.phonoN are sign. 

#For step 2 we want to compare these three models below to see which variable is a better predictor
#Erin added both after discovering the correlation between them is not too high. 
#R2 values should be >.01 for you to consider they are "better". 
#If they are all equal (so to speak), then use the computationally easier one (swow or cosine, not pmi).

#2a swowfsg added now
model.2a=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+swowfsg, 
           data=spp.data.first.name)
summary(model.2a)
###gives us sign pred for p.freq, t.freq, p.phonoN, and intcpt
###Multiple R^2=0.03679

#2b pmi_swow added now
model.2b=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+pmi_swow, 
           data=spp.data.first.name)
summary(model.2b)
###gives us sign. pred for p.freq, t.freq, p.phonoN, and intcpt
###Multiple R^2=0.03677

#2c swowfsg and pmi_swow together
model.2c=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+swowfsg+pmi_swow, 
            data=spp.data.first.name)
summary(model.2c)
###gives us sing for p.freq, t.freq, and p.phonoN
###Multiple R^2=0.03684
#compare r^2
#now compare the  models. Here, you can see that all models are quite similar. 
#given this, we will choose the simplest computational model--swowfsg

######################### Keep in mind that from here on out, only models I needed are kept
######################### All other models are included in this code, but commented out
######################### Use ONLY those models that make sense given your R^2 above!

#2.2 add fsg_ss and fanss

#IF swowfsg is the better predictor, OR all models equal, use this model
model.2.2a=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss, 
            data=spp.data.first.name)
summary(model.2.2a)
###gives us sign for p.freq, p.phonoN, swow.t.fsg_ss, and swow.t.fan_ss
##R2=0.04604

#IFF pmi_swow is the better predictor, use this model
#model.2.2b=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
#                pmi_swow+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss, 
#            data=spp.data.other.ldt)

#IFF pmi_swow AND swowfsg is the better model, use this model
#model.2.2c=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
#                pmi_swow+swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss, 
#              data=spp.data.other.ldt)


#3rd-semantics. For this, it will be like the second step; 
#run one model adding full_cos_final, get output. Then, remove full_cos_final, 
#addpmi_cosine, get output. Then try model with both in there, then compare r2.

#IF model swowfsg won above, OR  models tied, use these two models
model.3a.a=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                          swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+full_cos_final, 
                        data=spp.data.first.name)
model.3a.b=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+pmi_cosine, 
              data=spp.data.first.name)
model.3a.c=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+full_cos_final+pmi_cosine, 
              data=spp.data.first.name)
summary(model.3a.a)
summary(model.3a.b)
summary(model.3a.c)

#model.3a.a sign are int, p.freq, p.phonoN, swow.t.fsg_ss, swow.t.fan_ss, r2=0.04695
#model .b sing are intc, p.freq, p.phonoN, swow.t.fsg_ss, swow.t.fan_ss, R2= 0.04713
#model .c sig are int, p.freq, p.phonoN, swow.t.fsg_ss, swow.t.fan_ss, r2=0.04754
#again, both models tie, so we will keep full_cos_final

#IF model pmi_swow won above, use these two models
#model.3b.a=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
#                pmi_swow+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+full_cos_final, 
#              data=spp.data.other.ldt)
#model.3b.b=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
#                pmi_swow+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+pmi_cosine, 
#              data=spp.data.other.ldt)
#model.3b.c=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
#                pmi_swow+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+full_cos_final+pmi_cosine, 
#              data=spp.data.other.ldt)

#IF model with pmi_swow and swowfsg won, use these two models
#model.3c.a=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
#                pmi_swow+swowofsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+full_cos_final, 
#              data=spp.data.other.ldt)
#model.3c.b=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
#                pmi_swow+swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+pmi_cosine, 
#              data=spp.data.other.ldt)
#model.3c.c=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
#                pmi_swow+swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+full_cos_final+pmi_cosine, 
#              data=spp.data.other.ldt)

#3.2-Cosine set size p.css, t.css, p.fss, t.fss
#IF model 3a.a won above, use this model
model.3.2a.a=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+full_cos_final+
                p.css+t.css+p.fss+t.fss, 
              data=spp.data.first.name)
summary(model.3.2a.a)
#sign are intc, p.freq, p.phonoN, swow.t.fsg_ss, swow.t.fan_ss
#r2=0.04952

#IF model 3a.b won above, use this model
#model.3.2a.b=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
#                swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_sss+pmi_cosine+
#                p.css+t.css+p.fss+t.fss, 
#              data=spp.data.other.ldt)

#IF model 3a.c won above, use this model
#model.3.2a.c=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
#                swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_sss+full_cos_final+pmi_cosine+
#                p.css+t.css+p.fss+t.fss, 
#              data=spp.data.other.ldt)

#IF model 3b.a won above, use this model
#model.3.2b.a=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
#                pmi_swow+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+full_cos_final+
#                  p.css+t.css+p.fss+t.fss, 
#              data=spp.data.other.ldt)

#IF model 3b.b won above, use this model
#model.3.2b.b=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
#                pmi_swow+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+pmi_cosine+
#                  p.css+t.css+p.fss+t.fss, 
#              data=spp.data.other.ldt)

#IF model 3b.c won above, use this model
#model.3.2b.c=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
#                pmi_swow+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+full_cos_final+pmi_cosine+
#                  p.css+t.css+p.fss+t.fss, 
#              data=spp.data.other.ldt)

#IF model 3c.a won above, use this model
#model.3.2c.a=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
#                pmi_swow+swowofsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+full_cos_final+
#                  p.css+t.css+p.fss+t.fss, 
#              data=spp.data.other.ldt)

#IF model 3c.b won above, use this model
#model.3.2c.b=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
#                pmi_swow+swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+pmi_cosine+
#                  p.css+t.css+p.fss+t.fss, 
#              data=spp.data.other.ldt)

#IF model 3c.c won above, use this model
#model.3.2c.c=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
#                pmi_swow+swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+full_cos_final+pmi_cosine+
#                  p.css+t.css+p.fss+t.fss, 
#              data=spp.data.other.ldt)

#4th-thematics; association and semantic relationships take together.
#distance, LSA, beagle
#IF model 3a.a won above, use this model
model.4a.a=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                  swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+full_cos_final+
                  p.css+t.css+p.fss+t.fss+distance+LSA+beagle, 
                data=spp.data.first.name)
summary(model.4a.a)
#sign are int, p.freq, p.length, p.phonoN, swow.t.fsg_ss, swow.p.fsg_ss, swow.t.fan_ss, distance
#R^2=0.05769

#IF model 3a.b won above, use this model
#model.4a.b=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
#                  swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_sss+pmi_cosine+
#                  p.css+t.css+p.fss+t.fss+distance+LSA+beagle, 
#                data=spp.data.other.ldt)

#IF model 3a.c won above, use this model
#model.4a.c=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
#                  swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_sss+full_cos_final+pmi_cosine+
#                  p.css+t.css+p.fss+t.fss+distance+LSA+beagle, 
#                data=spp.data.other.ldt)

#IF model 3b.a won above, use this model
#model.4b.a=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
#                  pmi_swow+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+full_cos_final+
#                  p.css+t.css+p.fss+t.fss+distance+LSA+beagle, 
#                data=spp.data.other.ldt)

#IF model 3b.b won above, use this model
#model.4b.b=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
#                  pmi_swow+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+pmi_cosine+
#                  p.css+t.css+p.fss+t.fss+distance+LSA+beagle, 
#                data=spp.data.other.ldt)

#IF model 3b.c won above, use this model
#model.4b.c=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
#                  pmi_swow+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+full_cos_final+pmi_cosine+
#                  p.css+t.css+p.fss+t.fss+distance+LSA+beagle, 
#                data=spp.data.other.ldt)

#IF model 3c.a won above, use this model
#model.4c.a=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
#                  pmi_swow+swowofsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+full_cos_final+
#                  p.css+t.css+p.fss+t.fss+distance+LSA+beagle, 
#                data=spp.data.other.ldt)

#IF model 3c.b won above, use this model
#model.4c.b=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
#                  pmi_swow+swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+pmi_cosine+
#                  p.css+t.css+p.fss+t.fss+distance+LSA+beagle, 
#                data=spp.data.other.ldt)

#IF model 3c.c won above, use this model
#model.4c.c=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
#                  pmi_swow+swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+full_cos_final+pmi_cosine+
#                  p.css+t.css+p.fss+t.fss+distance+LSA+beagle, 
#                data=spp.data.other.ldt)
