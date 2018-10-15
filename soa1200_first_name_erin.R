#given prime type = first , task type = name , and DV = SOA1200
#fancy code to set the working directory to the current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
spp.data=read.csv("itemdata.csv") 

spp.data.subset = subset(spp.data, relation == "first" & task == "NAME" )

#our first step is to use these lexical variables to predict our DV
model.1=lm(SOA1200~p.freq+t.freq+t.length+p.length+
             p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr, 
           data=spp.data.subset) 

summary(model.1) 

#For step 2 we want to compare these three 
#models below to see which variable is a better predictor
#R2 values should be >.01 for you to consider they are "better". 
#If they are all equal (so to speak), 
#then use the computationally easier one (swow or cosine, not pmi).

#2a swowfsg added now
model.2a=lm(SOA1200~p.freq+t.freq+t.length+p.length+
              p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
              swowfsg, 
           data=spp.data.subset)
summary(model.2a)

#2b pmi_swow added now
model.2b=lm(SOA1200~p.freq+t.freq+t.length+p.length+
              p.orthoN+t.orthoN+p.phonoN+t.phonoN+
              p.POSr+t.POSr+pmi_swow, 
           data=spp.data.subset)
summary(model.2b)

#2c swowfsg and pmi_swow together
model.2c=lm(SOA1200~p.freq+t.freq+t.length+
              p.length+p.orthoN+t.orthoN+p.phonoN+
              t.phonoN+p.POSr+t.POSr+swowfsg+pmi_swow, 
            data=spp.data.subset)
summary(model.2c)

#2.2 add fsg_ss and fanss

#pmi_swow is the winner 
model.2.2b=lm(SOA1200~p.freq+t.freq+t.length+
                p.length+p.orthoN+t.orthoN+p.phonoN+
                t.phonoN+p.POSr+t.POSr+
                pmi_swow+swow.t.fsg_ss+ swow.p.fsg_ss+ 
                swow.t.fan_ss+ swow.p.fan_ss, 
            data=spp.data.subset)
summary(model.2.2b)

#3rd-semantics. For this, it will be like the second step; 

model.3b.a=lm(SOA1200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+
                p.phonoN+t.phonoN+p.POSr+t.POSr+
                pmi_swow+swow.t.fsg_ss+ swow.p.fsg_ss+ 
                swow.t.fan_ss+ swow.p.fan_ss+full_cos_final, 
                        data=spp.data.subset)
summary(model.3b.a)

model.3b.b=lm(SOA1200~p.freq+t.freq+t.length+p.length+
                p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                pmi_swow+swow.t.fsg_ss+ swow.p.fsg_ss+ 
                swow.t.fan_ss+ swow.p.fan_ss+pmi_cosine, 
              data=spp.data.subset)

summary(model.3b.b)

model.3b.c=lm(SOA1200~p.freq+t.freq+t.length+p.length+
                p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                pmi_swow+swow.t.fsg_ss+ swow.p.fsg_ss+ 
                swow.t.fan_ss+ swow.p.fan_ss+full_cos_final+pmi_cosine, 
              data=spp.data.subset)

summary(model.3b.c)

#3.2-Cosine set size p.css, t.css, p.fss, t.fss
model.3.2b.a=lm(SOA1200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+
                  p.phonoN+t.phonoN+p.POSr+t.POSr+
                  pmi_swow+swow.t.fsg_ss+ swow.p.fsg_ss+ 
                  swow.t.fan_ss+ swow.p.fan_ss+full_cos_final+
                  p.css+t.css+p.fss+t.fss, 
              data=spp.data.subset)
summary(model.3.2b.a)

#4th-thematics; association and semantic relationships take together.
#distance, LSA, beagle
model.4b.a=lm(SOA1200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+
                  p.phonoN+t.phonoN+p.POSr+t.POSr+
                  pmi_swow+swow.t.fsg_ss+ swow.p.fsg_ss+ 
                  swow.t.fan_ss+ swow.p.fan_ss+full_cos_final+
                  p.css+t.css+p.fss+t.fss+
                distance+LSA+beagle, 
                data=spp.data.subset)
summary(model.4b.a)

