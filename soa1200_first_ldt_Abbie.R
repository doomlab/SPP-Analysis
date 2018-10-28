#fancy code to set the working directory to the current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
spp.data=read.csv("itemdata.csv")

spp.data.first=spp.data[spp.data$relation=="first",] #this keeps only rows where relation=other and keeps all columns
spp.data.first.ldt=spp.data.first[spp.data.first$task=="LDT",] #this keeps only rows where task=LDT and keeps all columns


# Step One --------------------------------------------------------------

#our first step is to use these lexical variables to predict our DV
model.1=lm(SOA1200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr, 
           data=spp.data.first.ldt) 
summary(model.1)



# Step Two ----------------------------------------------------------------


#For step 2 we want to compare these three models below to see which variable is a better predictor
#R2 values should be >.01 for you to consider they are "better". 
#If they are all equal (so to speak), then use the computationally easier one (swow or cosine, not pmi).

#2a swowfsg added now
model.2a=lm(SOA1200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+swowfsg, 
            data=spp.data.first.ldt)
summary(model.2a)
##Multiple R^2=0.032

#2b pmi_swow added now
model.2b=lm(SOA1200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+pmi_swow, 
            data=spp.data.first.ldt)
summary(model.2b)
###gives us sign. pred for pmi_swow, t.phonoN, t.freq, and int
###Multiple R^2=0.034

#2c swowfsg and pmi_swow together
model.2c=lm(SOA1200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+swowfsg+pmi_swow, 
            data=spp.data.first.ldt)
summary(model.2c)
###gives us sing for t.phonoN, t.freq, int
###Multiple R^2=0.034

#All R^2 seem really similar, what do i do here? I chose the simplest model like the example

######################### Keep in mind that from here on out, only models I needed are kept
######################### All other models are included in this code, but commented out
######################### Use ONLY those models that make sense given your R^2 above!

#2.2 add fsg_ss and fanss

#IF swowfsg is the better predictor, OR all models equal, use this model
model.2.2a=lm(SOA1200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss, 
              data=spp.data.first.ldt)
summary(model.2.2a)
###gives us sign for int, t.freq, t.phonoN, swowfsg, swow.t.fan_ss
##R2=0.035


# Step Three ------------------------------------------------------------------

#IF model swowfsg won above, OR  models tied, use these two models
model.3a.a=lm(SOA1200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+full_cos_final, 
              data=spp.data.first.ldt)
model.3a.b=lm(SOA1200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+pmi_cosine, 
              data=spp.data.first.ldt)
model.3a.c=lm(SOA1200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ 
                swow.t.fan_ss+ swow.p.fan_ss+pmi_cosine+full_cos_final, 
              data=spp.data.first.ldt)
summary(model.3a.a) #R2 = .037
summary(model.3a.b) #R2 = .035
summary(model.3a.c)
#Not really sure if these are that different?

#3.2-Cosine set size p.css, t.css, p.fss, t.fss
#IF model 3a.c won above, use this model
model.3.2a.a=lm(SOA1200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                  swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+full_cos_final+
                  p.css+t.css+p.fss+t.fss, 
                data=spp.data.first.ldt)
summary(model.3.2a.a)
#sign are intc, t.freq, t.phonoN, full_cos_final, t.css
#r2=0.038


# Step Four ---------------------------------------------------------------

#4th-thematics; association and semantic relationships take together.
#distance, LSA, beagle
#IF model 3a.c won above, use this model
model.4a.a=lm(SOA200~p.freq+t.freq+t.length+p.length+p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                swowfsg+swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss+full_cos_final+
                p.css+t.css+p.fss+t.fss+distance+LSA+beagle, 
              data=spp.data.first.ldt)
summary(model.4a.a)
#sign are int, t.freq, t.phonoN, t.css, LSA, beagal
#R^2=0.06296

