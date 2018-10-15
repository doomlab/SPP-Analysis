#fancy code to set the working directory to the current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#use the right file either LDT or N
spp.data = read.csv("subjectdataN.csv") 

#pull only the information you are interested in
spp.data.rel = subset(spp.data, 
                  target.ACC == 1 & #only trials they got right
                  isi == 50 & #use 50 for 200; use 1050 for 1200 SOA
                  type == "other" & #use first or other
                  rel == "rel") #for related words

spp.data.un = subset(spp.data, 
                     target.ACC == 1 & #only trials they got right
                       isi == 50 & #use 50 for 200; use 1050 for 1200 SOA
                       type == "other" & #use first or other
                       rel == "un") #for unrelated words

#create priming score
library(nlme)
unrelated.model = lme(target.RT ~ 1,
                      data = spp.data.un,
                      method = "ML",
                      random = list(~1|target),
                      na.action = "na.omit")

RTtosubtract = as.data.frame(unrelated.model$coefficients$random$target + unrelated.model$coefficients$fixed)
RTtosubtract$target = rownames(RTtosubtract)
colnames(RTtosubtract)[1] = "RTscore"

library(expss)
spp.data.rel$RTunrelated = vlookup(as.character(spp.data.rel$target), RTtosubtract, "RTscore", "target")

spp.data.rel$priming.RT = spp.data.rel$RTunrelated - spp.data.rel$target.RT
#unrelated minus related meaning positive = priming, negative = slowing

#these are the same steps as last time

library(MuMIn)

#step 1
model.1 = lme(priming.RT ~ p.freq+t.freq+t.length+p.length+
                p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr,
              data = spp.data.rel,
              method = "ML",
              random = list(~1|target, ~1|Subject),
              na.action = "na.omit")
summary(model.1)
r.squaredGLMM(model.1) #use r2m

#step 2
#2a swowfsg added now
model.2a = lme(priming.RT ~ p.freq+t.freq+t.length+p.length+
              p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
              swowfsg, 
              data = spp.data.rel,
              method = "ML",
              random = list(~1|target, ~1|Subject),
              na.action = "na.omit")

summary(model.2a)
r.squaredGLMM(model.2a)

#2b pmi_swow added now
model.2b = lme(priming.RT ~ p.freq+t.freq+t.length+p.length+
                 p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                 pmi_swow, 
               data = spp.data.rel,
               method = "ML",
               random = list(~1|target, ~1|Subject),
               na.action = "na.omit")

summary(model.2b)
r.squaredGLMM(model.2b)

#2c swowfsg and pmi_swow together
model.2c = lme(priming.RT ~ p.freq+t.freq+t.length+p.length+
                 p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                 swowfsg+pmi_swow, 
               data = spp.data.rel,
               method = "ML",
               random = list(~1|target, ~1|Subject),
               na.action = "na.omit")

summary(model.2c)
r.squaredGLMM(model.2c)

#step 2.2 add variables based on winning model above 

model.2.2a = lme(priming.RT ~ p.freq+t.freq+t.length+p.length+ p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                 swowfsg + 
                 swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss, 
               data = spp.data.rel,
               method = "ML",
               random = list(~1|target, ~1|Subject),
               na.action = "na.omit")

summary(model.2.2a)
r.squaredGLMM(model.2.2a)

#step 3

#just cosine
model.3a.a = lme(priming.RT ~ p.freq+t.freq+t.length+p.length+ p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                   swowfsg + 
                   swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss +
                   full_cos_final, 
                 data = spp.data.rel,
                 method = "ML",
                 random = list(~1|target, ~1|Subject),
                 na.action = "na.omit")

summary(model.3a.a)
r.squaredGLMM(model.3a.a)

#just pmi cosine
model.3a.b = lme(priming.RT ~ p.freq+t.freq+t.length+p.length+ p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                   swowfsg + 
                   swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss +
                   pmi_cosine, 
                 data = spp.data.rel,
                 method = "ML",
                 random = list(~1|target, ~1|Subject),
                 na.action = "na.omit")

summary(model.3a.b)
r.squaredGLMM(model.3a.b)

#just both cosine and pmi cosine
model.3a.c = lme(priming.RT ~ p.freq+t.freq+t.length+p.length+ p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                   swowfsg + 
                   swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss +
                   full_cos_final + pmi_cosine, 
                 data = spp.data.rel,
                 method = "ML",
                 random = list(~1|target, ~1|Subject),
                 na.action = "na.omit")

summary(model.3a.c)
r.squaredGLMM(model.3a.c)

#step 3.2 add p.css+t.css+p.fss+t.fss to final model above 
model.3.2a.a = lme(priming.RT ~ p.freq+t.freq+t.length+p.length+ p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                   swowfsg + 
                   swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss +
                   full_cos_final + 
                   p.css+t.css+p.fss+t.fss, 
                 data = spp.data.rel,
                 method = "ML",
                 random = list(~1|target, ~1|Subject),
                 na.action = "na.omit")

summary(model.3.2a.a)
r.squaredGLMM(model.3.2a.a)

#step 4 add distance, LSA, beagle
model.4a.a = lme(priming.RT ~ p.freq+t.freq+t.length+p.length+ p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                     swowfsg + 
                     swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss +
                     full_cos_final + 
                     p.css+t.css+p.fss+t.fss +
                     distance + LSA + beagle, 
                   data = spp.data.rel,
                   method = "ML",
                   random = list(~1|target, ~1|Subject),
                   na.action = "na.omit")

summary(model.4a.a)
r.squaredGLMM(model.4a.a)
