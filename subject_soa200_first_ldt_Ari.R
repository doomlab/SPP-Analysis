#fancy code to set the working directory to the current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#use the right file either LDT or N
spp.data = read.csv("subjectdataLDT.csv") 

#pull only the information you are interested in
spp.data.rel = subset(spp.data, 
                  target.ACC == 1 & #only trials they got right
                  isi == 50 & #use 50 for 200; use 1050 for 1200 SOA
                  type == "first" & #use first or other
                  rel == "rel") #for related words

spp.data.un = subset(spp.data, 
                     target.ACC == 1 & #only trials they got right
                       isi == 50 & #use 50 for 200; use 1050 for 1200 SOA
                       type == "first" & #use first or other
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
###r2m = 0.0013
###S: Int, p.freq, t.length

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
###R2m = 0.0013
###S: p.freq, t.length, t.POSrother
###M: Int, swowfsg

#2b pmi_swow added now
model.2b = lme(priming.RT ~ p.freq+t.freq+t.length+p.length+
                 p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                 pmi_swow, 
               data = spp.data.rel,
               method = "ML",
               random = list(~1|target, ~1|Subject),
               na.action = "na.omit",
               control=lmeControl(opt = "optim"))

summary(model.2b)
r.squaredGLMM(model.2b)
###r2m = 0.0014
###S: p.freq, t.length, t.POSrother, pmi_swow

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
###r2m = 0.0014
###S: p.freq, t.length, t.POSrother
###continuing with a

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
###r2m = .0015
###S: Int, p.freq, t.length, t.POSrother

#step 3

#just cosine
model.3a.a = lme(priming.RT ~ p.freq+t.freq+t.length+p.length+ p.orthoN+t.orthoN+p.phonoN+t.phonoN+p.POSr+t.POSr+
                   swowfsg + 
                   swow.t.fsg_ss+ swow.p.fsg_ss+ swow.t.fan_ss+ swow.p.fan_ss +
                   full_cos_final, 
                 data = spp.data.rel,
                 method = "ML",
                 random = list(~1|target, ~1|Subject),
                 na.action = "na.omit",
                 control=lmeControl(opt = "optim"))

summary(model.3a.a)
r.squaredGLMM(model.3a.a)
###r2m = 0.0015
###S: p.freq, t.length, t.POSrother
###M: Int

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
###r2m = 0.0015
###S: Int, p.freq, t.length, t.POSrother

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
###r2m = 0.0015
###S: p.freq, t.length, t.POSrother
###M: Int

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
###r2m = 0.0017
###S: p.freq, t.length, t.POSrother

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
###r2m = 0.0020
###S: t.length, LSA
###M: t.POSrother

