setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/SPP-Analysis")
itemdata = read.csv("itemdata.csv")

####first associate 200 ms item analysis for LDT####
fa200LDT.1 = lm(SOA200 ~ p.length + t.length + p.orthoN + t.orthoN + 
               p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr,
             data = itemdata[itemdata$relation == "first" & itemdata$task == "LDT" , ])
summary(fa200LDT.1, correlation = T)

fa200LDT.2 = lm(SOA200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                  p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr +
                  fsg + bsg + p.fsg_ss + t.fsg_ss + p.fanss + t.fanss,
                data = itemdata[itemdata$relation == "first" & itemdata$task == "LDT" , ])
summary(fa200LDT.2, correlation = T)

fa200LDT.3 = lm(SOA200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                  p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr +
                  fsg + bsg + p.fsg_ss + t.fsg_ss + p.fanss + t.fanss + 
                  p.css + t.css + p.fss + t.fss + root + affix,
                data = itemdata[itemdata$relation == "first" & itemdata$task == "LDT" , ])
summary(fa200LDT.3, correlation = T)

fa200LDT.4 = lm(SOA200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                  p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr +
                  fsg + bsg + p.fsg_ss + t.fsg_ss + p.fanss + t.fanss + p.css + t.css + p.fss + t.fss +
                  root + affix + distance + beagle + LSA,
                data = itemdata[itemdata$relation == "first" & itemdata$task == "LDT" , ])
summary(fa200LDT.4, correlation = T)

####other associate 200 ms item analysis for LDT####
oa200LDT.1 = lm(SOA200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                  p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr,
                data = itemdata[itemdata$relation == "other" & itemdata$task == "LDT" , ])
summary(oa200LDT.1, correlation = T)

oa200LDT.2 = lm(SOA200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                  p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr +
                  fsg + bsg + p.fsg_ss + t.fsg_ss + p.fanss + t.fanss + p.fanss + t.fanss,
                data = itemdata[itemdata$relation == "other" & itemdata$task == "LDT" , ])
summary(oa200LDT.2, correlation = T)

oa200LDT.3 = lm(SOA200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                  p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr +
                  fsg + bsg + p.fsg_ss + t.fsg_ss + p.fanss + t.fanss + p.css + t.css + p.fss + t.fss +
                  root + affix,
                data = itemdata[itemdata$relation == "other" & itemdata$task == "LDT" , ])
summary(oa200LDT.3, correlation = T)

oa200LDT.4 = lm(SOA200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                  p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr +
                  fsg + bsg + p.fsg_ss + t.fsg_ss + p.fanss + t.fanss + p.css + t.css + p.fss + t.fss +
                  root + affix + distance + beagle + LSA,
                data = itemdata[itemdata$relation == "other" & itemdata$task == "LDT" , ])
summary(oa200LDT.4, correlation = T)

####first associate 1200 ms item analysis for LDT####
fa1200LDT.1 = lm(SOA1200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                  p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr,
                data = itemdata[itemdata$relation == "first" & itemdata$task == "LDT" , ])
summary(fa1200LDT.1, correlation = T)

fa1200LDT.2 = lm(SOA1200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                  p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr +
                  fsg + bsg + p.fsg_ss + t.fsg_ss + p.fanss + t.fanss + p.fanss + t.fanss,
                data = itemdata[itemdata$relation == "first" & itemdata$task == "LDT" , ])
summary(fa1200LDT.2, correlation = T)

fa1200LDT.3 = lm(SOA1200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                  p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr +
                  fsg + bsg + p.fsg_ss + t.fsg_ss + p.fanss + t.fanss + p.css + t.css + p.fss + t.fss +
                  root + affix,
                data = itemdata[itemdata$relation == "first" & itemdata$task == "LDT" , ])
summary(fa1200LDT.3, correlation = T)

fa1200LDT.4 = lm(SOA1200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                  p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr +
                  fsg + bsg + p.fsg_ss + t.fsg_ss + p.fanss + t.fanss + p.css + t.css + p.fss + t.fss +
                  root + affix + distance + beagle + LSA,
                data = itemdata[itemdata$relation == "first" & itemdata$task == "LDT" , ])
summary(fa1200LDT.4, correlation = T)

####other associate 1200 ms item analysis for LDT####
oa1200LDT.1 = lm(SOA1200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                  p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr,
                data = itemdata[itemdata$relation == "other" & itemdata$task == "LDT" , ])
summary(oa1200LDT.1, correlation = T)

oa1200LDT.2 = lm(SOA1200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                  p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr +
                  fsg + bsg + p.fsg_ss + t.fsg_ss + p.fanss + t.fanss + p.fanss + t.fanss,
                data = itemdata[itemdata$relation == "other" & itemdata$task == "LDT" , ])
summary(oa1200LDT.2, correlation = T)

oa1200LDT.3 = lm(SOA1200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                  p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr +
                  fsg + bsg + p.fsg_ss + t.fsg_ss + p.fanss + t.fanss + p.css + t.css + p.fss + t.fss +
                  root + affix,
                data = itemdata[itemdata$relation == "other" & itemdata$task == "LDT" , ])
summary(oa1200LDT.3, correlation = T)

oa1200LDT.4 = lm(SOA1200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                  p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr +
                  fsg + bsg + p.fsg_ss + t.fsg_ss + p.fanss + t.fanss + p.css + t.css + p.fss + t.fss +
                  root + affix + distance + beagle + LSA,
                data = itemdata[itemdata$relation == "other" & itemdata$task == "LDT" , ])
summary(oa1200LDT.4, correlation = T)

####first associate 200 ms item analysis for NAME####
fa200NAME.1 = lm(SOA200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                  p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr,
                data = itemdata[itemdata$relation == "first" & itemdata$task == "NAME" , ])
summary(fa200NAME.1, correlation = T)

fa200NAME.2 = lm(SOA200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                  p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr +
                  fsg + bsg + p.fsg_ss + t.fsg_ss + p.fanss + t.fanss + p.fanss + t.fanss,
                data = itemdata[itemdata$relation == "first" & itemdata$task == "NAME" , ])
summary(fa200NAME.2, correlation = T)

fa200NAME.3 = lm(SOA200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                  p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr +
                  fsg + bsg + p.fsg_ss + t.fsg_ss + p.fanss + t.fanss + p.css + t.css + p.fss + t.fss +
                  root + affix,
                data = itemdata[itemdata$relation == "first" & itemdata$task == "NAME" , ])
summary(fa200NAME.3, correlation = T)

fa200NAME.4 = lm(SOA200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                  p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr +
                  fsg + bsg + p.fsg_ss + t.fsg_ss + p.fanss + t.fanss + p.css + t.css + p.fss + t.fss +
                  root + affix + distance + beagle + LSA,
                data = itemdata[itemdata$relation == "first" & itemdata$task == "NAME" , ])
summary(fa200NAME.4, correlation = T)

####other associate 200 ms item analysis for NAME####
oa200NAME.1 = lm(SOA200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                  p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr,
                data = itemdata[itemdata$relation == "other" & itemdata$task == "NAME" , ])
summary(oa200NAME.1, correlation = T)

oa200NAME.2 = lm(SOA200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                  p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr +
                  fsg + bsg + p.fsg_ss + t.fsg_ss + p.fanss + t.fanss + p.fanss + t.fanss,
                data = itemdata[itemdata$relation == "other" & itemdata$task == "NAME" , ])
summary(oa200NAME.2, correlation = T)

oa200NAME.3 = lm(SOA200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                  p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr +
                  fsg + bsg + p.fsg_ss + t.fsg_ss + p.fanss + t.fanss + p.css + t.css + p.fss + t.fss +
                  root + affix,
                data = itemdata[itemdata$relation == "other" & itemdata$task == "NAME" , ])
summary(oa200NAME.3, correlation = T)

oa200NAME.4 = lm(SOA200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                  p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr +
                  fsg + bsg + p.fsg_ss + t.fsg_ss + p.fanss + t.fanss + p.css + t.css + p.fss + t.fss +
                  root + affix + distance + beagle + LSA,
                data = itemdata[itemdata$relation == "other" & itemdata$task == "NAME" , ])
summary(oa200NAME.4, correlation = T)

####first associate 1200 ms item analysis for NAME####
fa1200NAME.1 = lm(SOA1200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                   p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr,
                 data = itemdata[itemdata$relation == "first" & itemdata$task == "NAME" , ])
summary(fa1200NAME.1, correlation = T)

fa1200NAME.2 = lm(SOA1200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                   p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr +
                   fsg + bsg + p.fsg_ss + t.fsg_ss + p.fanss + t.fanss + p.fanss + t.fanss,
                 data = itemdata[itemdata$relation == "first" & itemdata$task == "NAME" , ])
summary(fa1200NAME.2, correlation = T)

fa1200NAME.3 = lm(SOA1200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                   p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr +
                   fsg + bsg + p.fsg_ss + t.fsg_ss + p.fanss + t.fanss + p.css + t.css + p.fss + t.fss +
                   root + affix,
                 data = itemdata[itemdata$relation == "first" & itemdata$task == "NAME" , ])
summary(fa1200NAME.3, correlation = T)

fa1200NAME.4 = lm(SOA1200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                   p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr +
                   fsg + bsg + p.fsg_ss + t.fsg_ss + p.fanss + t.fanss + p.css + t.css + p.fss + t.fss +
                   root + affix + distance + beagle + LSA,
                 data = itemdata[itemdata$relation == "first" & itemdata$task == "NAME" , ])
summary(fa1200NAME.4, correlation = T)

####other associate 1200 ms item analysis for NAME####
oa1200NAME.1 = lm(SOA1200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                   p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr,
                 data = itemdata[itemdata$relation == "other" & itemdata$task == "NAME" , ])
summary(oa1200NAME.1, correlation = T)

oa1200NAME.2 = lm(SOA1200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                   p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr +
                   fsg + bsg + p.fsg_ss + t.fsg_ss + p.fanss + t.fanss + p.fanss + t.fanss,
                 data = itemdata[itemdata$relation == "other" & itemdata$task == "NAME" , ])
summary(oa1200NAME.2, correlation = T)

oa1200NAME.3 = lm(SOA1200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                   p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr +
                   fsg + bsg + p.fsg_ss + t.fsg_ss + p.fanss + t.fanss + p.css + t.css + p.fss + t.fss +
                   root + affix,
                 data = itemdata[itemdata$relation == "other" & itemdata$task == "NAME" , ])
summary(oa1200NAME.3, correlation = T)

oa1200NAME.4 = lm(SOA1200 ~ p.length + t.length + p.orthoN + t.orthoN + 
                   p.freq + t.freq + p.phonoN + t.phonoN + p.POSr + t.POSr +
                   fsg + bsg + p.fsg_ss + t.fsg_ss + p.fanss + t.fanss + p.css + t.css + p.fss + t.fss +
                   root + affix + distance + beagle + LSA,
                 data = itemdata[itemdata$relation == "other" & itemdata$task == "NAME" , ])
summary(oa1200NAME.4, correlation = T)
