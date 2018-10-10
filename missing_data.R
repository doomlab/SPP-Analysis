setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

master = read.csv("itemdata.csv")

summary(master)

#control shift r


# length ------------------------------------------------------------------

#length we can fill in with a count character thing
master$prime[is.na(master$p.length)]
master$p.length = nchar(as.character(master$prime))

# orthoN ------------------------------------------------------------------

master$prime[is.na(master$p.orthoN)]
master$target[is.na(master$t.orthoN)]

# phonoN ------------------------------------------------------------------
master$prime[is.na(master$p.phonoN)]
master$target[is.na(master$t.phonoN)]

# freq --------------------------------------------------------------------

master$prime[is.na(master$p.freq)]
master$target[is.na(master$t.freq)]

#http://subtlexus.lexique.org/moteur2/index.php
#Lg10WF

master$p.freq[master$prime == "hyper"] = 1.6628	

# swow fsg and pmi ----------------------------------------------------------------

master$index[is.na(master$swowfsg)]
master$target[is.na(master$swow.t.fsg_ss)]
master$prime[is.na(master$swow.p.fsg_ss)]
master$index[is.na(master$pmi_swow)]

#add ax
#in general if the prime is in the SWOW data, then the relationship is zero if target is not

# swow fan ----------------------------------------------------------------

master$target[is.na(master$swow.t.fan_ss)]
master$prime[is.na(master$swow.p.fan_ss)]

# fan_ss ------------------------------------------------------------------

master$prime[is.na(master$p.fan_ss)]

# POSr --------------------------------------------------------------------

master$prime[is.na(master$p.POSr)]
master$target[is.na(master$t.POSr)]

table(master$p.POSr)

master$p.POSr[master$prime == "hyper"] = "other" #adj


