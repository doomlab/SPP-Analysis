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

#p.freq
master$p.freq[master$prime == "hyper"] = 1.6628	


#t.freq
#master$t.freq[master$target == "hyper"] = __


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

#p.POSr
table(master$p.POSr)

master$p.POSr[master$prime == "hyper"] = "other" #adj
master$p.POSr[master$prime == "usa"] = "NN"
master$p.POSr[master$prime == "clorox"] = "NN"
master$p.POSr[master$prime == "recliner"] = "NN"
master$p.POSr[master$prime == "juggler"] = "NN"
master$p.POSr[master$prime == "saltine"] = "NN"
master$p.POSr[master$prime == "crayola"] = "NN"
master$p.POSr[master$prime == "bandaid"] = "NN"
master$p.POSr[master$prime == "antlers"] = "NN"
master$p.POSr[master$prime == "sinker"] = "NN"
master$p.POSr[master$prime == "nerd"] = "NN"
master$p.POSr[master$prime == "demi"] = "other" #prefix
master$p.POSr[master$prime == "mcdonalds"] = "NN"
master$p.POSr[master$prime == "frill"] = "NN"
master$p.POSr[master$prime == "pissed"] = "other" #adj
master$p.POSr[master$prime == "frankenstein"] = "NN"
master$p.POSr[master$prime == "axon"] = "NN"
master$p.POSr[master$prime == "aright"] = "other" #adv
master$p.POSr[master$prime == "zit"] = "NN"
master$p.POSr[master$prime == "tupperware"] = "NN"
master$p.POSr[master$prime == "uncooked"] = "other" #adj
master$p.POSr[master$prime == "shears"] = "NN"
master$p.POSr[master$prime == "typist"] = "NN"
master$p.POSr[master$prime == "escargot"] = "NN"
master$p.POSr[master$prime == "popeye"] = "NN"
master$p.POSr[master$prime == "hairspray"] = "NN"
master$p.POSr[master$prime == "pancakes"] = "NN"
master$p.POSr[master$prime == "breezeway"] = "NN"
master$p.POSr[master$prime == "cornbeef"] = "NN"
master$p.POSr[master$prime == "britannica"] = "NN" #not sure on this one
master$p.POSr[master$prime == "anisette"] = "NN"
master$p.POSr[master$prime == "lightheaded"] = "other" #adj
master$p.POSr[master$prime == "dracula"] = "NN"
master$p.POSr[master$prime == "mints"] = "NN"
master$p.POSr[master$prime == "excedrin"] = "NN"
master$p.POSr[master$prime == "harley"] = "NN"
master$p.POSr[master$prime == "walkway"] = "NN"
master$p.POSr[master$prime == "overview"] = "NN"
master$p.POSr[master$prime == "seatbelt"] = "NN"
master$p.POSr[master$prime == "bookbag"] = "NN"
master$p.POSr[master$prime == "checkers"] = "NN"
master$p.POSr[master$prime == "chromosomes"] = "NN"
master$p.POSr[master$prime == "bagel"] = "NN"
master$p.POSr[master$prime == "paperclip"] = "NN"
master$p.POSr[master$prime == "yummy"] = "other" #adj
master$p.POSr[master$prime == "chalkboard"] = "NN"

#t.POSr
table(master$t.POSr)

master$t.POSr[master$target == "cant"] = "NN" #unless this is the contraction
master$t.POSr[master$target == "conceited"] = "other" #adj
master$t.POSr[master$target == "dont"] = "other" #contraction
master$t.POSr[master$target == "frankenstein"] = "NN"
master$t.POSr[master$target == "geek"] = "NN"
master$t.POSr[master$target == "klutz"] = "NN"
master$t.POSr[master$target == "noodles"] = "NN"
master$t.POSr[master$target == "pancakes"] = "NN"
master$t.POSr[master$target == "pepsi"] = "NN"
master$t.POSr[master$target == "seatbelt"] = "NN"
master$t.POSr[master$target == "tupperware"] = "NN"
master$t.POSr[master$target == "usa"] = "NN"
master$t.POSr[master$target == "walkway"] = "NN"
master$t.POSr[master$target == "wimp"] = "NN"
master$t.POSr[master$target == "zit"] = "NN"
