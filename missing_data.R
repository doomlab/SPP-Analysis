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
master$p.freq[master$prime == "usa"] = 2.3096
master$p.freq[master$prime == "clorox"] = NA
master$p.freq[master$prime == "recliner"] = 0.8451
master$p.freq[master$prime == "juggler"] = 1.3979
master$p.freq[master$prime == "saltine"] = 1.0414
master$p.freq[master$prime == "crayola"] = NA
master$p.freq[master$prime == "bandaid"] = NA
master$p.freq[master$prime == "antlers"] = 1.6721
master$p.freq[master$prime == "sinker"] = 1.4624
master$p.freq[master$prime == "nerd"] = 2.2279
master$p.freq[master$prime == "demi"] = 1.3222
master$p.freq[master$prime == "mcdonalds"] = NA
master$p.freq[master$prime == "frill"] = 0.301
master$p.freq[master$prime == "pissed"] = 3.1143
master$p.freq[master$prime == "frankenstein"] = 2.549
master$p.freq[master$prime == "axon"] = 0.4771
master$p.freq[master$prime == "aright"] = 1.2553
master$p.freq[master$prime == "zit"] = 1.7243
master$p.freq[master$prime == "tupperware"] = NA
master$p.freq[master$prime == "uncooked"] = 0.7782
master$p.freq[master$prime == "shears"] = 1.6902
master$p.freq[master$prime == "typist"] = 1.6335
master$p.freq[master$prime == "escargot"] = 1.2041
master$p.freq[master$prime == "popeye"] = 1.9085
master$p.freq[master$prime == "hairspray"] = 1.5911
master$p.freq[master$prime == "pancakes"] = 2.6928
master$p.freq[master$prime == "breezeway"] = 0.4771
master$p.freq[master$prime == "cornbeef"] = NA
master$p.freq[master$prime == "britannica"] = NA
master$p.freq[master$prime == "anisette"] = 0.4771
master$p.freq[master$prime == "lightheaded"] = 1.4624
master$p.freq[master$prime == "fairytale"] = NA
master$p.freq[master$prime == "dracula"] = NA
master$p.freq[master$prime == "mints"] = 1.9138
master$p.freq[master$prime == "excedrin"] = NA
master$p.freq[master$prime == "harley"] = NA
master$p.freq[master$prime == "walkway"] = 	1.301
master$p.freq[master$prime == "overview"] = 1.3617
master$p.freq[master$prime == "seatbelt"] = 1.6812
master$p.freq[master$prime == "bookbag"] = NA
master$p.freq[master$prime == "checkers"] = 1.9868
master$p.freq[master$prime == "chromosomes"] = 1.6435
master$p.freq[master$prime == "bagel"] = 2.1959
master$p.freq[master$prime == "paperclip"] = 0.4771
master$p.freq[master$prime == "yummy"] = 2.3598
master$p.freq[master$prime == "chalkboard"] = 1.1139

#t.freq
master$t.freq[master$target == "cant"] = 2.0792
master$t.freq[master$target == "conceited"] = 1.9777
master$t.freq[master$target == "dont"] = 2.4249
master$t.freq[master$target == "frankenstein"] = 2.549
master$t.freq[master$target == "geek"] = 2.549
master$t.freq[master$target == "klutz"] = 1.6812
master$t.freq[master$target == "noodles"] = 2.4914
master$t.freq[master$target == "pancakes"] = 2.6928
master$t.freq[master$target == "pepsi"] = 1.9777
master$t.freq[master$target == "seatbelt"] = 1.6812
master$t.freq[master$target == "tupperware"] = NA
master$t.freq[master$target == "usa"] = 2.3096
master$t.freq[master$target == "walkway"] = 1.301
master$t.freq[master$target == "wimp"] = 2.2041
master$t.freq[master$target == "zit"] = 1.7243


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
