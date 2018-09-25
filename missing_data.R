setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

master = read.csv("itemdata.csv")

summary(master)

#control shift r


# length ------------------------------------------------------------------

#length we can fill in with a count character thing
master$prime[is.na(master$p.length)]


# orthoN ------------------------------------------------------------------

master$prime[is.na(master$p.orthoN)]
master$target[is.na(master$t.orthoN)]


# phonoN ------------------------------------------------------------------
master$target[is.na(master$p.phonoN)]
master$target[is.na(master$t.phonoN)]


# freq --------------------------------------------------------------------

master$target[is.na(master$p.freq)]
master$target[is.na(master$t.freq)]


# swow fsg and pmi ----------------------------------------------------------------

master$index[is.na(master$swowfsg)]
master$index[is.na(master$swow.t.fsg_ss)]
master$index[is.na(master$swow.p.fsg_ss)]
master$index[is.na(master$pmi_swow)]


# swow fan ----------------------------------------------------------------

master$index[is.na(master$swow.t.fan_ss)]
master$index[is.na(master$swow.p.fan_ss)]


# fan_ss ------------------------------------------------------------------

master$index[is.na(master$p.fan_ss)]


# POSr --------------------------------------------------------------------

master$index[is.na(master$p.POSr)]
master$index[is.na(master$t.POSr)]

#dont: POS, anything fsg/bsg that doesn't say swow, JCN, root, affix, LSA, LSA2, SOA

#do POSr