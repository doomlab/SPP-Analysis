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


# swow fsg and pmi ----------------------------------------------------------------

master$index[is.na(master$swowfsg)]

#dont: POS, anything fsg/bsg that doesn't say swow, JCN, root, affix, LSA, LSA2, SOA

#do POSr