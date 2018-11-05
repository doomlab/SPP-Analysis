setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
spp.data=read.csv("itemdata.csv") 

library(ggplot2)
library(nlme)
library(expss)

cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

library(reshape)
longdata = melt(spp.data[ , c("SOA200", "SOA1200", "task", "relation")],
                id = c("task", "relation"))
#fancy code that gets us the split violins
GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

tiff(filename = "ldt_item.tiff", res = 300, width = 8, 
     height = 4, units = 'in', compression = "lzw")

ggplot(longdata[ longdata$task == "LDT" , ], aes(variable, value, color = relation, fill = relation)) + geom_split_violin()+cleanup +
  xlab("Frequency at SOA") + 
  ylab("Z-Priming") + ylim(-2, 2)+ geom_hline(yintercept=0)+
  scale_color_manual(name = "Relation",
                     labels = c("First", "Other"),
                     values = c("#778899", "#591300")) +
  scale_fill_manual(name = "Relation",
                    labels = c("First", "Other"),
                    values = c("#778899", "#591300"))
dev.off()

tiff(filename = "name_item.tiff", res = 300, width = 8, 
     height = 4, units = 'in', compression = "lzw")

ggplot(longdata[ longdata$task == "NAME" , ], aes(variable, value, color = relation, fill = relation)) + geom_split_violin()+cleanup +
  xlab("Frequency at SOA") + 
  ylab("Z-Priming") + ylim(-2, 2)+geom_hline(yintercept=0)+
  scale_color_manual(name = "Relation",
                     labels = c("First", "Other"),
                     values = c("#778899", "#591300")) +
  scale_fill_manual(name = "Relation",
                    labels = c("First", "Other"),
                    values = c("#778899", "#591300"))

dev.off()

####do the subject ldt####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
spp.data=read.csv("subjectdataLDT.csv") 

##ldt data creation

# 200 first ldt -----------------------------------------------------------
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
unrelated.model = lme(target.RT ~ 1,
                      data = spp.data.un,
                      method = "ML",
                      random = list(~1|target),
                      na.action = "na.omit")

RTtosubtract = as.data.frame(unrelated.model$coefficients$random$target + unrelated.model$coefficients$fixed)
RTtosubtract$target = rownames(RTtosubtract)
colnames(RTtosubtract)[1] = "RTscore"

spp.data.rel$RTunrelated = vlookup(as.character(spp.data.rel$target), RTtosubtract, "RTscore", "target")

spp.data.rel$priming.RT = spp.data.rel$RTunrelated - spp.data.rel$target.RT
#unrelated minus related meaning positive = priming, negative = slowing

#merge to the new dataset
final_subject_ldt = spp.data.rel[ , c("isi", "type", "priming.RT")]

# 200 other ldt -----------------------------------------------------------
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

#merge to the new dataset
final_subject_ldt = rbind(final_subject_ldt, spp.data.rel[ , c("isi", "type", "priming.RT")])

# 1200 first ldt ----------------------------------------------------------------

spp.data.rel = subset(spp.data, 
                      target.ACC == 1 & #only trials they got right
                        isi == 1050 & #use 50 for 200; use 1050 for 1200 SOA
                        type == "first" & #use first or other
                        rel == "rel") #for related words

spp.data.un = subset(spp.data, 
                     target.ACC == 1 & #only trials they got right
                       isi == 1050 & #use 50 for 200; use 1050 for 1200 SOA
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

#merge to the new dataset
final_subject_ldt = rbind(final_subject_ldt, spp.data.rel[ , c("isi", "type", "priming.RT")])


# 1200 other ldt ----------------------------------------------------------

#pull only the information you are interested in
spp.data.rel = subset(spp.data, 
                      target.ACC == 1 & #only trials they got right
                        isi == 1050 & #use 50 for 200; use 1050 for 1200 SOA
                        type == "other" & #use first or other
                        rel == "rel") #for related words

spp.data.un = subset(spp.data, 
                     target.ACC == 1 & #only trials they got right
                       isi == 1050 & #use 50 for 200; use 1050 for 1200 SOA
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

#merge to the new dataset
final_subject_ldt = rbind(final_subject_ldt, spp.data.rel[ , c("isi", "type", "priming.RT")])

final_subject_ldt$isi = factor(final_subject_ldt$isi,
                               levels = c(50, 1050),
                               labels = c("SOA200", "SOA1200"))

tiff(filename = "ldt_subject.tiff", res = 300, width = 8, 
     height = 4, units = 'in', compression = "lzw")

ggplot(final_subject_ldt, 
       aes(isi, priming.RT, color = type, fill = type)) + 
  geom_split_violin()+ 
  cleanup +
  xlab("Frequency at SOA") + 
  ylab("Priming") + 
  ylim(-3000, 1000)+ geom_hline(yintercept=0)+
  scale_color_manual(name = "Relation",
                     labels = c("First", "Other"),
                     values = c("#778899", "#591300")) +
  scale_fill_manual(name = "Relation",
                    labels = c("First", "Other"),
                    values = c("#778899", "#591300"))
dev.off()

####do the subject naming####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
spp.data=read.csv("subjectdataN.csv") 

##naming data creation

# 200 first name -----------------------------------------------------------
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
unrelated.model = lme(target.RT ~ 1,
                      data = spp.data.un,
                      method = "ML",
                      random = list(~1|target),
                      na.action = "na.omit")

RTtosubtract = as.data.frame(unrelated.model$coefficients$random$target + unrelated.model$coefficients$fixed)
RTtosubtract$target = rownames(RTtosubtract)
colnames(RTtosubtract)[1] = "RTscore"

spp.data.rel$RTunrelated = vlookup(as.character(spp.data.rel$target), RTtosubtract, "RTscore", "target")

spp.data.rel$priming.RT = spp.data.rel$RTunrelated - spp.data.rel$target.RT
#unrelated minus related meaning positive = priming, negative = slowing

#merge to the new dataset
final_subject_name = spp.data.rel[ , c("isi", "type", "priming.RT")]

# 200 other name -----------------------------------------------------------
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

#merge to the new dataset
final_subject_name = rbind(final_subject_name, spp.data.rel[ , c("isi", "type", "priming.RT")])

# 1200 first name ----------------------------------------------------------------

spp.data.rel = subset(spp.data, 
                      target.ACC == 1 & #only trials they got right
                        isi == 1050 & #use 50 for 200; use 1050 for 1200 SOA
                        type == "first" & #use first or other
                        rel == "rel") #for related words

spp.data.un = subset(spp.data, 
                     target.ACC == 1 & #only trials they got right
                       isi == 1050 & #use 50 for 200; use 1050 for 1200 SOA
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

#merge to the new dataset
final_subject_name = rbind(final_subject_name, spp.data.rel[ , c("isi", "type", "priming.RT")])


# 1200 other name ----------------------------------------------------------

#pull only the information you are interested in
spp.data.rel = subset(spp.data, 
                      target.ACC == 1 & #only trials they got right
                        isi == 1050 & #use 50 for 200; use 1050 for 1200 SOA
                        type == "other" & #use first or other
                        rel == "rel") #for related words

spp.data.un = subset(spp.data, 
                     target.ACC == 1 & #only trials they got right
                       isi == 1050 & #use 50 for 200; use 1050 for 1200 SOA
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

#merge to the new dataset
final_subject_name = rbind(final_subject_name, spp.data.rel[ , c("isi", "type", "priming.RT")])

final_subject_name$isi = factor(final_subject_name$isi,
                               levels = c(50, 1050),
                               labels = c("SOA200", "SOA1200"))

tiff(filename = "name_subject.tiff", res = 300, width = 8, 
     height = 4, units = 'in', compression = "lzw")

ggplot(final_subject_name, 
       aes(isi, priming.RT, color = type, fill = type)) + 
  geom_split_violin()+ 
  cleanup +
  xlab("Frequency at SOA") + 
  ylab("Priming") + 
  ylim(-3000, 1000)+ geom_hline(yintercept=0)+
  scale_color_manual(name = "Relation",
                     labels = c("First", "Other"),
                     values = c("#778899", "#591300")) +
  scale_fill_manual(name = "Relation",
                    labels = c("First", "Other"),
                    values = c("#778899", "#591300"))
dev.off()
