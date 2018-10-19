setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
spp.data=read.csv("itemdata.csv") 

library(ggplot2)

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


ggplot(longdata[ longdata$task == "LDT" , ], 
       aes(value, color = relation, fill = relation)) +
  geom_density() + 
  cleanup +
  xlab("Z-Priming") + 
  ylab("Frequency") +
  scale_color_manual(name = "Relation",
                     labels = c("First", "Other"),
                     values = c("Gray", "Maroon")) +
  scale_fill_manual(name = "Relation",
                     labels = c("First", "Other"),
                     values = c("Gray", "Maroon")) +
  facet_grid(~variable)

ggplot(longdata[ longdata$task == "NAME" , ], aes(value, color = relation, fill = relation)) +
  geom_histogram() + 
  cleanup +
  xlab("Z-Priming") + 
  ylab("Frequency") +
  scale_color_manual(name = "Relation",
                     labels = c("First", "Other"),
                     values = c("Gray", "Maroon")) +
  scale_fill_manual(name = "Relation",
                    labels = c("First", "Other"),
                    values = c("Gray", "Maroon")) +
  facet_grid(~variable)
