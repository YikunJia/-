rm(list=ls())
setwd("D:/")

devtools::install_github("Hy4m/linkET", force = TRUE)
library(linkET)
library(ggplot2)
library(dplyr)
library(cols4all)

varespec <- read.csv("hunhe-DOMcohesion.csv", row.names = 1, header=T, sep=",",check.names = FALSE)
varechem <- read.csv("env-hunhe.csv", row.names = 1, header=T, sep=",",check.names = FALSE)

lshap <- lapply(varespec, shapiro.test)
lres <- sapply(lshap, `[`, c("statistic","p.value"))
lres <- sapply(lshap, `[`, c("p.value"))
lres


lshap <- lapply(varechem, shapiro.test)
lres <- sapply(lshap, `[`, c("statistic","p.value"))
lres <- sapply(lshap, `[`, c("p.value"))
lres
#t(lres)

cor2 <- correlate(varechem, method = "spearman")
corr2 <- cor2 %>% as_md_tbl()
head(corr2)
write.csv(corr2, file = "spearman_correlate(env&env).csv", row.names = TRUE)


cor3 <- correlate(varespec, varechem, method = "spearman")
corr3 <- cor3 %>% as_md_tbl()
head(corr3)
write.csv(corr3, file = "spearman_result(bio&env).csv", row.names = TRUE)



r.p.data.plot <- corr3 %>% 
  mutate(r.sign = cut(r, breaks = c(-Inf, 0, Inf), 
                      labels = c("Negative", "Positive")),
         p.sign = cut(p, breaks = c(0, 0.05, Inf), 
                      labels = c("P<0.05", "P>=0.05"),
                      include.lowest = TRUE,
                      right = FALSE), 
         r.abs = cut(abs(r), breaks = c(0, 0.25, 0.5, 1),
                     labels = c("<0.25","0.25-0.5", "0.5-1"),
                     include.lowest = TRUE,
                     right = FALSE),    
  )  


p4 <- qcorrplot(cor2,
                grid_col = "grey50",
                grid_size = 0.2,
                type = 
                diag = F) +
  geom_square() +
  scale_fill_gradientn(colours = rev(c4a('rd_bu',30)),
                       limits = c(-1, 1))
p4


p5 <- p4 +
  geom_mark(size = 4,
            only_mark = T,
            sig_level = c(0.05, 0.01, 0.001),
            sig_thres = 0.05,
            colour = '#deb978')
p5

p6 <- p5 +
  geom_couple(data = r.p.data.plot,   
              aes(colour = r.sign,
                  size = r.abs,
                  linetype = p.sign), 
              nudge_x = 0.15,
              curvature = 0.1,
              label.fontface=1,
              label.family = "serif",
              label.size = 5.5)
p6


p7 <- p6 +
  scale_size_manual(values = c("<0.25" = 0.3,
                               "0.25-0.5" = 0.5,
                               "0.5-1" = 0.8)) +  
  scale_colour_manual(values = c("Negative" = "#009f76",
                                 "Positive" = "#d95e27"))+  
  scale_linetype_manual(values = c("P<0.05" = "solid",
                                   "P>=0.05" = "dashed"))+
  scale_fill_gradientn(colours = rev(c("#730922", "#e4795d", "#fbfbf7", "#4895c7", "#2e405f")),
                       breaks = seq(-1, 1, 0.5),
                       limits = c(-1, 1))+ 
  #geom_diag_label()+
  guides(
    fill = guide_colorbar(title = "Spearman's r",barwidth = 1,barheight = 8, order = 1), 
    linetype = guide_legend(title = NULL,override.aes = list(size = 6,linewidth = 0.6, order = 3)),
    colour = guide_legend(title = NULL,override.aes = list(size = 1,linewidth = 0.6, order = 4)),
    size = guide_legend(title = "|Spearman's r|",override.aes = list(colour = "black",size = 1),
                        order = 2)  
  )+
  theme(
    axis.text=element_text(color="black",size=16,family = "serif",face = "plain"),
    axis.text.x.top =element_text(color="black",size=16,family = "serif",face = "plain",angle = 45,hjust = 0,vjust = 0),
    legend.key = element_blank(), 
    legend.key.size = unit(0.36, "cm"), 
    legend.spacing.y = unit(0.8,"cm"),
    legend.key.spacing.y = unit(0.2,"cm"),
    legend.text = element_text(color="black",size=16,family = "serif",face = "plain"),
    legend.title = element_text(color="black",size=16,family = "serif",face = "plain",margin = margin(b = 12))
  )
p7
ggsave(filename = "Spearman_correlation_heatmaptest.pdf", p7, height = 10, width = 13, dpi = 600)