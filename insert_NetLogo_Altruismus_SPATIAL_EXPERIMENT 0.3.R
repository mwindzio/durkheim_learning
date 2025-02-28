
# insert_NetLogo_exp.R

install.packages("pacman")         # Install pacman package
library("pacman")                  # Load pacman package

p_load(readr, tidyverse, haven, dplyr, ggplot2, scales)


rm(list=ls())
# E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/out/
setwd("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/out/")

# ---- read both data sets, one and two schools

df1 <- read_csv("Altruismus_SPATIAL_EXPERIMENT_0.1.txt Altruism_SPATIAL-table.csv",
               skip = 6) # Altruismus und Gruppenselektion KULTUR_COMPETE EXPERIMENT 1.1.txt conflict_culture-table.csv


# ---------------- prepare data 

# ---- one school
df1 <- data.frame(df1)
class(df1)
head(df1[order(df1$X.run.number.),] )
head(df1)

df1 <- df1[order(df1$X.run.number.),] # just sorting
table(df1$spatial_prob)


hist(df1$share.altruists)

df1$spatial_prob.f <- as.factor(df1$spatial_prob)


df1 <- df1%>% 
  mutate(color.prox = 
           ifelse(spatial_prob.f ==0, "plum1" , 
           ifelse(spatial_prob.f ==10, "orange","palegreen1")))
table(data.frame(df1$color.prox))         



#   +  geom_point()  + geom_point(position = position_jitter(w = 0.1, h = 0.1))

plot_spatial <- ggplot(df1, aes(x=X.step., y=share.altruists,  color = spatial_prob.f)) + theme_bw() +
  labs(x ="simulation step", y = "% altruists") + geom_point(color=df1$color.prox, alpha=0.1, shape=21)  + # geom_point()  +    
  aes(color = spatial_prob.f) + geom_smooth(span = 0.1, aes(group=spatial_prob.f) , level=.99) + #, method="loess"
  theme(axis.text.x=element_text(size=20)) + theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=20)) + theme(axis.title.y=element_text(size=20)) + # +   theme(legend.position = "none")
  theme(legend.position = "none") + coord_cartesian(ylim = c(0,110), xlim = c(0,520), expand=0)  +
  annotate("text", x=350, y=85, label= "spatial boundary 20%", size = unit(8, "pt"))+
  annotate("text", x=350, y=61, label= "spatial boundary 10%", size = unit(8, "pt")) +
  annotate("text", x=350, y=38, label= "no spatial boundary", size = unit(8, "pt")) # +   theme(legend.position = "none")
plot_spatial
plot_spatial_legend <- ggplot(df1, aes(x=X.step., y=share.altruists,  color = spatial_prob.f)) + theme_bw() +
  labs(x ="simulation step", y = "% altruists") + geom_point(color=df1$color.prox, alpha=0.1, shape=21)  + # geom_point()  +    
  aes(color = spatial_prob.f) + geom_smooth(span = 0.1, aes(group=spatial_prob.f) , level=.99) + #, method="loess"
  theme(axis.text.x=element_text(size=20)) + theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=20)) + theme(axis.title.y=element_text(size=20)) +
  annotate("text", x=350, y=85, label= "spatial boundary 20%", size = unit(8, "pt"))+
  annotate("text", x=350, y=60, label= "spatial boundary 10%", size = unit(8, "pt")) +
  annotate("text", x=350, y=32, label= "no spatial boundary", size = unit(8, "pt"))
plot_spatial_legend

#+ 
#  annotate("text", x=400, y=25, label= "no conflict", size = unit(12, "pt"))  # +   theme(legend.position = "none")


# --- graph paper spatial boundary
# --- cyan
bmp("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/plot_spatial.bmp", width=4000, height=4000, res=600) 
plot_spatial
dev.off()


# -------------------------- END ---------------------------------



# --- cyan
bmp("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/plot_spatial_legend.bmp", width=4000, height=4000, res=600) 
plot_spatial_legend
dev.off()

# --- cyan
pdf("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/plot_spatial.pdf") 
plot_spatial
dev.off()
# --- cyan
pdf("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/plot_spatial_legend.pdf") 
plot_spatial_legend
dev.off()

# ---------- english

# ------------------------------------------------
# ------------------------------------------------
# ------------------------------------------------
# --- Cyan: group with norm transmission ---------
# ------------------------------------------------
# ------------------------------------------------
# ------------------------------------------------
# adjust data set   +  labs(fill = "cost of loosing conflict") theme(legend.position = c(0.35, 0.85)) 

#+ 
#  annotate("text", x=400, y=25, label= "no conflict", size = unit(12, "pt"))  # +   theme(legend.position = "none")

# ------ conflict
# ------ trans_40 trans_60 trans_80
plot_cyanT60 <- ggplot(trans_60, aes(x=X.step., y=share.cyan,  color = looser.energy.f)) + theme_bw() +
   labs(x ="simulation step", y = "% with norm transmission") + geom_point(color=trans_60$color.conflict, alpha=0.2, shape=21)  + # geom_point()  +    
  aes(color = looser.energy.f) + geom_smooth(span = 0.1, aes(group=looser.energy.f) , level=.99) + #, method="loess"
  theme(axis.text.x=element_text(size=20)) + theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=20)) + theme(axis.title.y=element_text(size=20)) + # +   theme(legend.position = "none")
  theme(legend.position = "none") + coord_cartesian(ylim = c(0,110), xlim = c(0,520), expand=0)
  plot_cyanT60
plot_cyanT40 <- ggplot(trans_40, aes(x=X.step., y=share.cyan,  color = looser.energy.f)) + theme_bw() +
  labs(x ="simulation step", y = "% with norm transmission") + geom_point(color=trans_40$color.conflict, alpha=0.2, shape=21)  + # geom_point()  +    
  aes(color = looser.energy.f) + geom_smooth(span = 0.1, aes(group=looser.energy.f) , level=.99) + #, method="loess"
  theme(axis.text.x=element_text(size=20)) + theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=20)) + theme(axis.title.y=element_text(size=20)) + # +   theme(legend.position = "none")
  theme(legend.position = "none") + coord_cartesian(ylim = c(0,110), xlim = c(0,520), expand=0)
  plot_cyanT40
plot_cyanT80 <- ggplot(trans_80, aes(x=X.step., y=share.cyan,  color = looser.energy.f)) + theme_bw() +
  labs(x ="simulation step", y = "% with norm transmission") + geom_point(color=trans_80$color.conflict, alpha=0.2, shape=21)  + # geom_point()  +    
  aes(color = looser.energy.f) + geom_smooth(span = 0.1, aes(group=looser.energy.f) , level=.99) + #, method="loess"
  theme(axis.text.x=element_text(size=20)) + theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=20)) + theme(axis.title.y=element_text(size=20)) + # +   theme(legend.position = "none")
  theme(legend.position = "none") + coord_cartesian(ylim = c(0,110), xlim = c(0,520), expand=0)
plot_cyanT80
# ------------------------------------------------
# ------------------------------------------------
# ------------------------------------------------
# --- green: group with high base altruism  ------
# ------------------------------------------------
# ------------------------------------------------
# ------------------------------------------------
# adjust data set
# adjust data set

# ------ conflict
# ------ trans_60 trans_40 trans_80
plot_greenT60 <- ggplot(trans_60, aes(x=X.step., y=share.green,  color = looser.energy.f)) + theme_bw() +
  labs(x ="simulation step", y = "% high altruists") + geom_point(color=trans_60$color.conflict, alpha=0.2, shape=21)  + # geom_point()  +    
  aes(color = looser.energy.f) + geom_smooth(span = 0.1, aes(group=looser.energy.f) , level=.99) + #, method="loess"
  theme(axis.text.x=element_text(size=20)) + theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=20)) + theme(axis.title.y=element_text(size=20)) + # +   theme(legend.position = "none")
  theme(legend.position = "none") + coord_cartesian(ylim = c(0,110), xlim = c(0,520), expand=0)
  plot_greenT60
plot_greenT40 <- ggplot(trans_40, aes(x=X.step., y=share.green,  color = looser.energy.f)) + theme_bw() +
  labs(x ="simulation step", y = "% high altruists") + geom_point(color=trans_40$color.conflict, alpha=0.2, shape=21)  + # geom_point()  +    
  aes(color = looser.energy.f) + geom_smooth(span = 0.1, aes(group=looser.energy.f) , level=.99) + #, method="loess"
  theme(axis.text.x=element_text(size=20)) + theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=20)) + theme(axis.title.y=element_text(size=20)) + # +   theme(legend.position = "none")
  theme(legend.position = "none") + coord_cartesian(ylim = c(0,110), xlim = c(0,520), expand=0)
  plot_greenT40
plot_greenT80 <- ggplot(trans_80, aes(x=X.step., y=share.green,  color = looser.energy.f)) + theme_bw() +
  labs(x ="simulation step", y = "% high altruists") + geom_point(color=trans_80$color.conflict, alpha=0.2, shape=21)  + # geom_point()  +    
  aes(color = looser.energy.f) + geom_smooth(span = 0.1, aes(group=looser.energy.f) , level=.99) + #, method="loess"
  theme(axis.text.x=element_text(size=20)) + theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=20)) + theme(axis.title.y=element_text(size=20)) + # +   theme(legend.position = "none")
  theme(legend.position = "none") + coord_cartesian(ylim = c(0,110), xlim = c(0,520), expand=0)
  plot_greenT80

# ------------------------------------------------
# ------------------------------------------------
# ------------------------------------------------
# --- red: group with low base altruism  ---------
# ------------------------------------------------
# ------------------------------------------------
# ------------------------------------------------
# adjust data set
plot_redT60 <- ggplot(trans_60, aes(x=X.step., y=share.red,  color = looser.energy.f)) + theme_bw() +
  labs(x ="simulation step", y = "% low altruists") + geom_point(color=trans_60$color.conflict, alpha=0.2, shape=21)  + # geom_point()  +    
  aes(color = looser.energy.f) + geom_smooth(span = 0.1, aes(group=looser.energy.f) , level=.99) + #, method="loess"
  theme(axis.text.x=element_text(size=20)) + theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=20)) + theme(axis.title.y=element_text(size=20)) + # +   theme(legend.position = "none")
  theme(legend.position = "none") + coord_cartesian(ylim = c(0,44), xlim = c(0,520), expand=0)
  plot_redT60
plot_redT40 <- ggplot(trans_40, aes(x=X.step., y=share.red,  color = looser.energy.f)) + theme_bw() +
  labs(x ="simulation step", y = "% low altruists") + geom_point(color=trans_40$color.conflict, alpha=0.2, shape=21)  + # geom_point()  +    
  aes(color = looser.energy.f) + geom_smooth(span = 0.1, aes(group=looser.energy.f) , level=.99) + #, method="loess"
  theme(axis.text.x=element_text(size=20)) + theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=20)) + theme(axis.title.y=element_text(size=20)) + # +   theme(legend.position = "none")
  theme(legend.position = "none") + coord_cartesian(ylim = c(0,44), xlim = c(0,520), expand=0)
  plot_redT40
plot_redT80 <- ggplot(trans_80, aes(x=X.step., y=share.red,  color = looser.energy.f)) + theme_bw() +
  labs(x ="simulation step", y = "% low altruists") + geom_point(color=trans_80$color.conflict, alpha=0.2, shape=21)  + # geom_point()  +    
  aes(color = looser.energy.f) + geom_smooth(span = 0.1, aes(group=looser.energy.f) , level=.99) + #, method="loess"
  theme(axis.text.x=element_text(size=20)) + theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=20)) + theme(axis.title.y=element_text(size=20)) + # +   theme(legend.position = "none")
  theme(legend.position = "none") + coord_cartesian(ylim = c(0,44), xlim = c(0,520), expand=0)
  plot_redT80




path_graph <- "E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/" 

tiff("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/cyanT60.tif", width=4000, height=4000, res=600) 
plot_cyanT60
dev.off()
bmp("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/cyanT60.bmp", width=4000, height=4000, res=400) 
plot_cyanT60
dev.off()

# ---------------------------------
# --- start here writing graphs  --
# ---------------------------------

# --- cyan
bmp("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/cyanT60.bmp", width=4000, height=4000, res=600) 
plot_cyanT60
dev.off()
bmp("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/cyanT40.bmp", width=4000, height=4000, res=600) 
plot_cyanT40
dev.off()
bmp("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/cyanT80.bmp", width=4000, height=4000, res=600) 
plot_cyanT80
dev.off()

# --- green
bmp("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/greenT60.bmp", width=4000, height=4000, res=600) 
plot_greenT60
dev.off()
bmp("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/greenT40.bmp", width=4000, height=4000, res=600) 
plot_greenT40
dev.off()
bmp("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/greenT80.bmp", width=4000, height=4000, res=600) 
plot_greenT80
dev.off()

# --- red
bmp("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/redT60.bmp", width=4000, height=4000, res=600) 
plot_redT60
dev.off()
bmp("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/redT40.bmp", width=4000, height=4000, res=600) 
plot_redT40
dev.off()
bmp("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/redT80.bmp", width=4000, height=4000, res=600) 
plot_redT80
dev.off()










# --- cyan
png("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/cyanT60.png", pointsize=10, width=2800, height=2000, res=600) 
plot_cyanT60
dev.off()
png("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/cyanT40.png") 
plot_cyanT40
dev.off()
png("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/cyanT80.png") 
plot_cyanT80
dev.off()

# --- green
png("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/greenT60.png") 
plot_greenT60
dev.off()
png("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/greenT40.png") 
plot_greenT40
dev.off()
png("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/greenT80.png") 
plot_greenT80
dev.off()

# --- red
png("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/redT60.png") 
plot_redT60
dev.off()
png("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/redT40.png") 
plot_redT40
dev.off()
png("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/redT80.png") 
plot_redT80
dev.off()

