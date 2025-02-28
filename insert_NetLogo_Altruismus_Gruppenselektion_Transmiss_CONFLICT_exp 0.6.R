
# insert_NetLogo_exp.R

install.packages("pacman")         # Install pacman package
library("pacman")                  # Load pacman package

p_load(readr, tidyverse, haven, dplyr, ggplot2, scales)


rm(list=ls())
# E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/out/
setwd("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/out/")

# ---- read both data sets, one and two schools

df1 <- read_csv("Altruismus und Gruppenselektion KULTUR_COMPETE EXPERIMENT 1.2.txt conflict_culture-table.csv",
               skip = 6) # Altruismus und Gruppenselektion KULTUR_COMPETE EXPERIMENT 1.1.txt conflict_culture-table.csv


# ---------------- prepare data 

# ---- one school
df1 <- data.frame(df1)
class(df1)
head(df1[order(df1$X.run.number.),] )
head(df1)

df1 <- df1[order(df1$X.run.number.),] # just sorting
table(df1$looser.energy)
table(df1$transmiss.global)

hist(df1$share.green)
hist(df1$share.cyan)
hist(df1$share.red)


df1$looser.energy.f <- as.factor(df1$looser.energy)
df1$transmiss.global.f <- as.factor(df1$transmiss.global)
df1$color.conflict <- ifelse(df1$looser.energy.f==1, "plum1", "palegreen1") # indianred1 tomato
#df1$color.conflict <- ifelse(df1$looser.energy.f==5, "plum1", "palegreen1") # indianred1 tomato

table(df1$color.conflict)
#df1 <- subset(df1, select = c( 'X.run.number.' , 'spatial_prox', 'X.step.', 'share.altruists'))              
#head(df1)             
#table(df1$spatial_prox)

#class(df1$share.altruists)
#class(df1$X.step.)

# ---- select combination of treatments here
# 1. transmission low, no conflict
# 2. transmission high, no conflict
# 3. transmission low, conflict
# 4. transmission high, conflict

#trans_60_no_confl <- subset(df1, transmiss.global.f == 60 & looser.energy.f == 0)
#trans_80_no_confl <- subset(df1, transmiss.global.f == 80 & looser.energy.f == 0)
#trans_60_confl <- subset(df1, transmiss.global.f == 60 & looser.energy.f == 3)
#trans_80_confl <- subset(df1, transmiss.global.f == 80 & looser.energy.f == 3)

trans_40 <- subset(df1, transmiss.global.f == 40 )
trans_60 <- subset(df1, transmiss.global.f == 60 )
trans_80 <- subset(df1, transmiss.global.f == 80 )

#   +  geom_point()  + geom_point(position = position_jitter(w = 0.1, h = 0.1))

plot_cyanT60 <- ggplot(trans_60, aes(x=X.step., y=share.cyan,  color = looser.energy.f)) + theme_bw() +
  labs(x ="simulation step", y = "% with norm transmission") + geom_point(color=trans_60$color.conflict, alpha=0.4, shape=21)  + # geom_point()  +    
  aes(color = looser.energy.f) + geom_smooth(span = 0.1, aes(group=looser.energy.f) , level=.99) + #, method="loess"
  theme(axis.text.x=element_text(size=20)) + theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=20)) + theme(axis.title.y=element_text(size=20))  # +   theme(legend.position = "none")
plot_cyanT60
plot_redT80 <- ggplot(trans_80, aes(x=X.step., y=share.red,  color = looser.energy.f)) + theme_bw() +
  labs(x ="simulation step", y = "% low altruists") + geom_point(color=trans_80$color.conflict,  alpha=0.4, shape=21)  + # geom_point()  +    
  aes(color = looser.energy.f) + geom_smooth(span = 0.1, aes(group=looser.energy.f) , level=.99) + #, method="loess"
  theme(axis.text.x=element_text(size=20)) + theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=20)) + theme(axis.title.y=element_text(size=20)) # +   theme(legend.position = "none")
plot_redT80


# ---- test text in Graph
plot_redT80 <- ggplot(trans_80, aes(x=X.step., y=share.red,  color = looser.energy.f)) + theme_bw() +
  labs(x ="simulation step", y = "% low altruists") + geom_point(color=trans_80$color.conflict,  alpha=0.4, shape=21)  + # geom_point()  +    
  aes(color = looser.energy.f) + geom_smooth(span = 0.1, aes(group=looser.energy.f) , level=.99) + #, method="loess"
  theme(axis.text.x=element_text(size=20)) + theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=20)) + theme(axis.title.y=element_text(size=20))+ 
  annotate("text", x=400, y=25, label= "no conflict", size = unit(12, "pt"))  # +   theme(legend.position = "none")
plot_redT80
 

# ---------- english

#+
#  annotate("text", x=8, y=13000, label= "boat") + 
#  annotate("text", x = 4, y=13000, label = "ship")
#+
#  annotate("text", x=350, y=85, label= "spatial boundary 20%", size = unit(8, "pt"))+
#  annotate("text", x=350, y=60, label= "spatial boundary 10%", size = unit(8, "pt")) +
#  annotate("text", x=350, y=32, label= "no spatial boundary", size = unit(8, "pt"))
head(trans_40)

descr_40_C1 <- trans_40 %>% filter(X.step. == 500 & looser.energy == 1)
descr_40_C5 <- trans_40 %>% filter(X.step. == 500 & looser.energy == 5)
descr_80_C1 <- trans_80 %>% filter(X.step. == 500 & looser.energy == 1)
descr_80_C5 <- trans_80 %>% filter(X.step. == 500 & looser.energy == 5)


mean(descr_40_C1$share.cyan)
mean(descr_40_C1$share.green)
mean(descr_40_C1$share.red)

mean(descr_40_C5$share.cyan)
mean(descr_40_C5$share.green)
mean(descr_40_C5$share.red)

mean(descr_80_C1$share.cyan)
mean(descr_80_C1$share.green)
mean(descr_80_C1$share.red)

mean(descr_80_C5$share.cyan)
mean(descr_80_C5$share.green)
mean(descr_80_C5$share.red)

mean(descr_40_C1$share.cyan) - mean(descr_40_C5$share.cyan)
mean(descr_40_C1$share.green) - mean(descr_40_C5$share.green)
mean(descr_40_C1$share.red) - mean(descr_40_C5$share.red)

mean(descr_80_C1$share.cyan) - mean(descr_80_C5$share.cyan)
mean(descr_80_C1$share.green) - mean(descr_80_C5$share.green)
mean(descr_80_C1$share.red) - mean(descr_80_C5$share.red)


# ------------------------------------------------
# ------------------------------------------------
# ------------------------------------------------
# --- Cyan: group with norm transmission ---------
# ------------------------------------------------
# ------------------------------------------------
# ------------------------------------------------
# adjust data set   +  labs(fill = "cost of loosing conflict") theme(legend.position = c(0.35, 0.85)) 

# ------ conflict
# ------ trans_40 trans_60 trans_80
plot_cyanT60 <- ggplot(trans_60, aes(x=X.step., y=share.cyan,  color = looser.energy.f)) + theme_bw() +
   labs(x ="simulation step", y = "% Group N: with norm transmission") + geom_point(color=trans_60$color.conflict, alpha=0.2, shape=21)  + # geom_point()  +    
  aes(color = looser.energy.f) + geom_smooth(span = 0.1, aes(group=looser.energy.f) , level=.99) + #, method="loess"
  theme(axis.text.x=element_text(size=20)) + theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=20)) + theme(axis.title.y=element_text(size=20)) + # +   theme(legend.position = "none")
  theme(legend.position = "none") + coord_cartesian(ylim = c(0,110), xlim = c(0,520), expand=0) +
  annotate("text", x=100, y=90, label= "p(transmiss.) = 60%", size = unit(6, "pt")) +
  annotate("text", x=340, y=50, label= "inferior in intergroup conflict = -5 pts.", size = unit(6, "pt")) +
  annotate("text", x=390, y=32, label= "intergroup conflict = -1 pt. each", size = unit(6, "pt"))
  plot_cyanT60
plot_cyanT40 <- ggplot(trans_40, aes(x=X.step., y=share.cyan,  color = looser.energy.f)) + theme_bw() +
  labs(x ="simulation step", y = "% Group N: with norm transmission") + geom_point(color=trans_40$color.conflict, alpha=0.2, shape=21)  + # geom_point()  +    
  aes(color = looser.energy.f) + geom_smooth(span = 0.1, aes(group=looser.energy.f) , level=.99) + #, method="loess"
  theme(axis.text.x=element_text(size=20)) + theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=20)) + theme(axis.title.y=element_text(size=20)) + # +   theme(legend.position = "none")
  theme(legend.position = "none") + coord_cartesian(ylim = c(0,110), xlim = c(0,520), expand=0)+
  annotate("text", x=120, y=90, label= "p(transmiss.) = 40%", size = unit(6, "pt")) +
  annotate("text", x=340, y=28, label= "inferior in intergroup conflict = -5 pts.", size = unit(6, "pt")) +
  annotate("text", x=340, y=43, label= "intergroup conflict = -1 pt. each", size = unit(6, "pt")) +
  annotate("text", x=470, y=100, label= "A", size = unit(15, "pt"))
  plot_cyanT40
bmp("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/cyanT40.bmp", width=4000, height=4000, res=600) 
plot_cyanT40
dev.off()

plot_cyanT80 <- ggplot(trans_80, aes(x=X.step., y=share.cyan,  color = looser.energy.f)) + theme_bw() +
  labs(x ="simulation step", y = "% Group N: with norm transmission") + geom_point(color=trans_80$color.conflict, alpha=0.2, shape=21)  + # geom_point()  +    
  aes(color = looser.energy.f) + geom_smooth(span = 0.1, aes(group=looser.energy.f) , level=.99) + #, method="loess"
  theme(axis.text.x=element_text(size=20)) + theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=20)) + theme(axis.title.y=element_text(size=20)) + # +   theme(legend.position = "none")
  theme(legend.position = "none") + coord_cartesian(ylim = c(0,110), xlim = c(0,520), expand=0) +
  annotate("text", x=120, y=90, label= "p(transmiss.) = 80%", size = unit(6, "pt")) +
  annotate("text", x=180, y=45, label= "inferior in intergroup conflict = -5 pts.", size = unit(6, "pt")) +
  annotate("text", x=345, y=55, label= "intergroup conflict = -1 pt. each", size = unit(6, "pt"))+
  annotate("text", x=470, y=100, label= "B", size = unit(15, "pt"))
plot_cyanT80
bmp("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/cyanT80.bmp", width=4000, height=4000, res=600) 
plot_cyanT80
dev.off()



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
  labs(x ="simulation step", y = "% Group A: high altruists") + geom_point(color=trans_60$color.conflict, alpha=0.2, shape=21)  + # geom_point()  +    
  aes(color = looser.energy.f) + geom_smooth(span = 0.1, aes(group=looser.energy.f) , level=.99) + #, method="loess"
  theme(axis.text.x=element_text(size=20)) + theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=20)) + theme(axis.title.y=element_text(size=20)) + # +   theme(legend.position = "none")
  theme(legend.position = "none") + coord_cartesian(ylim = c(0,110), xlim = c(0,520), expand=0)
  plot_greenT60
plot_greenT40 <- ggplot(trans_40, aes(x=X.step., y=share.green,  color = looser.energy.f)) + theme_bw() +
  labs(x ="simulation step", y = "% Group A: high altruists") + geom_point(color=trans_40$color.conflict, alpha=0.2, shape=21)  + # geom_point()  +    
  aes(color = looser.energy.f) + geom_smooth(span = 0.1, aes(group=looser.energy.f) , level=.99) + #, method="loess"
  theme(axis.text.x=element_text(size=20)) + theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=20)) + theme(axis.title.y=element_text(size=20)) + # +   theme(legend.position = "none")
  theme(legend.position = "none") + coord_cartesian(ylim = c(0,110), xlim = c(0,520), expand=0)+
  annotate("text", x=120, y=90, label= "p(transmiss.) = 40%", size = unit(6, "pt")) +
  annotate("text", x=345, y=65, label= "inferior in intergroup conflict = -5 pts.", size = unit(6, "pt")) +
  annotate("text", x=345, y=30, label= "intergroup conflict = -1 pt. each", size = unit(6, "pt"))+
  annotate("text", x=470, y=100, label= "C", size = unit(15, "pt"))
  plot_greenT40
bmp("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/greenT40.bmp", width=4000, height=4000, res=600) 
plot_greenT40
dev.off()

plot_greenT80 <- ggplot(trans_80, aes(x=X.step., y=share.green,  color = looser.energy.f)) + theme_bw() +
  labs(x ="simulation step", y = "% Group A: high altruists") + geom_point(color=trans_80$color.conflict, alpha=0.2, shape=21)  + # geom_point()  +    
  aes(color = looser.energy.f) + geom_smooth(span = 0.1, aes(group=looser.energy.f) , level=.99) + #, method="loess"
  theme(axis.text.x=element_text(size=20)) + theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=20)) + theme(axis.title.y=element_text(size=20)) + # +   theme(legend.position = "none")
  theme(legend.position = "none") + coord_cartesian(ylim = c(0,110), xlim = c(0,520), expand=0)+
  annotate("text", x=120, y=90, label= "p(transmiss.) = 80%", size = unit(6, "pt")) +
  annotate("text", x=345, y=50, label= "inferior in intergroup conflict = -5 pts.", size = unit(6, "pt")) +
  annotate("text", x=345, y=25, label= "intergroup conflict = -1 pt. each", size = unit(6, "pt"))+
  annotate("text", x=470, y=100, label= "D", size = unit(15, "pt"))
  plot_greenT80
bmp("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/greenT80.bmp", width=4000, height=4000, res=600) 
plot_greenT80
dev.off()  
  
# ------------------------------------------------
# ------------------------------------------------
# ------------------------------------------------
# --- red: group with low base altruism  ---------
# ------------------------------------------------
# ------------------------------------------------
# ------------------------------------------------
# adjust data set
plot_redT60 <- ggplot(trans_60, aes(x=X.step., y=share.red,  color = looser.energy.f)) + theme_bw() +
  labs(x ="simulation step", y = "% Group E: low altruists") + geom_point(color=trans_60$color.conflict, alpha=0.2, shape=21)  + # geom_point()  +    
  aes(color = looser.energy.f) + geom_smooth(span = 0.1, aes(group=looser.energy.f) , level=.99) + #, method="loess"
  theme(axis.text.x=element_text(size=20)) + theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=20)) + theme(axis.title.y=element_text(size=20)) + # +   theme(legend.position = "none")
  theme(legend.position = "none") + coord_cartesian(ylim = c(0,44), xlim = c(0,520), expand=0)
  plot_redT60
plot_redT40 <- ggplot(trans_40, aes(x=X.step., y=share.red,  color = looser.energy.f)) + theme_bw() +
  labs(x ="simulation step", y = "% Group E: low altruists") + geom_point(color=trans_40$color.conflict, alpha=0.2, shape=21)  + # geom_point()  +    
  aes(color = looser.energy.f) + geom_smooth(span = 0.1, aes(group=looser.energy.f) , level=.99) + #, method="loess"
  theme(axis.text.x=element_text(size=20)) + theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=20)) + theme(axis.title.y=element_text(size=20)) + # +   theme(legend.position = "none")
  theme(legend.position = "none") + coord_cartesian(ylim = c(0,44), xlim = c(0,520), expand=0)+
  annotate("text", x=100, y=38, label= "p(transmiss.) = 40%", size = unit(6, "pt")) +
  annotate("text", x=200, y=17, label= "inferior in intergroup conflict = -5 pts.", size = unit(6, "pt")) +
  annotate("text", x=353, y=30, label= "intergroup conflict = -1 pt. each", size = unit(6, "pt"))+
  annotate("text", x=470, y=40, label= "E", size = unit(15, "pt"))
  plot_redT40
bmp("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/redT40.bmp", width=4000, height=4000, res=600) 
  plot_redT40
dev.off()
plot_redT80 <- ggplot(trans_80, aes(x=X.step., y=share.red,  color = looser.energy.f)) + theme_bw() +
  labs(x ="simulation step", y = "% Group E: low altruists") + geom_point(color=trans_80$color.conflict, alpha=0.2, shape=21)  + # geom_point()  +    
  aes(color = looser.energy.f) + geom_smooth(span = 0.1, aes(group=looser.energy.f) , level=.99) + #, method="loess"
  theme(axis.text.x=element_text(size=20)) + theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=20)) + theme(axis.title.y=element_text(size=20)) + # +   theme(legend.position = "none")
  theme(legend.position = "none") + coord_cartesian(ylim = c(0,44), xlim = c(0,520), expand=0)+
  annotate("text", x=100, y=38, label= "p(transmiss.) = 80%", size = unit(6, "pt")) +
  annotate("text", x=200, y=17, label= "inferior in intergroup conflict = -5 pts.", size = unit(6, "pt")) +
  annotate("text", x=350, y=30, label= "intergroup conflict = -1 pt. each", size = unit(6, "pt")) +
  annotate("text", x=470, y=40, label= "F", size = unit(15, "pt"))
  plot_redT80

bmp("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/redT80.bmp", width=4000, height=4000, res=600) 
  plot_redT80
dev.off()
  



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
pdf("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/cyanT60.pdf") 
plot_cyanT60
dev.off()
pdf("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/cyanT40.pdf") 
plot_cyanT40
dev.off()
pdf("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/cyanT80.pdf") 
plot_cyanT80
dev.off()

# --- green
pdf("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/greenT60.pdf")
plot_greenT60
dev.off()
pdf("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/greenT40.pdf")
plot_greenT40
dev.off()
pdf("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/greenT80.pdf")
plot_greenT80
dev.off()

# --- red
pdf("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/redT60.pdf")
plot_redT60
dev.off()
pdf("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/redT40.pdf")
plot_redT40
dev.off()
pdf("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/redT80.pdf")
plot_redT80
dev.off()






# --- cyan
png("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/cyanT60.png", pointsize=10, width=2800, height=2000, res=600)
plot_cyanT60
dev.off()
png("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/cyanT40.png", pointsize=10, width=2800, height=2000, res=600)
plot_cyanT40
dev.off()
png("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/cyanT80.png", pointsize=10, width=2800, height=2000, res=600)
plot_cyanT80
dev.off()

# --- green
png("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/greenT60.png", pointsize=10, width=2800, height=2000, res=600)
plot_greenT60
dev.off()
png("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/greenT40.png", pointsize=10, width=2800, height=2000, res=600)
plot_greenT40
dev.off()
png("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/greenT80.png", pointsize=10, width=2800, height=2000, res=600)
plot_greenT80
dev.off()

# --- red
png("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/redT60.png", pointsize=10, width=2800, height=2000, res=600)
plot_redT60
dev.off()
png("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/redT40.png", pointsize=10, width=2800, height=2000, res=600)
plot_redT40
dev.off()
png("E:/projekte/agent_based_modeling/altruismus_gruppenselektion/article/graphs/redT80.png", pointsize=10, width=2800, height=2000, res=600)
plot_redT80
dev.off()

