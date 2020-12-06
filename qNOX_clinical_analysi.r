############################################
#Rscript_clinical_analysis
#Author: Yunlong Ma
#E-mail: glb-biotech@zju.edu.cn
#Date:2020-12-03
############################################



##-----------------------------##--------------------------------------##
#step 1: get the working directory
setwd("F:\\Desktop\\Data_analysis_Wang")


##-----------------------------##--------------------------------------##
#step 2: install and load packages
if(!require("ggplot2"))install.packages("ggplot2")
if(!require("dplyr"))install.packages("dplyr")
if(!require("ggalt"))install.packages("ggalt")
if(!require("Hmisc"))install.packages("Hmisc")
if(!require("ggsci"))install.packages("ggsci")
if(!require("ggpubr"))install.packages("ggpubr")
if(!require("reshape"))install.packages("reshape")



##Install reshape2 package
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install()
#BiocManager::install(c("reshape2"))


packageVersion("dplyr")
remove.packages("dplyr")
remove.packages("rlang")
remove.packages("vctrs")

install.packages("rlang",version="0.4.7")
install.packages("dplyr",version="1.0.0")
install.packages("vctrs",version="0.3.2")
install.packages("pwr")

library(ggplot2)
library(dplyr)
library(ggalt)
library(ggsci)
library(ggpubr)
library(reshape)
library(reshape2)
library(pwr)
#step 3 import data


##-----------------------------##--------------------------------------##
#T-test
data1 <- read.table("table_1_data.txt",header = T)

p.age <- t.test(data1$age[which(data1$Group=="P")],data1$age[which(data1$Group=="PS")])
p.height <- t.test(data1$height[which(data1$Group=="P")],data1$height[which(data1$Group=="PS")])
p.weight <- t.test(data1$weight[which(data1$Group=="P")],data1$weight[which(data1$Group=="PS")])
p.time <- t.test(data1$surgery_time[which(data1$Group=="P")],data1$surgery_time[which(data1$Group=="PS")])

p.time2 <- t.test(data1$anesthesia_time[which(data1$Group=="P")],data1$anesthesia_time[which(data1$Group=="PS")])

tmp <- data.frame(p=c(52,20), PS=c(54,15))
p.grade <- chisq.test(tmp)

sex <- data.frame(p=c(42,30), PS=c(30,39))
p.sex <- chisq.test(sex)


#power analysis
?pwr.2p.test()
?pwr.2p2n.test()
pwr.2p2n.test(h=NULL, n1=71, n2=69, sig.level = 0.05, power = 0.8, 
              alternative = "two.sided")

##calculate ANOVA power
es<- seq(0.1,0.5,0.01)
nes <- length(es)
samsize <-NULL
for(i in 1:nes){
  
  result <- pwr.anova.test(k=5, f=es[i],sig.level = 0.05, power = 0.9)
  samsize[i]<-ceiling(result$n)
}


plot(samsize,es,type="l",lwd=2, col="red",ylab="Effect size",xlab="Sample size", main = "One Way ANOVA with Power =0.9")


##calculate power analysis
es<- seq(0.2,0.9,0.01)
nes <- length(es)
samsize <-NULL
for(i in 1:nes){
  result <- pwr.2p2n.test(h=es[i],n1 = 71, n2=69, sig.level = 0.05)
  samsize[i]<-result$power
}


plot(es,samsize,type="l",lwd=2, col="red",xlab="Effect size",ylab="Power value")
power1 <- pwr.2p2n.test(h=0.5,n1 = 71, n2=69, sig.level = 0.05)
pw1 <-power1$power
abline(v=0.5,col="blue",lty=3)

power2 <- pwr.2p2n.test(h=0.8,n1 = 71, n2=69, sig.level = 0.05)
pw2 <-power2$power
abline(v=0.8,col="blue",lty=3)

power3 <- pwr.2p2n.test(h=0.3,n1 = 71, n2=69, sig.level = 0.05)
pw3 <-power3$power
abline(v=0.3,col="blue",lty=3)

##-----------------------------##--------------------------------------##

##example code
ggplot(data=data3, aes(x=variable, y=value)) +
  stat_summary(aes(group=Group, shape=Group),fun.y =mean, geom='point', fill="black", size=3, position = position_dodge(0.25))+
  stat_summary(aes(group=Group),fun.y =mean, geom='line', cex=1, color="black", position = position_dodge(0.25)) +
  stat_summary(aes(group=Group),fun.data  =  mean_se, geom = "errorbar", position = position_dodge(0.25), width=0.2, color="black")+
  theme_classic()+
  scale_x_discrete("Time Point")+
  scale_y_continuous("qNOX value")+
  theme(legend.position = c(0.8,0.9), 
        legend.box.background = element_rect(color="black", size=1), 
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(color = "black",  size=14))


##-----------------------------##--------------------------------------##
##code for qNOX test
data2 <- read.table("table_2_qNOX_data.txt",header = T)
data3 <- melt(data2, id=c("Group","ID","sample"))

#1. P value = ***
max(na.omit(data3$value))
min(na.omit(data3$value))
windowsFonts(myFont = windowsFont("Calibri"))
ggplot(data=data3, aes(x=variable, y=value)) +
  stat_summary(aes(group=Group, color=Group),fun.y =mean, geom='point', size=2.7)+
  stat_summary(aes(group=Group,color=Group),fun.y =mean, geom='line', cex=0.7) +
  stat_summary(aes(group=Group,color=Group),fun.data  =  mean_se, geom = "errorbar", width=0.3, cex=0.9)+
  theme_classic()+
  stat_compare_means(aes(group=as.factor(Group)),method="wilcox.test",label = "p.signif", hide.ns = TRUE, label.y=c(100,rep(80,14)))+
  scale_color_manual(values = pal_jama()(7)[c(1,7)])+
  scale_fill_manual(values = pal_jama()(7)[c(1,7)])+
  theme(legend.position = c(0.8,0.9), 
        legend.box.background = element_rect(color="black", size=1), 
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(color = "black",  size=12, family="myFont"),
        axis.text = element_text(color = "black",  size=12, family="myFont"))+
  scale_x_discrete("Time points")+
  scale_y_continuous("qNOX value",limits = c(20,100))
ggsave("qNOX.PDF", width = 600, height = 300, units = "mm")  


  
  
#1.1. P value = *** color changed
windowsFonts(myFont = windowsFont("Calibri"))
ggplot(data=data3, aes(x=variable, y=value)) +
  stat_summary(aes(group=Group, color=Group),fun.y =mean, geom='point', size=3.0)+
  stat_summary(aes(group=Group,color=Group),fun.y =mean, geom='line', cex=0.9) +
  stat_summary(aes(group=Group,color=Group),fun.data  =  mean_se, geom = "errorbar", width=0.3, cex=0.9)+
  theme_classic()+
  stat_compare_means(aes(group=as.factor(Group)),method="wilcox.test",label = "p.signif", p.adjust.method = 'holm', hide.ns = TRUE, label.y=c(100,rep(80,14)))+
  scale_color_manual(values = pal_jama()(7)[c(2,3)])+
  scale_fill_manual(values = pal_jama()(7)[c(2,3)])+
  theme(legend.position = c(0.8,0.9), 
        legend.box.background = element_rect(color="black", size=1), 
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(color = "black",  size=14, family="myFont"),
        axis.text = element_text(color = "black",  size=13, family="myFont"))+
  scale_x_discrete("Time points")+
  scale_y_continuous("qNOX value",limits = c(20,100))



#2 P value = 0.005
windowsFonts(myFont = windowsFont("Calibri"))
ggplot(data=data3, aes(x=variable, y=value)) +
  stat_summary(aes(group=Group, color=Group),fun.y =mean, geom='point', size=3.0)+
  stat_summary(aes(group=Group,color=Group),fun.y =mean, geom='line', cex=0.9) +
  stat_summary(aes(group=Group,color=Group),fun.data  =  mean_se, geom = "errorbar", width=0.3, cex=0.9)+
  theme_classic()+
  stat_compare_means(aes(group=Group),method="wilcox.test",label = "..p.format..", hide.ns = TRUE, label.y=c(100,rep(62,14)))+
  scale_color_manual(values = pal_jama()(7)[c(1,7)])+
  scale_fill_manual(values = pal_jama()(7)[c(1,7)])+
  theme(legend.position = c(0.8,0.9), 
        legend.box.background = element_rect(color="black", size=1), 
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(color = "black",  size=14, family="myFont"),
        axis.text = element_text(color = "black",  size=13, family="myFont"))+
  scale_x_discrete("Time points")+
  scale_y_continuous("qNOX value",limits = c(20,100))


###calculate the P value and mean
library(ggpubr)
dataA = data3
i="qNOX"
P <- compare_means(data=dataA, value~Group, group.by = "variable", method = 'wilcox.test')
#compare_means(data=data3, value~Group, group.by = "variable")
#data33<-na.omit(dataA)
Mean<-aggregate(data33$value,by=list(Group=data33$Group,variable=data33$variable),mean, na.rm=TRUE)
SD<- aggregate(data33$value,by=list(Group=data33$Group,variable=data33$variable),sd, na.rm=TRUE)

P <- as.data.frame(P)
P$P_mean <- NA
P$P_sd <-NA
P$PS_mean <- NA
P$PS_sd <-NA

P$P_mean <- Mean$x[which(Mean$Group=="P")]
P$P_sd <- SD$x[which(SD$Group=="P")]
P$PS_mean <- Mean$x[which(Mean$Group=="PS")]
P$PS_sd <- SD$x[which(SD$Group=="PS")]
P
write.csv(P,file=paste(i,"_wilcox_test.csv",sep = ""))  #use paste() function to establish a common usd file name




##-----------------------------##--------------------------------------##

data4 <- read.table("table_3_BIS_data.txt",header = T)
data5 <- melt(data4, id=c("Group","ID","sample"))
max(na.omit(data5$value))
min(na.omit(data5$value))
#code for BIS test
#1. P value = ***
windowsFonts(myFont = windowsFont("Calibri"))
ggplot(data=data5, aes(x=variable, y=value))+
  stat_summary(aes(group=Group, color=Group), fun.y=mean, geom='point', size=3.0)+
  stat_summary(aes(group=Group, color=Group),fun.y=mean,geom="line",cex=0.9)+
  stat_summary(aes(group=Group, color=Group),fun.data = mean_se,geom="errorbar",width=0.3,cex=0.9)+
  theme_classic()+
  stat_compare_means(aes(group=as.factor(Group)),method="wilcox.test",label = "p.signif", hide.ns = TRUE, label.y=c(100,rep(62,14)))+
  scale_color_manual(values = pal_jama()(7)[c(1,7)])+
  scale_fill_manual(values = pal_jama()(7)[c(1,7)])+
  theme(legend.position = c(0.8,0.9), 
        legend.box.background = element_rect(color="black", size=1), 
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(color = "black",  size=14, family="myFont"),
        axis.text = element_text(color = "black",  size=13, family="myFont"))+
  scale_x_discrete("Time points")+
  scale_y_continuous("BIS value",limits = c(20,150))


#1.1. P value = ***, color changed
windowsFonts(myFont = windowsFont("Calibri"))
ggplot(data=data5, aes(x=variable, y=value))+
  stat_summary(aes(group=Group, color=Group), fun.y=mean, geom='point', size=3.0)+
  stat_summary(aes(group=Group, color=Group),fun.y=mean,geom="line",cex=0.9)+
  stat_summary(aes(group=Group, color=Group),fun.data = mean_se,geom="errorbar",width=0.3,cex=0.9)+
  theme_classic()+
  stat_compare_means(aes(group=as.factor(Group)),method="wilcox.test",label = "p.signif", hide.ns = TRUE, label.y=c(100,rep(62,14)))+
  scale_color_manual(values = pal_jama()(7)[c(2,3)])+
  scale_fill_manual(values = pal_jama()(7)[c(2,3)])+
  theme(legend.position = c(0.8,0.9), 
        legend.box.background = element_rect(color="black", size=1), 
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(color = "black",  size=14, family="myFont"),
        axis.text = element_text(color = "black",  size=13, family="myFont"))+
  scale_x_discrete("Time points")+
  scale_y_continuous("BIS value",limits = c(20,150))

#2 P value = 0.005
windowsFonts(myFont = windowsFont("Calibri"))
ggplot(data=data5, aes(x=variable, y=value))+
  stat_summary(aes(group=Group, color=Group), fun.y=mean, geom='point', size=3.0)+
  stat_summary(aes(group=Group, color=Group),fun.y=mean,geom="line",cex=0.9)+
  stat_summary(aes(group=Group, color=Group),fun.data = mean_se,geom="errorbar",width=0.3,cex=0.9)+
  theme_classic()+
  stat_compare_means(aes(group=as.factor(Group),label = ..p.format..),method="wilcox.test", hide.ns = TRUE, label.y=c(100,rep(62,14)))+
  scale_color_manual(values = pal_jama()(7)[c(1,7)])+
  scale_fill_manual(values = pal_jama()(7)[c(1,7)])+
  theme(legend.position = c(0.8,0.9), 
        legend.box.background = element_rect(color="black", size=1), 
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(color = "black",  size=14, family="myFont"),
        axis.text = element_text(color = "black",  size=13, family="myFont"))+
  scale_x_discrete("Time points")+
  scale_y_continuous("BIS value",limits = c(20,150))


###calculate the P value and mean
library(ggpubr)
dataA = data5
i="BIS"
P <- compare_means(data=dataA, value~Group, group.by = "variable", method = 'wilcox.test')
#compare_means(data=data3, value~Group, group.by = "variable")
data33<-na.omit(dataA)
Mean<-aggregate(data33$value,by=list(Group=data33$Group,variable=data33$variable),mean)
SD<- aggregate(data33$value,by=list(Group=data33$Group,variable=data33$variable),sd)

P <- as.data.frame(P)
P$P_mean <- NA
P$P_sd <-NA
P$PS_mean <- NA
P$PS_sd <-NA

P$P_mean <- Mean$x[which(Mean$Group=="P")]
P$P_sd <- SD$x[which(SD$Group=="P")]
P$PS_mean <- Mean$x[which(Mean$Group=="PS")]
P$PS_sd <- SD$x[which(SD$Group=="PS")]
P
write.csv(P,file=paste(i,"_wilcox_test.csv",sep = ""))  #use paste() function to establish a common usd file name


##-----------------------------##--------------------------------------##

data6 <- read.table("table_4_SBP_data.txt",header = T)
data7 <- melt(data6, id=c("Group","ID","sample"))
max(na.omit(data7$value))
min(na.omit(data7$value))
#code for SBP test
#1. P value = ***
windowsFonts(myFont = windowsFont("Calibri"))
ggplot(data=data7, aes(x=variable, y=value))+
  stat_summary(aes(group=Group, color=Group), fun.y=mean, geom='point', size=3.0)+
  stat_summary(aes(group=Group, color=Group),fun.y=mean,geom="line",cex=0.9)+
  stat_summary(aes(group=Group, color=Group),fun.data = mean_se,geom="errorbar",width=0.3,cex=0.9)+
  theme_classic()+
  stat_compare_means(aes(group=as.factor(Group)),method="wilcox.test",label = "p.signif", hide.ns = T, label.y=c(100,rep(130,14)))+
  scale_color_manual(values = pal_jama()(7)[c(1,7)])+
  scale_fill_manual(values = pal_jama()(7)[c(1,7)])+
  theme(legend.position = c(0.8,0.2), 
        legend.box.background = element_rect(color="black", size=1), 
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(color = "black",  size=14, family="myFont"),
        axis.text = element_text(color = "black",  size=13, family="myFont"))+
  scale_x_discrete("Time points")+
  scale_y_continuous("SBP value",limits = c(10,210))

#1.1. P value = *** color changed 
windowsFonts(myFont = windowsFont("Calibri"))
ggplot(data=data7, aes(x=variable, y=value))+
  stat_summary(aes(group=Group, color=Group), fun.y=mean, geom='point', size=3.0)+
  stat_summary(aes(group=Group, color=Group),fun.y=mean,geom="line",cex=0.9)+
  stat_summary(aes(group=Group, color=Group),fun.data = mean_se,geom="errorbar",width=0.3,cex=0.9)+
  theme_classic()+
  stat_compare_means(aes(group=as.factor(Group)),method="wilcox.test",label = "p.signif", hide.ns = T, label.y=c(100,rep(130,14)))+
  scale_color_manual(values = pal_jama()(7)[c(2,3)])+
  scale_fill_manual(values = pal_jama()(7)[c(2,3)])+
  theme(legend.position = c(0.8,0.2), 
        legend.box.background = element_rect(color="black", size=1), 
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(color = "black",  size=14, family="myFont"),
        axis.text = element_text(color = "black",  size=13, family="myFont"))+
  scale_x_discrete("Time points")+
  scale_y_continuous("SBP value",limits = c(10,210))

#2 P value = 0.005
windowsFonts(myFont = windowsFont("Calibri"))
ggplot(data=data7, aes(x=variable, y=value))+
  stat_summary(aes(group=Group, color=Group), fun.y=mean, geom='point', size=3.0)+
  stat_summary(aes(group=Group, color=Group),fun.y=mean,geom="line",cex=0.9)+
  stat_summary(aes(group=Group, color=Group),fun.data = mean_se,geom="errorbar",width=0.3,cex=0.9)+
  theme_classic()+
  stat_compare_means(aes(group=as.factor(Group),label = ..p.format..),method="wilcox.test", hide.ns = TRUE, label.y=c(100,rep(130,14)))+
  scale_color_manual(values = pal_jama()(7)[c(1,7)])+
  scale_fill_manual(values = pal_jama()(7)[c(1,7)])+
  theme(legend.position = c(0.8,0.2), 
        legend.box.background = element_rect(color="black", size=1), 
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(color = "black",  size=14, family="myFont"),
        axis.text = element_text(color = "black",  size=13, family="myFont"))+
  scale_x_discrete("Time points")+
  scale_y_continuous("SBP value",limits = c(10,210))


###calculate the P value and mean
library(ggpubr)
dataA = data7
i="SBP"
P <- compare_means(data=dataA, value~Group, group.by = "variable", method = 'wilcox.test')
#compare_means(data=data3, value~Group, group.by = "variable")
data33<-na.omit(dataA)
Mean<-aggregate(data33$value,by=list(Group=data33$Group,variable=data33$variable),mean)
SD<- aggregate(data33$value,by=list(Group=data33$Group,variable=data33$variable),sd)

P <- as.data.frame(P)
P$P_mean <- NA
P$P_sd <-NA
P$PS_mean <- NA
P$PS_sd <-NA

P$P_mean <- Mean$x[which(Mean$Group=="P")]
P$P_sd <- SD$x[which(SD$Group=="P")]
P$PS_mean <- Mean$x[which(Mean$Group=="PS")]
P$PS_sd <- SD$x[which(SD$Group=="PS")]
P
write.csv(P,file=paste(i,"_wilcox_test.csv",sep = ""))  #use paste() function to establish a common usd file name


##-----------------------------##--------------------------------------##

data8 <- read.table("table_5_DBP_data.txt",header = T)
data9 <- melt(data8, id=c("Group","ID","sample"))
max(na.omit(data9$value))
min(na.omit(data9$value))
#code for SBP test
#1. P value = ***
windowsFonts(myFont = windowsFont("Calibri"))
ggplot(data=data9, aes(x=variable, y=value))+
  stat_summary(aes(group=Group, color=Group), fun.y=mean, geom='point', size=3.0)+
  stat_summary(aes(group=Group, color=Group),fun.y=mean,geom="line",cex=0.9)+
  stat_summary(aes(group=Group, color=Group),fun.data = mean_se,geom="errorbar",width=0.3,cex=0.9)+
  theme_classic()+
  stat_compare_means(aes(group=as.factor(Group)),method="wilcox.test",label = "p.signif", hide.ns = T, label.y=c(100,rep(62,14)))+
  scale_color_manual(values = pal_jama()(7)[c(1,7)])+
  scale_fill_manual(values = pal_jama()(7)[c(1,7)])+
  theme(legend.position = c(0.8,0.9), 
        legend.box.background = element_rect(color="black", size=1), 
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(color = "black",  size=14, family="myFont"),
        axis.text = element_text(color = "black",  size=13, family="myFont"))+
  scale_x_discrete("Time points")+
  scale_y_continuous("DBP value",limits = c(3,155))


#code for SBP test
#1.1. P value = *** color changed
windowsFonts(myFont = windowsFont("Calibri"))
ggplot(data=data9, aes(x=variable, y=value))+
  stat_summary(aes(group=Group, color=Group), fun.y=mean, geom='point', size=3.0)+
  stat_summary(aes(group=Group, color=Group),fun.y=mean,geom="line",cex=0.9)+
  stat_summary(aes(group=Group, color=Group),fun.data = mean_se,geom="errorbar",width=0.3,cex=0.9)+
  theme_classic()+
  stat_compare_means(aes(group=as.factor(Group)),method="wilcox.test",label = "p.signif", hide.ns = T, label.y=c(100,rep(62,14)))+
  scale_color_manual(values = pal_jama()(7)[c(2,3)])+
  scale_fill_manual(values = pal_jama()(7)[c(2,3)])+
  theme(legend.position = c(0.8,0.9), 
        legend.box.background = element_rect(color="black", size=1), 
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(color = "black",  size=14, family="myFont"),
        axis.text = element_text(color = "black",  size=13, family="myFont"))+
  scale_x_discrete("Time points")+
  scale_y_continuous("DBP value",limits = c(3,155))


#2 P value = 0.005
windowsFonts(myFont = windowsFont("Calibri"))
ggplot(data=data9, aes(x=variable, y=value))+
  stat_summary(aes(group=Group, color=Group), fun.y=mean, geom='point', size=3.0)+
  stat_summary(aes(group=Group, color=Group),fun.y=mean,geom="line",cex=0.9)+
  stat_summary(aes(group=Group, color=Group),fun.data = mean_se,geom="errorbar",width=0.3,cex=0.9)+
  theme_classic()+
  stat_compare_means(aes(group=as.factor(Group),label = ..p.format..),method="wilcox.test", hide.ns = TRUE, label.y=c(100,rep(62,14)))+
  scale_color_manual(values = pal_jama()(7)[c(1,7)])+
  scale_fill_manual(values = pal_jama()(7)[c(1,7)])+
  theme(legend.position = c(0.8,0.9), 
        legend.box.background = element_rect(color="black", size=1), 
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(color = "black",  size=14, family="myFont"),
        axis.text = element_text(color = "black",  size=13, family="myFont"))+
  scale_x_discrete("Time points")+
  scale_y_continuous("DBP value",limits = c(3,155))



###calculate the P value and mean
library(ggpubr)
dataA = data9
i="DBP"
P <- compare_means(data=dataA, value~Group, group.by = "variable", method = 'wilcox.test')
#compare_means(data=data3, value~Group, group.by = "variable")
data33<-na.omit(dataA)
Mean<-aggregate(data33$value,by=list(Group=data33$Group,variable=data33$variable),mean)
SD<- aggregate(data33$value,by=list(Group=data33$Group,variable=data33$variable),sd)

P <- as.data.frame(P)
P$P_mean <- NA
P$P_sd <-NA
P$PS_mean <- NA
P$PS_sd <-NA

P$P_mean <- Mean$x[which(Mean$Group=="P")]
P$P_sd <- SD$x[which(SD$Group=="P")]
P$PS_mean <- Mean$x[which(Mean$Group=="PS")]
P$PS_sd <- SD$x[which(SD$Group=="PS")]
P
write.csv(P,file=paste(i,"_wilcox_test.csv",sep = ""))  #use paste() function to establish a common usd file name






##-----------------------------##--------------------------------------##

data10 <- read.table("table_6_MAP_data.txt",header = T)
data11 <- melt(data10, id=c("Group","ID","sample"))
max(na.omit(data11$value))
min(na.omit(data11$value))
#code for SBP test
#1. P value = ***
windowsFonts(myFont = windowsFont("Calibri"))
ggplot(data=data11, aes(x=variable, y=value))+
  stat_summary(aes(group=Group, color=Group), fun.y=mean, geom='point', size=3.0)+
  stat_summary(aes(group=Group, color=Group),fun.y=mean,geom="line",cex=0.9)+
  stat_summary(aes(group=Group, color=Group),fun.data = mean_se,geom="errorbar",width=0.3,cex=0.9)+
  theme_classic()+
  stat_compare_means(aes(group=as.factor(Group)),method="wilcox.test",label = "p.signif", hide.ns = T, label.y=c(100,rep(90,14)))+
  scale_color_manual(values = pal_jama()(7)[c(1,7)])+
  scale_fill_manual(values = pal_jama()(7)[c(1,7)])+
  theme(legend.position = c(0.8,0.9), 
        legend.box.background = element_rect(color="black", size=1), 
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(color = "black",  size=14, family="myFont"),
        axis.text = element_text(color = "black",  size=13, family="myFont"))+
  scale_x_discrete("Time points")+
  scale_y_continuous("MAP value",limits = c(1,142))

#1.1. P value = *** color changed
windowsFonts(myFont = windowsFont("Calibri"))
ggplot(data=data11, aes(x=variable, y=value))+
  stat_summary(aes(group=Group, color=Group), fun.y=mean, geom='point', size=3.0)+
  stat_summary(aes(group=Group, color=Group),fun.y=mean,geom="line",cex=0.9)+
  stat_summary(aes(group=Group, color=Group),fun.data = mean_se,geom="errorbar",width=0.3,cex=0.9)+
  theme_classic()+
  stat_compare_means(aes(group=as.factor(Group)),method="wilcox.test",label = "p.signif", hide.ns = T, label.y=c(100,rep(90,14)))+
  scale_color_manual(values = pal_jama()(7)[c(2,3)])+
  scale_fill_manual(values = pal_jama()(7)[c(2,3)])+
  theme(legend.position = c(0.8,0.9), 
        legend.box.background = element_rect(color="black", size=1), 
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(color = "black",  size=14, family="myFont"),
        axis.text = element_text(color = "black",  size=13, family="myFont"))+
  scale_x_discrete("Time points")+
  scale_y_continuous("MAP value",limits = c(1,142))

#2 P value = 0.005
windowsFonts(myFont = windowsFont("Calibri"))
ggplot(data=data11, aes(x=variable, y=value))+
  stat_summary(aes(group=Group, color=Group), fun.y=mean, geom='point', size=3.0)+
  stat_summary(aes(group=Group, color=Group),fun.y=mean,geom="line",cex=0.9)+
  stat_summary(aes(group=Group, color=Group),fun.data = mean_se,geom="errorbar",width=0.3,cex=0.9)+
  theme_classic()+
  stat_compare_means(aes(group=as.factor(Group),label = ..p.format..),method="wilcox.test", hide.ns = TRUE, label.y=c(100,rep(62,14)))+
  scale_color_manual(values = pal_jama()(7)[c(1,7)])+
  scale_fill_manual(values = pal_jama()(7)[c(1,7)])+
  theme(legend.position = c(0.8,0.9), 
        legend.box.background = element_rect(color="black", size=1), 
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(color = "black",  size=14, family="myFont"),
        axis.text = element_text(color = "black",  size=13, family="myFont"))+
  scale_x_discrete("Time points")+
  scale_y_continuous("MAP value",limits = c(1,142))


###calculate the P value and mean
library(ggpubr)
dataA = data11
i="MAP"
P <- compare_means(data=dataA, value~Group, group.by = "variable", method = 'wilcox.test')
#compare_means(data=data3, value~Group, group.by = "variable")
data33<-na.omit(dataA)
Mean<-aggregate(data33$value,by=list(Group=data33$Group,variable=data33$variable),mean)
SD<- aggregate(data33$value,by=list(Group=data33$Group,variable=data33$variable),sd)

P <- as.data.frame(P)
P$P_mean <- NA
P$P_sd <-NA
P$PS_mean <- NA
P$PS_sd <-NA

P$P_mean <- Mean$x[which(Mean$Group=="P")]
P$P_sd <- SD$x[which(SD$Group=="P")]
P$PS_mean <- Mean$x[which(Mean$Group=="PS")]
P$PS_sd <- SD$x[which(SD$Group=="PS")]
P
write.csv(P,file=paste(i,"_wilcox_test.csv",sep = ""))  #use paste() function to establish a common usd file name


##-----------------------------##--------------------------------------##

data12 <- read.table("table_7_HR_data.txt",header = T)
data13 <- melt(data12, id=c("Group","ID","sample"))
max(na.omit(data13$value))
min(na.omit(data13$value))
#code for SBP test
#1. P value = ***
windowsFonts(myFont = windowsFont("Calibri"))
ggplot(data=data13, aes(x=variable, y=value))+
  stat_summary(aes(group=Group, color=Group), fun.y=mean, geom='point', size=3.0)+
  stat_summary(aes(group=Group, color=Group),fun.y=mean,geom="line",cex=0.9)+
  stat_summary(aes(group=Group, color=Group),fun.data = mean_se,geom="errorbar",width=0.3,cex=0.9)+
  theme_classic()+
  stat_compare_means(aes(group=as.factor(Group)),method="wilcox.test",label = "p.signif", hide.ns = T, label.y=c(100,rep(62,12),63,62))+
  scale_color_manual(values = pal_jama()(7)[c(1,7)])+
  scale_fill_manual(values = pal_jama()(7)[c(1,7)])+
  theme(legend.position = c(0.8,0.9), 
        legend.box.background = element_rect(color="black", size=1), 
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(color = "black",  size=14, family="myFont"),
        axis.text = element_text(color = "black",  size=13, family="myFont"))+
  scale_x_discrete("Time points")+
  scale_y_continuous("HR value",limits = c(0,115))

#1. P value = *** color changed
windowsFonts(myFont = windowsFont("Calibri"))
ggplot(data=data13, aes(x=variable, y=value))+
  stat_summary(aes(group=Group, color=Group), fun.y=mean, geom='point', size=3.0)+
  stat_summary(aes(group=Group, color=Group),fun.y=mean,geom="line",cex=0.9)+
  stat_summary(aes(group=Group, color=Group),fun.data = mean_se,geom="errorbar",width=0.3,cex=0.9)+
  theme_classic()+
  stat_compare_means(aes(group=as.factor(Group)),method="wilcox.test",label = "p.signif", hide.ns = T, label.y=c(100,rep(62,12),63,62))+
  scale_color_manual(values = pal_jama()(7)[c(2,3)])+
  scale_fill_manual(values = pal_jama()(7)[c(2,3)])+
  theme(legend.position = c(0.8,0.9), 
        legend.box.background = element_rect(color="black", size=1), 
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(color = "black",  size=14, family="myFont"),
        axis.text = element_text(color = "black",  size=13, family="myFont"))+
  scale_x_discrete("Time points")+
  scale_y_continuous("HR value",limits = c(0,115))


#2 P value = 0.005
windowsFonts(myFont = windowsFont("Calibri"))
ggplot(data=data13, aes(x=variable, y=value))+
  stat_summary(aes(group=Group, color=Group), fun.y=mean, geom='point', size=3.0)+
  stat_summary(aes(group=Group, color=Group),fun.y=mean,geom="line",cex=0.9)+
  stat_summary(aes(group=Group, color=Group),fun.data = mean_se,geom="errorbar",width=0.3,cex=0.9)+
  theme_classic()+
  stat_compare_means(aes(group=as.factor(Group),label = ..p.format..),method="wilcox.test", hide.ns = TRUE, label.y=c(100,rep(62,14)))+
  scale_color_manual(values = pal_jama()(7)[c(1,7)])+
  scale_fill_manual(values = pal_jama()(7)[c(1,7)])+
  theme(legend.position = c(0.8,0.9), 
        legend.box.background = element_rect(color="black", size=1), 
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(color = "black",  size=14, family="myFont"),
        axis.text = element_text(color = "black",  size=13, family="myFont"))+
  scale_x_discrete("Time points")+
  scale_y_continuous("HR value",limits = c(0,115))



###calculate the P value and mean
library(ggpubr)
dataA = data13
i="HR"
P <- compare_means(data=dataA, value~Group, group.by = "variable", method = 'wilcox.test')
#compare_means(data=data3, value~Group, group.by = "variable")
data33<-na.omit(dataA)
Mean<-aggregate(data33$value,by=list(Group=data33$Group,variable=data33$variable),mean)
SD<- aggregate(data33$value,by=list(Group=data33$Group,variable=data33$variable),sd)

P <- as.data.frame(P)
P$P_mean <- NA
P$P_sd <-NA
P$PS_mean <- NA
P$PS_sd <-NA

P$P_mean <- Mean$x[which(Mean$Group=="P")]
P$P_sd <- SD$x[which(SD$Group=="P")]
P$PS_mean <- Mean$x[which(Mean$Group=="PS")]
P$PS_sd <- SD$x[which(SD$Group=="PS")]
P
write.csv(P,file=paste(i,"_wilcox_test.csv",sep = ""))  #use paste() function to establish a common usd file name


##-----------------------------##--------------------------------------##

data14 <- read.table("table_8_EMG_data.txt",header = T)
data15 <- melt(data14, id=c("Group","ID","sample"))

#code for SBP test
#1. P value = ***
windowsFonts(myFont = windowsFont("Calibri"))
ggplot(data=data15, aes(x=variable, y=value))+
  stat_summary(aes(group=Group, color=Group), fun=mean, geom='point', size=3.0)+
  stat_summary(aes(group=Group, color=Group),fun=mean,geom="line",cex=0.9)+
  stat_summary(aes(group=Group, color=Group),fun.data = mean_se,geom="errorbar",width=0.3,cex=0.9)+
  theme_classic()+
  stat_compare_means(aes(group=as.factor(Group)),method="wilcox.test",label = "p.signif", hide.ns = T, label.y=c(100,rep(62,12),63,62))+
  scale_color_manual(values = pal_jama()(7)[c(1,7)])+
  scale_fill_manual(values = pal_jama()(7)[c(1,7)])+
  theme(legend.position = c(0.8,0.9), 
        legend.box.background = element_rect(color="black", size=1), 
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(color = "black",  size=14, family="myFont"),
        axis.text = element_text(color = "black",  size=13, family="myFont"))+
  scale_x_discrete("Time points")+
  scale_y_continuous("EMG value",limits = c(40,100))

#2 P value = 0.005
windowsFonts(myFont = windowsFont("Calibri"))
ggplot(data=data15, aes(x=variable, y=value))+
  stat_summary(aes(group=Group, color=Group), fun.y=mean, geom='point', size=3.0)+
  stat_summary(aes(group=Group, color=Group),fun.y=mean,geom="line",cex=0.9)+
  stat_summary(aes(group=Group, color=Group),fun.data = mean_se,geom="errorbar",width=0.3,cex=0.9)+
  theme_classic()+
  stat_compare_means(aes(group=as.factor(Group),label = ..p.format..),method="wilcox.test", hide.ns = TRUE, label.y=c(100,rep(62,14)))+
  scale_color_manual(values = pal_jama()(7)[c(1,7)])+
  scale_fill_manual(values = pal_jama()(7)[c(1,7)])+
  theme(legend.position = c(0.8,0.9), 
        legend.box.background = element_rect(color="black", size=1), 
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(color = "black",  size=14, family="myFont"),
        axis.text = element_text(color = "black",  size=13, family="myFont"))+
  scale_x_discrete("Time points")+
  scale_y_continuous("EMG value",limits = c(40,100))















