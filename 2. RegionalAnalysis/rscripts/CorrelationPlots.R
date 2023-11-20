#
library(data.table)
library(ggplot2)
library(ggpmisc)
library(ggpubr)
theme_set(theme_minimal())
# Read in the files
bird_data<- fread("../data/Bird_Clm_17Jul.csv")
#
title_size= 8
#
mod<- lm(TMPprd_Bot~TMPavg, data= bird_data)
focal_model<- mod
eff<- coef(focal_model)[2]
eff_ci<- abs(confint(focal_model, level= 0.95)[2]-confint(focal_model, level= 0.95)[4])/2
pvalue<- cor.test(bird_data$TMPavg, (bird_data$TMPprd_Bot))$p.value
r2<- summary(focal_model)$r.squared
ifelse(pvalue> 0.001,
       text<- paste("Beta=", round(eff,3),"±", round(eff_ci,3),", p=", signif(pvalue,3),", R2=", round(r2,3)),
       text<- paste("Beta=", round(eff,3),"±", round(eff_ci,3),", p< 0.001",", R2=", round(r2,3)))
#
a<- ggplot(bird_data, aes(x=TMPavg, y=TMPprd_Bot))+
  geom_point(alpha=0.5, stroke=0)+
  geom_smooth(method="lm",color="black")+
  coord_cartesian(ylim=c(0.4, 0.9))+
  xlab("Average temperature")+ ylab("Predictability (fixed)")
a<- annotate_figure(a, top= text_grob(text, face="italic", size= title_size))
#
mod<- lm(TMPprd_Dym~TMPavg, data= bird_data)
focal_model<- mod
eff<- coef(focal_model)[2]
eff_ci<- abs(confint(focal_model, level= 0.95)[2]-confint(focal_model, level= 0.95)[4])/2
pvalue<- cor.test(bird_data$TMPavg, (bird_data$TMPprd_Dym))$p.value
r2<- summary(focal_model)$r.squared
ifelse(pvalue> 0.001,
       text<- paste("Beta=", round(eff,3),"±", round(eff_ci,3),", p=", signif(pvalue,3),", R2=", round(r2,3)),
       text<- paste("Beta=", round(eff,3),"±", round(eff_ci,3),", p< 0.001",", R2=", round(r2,3)))
#
b<- ggplot(bird_data, aes(x=TMPavg, y=TMPprd_Dym))+
  geom_point(alpha=0.5, stroke=0)+
  geom_smooth(method="lm",color="black")+
  coord_cartesian(ylim=c(0.2, 0.7))+
  xlab("Average temperature")+ ylab("Predictability (dynamic)")
b<- annotate_figure(b, top= text_grob(text, face="italic", size= title_size))
#
mod<- lm(TMPprd_Ent~TMPavg, data= bird_data)
focal_model<- mod
eff<- coef(focal_model)[2]
eff_ci<- abs(confint(focal_model, level= 0.95)[2]-confint(focal_model, level= 0.95)[4])/2
pvalue<- cor.test(bird_data$TMPavg, (bird_data$TMPprd_Ent))$p.value
r2<- summary(focal_model)$r.squared
ifelse(pvalue> 0.001,
       text<- paste("Beta=", round(eff,3),"±", round(eff_ci,3),", p=", signif(pvalue,3),", R2=", round(r2,3)),
       text<- paste("Beta=", round(eff,3),"±", round(eff_ci,3),", p< 0.001",", R2=", round(r2,3)))
#
c<- ggplot(bird_data, aes(x=TMPavg, y=TMPprd_Ent))+
  geom_point(alpha=0.5, stroke=0)+
  geom_smooth(method="lm",color="black")+
  coord_cartesian(ylim=c(0.4, 0.9))+
  xlab("Average temperature")+ ylab("Predictability (entropy)")
c<- annotate_figure(c, top= text_grob(text, face="italic", size= title_size))
#
mod<- lm(TMPprd_Bot~sqrt(TMPsd), data= bird_data)
focal_model<- mod
eff<- coef(focal_model)[2]
eff_ci<- abs(confint(focal_model, level= 0.95)[2]-confint(focal_model, level= 0.95)[4])/2
pvalue<- cor.test(sqrt(bird_data$TMPsd), bird_data$TMPprd_Bot)$p.value
r2<- summary(focal_model)$r.squared
ifelse(pvalue> 0.001,
       text<- paste("Beta=", round(eff,3),"±", round(eff_ci,3),", p=", signif(pvalue,3),", R2=", round(r2,3)),
       text<- paste("Beta=", round(eff,3),"±", round(eff_ci,3),", p< 0.001",", R2=", round(r2,3)))
#
d<- ggplot(bird_data, aes(x=sqrt(TMPsd), y=TMPprd_Bot))+
  geom_point(alpha=0.5, stroke=0)+
  geom_smooth(method="lm",color="black")+
  coord_cartesian(ylim=c(0.4, 0.9))+
  xlab("SD of temperature")+ ylab("Predictability (fixed)")
d<- annotate_figure(d, top= text_grob(text, face="italic", size= title_size))
#
mod<- lm(TMPprd_Dym~sqrt(TMPsd), data= bird_data)
focal_model<- mod
eff<- coef(focal_model)[2]
eff_ci<- abs(confint(focal_model, level= 0.95)[2]-confint(focal_model, level= 0.95)[4])/2
pvalue<- cor.test(sqrt(bird_data$TMPsd), bird_data$TMPprd_Dym)$p.value
r2<- summary(focal_model)$r.squared
ifelse(pvalue> 0.001,
       text<- paste("Beta=", round(eff,3),"±", round(eff_ci,3),", p=", signif(pvalue,3),", R2=", round(r2,3)),
       text<- paste("Beta=", round(eff,3),"±", round(eff_ci,3),", p< 0.001",", R2=", round(r2,3)))
#
e<- ggplot(bird_data, aes(x=sqrt(TMPsd), y=TMPprd_Dym))+
  geom_point(alpha=0.5, stroke=0)+
  geom_smooth(method="lm",color="black")+
  coord_cartesian(ylim=c(0.2, 0.7))+
  xlab("SD of temperature")+ ylab("Predictability (dynamic)")
e<- annotate_figure(e, top= text_grob(text, face="italic", size= title_size))
#
mod<- lm(TMPprd_Ent~sqrt(TMPsd), data= bird_data)
focal_model<- mod
eff<- coef(focal_model)[2]
eff_ci<- abs(confint(focal_model, level= 0.95)[2]-confint(focal_model, level= 0.95)[4])/2
pvalue<- cor.test(sqrt(bird_data$TMPsd), bird_data$TMPprd_Ent)$p.value
r2<- summary(focal_model)$r.squared
ifelse(pvalue> 0.001,
       text<- paste("Beta=", round(eff,3),"±", round(eff_ci,3),", p=", signif(pvalue,3),", R2=", round(r2,3)),
       text<- paste("Beta=", round(eff,3),"±", round(eff_ci,3),", p< 0.001",", R2=", round(r2,3)))
#
f<- ggplot(bird_data, aes(x=sqrt(TMPsd), y=TMPprd_Ent))+
  geom_point(alpha=0.5, stroke=0)+
  geom_smooth(method="lm",color="black")+
  coord_cartesian(ylim=c(0.4, 0.9))+
  xlab("SD of temperature")+ ylab("Predictability (entropy)")
f<- annotate_figure(f, top= text_grob(text, face="italic", size= title_size))
#
mod<- lm(PREprd_Bot~sqrt(PREavg), data= bird_data)
focal_model<- mod
eff<- coef(focal_model)[2]
eff_ci<- abs(confint(focal_model, level= 0.95)[2]-confint(focal_model, level= 0.95)[4])/2
pvalue<- cor.test(bird_data$PREprd_Bot, sqrt(bird_data$PREavg))$p.value
r2<- summary(focal_model)$r.squared
ifelse(pvalue> 0.001,
       text<- paste("Beta=", round(eff,3),"±", round(eff_ci,3),", p=", signif(pvalue,3),", R2=", round(r2,3)),
       text<- paste("Beta=", round(eff,3),"±", round(eff_ci,3),", p< 0.001",", R2=", round(r2,3)))
#
g<- ggplot(bird_data, aes(x=sqrt(PREavg), y=PREprd_Bot))+
  geom_point(alpha=0.5, stroke=0)+
  geom_smooth(method="lm",color="black")+
  coord_cartesian(ylim=c(0.2, 1.0))+
  xlab("Average precipitation")+ ylab("Predictability (fixed)")
g<- annotate_figure(g, top= text_grob(text, face="italic", size= title_size))
#
mod<- lm(PREprd_Dym~sqrt(PREavg), data= bird_data)
focal_model<- mod
eff<- coef(focal_model)[2]
eff_ci<- abs(confint(focal_model, level= 0.95)[2]-confint(focal_model, level= 0.95)[4])/2
pvalue<- cor.test(bird_data$PREprd_Dym, sqrt(bird_data$PREavg))$p.value
r2<- summary(focal_model)$r.squared
ifelse(pvalue> 0.001,
       text<- paste("Beta=", round(eff,3),"±", round(eff_ci,3),", p=", signif(pvalue,3),", R2=", round(r2,3)),
       text<- paste("Beta=", round(eff,3),"±", round(eff_ci,3),", p< 0.001",", R2=", round(r2,3)))
#
h<- ggplot(bird_data, aes(x=sqrt(PREavg), y=PREprd_Dym))+
  geom_point(alpha=0.5, stroke=0)+
  geom_smooth(method="lm",color="black")+
  coord_cartesian(ylim=c(0.2, 1.0))+
  xlab("Average precipitation")+ ylab("Predictability (dynamic)")
h<- annotate_figure(h, top= text_grob(text, face="italic", size= title_size))
#
mod<- lm(PREprd_Ent~sqrt(PREavg), data= bird_data)
focal_model<- mod
eff<- coef(focal_model)[2]
eff_ci<- abs(confint(focal_model, level= 0.95)[2]-confint(focal_model, level= 0.95)[4])/2
pvalue<- cor.test(bird_data$PREprd_Ent, sqrt(bird_data$PREavg))$p.value
r2<- summary(focal_model)$r.squared
ifelse(pvalue> 0.001,
       text<- paste("Beta=", round(eff,3),"±", round(eff_ci,3),", p=", signif(pvalue,3),", R2=", round(r2,3)),
       text<- paste("Beta=", round(eff,3),"±", round(eff_ci,3),", p< 0.001",", R2=", round(r2,3)))
#
i<- ggplot(bird_data, aes(x=sqrt(PREavg), y=PREprd_Ent))+
  geom_point(alpha=0.5, stroke=0)+
  geom_smooth(method="lm",color="black")+
  coord_cartesian(ylim=c(0.0, 0.8))+
  xlab("Average precipitation")+ ylab("Predictability (entropy)")
i<- annotate_figure(i, top= text_grob(text, face="italic", size= title_size))
#
mod<- lm(PREprd_Bot~log(PREsd), data= bird_data)
focal_model<- mod
eff<- coef(focal_model)[2]
eff_ci<- abs(confint(focal_model, level= 0.95)[2]-confint(focal_model, level= 0.95)[4])/2
pvalue<- cor.test(bird_data$PREprd_Bot, log(bird_data$PREsd))$p.value
r2<- summary(focal_model)$r.squared
ifelse(pvalue> 0.001,
       text<- paste("Beta=", round(eff,3),"±", round(eff_ci,3),", p=", signif(pvalue,3),", R2=", round(r2,3)),
       text<- paste("Beta=", round(eff,3),"±", round(eff_ci,3),", p< 0.001",", R2=", round(r2,3)))
#
j<- ggplot(bird_data, aes(x=log(PREsd), y=PREprd_Bot))+
  geom_point(alpha=0.5, stroke=0)+
  geom_smooth(method="lm",color="black")+
  coord_cartesian(xlim = c(1.5, 5.5), ylim=c(0.2, 1.0))+
  xlab("SD of precipitation")+ ylab("Predictability (fixed)")
j<- annotate_figure(j, top= text_grob(text, face="italic", size= title_size))
#
mod<- lm(PREprd_Dym~log(PREsd), data= bird_data)
focal_model<- mod
eff<- coef(focal_model)[2]
eff_ci<- abs(confint(focal_model, level= 0.95)[2]-confint(focal_model, level= 0.95)[4])/2
pvalue<- cor.test(bird_data$PREprd_Dym, log(bird_data$PREsd))$p.value
r2<- summary(focal_model)$r.squared
ifelse(pvalue> 0.001,
       text<- paste("Beta=", round(eff,3),"±", round(eff_ci,3),", p=", signif(pvalue,3),", R2=", round(r2,3)),
       text<- paste("Beta=", round(eff,3),"±", round(eff_ci,3),", p< 0.001",", R2=", round(r2,3)))
#
k<- ggplot(bird_data, aes(x=log(PREsd), y=PREprd_Dym))+
  geom_point(alpha=0.5, stroke=0)+
  geom_smooth(method="lm",color="black")+
  coord_cartesian(xlim = c(1.5, 5.5), ylim=c(0.2, 1.0))+
  xlab("SD of precipitation")+ ylab("Predictability (dynamic)")
k<- annotate_figure(k, top= text_grob(text, face="italic", size= title_size))
#
mod<- lm(PREprd_Ent~log(PREsd), data= bird_data)
focal_model<- mod
eff<- coef(focal_model)[2]
eff_ci<- abs(confint(focal_model, level= 0.95)[2]-confint(focal_model, level= 0.95)[4])/2
pvalue<- cor.test(bird_data$PREprd_Ent, log(bird_data$PREsd))$p.value
r2<- summary(focal_model)$r.squared
ifelse(pvalue> 0.001,
       text<- paste("Beta=", round(eff,3),"±", round(eff_ci,3),", p=", signif(pvalue,3),", R2=", round(r2,3)),
       text<- paste("Beta=", round(eff,3),"±", round(eff_ci,3),", p< 0.001",", R2=", round(r2,3)))
#
l<- ggplot(bird_data, aes(x=log(PREsd), y=PREprd_Ent))+
  geom_point(alpha=0.5, stroke=0)+
  geom_smooth(method="lm",color="black")+
  coord_cartesian(xlim = c(1.5, 5.5), ylim=c(0, 0.8))+
  xlab("SD of precipitation")+ ylab("Predictability (entropy)")
l<- annotate_figure(l, top= text_grob(text, face="italic", size= title_size))
#
fig<- ggarrange(a,d,g,j,b,e,h,k,c,f,i,l, nrow=3, ncol=4, 
          labels= c("(a)","(d)","(g)","(j)","(b)","(e)","(h)","(k)","(c)","(f)","(i)","(l)"))
##
ggsave("Fig4.pdf",fig, width = 11.5, height= 9, units="in")
