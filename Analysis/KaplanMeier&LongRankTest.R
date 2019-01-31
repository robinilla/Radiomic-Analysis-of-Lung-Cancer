#Script created by Sonia Illanas
#Creation data: 27/12/2018
#Last actualization: 31/12/2018

library(readxl)         #load  the package for reading excel files
library(tidyverse)      #load  the package for data management
library(ggplot2)        #load  the package for making graphs
library(survival)       #load  the package for making Kaplan-Meier Analysis
library(ggfortify)


setwd("C:/Docs/San")    #set your working directory
rm(list=ls())           #clean your global environment
datos<-read_xlsx("jsonCompletoNodule_modificadoSonia_.xlsx") #load your data
#for this analysis they have into account all data 
#time survival don't need to be changed for censored patients (alive people)


#Separe your data in categories:
#Do it only for those which have correlation wiht survival_time in Spearman Correlation Test, (Table 2 of the article)
#(We were interested in LCC, T2, I and N0, because they were which have significative correlation in our analysis)
LCC<-datos %>% filter(NSCLC=="LCC")
T2<-datos %>% filter(EstadioT=="T2")
I<-datos %>% filter(overall_stage=="I")
N0<-datos %>% filter(EstadioN=="N0")

#I wrote down in the loop several ways for doing it and save the results.
#I don't know why I had some problemes running the lines that become with "#", but a friend ran it and hadn't, so it works. Just try...and if you're lucky you can remove some lines from the code.
#You need to repeat the loop for each group you're interested. Changing it, it's that easy as select the loop, search in seletion "LCC" and change it with the name of the group you're interested

#For LCC
mediana<-vector()
Categoria2<-vector()
median_survival_m<-vector()
median_survival_M<-vector()
medianasLCC_mayor<-data.frame()
medianasLCC_menor<-data.frame()
#medianasLCC<-data.frame()
for (i in 1:25) {
  mediana[i]<-median(LCC[[i+13]])
  Categoria2[i]<-(colnames(LCC)[i+13])
  LCC1<-LCC %>% mutate(columna=factor(if_else(LCC[Categoria2[i]]<mediana[i], "Menor", "Mayor"))) %>% 
    group_by(columna) %>% summarise(median_survival_month=(median(survival_time)/30)) %>% mutate(Categoria=Categoria2[i])
  
  #medianasLCC[c(2*i-1,2*i), c(1:3)]<-data.frame(LCC1[c(1:2),c(1:3)])
  #medianasLCC_menor[i, c(1:3)]<-data.frame(LCC1[2, c(1:3)])
  #medianasLCC_mayor[i, c(1:3)]<-data.frame(LCC1[1, c(1:3)])
  
  LCC1_menor<-LCC1 %>% filter(columna=="Menor")
  median_survival_m[i]<-round(LCC1_menor$median_survival_month,2)
  medianasLCC_menor<-data.frame("Menor",median_survival_m, Categoria2)
  
  LCC1_mayor<-LCC1 %>% filter(columna=="Mayor")
  median_survival_M[i]<-round(LCC1_mayor$median_survival_month,2)
  medianasLCC_mayor<-data.frame("Mayor",median_survival_M, Categoria2)
}
remove(medianasLCC_todos)
names(medianasLCC_mayor)<-c("Corte", "Medianas_Supervivencia", "Categoria")
names(medianasLCC_menor)<-c("Corte", "Medianas_Supervivencia", "Categoria")
head(medianasLCC_mayor)
medianasLCC_todos<-rbind(medianasLCC_mayor,medianasLCC_menor)

#write.csv(medianasLCC_todos, "medianasN0_todos.csv") #remove the "#" if you want to save the results in a .csv file
#write.csv(mediana, "mediana_I_todos.csv")


########################################################
########################################################
#ANALYSIS LOG-RANK TEST
########################################################
########################################################
rm(list=ls()) #clean your global environment
#Lines 76 to 504 are for doing Kaplan-Meier Analysis and graphics
#Load your data as it's indicated you should prepare it in methodology and run the code

dataKM<-read_xlsx("Datos_KaplanEntero.xlsx", sheet="Tipo_Cancer") 
dataKM$deadstatus_event<-as.numeric(dataKM$deadstatus_event)
dataKM$`Angular Second Moment`<-as.factor(dataKM$`Angular Second Moment`)
dataKM$Correlation<-as.factor(dataKM$Correlation)
dataKM$`Inverse Difference Moment`<-as.factor(dataKM$`Inverse Difference Moment`)
dataKM$`Sum average`<-as.factor(dataKM$`Sum average`)
dataKM$`Sum variance`<-as.factor(dataKM$`Sum variance`)
dataKM$Entropy<-as.factor(dataKM$Entropy)
dataKM$`Difference Variance`<-as.factor(dataKM$`Difference Variance`)
dataKM$`Information Correlation1`<-as.factor(dataKM$`Information Correlation1`)
dataKM$`Information Correlation2`<-as.factor(dataKM$`Information Correlation2`)
dataKM$Dissimilarity<-as.factor(dataKM$Dissimilarity)                     
dataKM$`Cluster Shade`<-as.factor(dataKM$`Cluster Shade`)
dataKM$`Cluster Prominence`<-as.factor(dataKM$`Cluster Prominence`)
dataKM$Coarseness<-as.factor(dataKM$Coarseness)
dataKM$`Texture Strength`<-as.factor(dataKM$`Texture Strength`)
dataKM$`Small Zone Size Emphasis`<-as.factor(dataKM$`Small Zone Size Emphasis`)
dataKM$`Large Zone/High Gray Emphasis`<-as.factor(dataKM$`Large Zone/High Gray Emphasis`)
dataKM$`Gray-Level/Non-Uniformity`<-as.factor(dataKM$`Gray-Level/Non-Uniformity`)
dataKM$`Zone Size Non-Uniformity`<-as.factor(dataKM$`Zone Size Non-Uniformity`)
dataKM$`Zone Size Percentage`<-as.factor(dataKM$`Zone Size Percentage`)
dataKM$`Major Axis Length`<-as.factor(dataKM$`Major Axis Length`)
dataKM$Eccentricity<-as.factor(dataKM$Eccentricity)
dataKM$Volume<-as.factor(dataKM$Volume)
dataKM$`Fractal Diffraction`<-as.factor(dataKM$`Fractal Diffraction`)
dataKM$`Surface Area`<-as.factor(dataKM$`Surface Area`)
dataKM$Age<-as.factor(dataKM$Age)


KM_LCC<-dataKM %>% filter(NSCLC=="LCC")
KM_SCC<-dataKM %>% filter(NSCLC=="SCC")
KM_ADC<-dataKM %>% filter(NSCLC=="ADC")
KM_NOS<-dataKM %>% filter(NSCLC=="NOS")


#For doing long-rank test, we need all the groups:

#LCC
ValoresKaplan<-vector()
Categoria<-vector()
p.valor<-vector()
LCC_Kaplan<-data.frame()

for (i in 1:25) {
  surv<-survdiff(Surv(survival_time, deadstatus_event)~(KM_LCC[[i+13]]), data=KM_LCC)
  Categoria[i]<-noquote(colnames(KM_LCC)[i+13])
  ValoresKaplan[i]<-surv$chisq
  p.valor[i]<- 1 - pchisq(surv$chisq, length(surv$n) - 1)
  LCC_Kaplan<-data.frame(Categoria, ValoresKaplan, p.valor)
}
LCC_Kaplan$p.adj<-p.adjust(LCC_Kaplan$p.valor, method = "holm", n=length(LCC_Kaplan$p.valor))

#SCC
ValoresKaplan<-vector()
Categoria<-vector()
p.valor<-vector()
SCC_Kaplan<-data.frame()

for (i in 1:25) {
  surv<-survdiff(Surv(survival_time, deadstatus_event)~(KM_SCC[[i+13]]), data=KM_SCC)
  Categoria[i]<-noquote(colnames(KM_SCC)[i+13])
  ValoresKaplan[i]<-surv$chisq
  p.valor[i]<- 1 - pchisq(surv$chisq, length(surv$n) - 1)
  SCC_Kaplan<-data.frame(Categoria, ValoresKaplan, p.valor)
}
SCC_Kaplan$p.adj<-p.adjust(SCC_Kaplan$p.valor, method = "holm", n=length(SCC_Kaplan$p.valor))


#ADC
ValoresKaplan<-vector()
Categoria<-vector()
p.valor<-vector()
ADC_Kaplan<-data.frame()

for (i in 1:25) {
  surv<-survdiff(Surv(survival_time, deadstatus_event)~(KM_ADC[[i+13]]), data=KM_ADC)
  Categoria[i]<-noquote(colnames(KM_ADC)[i+13])
  ValoresKaplan[i]<-surv$chisq
  p.valor[i]<- 1 - pchisq(surv$chisq, length(surv$n) - 1)
  ADC_Kaplan<-data.frame(Categoria, ValoresKaplan, p.valor)
}
ADC_Kaplan$p.adj<-p.adjust(ADC_Kaplan$p.valor, method = "holm", n=length(ADC_Kaplan$p.valor))


#NOS
ValoresKaplan<-vector()
Categoria<-vector()
p.valor<-vector()
NOS_Kaplan<-data.frame()

for (i in 1:25) {
  surv<-survdiff(Surv(survival_time, deadstatus_event)~(KM_NOS[[i+13]]), data=KM_NOS)
  Categoria[i]<-noquote(colnames(KM_NOS)[i+13])
  ValoresKaplan[i]<-surv$chisq
  p.valor[i]<- 1 - pchisq(surv$chisq, length(surv$n) - 1)
  NOS_Kaplan<-data.frame(Categoria, ValoresKaplan, p.valor)
}
NOS_Kaplan$p.adj<-p.adjust(NOS_Kaplan$p.valor, method = "holm", n=length(NOS_Kaplan$p.valor))

##################################
dataKM1<-read_xlsx("Datos_KaplanEntero.xlsx", sheet="T")
dataKM1$deadstatus_event<-as.numeric(dataKM1$deadstatus_event)
dataKM1$`Angular Second Moment`<-as.factor(dataKM1$`Angular Second Moment`)
dataKM1$Correlation<-as.factor(dataKM1$Correlation)
dataKM1$`Inverse Difference Moment`<-as.factor(dataKM1$`Inverse Difference Moment`)
dataKM1$`Sum average`<-as.factor(dataKM1$`Sum average`)
dataKM1$`Sum variance`<-as.factor(dataKM1$`Sum variance`)
dataKM1$Entropy<-as.factor(dataKM1$Entropy)
dataKM1$`Difference Variance`<-as.factor(dataKM1$`Difference Variance`)
dataKM1$`Information Correlation1`<-as.factor(dataKM1$`Information Correlation1`)
dataKM1$`Information Correlation2`<-as.factor(dataKM1$`Information Correlation2`)
dataKM1$Dissimilarity<-as.factor(dataKM1$Dissimilarity)                     
dataKM1$`Cluster Shade`<-as.factor(dataKM1$`Cluster Shade`)
dataKM1$`Cluster Prominence`<-as.factor(dataKM1$`Cluster Prominence`)
dataKM1$Coarseness<-as.factor(dataKM1$Coarseness)
dataKM1$`Texture Strength`<-as.factor(dataKM1$`Texture Strength`)
dataKM1$`Small Zone Size Emphasis`<-as.factor(dataKM1$`Small Zone Size Emphasis`)
dataKM1$`Large Zone/High Gray Emphasis`<-as.factor(dataKM1$`Large Zone/High Gray Emphasis`)
dataKM1$`Gray-Level/Non-Uniformity`<-as.factor(dataKM1$`Gray-Level/Non-Uniformity`)
dataKM1$`Zone Size Non-Uniformity`<-as.factor(dataKM1$`Zone Size Non-Uniformity`)
dataKM1$`Zone Size Percentage`<-as.factor(dataKM1$`Zone Size Percentage`)
dataKM1$`Major Axis Length`<-as.factor(dataKM1$`Major Axis Length`)
dataKM1$Eccentricity<-as.factor(dataKM1$Eccentricity)
dataKM1$Volume<-as.factor(dataKM1$Volume)
dataKM1$`Fractal Diffraction`<-as.factor(dataKM1$`Fractal Diffraction`)
dataKM1$`Surface Area`<-as.factor(dataKM1$`Surface Area`)
dataKM1$Age<-as.factor(dataKM1$Age)

KM_T1<-dataKM1 %>% filter(EstadioT=="T1")
KM_T2<-dataKM1 %>% filter(EstadioT=="T2")
KM_T3<-dataKM1 %>% filter(EstadioT=="T3")
KM_T4<-dataKM1 %>% filter(EstadioT=="T4")

#T1
ValoresKaplan<-vector()
Categoria<-vector()
p.valor<-vector()
T1_Kaplan<-data.frame()

for (i in 1:25) {
  surv<-survdiff(Surv(survival_time, deadstatus_event)~(KM_T1[[i+13]]), data=KM_T1)
  Categoria[i]<-noquote(colnames(KM_T1)[i+13])
  ValoresKaplan[i]<-surv$chisq
  p.valor[i]<- 1 - pchisq(surv$chisq, length(surv$n) - 1)
  T1_Kaplan<-data.frame(Categoria, ValoresKaplan, p.valor)
}
T1_Kaplan$p.adj<-p.adjust(T1_Kaplan$p.valor, method = "holm", n=length(T1_Kaplan$p.valor))

#T2
ValoresKaplan<-vector()
Categoria<-vector()
p.valor<-vector()
T2_Kaplan<-data.frame()

for (i in 1:25) {
  surv<-survdiff(Surv(survival_time, deadstatus_event)~(KM_T2[[i+13]]), data=KM_T2)
  Categoria[i]<-noquote(colnames(KM_T2)[i+13])
  ValoresKaplan[i]<-surv$chisq
  p.valor[i]<- 1 - pchisq(surv$chisq, length(surv$n) - 1)
  T2_Kaplan<-data.frame(Categoria, ValoresKaplan, p.valor)
}
T2_Kaplan$p.adj<-p.adjust(T2_Kaplan$p.valor, method = "holm", n=length(T2_Kaplan$p.valor))

#T3
ValoresKaplan<-vector()
Categoria<-vector()
p.valor<-vector()
T3_Kaplan<-data.frame()

for (i in 1:25) {
  surv<-survdiff(Surv(survival_time, deadstatus_event)~(KM_T3[[i+13]]), data=KM_T3)
  Categoria[i]<-noquote(colnames(KM_T3)[i+13])
  ValoresKaplan[i]<-surv$chisq
  p.valor[i]<- 1 - pchisq(surv$chisq, length(surv$n) - 1)
  T3_Kaplan<-data.frame(Categoria, ValoresKaplan, p.valor)
}
T3_Kaplan$p.adj<-p.adjust(T3_Kaplan$p.valor, method = "holm", n=length(T3_Kaplan$p.valor))

#T4
ValoresKaplan<-vector()
Categoria<-vector()
p.valor<-vector()
T4_Kaplan<-data.frame()

for (i in 1:25) {
  surv<-survdiff(Surv(survival_time, deadstatus_event)~(KM_T4[[i+13]]), data=KM_T4)
  Categoria[i]<-noquote(colnames(KM_T4)[i+13])
  ValoresKaplan[i]<-surv$chisq
  p.valor[i]<- 1 - pchisq(surv$chisq, length(surv$n) - 1)
  T4_Kaplan<-data.frame(Categoria, ValoresKaplan, p.valor)
}
T4_Kaplan$p.adj<-p.adjust(T4_Kaplan$p.valor, method = "holm", n=length(T4_Kaplan$p.valor))

#####################
dataKM2<-read_xlsx("Datos_KaplanEntero.xlsx", sheet="N")
dataKM2$deadstatus_event<-as.numeric(dataKM2$deadstatus_event)
dataKM2$`Angular Second Moment`<-as.factor(dataKM2$`Angular Second Moment`)
dataKM2$Correlation<-as.factor(dataKM2$Correlation)
dataKM2$`Inverse Difference Moment`<-as.factor(dataKM2$`Inverse Difference Moment`)
dataKM2$`Sum average`<-as.factor(dataKM2$`Sum average`)
dataKM2$`Sum variance`<-as.factor(dataKM2$`Sum variance`)
dataKM2$Entropy<-as.factor(dataKM2$Entropy)
dataKM2$`Difference Variance`<-as.factor(dataKM2$`Difference Variance`)
dataKM2$`Information Correlation1`<-as.factor(dataKM2$`Information Correlation1`)
dataKM2$`Information Correlation2`<-as.factor(dataKM2$`Information Correlation2`)
dataKM2$Dissimilarity<-as.factor(dataKM2$Dissimilarity)                     
dataKM2$`Cluster Shade`<-as.factor(dataKM2$`Cluster Shade`)
dataKM2$`Cluster Prominence`<-as.factor(dataKM2$`Cluster Prominence`)
dataKM2$Coarseness<-as.factor(dataKM2$Coarseness)
dataKM2$`Texture Strength`<-as.factor(dataKM2$`Texture Strength`)
dataKM2$`Small Zone Size Emphasis`<-as.factor(dataKM2$`Small Zone Size Emphasis`)
dataKM2$`Large Zone/High Gray Emphasis`<-as.factor(dataKM2$`Large Zone/High Gray Emphasis`)
dataKM2$`Gray-Level/Non-Uniformity`<-as.factor(dataKM2$`Gray-Level/Non-Uniformity`)
dataKM2$`Zone Size Non-Uniformity`<-as.factor(dataKM2$`Zone Size Non-Uniformity`)
dataKM2$`Zone Size Percentage`<-as.factor(dataKM2$`Zone Size Percentage`)
dataKM2$`Major Axis Length`<-as.factor(dataKM2$`Major Axis Length`)
dataKM2$Eccentricity<-as.factor(dataKM2$Eccentricity)
dataKM2$Volume<-as.factor(dataKM2$Volume)
dataKM2$`Fractal Diffraction`<-as.factor(dataKM2$`Fractal Diffraction`)
dataKM2$`Surface Area`<-as.factor(dataKM2$`Surface Area`)
dataKM2$Age<-as.factor(dataKM2$Age)

KM_N0<-dataKM2 %>% filter(EstadioN=="N0")
KM_N1<-dataKM2 %>% filter(EstadioN=="N1")
KM_N2<-dataKM2 %>% filter(EstadioN=="N2")
KM_N3<-dataKM2 %>% filter(EstadioN=="N3")


#N0
ValoresKaplan<-vector()
Categoria<-vector()
p.valor<-vector()
N0_Kaplan<-data.frame()

for (i in 1:25) {
  surv<-survdiff(Surv(survival_time, deadstatus_event)~(KM_N0[[i+13]]), data=KM_N0)
  Categoria[i]<-noquote(colnames(KM_N0)[i+13])
  ValoresKaplan[i]<-surv$chisq
  p.valor[i]<- 1 - pchisq(surv$chisq, length(surv$n) - 1)
  N0_Kaplan<-data.frame(Categoria, ValoresKaplan, p.valor)
}
N0_Kaplan$p.adj<-p.adjust(N0_Kaplan$p.valor, method = "holm", n=length(N0_Kaplan$p.valor))

#N1
ValoresKaplan<-vector()
Categoria<-vector()
p.valor<-vector()
N1_Kaplan<-data.frame()

for (i in 1:25) {
  surv<-survdiff(Surv(survival_time, deadstatus_event)~(KM_N1[[i+13]]), data=KM_N1)
  Categoria[i]<-noquote(colnames(KM_N1)[i+13])
  ValoresKaplan[i]<-surv$chisq
  p.valor[i]<- 1 - pchisq(surv$chisq, length(surv$n) - 1)
  N1_Kaplan<-data.frame(Categoria, ValoresKaplan, p.valor)
}
N1_Kaplan$p.adj<-p.adjust(N1_Kaplan$p.valor, method = "holm", n=length(N1_Kaplan$p.valor))

#N2
ValoresKaplan<-vector()
Categoria<-vector()
p.valor<-vector()
N2_Kaplan<-data.frame()

for (i in 1:25) {
  surv<-survdiff(Surv(survival_time, deadstatus_event)~(KM_N2[[i+13]]), data=KM_N2)
  Categoria[i]<-noquote(colnames(KM_N2)[i+13])
  ValoresKaplan[i]<-surv$chisq
  p.valor[i]<- 1 - pchisq(surv$chisq, length(surv$n) - 1)
  N2_Kaplan<-data.frame(Categoria, ValoresKaplan, p.valor)
}
N2_Kaplan$p.adj<-p.adjust(N2_Kaplan$p.valor, method = "holm", n=length(N2_Kaplan$p.valor))

#N3
ValoresKaplan<-vector()
Categoria<-vector()
p.valor<-vector()
N3_Kaplan<-data.frame()

for (i in 1:25) {
  surv<-survdiff(Surv(survival_time, deadstatus_event)~(KM_N3[[i+13]]), data=KM_N3)
  Categoria[i]<-noquote(colnames(KM_N3)[i+13])
  ValoresKaplan[i]<-surv$chisq
  p.valor[i]<- 1 - pchisq(surv$chisq, length(surv$n) - 1)
  N3_Kaplan<-data.frame(Categoria, ValoresKaplan, p.valor)
}
N3_Kaplan$p.adj<-p.adjust(N3_Kaplan$p.valor, method = "holm", n=length(N3_Kaplan$p.valor))


#################################
dataKM3<-read_xlsx("Datos_KaplanEntero.xlsx", sheet="TNM")
dataKM3$deadstatus_event<-as.numeric(dataKM3$deadstatus_event)
dataKM3$`Angular Second Moment`<-as.factor(dataKM3$`Angular Second Moment`)
dataKM3$Correlation<-as.factor(dataKM3$Correlation)
dataKM3$`Inverse Difference Moment`<-as.factor(dataKM3$`Inverse Difference Moment`)
dataKM3$`Sum average`<-as.factor(dataKM3$`Sum average`)
dataKM3$`Sum variance`<-as.factor(dataKM3$`Sum variance`)
dataKM3$Entropy<-as.factor(dataKM3$Entropy)
dataKM3$`Difference Variance`<-as.factor(dataKM3$`Difference Variance`)
dataKM3$`Information Correlation1`<-as.factor(dataKM3$`Information Correlation1`)
dataKM3$`Information Correlation2`<-as.factor(dataKM3$`Information Correlation2`)
dataKM3$Dissimilarity<-as.factor(dataKM3$Dissimilarity)                     
dataKM3$`Cluster Shade`<-as.factor(dataKM3$`Cluster Shade`)
dataKM3$`Cluster Prominence`<-as.factor(dataKM3$`Cluster Prominence`)
dataKM3$Coarseness<-as.factor(dataKM3$Coarseness)
dataKM3$`Texture Strength`<-as.factor(dataKM3$`Texture Strength`)
dataKM3$`Small Zone Size Emphasis`<-as.factor(dataKM3$`Small Zone Size Emphasis`)
dataKM3$`Large Zone/High Gray Emphasis`<-as.factor(dataKM3$`Large Zone/High Gray Emphasis`)
dataKM3$`Gray-Level/Non-Uniformity`<-as.factor(dataKM3$`Gray-Level/Non-Uniformity`)
dataKM3$`Zone Size Non-Uniformity`<-as.factor(dataKM3$`Zone Size Non-Uniformity`)
dataKM3$`Zone Size Percentage`<-as.factor(dataKM3$`Zone Size Percentage`)
dataKM3$`Major Axis Length`<-as.factor(dataKM3$`Major Axis Length`)
dataKM3$Eccentricity<-as.factor(dataKM3$Eccentricity)
dataKM3$Volume<-as.factor(dataKM3$Volume)
dataKM3$`Fractal Diffraction`<-as.factor(dataKM3$`Fractal Diffraction`)
dataKM3$`Surface Area`<-as.factor(dataKM3$`Surface Area`)
dataKM3$Age<-as.factor(dataKM3$Age)

KM_I<-dataKM3 %>% filter(overall_stage=="I")
KM_II<-dataKM3 %>% filter(overall_stage=="II")
KM_IIIa<-dataKM3 %>% filter(overall_stage=="IIIa")
KM_IIIb<-dataKM3 %>% filter(overall_stage=="IIIb")

#I
ValoresKaplan<-vector()
Categoria<-vector()
p.valor<-vector()
I_Kaplan<-data.frame()

for (i in 1:25) {
  surv<-survdiff(Surv(survival_time, deadstatus_event)~(KM_I[[i+13]]), data=KM_I)
  Categoria[i]<-noquote(colnames(KM_I)[i+13])
  ValoresKaplan[i]<-surv$chisq
  p.valor[i]<- 1 - pchisq(surv$chisq, length(surv$n) - 1)
  I_Kaplan<-data.frame(Categoria, ValoresKaplan, p.valor)
}
I_Kaplan$p.adj<-p.adjust(I_Kaplan$p.valor, method = "holm", n=length(I_Kaplan$p.valor))

#II
ValoresKaplan<-vector()
Categoria<-vector()
p.valor<-vector()
II_Kaplan<-data.frame()
for (i in 1:25) {
  surv<-survdiff(Surv(survival_time, deadstatus_event)~(KM_II[[i+13]]), data=KM_II)
  Categoria[i]<-noquote(colnames(KM_II)[i+13])
  ValoresKaplan[i]<-surv$chisq
  p.valor[i]<- 1 - pchisq(surv$chisq, length(surv$n) - 1)
  II_Kaplan<-data.frame(Categoria, ValoresKaplan, p.valor)
}
II_Kaplan$p.adj<-p.adjust(II_Kaplan$p.valor, method = "holm", n=length(II_Kaplan$p.valor))

#IIIa
ValoresKaplan<-vector()
Categoria<-vector()
p.valor<-vector()
IIIa_Kaplan<-data.frame()
for (i in 1:25) {
  surv<-survdiff(Surv(survival_time, deadstatus_event)~(KM_IIIa[[i+13]]), data=KM_IIIa)
  Categoria[i]<-noquote(colnames(KM_IIIa)[i+13])
  ValoresKaplan[i]<-surv$chisq
  p.valor[i]<- 1 - pchisq(surv$chisq, length(surv$n) - 1)
  IIIa_Kaplan<-data.frame(Categoria, ValoresKaplan, p.valor)
}
IIIa_Kaplan$p.adj<-p.adjust(IIIa_Kaplan$p.valor, method = "holm", n=length(IIIa_Kaplan$p.valor))

#IIIb
ValoresKaplan<-vector()
Categoria<-vector()
p.valor<-vector()
IIIb_Kaplan<-data.frame()
for (i in 1:25) {
  surv<-survdiff(Surv(survival_time, deadstatus_event)~(KM_IIIb[[i+13]]), data=KM_IIIb)
  Categoria[i]<-noquote(colnames(KM_IIIb)[i+13])
  ValoresKaplan[i]<-surv$chisq
  p.valor[i]<- 1 - pchisq(surv$chisq, length(surv$n) - 1)
  IIIb_Kaplan<-data.frame(Categoria, ValoresKaplan, p.valor)
}
IIIb_Kaplan$p.adj<-p.adjust(IIIb_Kaplan$p.valor, method = "holm", n=length(IIIb_Kaplan$p.valor))



#Heatmap
LCC_Kaplan1<-LCC_Kaplan %>% mutate(Survival=paste("LCC", paste("n=", nrow(KM_LCC), sep="")))
SCC_Kaplan1<-SCC_Kaplan %>% mutate(Survival=paste("SCC", paste("n=", nrow(KM_SCC), sep="")))
ADC_Kaplan1<-ADC_Kaplan %>% mutate(Survival=paste("ADC", paste("n=", nrow(KM_ADC), sep="")))
NOS_Kaplan1<-NOS_Kaplan %>% mutate(Survival=paste("NOS", paste("n=", nrow(KM_NOS), sep="")))
I_Kaplan1<-I_Kaplan %>% mutate(Survival=paste("I", paste("n=", nrow(KM_I), sep="")))
II_Kaplan1<-II_Kaplan %>% mutate(Survival=paste("II", paste("n=", nrow(KM_II), sep="")))
IIIa_Kaplan1<-IIIa_Kaplan %>% mutate(Survival=paste("IIIa", paste("n=", nrow(KM_IIIa), sep="")))
IIIb_Kaplan1<-IIIb_Kaplan %>% mutate(Survival=paste("IIIb", paste("n=", nrow(KM_IIIb), sep="")))
N0_Kaplan1<-N0_Kaplan %>% mutate(Survival=paste("N0", paste("n=", nrow(KM_N0), sep="")))
N1_Kaplan1<-N1_Kaplan %>% mutate(Survival=paste("N1", paste("n=", nrow(KM_N1), sep="")))
N2_Kaplan1<-N2_Kaplan %>% mutate(Survival=paste("N2", paste("n=", nrow(KM_N2), sep="")))
N3_Kaplan1<-N3_Kaplan %>% mutate(Survival=paste("N3", paste("n=", nrow(KM_N3), sep="")))
T1_Kaplan1<-T1_Kaplan %>% mutate(Survival=paste("T1", paste("n=", nrow(KM_T1), sep="")))
T2_Kaplan1<-T2_Kaplan %>% mutate(Survival=paste("T2", paste("n=", nrow(KM_T2), sep="")))
T3_Kaplan1<-T3_Kaplan %>% mutate(Survival=paste("T3", paste("n=", nrow(KM_T3), sep="")))
T4_Kaplan1<-T4_Kaplan %>% mutate(Survival=paste("T4", paste("n=", nrow(KM_T4), sep="")))

junto<-rbind(LCC_Kaplan1,SCC_Kaplan1, ADC_Kaplan1,NOS_Kaplan1,
             T1_Kaplan1,T2_Kaplan1,T3_Kaplan1,T4_Kaplan1,
             N0_Kaplan1,N1_Kaplan1,N2_Kaplan1,N3_Kaplan1,
             I_Kaplan1,II_Kaplan1,IIIa_Kaplan1,IIIb_Kaplan1) %>% mutate(N=1)

junto$Survival<-factor(junto$Survival)
#junto$Survival<-factor(junto$Survival, levels(junto$Survival)[c(3,1, 4,2)])
junto$Survival<-factor(junto$Survival, levels(junto$Survival)[c(5:2,10:7,16:13,11,1,12,6)])
junto$Categoria<-factor(junto$Categoria, levels(junto$Categoria)[c(2,6,15,19,20,10,7,13,14,8,4,3,5,22,18,16,12,24,25,17,9,23,11,21,1)])
#junto$p.valor2<-if_else(junto$p.valor<0.05, junto$p.valor,NULL)
junto$p.adjust2<-if_else(junto$p.adj<0.05, junto$p.adj, NULL)
junto2<-junto %>% na.omit()

#write.csv(junto, "Resultados_log-rank test.csv")

ggplot(data = junto, aes(x=Categoria, y=Survival)) + 
  geom_tile(aes(fill=ValoresKaplan), color="white")+
  scale_fill_distiller(palette="RdYlBu", breaks=c(seq(0,17, 10)),limits=c(0,17))+
  coord_equal()+
  labs(x=NULL, y=NULL, fill="")+
  ggtitle("Significance Log-rank test")+
  geom_point(data=junto2, aes(x=Categoria, y=Survival), size=3, fill="black", colour="green", pch=21)+
  theme(plot.title=element_text(size=12, face="bold", hjust=0.5))+
  theme(legend.title=element_text(size=10), legend.title.align = 1, legend.text = element_text(size=9))+
  theme(axis.text.x=element_text(angle = 90, hjust=1))+
  guides(fill = guide_colourbar(ticks=F,barwidth = 1, barheight = 10))



###################################################
#ANALYSIS KAPLAN-MEIER
###################################################
#You would need to plot the curves which are significative different (Table 2). 
#We have to do it for:
#LCC: coarseness, Texture strength, Gray-Level/Non-Uniformity, Zone Size Non-Uniformity, Fractal Diffraction, Surface Area
#T2: Large Zone/HIgh Gray Emphasis, Fractal Diffraction
#N0: Dissimilarity, Coarseness, Texture strength,Large Zone/High Gray Emphasis, Gray-Level/Non-Uniformity, Zone Size Percentage, Major Axis Length, Volume, Fractal Diffraction, Surface Area, Age
#I: Angular Second Moment, Inverse Difference Moment, Sum average, Entropy, Dissimilarity, Coarseness,Large Zone/High Gray Emphasis, Gray-Level/Non-Uniformity, Zone Size Percentage, Major Axis Length, Volume, Fractal Diffraction, Surface Area
library(survminer)
names(KM_N0[31])
fit <- survfit(Surv(survival_time,deadstatus_event)~KM_N0[[31]], data=KM_N0)
ggsurvplot(fit, data = KM_N0, risk.table = F,
           censor=T,
           #pval=T,  pval.size = 4, pval.coord=c(1500,0.7),
           #title= "LCC", 
           palette = c("black","red"), size=0.5,
           xlab="Días", ylab="Funciones de supervivencia estimadas",
           font.x = c(11), font.y = c(11),
           legend = c(0.75, 0.85),
           legend.title="(N0)",# \n +Censored",
           legend.labs = c("Zone Size Non-Uniformity  > Corte", "Zone Size Non-Uniformity < Corte"),
           ggtheme = theme_classic())
