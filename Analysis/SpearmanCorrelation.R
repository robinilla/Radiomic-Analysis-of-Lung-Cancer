#Script created by Sonia Illanas
#Creation data: 17/12/2018
#Last actualization: 27/12/2018

library(readxl)         #load  the package for reading excel files
library(tidyverse)      #load  the package for data management
library(ggplot2)        #load  the package for making graphs

setwd("C:/Docs/San")    #set your working directory
rm(list=ls())           #clean your global environment
datos<-read_xlsx("jsonCompletoNodule_modificadoSonia_.xlsx") #load your data

#All the code is commented following the methodology described in Chaddad et al 2017

datos_uncensored<-datos %>%  filter(deadstatus_event==1) %>% mutate (survival_time2=survival_time)  # separe your data: uncensored data (dead peoople)

#datos<-datos_uncensored            # If you want to do it only with uncensored data (dead peoople) remove "#" 
                                    #and remember to put "#" before the following two lines
                                    #then run the code

# If you want to do it with all your data, don't remove the previously "#" 

datos_censored<-datos %>% filter(deadstatus_event==0) %>% mutate(survival_time2=(ifelse(survival_time<mean(datos_uncensored$survival_time), survival_time, mean(datos_uncensored$survival_time))))

datos<-rbind(datos_uncensored, datos_censored)      #Complet data. Survival time for alive people is modified according to methodology 
datos$survival_time<-datos$survival_time2


#Separe your data in categories
LCC<-datos %>% filter(NSCLC=="LCC")
SCC<-datos %>% filter(NSCLC=="SCC")
ADC<-datos %>% filter(NSCLC=="ADC")
NOS<-datos %>% filter(NSCLC=="NOS")

I<-datos %>% filter(overall_stage=="I")
II<-datos %>% filter(overall_stage=="II")
IIIa<-datos %>% filter(overall_stage=="IIIa")
IIIb<-datos %>% filter(overall_stage=="IIIb")

T1<-datos %>% filter(EstadioT=="T1")
T2<-datos %>% filter(EstadioT=="T2")
T3<-datos %>% filter(EstadioT=="T3")
T4<-datos %>% filter(EstadioT=="T4")

N0<-datos %>% filter(EstadioN=="N0")
N1<-datos %>% filter(EstadioN=="N1")
N2<-datos %>% filter(EstadioN=="N2")
N3<-datos %>% filter(EstadioN=="N3")


###########################################################################
# Automatic Spearman Correlation Test between variables and survival time #
# p.values are adjusted with Holm-Bonferroni                              #
###########################################################################

#Spearman Test for LCC
LCCspearman<-data.frame()
Categoria<-vector()
ValorCorrelacion<-vector()
p.valor<-vector()
for (i in 1:25) {
  corr<-cor.test(~(LCC[[i+13]])+survival_time, 
                 data=LCC,
                 method = "spearman",
                 #continuity = FALSE,
                 conf.level = 0.95,
                 p.value=T)
  Categoria[i]<-noquote(colnames(LCC)[i+13])
  ValorCorrelacion[i]<-round(corr$estimate, 4)
  p.valor[i]<-round(corr$p.value,4)
  LCCspearman<-data.frame(Categoria, ValorCorrelacion, p.valor)
}
LCCspearman$p.adj<-p.adjust(LCCspearman$p.valor, method = "holm", n=length(LCCspearman$p.valor))

#Spearman Test for SCC
SCCspearman<-data.frame()
Categoria<-vector()
ValorCorrelacion<-vector()
p.valor<-vector()
for (i in 1:25) {
  corr<-cor.test(~(SCC[[i+13]])+survival_time, 
                 data=SCC,
                 method = "spearman",
                 #continuity = FALSE,
                 conf.level = 0.95,
                 p.value=T)
  Categoria[i]<-noquote(colnames(SCC)[i+13])
  ValorCorrelacion[i]<-round(corr$estimate, 4)
  p.valor[i]<-round(corr$p.value,4)
  SCCspearman<-data.frame(Categoria, ValorCorrelacion, p.valor)
}
SCCspearman$p.adj<-p.adjust(SCCspearman$p.valor, method = "holm", n=length(SCCspearman$p.valor))

#Spearman Test for ADC
ADCspearman<-data.frame()
Categoria<-vector()
ValorCorrelacion<-vector()
p.valor<-vector()
for (i in 1:25) {
  corr<-cor.test(~(ADC[[i+13]])+survival_time, 
                 data=ADC,
                 method = "spearman",
                 #continuity = FALSE,
                 conf.level = 0.95,
                 p.value=T)
  Categoria[i]<-noquote(colnames(ADC)[i+13])
  ValorCorrelacion[i]<-round(corr$estimate, 4)
  p.valor[i]<-round(corr$p.value,4)
  ADCspearman<-data.frame(Categoria, ValorCorrelacion, p.valor)
}
ADCspearman$p.adj<-p.adjust(ADCspearman$p.valor, method = "holm", n=length(ADCspearman$p.valor))

#Spearman Test for NOS
NOSspearman<-data.frame()
Categoria<-vector()
ValorCorrelacion<-vector()
p.valor<-vector()
for (i in 1:25) {
  corr<-cor.test(~(NOS[[i+13]])+survival_time, 
                 data=NOS,
                 method = "spearman",
                 #continuity = FALSE,
                 conf.level = 0.95,
                 p.value=T)
  Categoria[i]<-noquote(colnames(NOS)[i+13])
  ValorCorrelacion[i]<-round(corr$estimate, 4)
  p.valor[i]<-round(corr$p.value,4)
  NOSspearman<-data.frame(Categoria, ValorCorrelacion, p.valor)
}
NOSspearman$p.adj<-p.adjust(NOSspearman$p.valor, method = "holm", n=length(NOSspearman$p.valor))


#Spearman Test for I
Ispearman<-data.frame()
Categoria<-vector()
ValorCorrelacion<-vector()
p.valor<-vector()
for (i in 1:25) {
  corr<-cor.test(~(I[[i+13]])+survival_time, 
                 data=I,
                 method = "spearman",
                 #continuity = FALSE,
                 conf.level = 0.95,
                 p.value=T)
  Categoria[i]<-noquote(colnames(I)[i+13])
  ValorCorrelacion[i]<-round(corr$estimate, 4)
  p.valor[i]<-round(corr$p.value,4)
  Ispearman<-data.frame(Categoria, ValorCorrelacion, p.valor)
}
Ispearman$p.adj<-p.adjust(Ispearman$p.valor, method = "holm", n=length(Ispearman$p.valor))

#Spearman Test for II
IIspearman<-data.frame()
Categoria<-vector()
ValorCorrelacion<-vector()
p.valor<-vector()
p.adjusted<-vector()
for (i in 1:25) {
  corr<-cor.test(~(II[[i+13]])+survival_time, 
                 data=II,
                 method = "spearman",
                 #continuity = FALSE,
                 conf.level = 0.95,
                 p.value=T)
  Categoria[i]<-noquote(colnames(II)[i+13])
  ValorCorrelacion[i]<-round(corr$estimate, 4)
  p.valor[i]<-round(corr$p.value,4)
  IIspearman<-data.frame(Categoria, ValorCorrelacion, p.valor)
}
IIspearman$p.adj<-p.adjust(IIspearman$p.valor, method = "holm", n=length(IIspearman$p.valor))

#Spearman Test for IIIa
IIIaspearman<-data.frame()
Categoria<-vector()
ValorCorrelacion<-vector()
p.valor<-vector()
for (i in 1:25) {
  corr<-cor.test(~(IIIa[[i+13]])+survival_time, 
                 data=IIIa,
                 method = "spearman",
                 #continuity = FALSE,
                 conf.level = 0.95,
                 p.value=T)
  Categoria[i]<-noquote(colnames(IIIa)[i+13])
  ValorCorrelacion[i]<-round(corr$estimate, 4)
  p.valor[i]<-round(corr$p.value,4)
  IIIaspearman<-data.frame(Categoria, ValorCorrelacion, p.valor)
}
IIIaspearman$p.adj<-p.adjust(IIIaspearman$p.valor, method = "holm", n=length(IIIaspearman$p.valor))

#Spearman Test for IIIb
IIIbspearman<-data.frame()
Categoria<-vector()
ValorCorrelacion<-vector()
p.valor<-vector()
for (i in 1:25) {
  corr<-cor.test(~(IIIb[[i+13]])+survival_time, 
                 data=IIIb,
                 method = "spearman",
                 #continuity = FALSE,
                 conf.level = 0.95,
                 p.value=T)
  Categoria[i]<-noquote(colnames(IIIb)[i+13])
  ValorCorrelacion[i]<-round(corr$estimate, 4)
  p.valor[i]<-round(corr$p.value,4)
  IIIbspearman<-data.frame(Categoria, ValorCorrelacion, p.valor)
}
IIIbspearman$p.adj<-p.adjust(IIIbspearman$p.valor, method = "holm", n=length(IIIbspearman$p.valor))

#Spearman Test for T1
T1spearman<-data.frame()
Categoria<-vector()
ValorCorrelacion<-vector()
p.valor<-vector()
for (i in 1:25) {
  corr<-cor.test(~(T1[[i+13]])+survival_time, 
                 data=T1,
                 method = "spearman",
                 #continuity = FALSE,
                 conf.level = 0.95,
                 p.value=T)
  Categoria[i]<-noquote(colnames(T1)[i+13])
  ValorCorrelacion[i]<-round(corr$estimate, 4)
  p.valor[i]<-round(corr$p.value,4)
  T1spearman<-data.frame(Categoria, ValorCorrelacion, p.valor)
}
T1spearman$p.adj<-p.adjust(T1spearman$p.valor, method = "holm", n=length(T1spearman$p.valor))

#Spearman Test for T2
T2spearman<-data.frame()
Categoria<-vector()
ValorCorrelacion<-vector()
p.valor<-vector()
for (i in 1:25) {
  corr<-cor.test(~(T2[[i+13]])+survival_time, 
                 data=T2,
                 method = "spearman",
                 #continuity = FALSE,
                 conf.level = 0.95,
                 p.value=T)
  Categoria[i]<-noquote(colnames(T2)[i+13])
  ValorCorrelacion[i]<-round(corr$estimate, 4)
  p.valor[i]<-round(corr$p.value,4)
  T2spearman<-data.frame(Categoria, ValorCorrelacion, p.valor)
}
T2spearman$p.adj<-p.adjust(T2spearman$p.valor, method = "holm", n=length(T2spearman$p.valor))

#Spearman Test for T3
T3spearman<-data.frame()
Categoria<-vector()
ValorCorrelacion<-vector()
p.valor<-vector()
for (i in 1:25) {
  corr<-cor.test(~(T3[[i+13]])+survival_time, 
                 data=T3,
                 method = "spearman",
                 #continuity = FALSE,
                 conf.level = 0.95,
                 p.value=T)
  Categoria[i]<-noquote(colnames(T3)[i+13])
  ValorCorrelacion[i]<-round(corr$estimate, 4)
  p.valor[i]<-round(corr$p.value,4)
  T3spearman<-data.frame(Categoria, ValorCorrelacion, p.valor)
}
T3spearman$p.adj<-p.adjust(T3spearman$p.valor, method = "holm", n=length(T3spearman$p.valor))

#Spearman Test for T4
T4spearman<-data.frame()
Categoria<-vector()
ValorCorrelacion<-vector()
p.valor<-vector()
for (i in 1:25) {
  corr<-cor.test(~(T4[[i+13]])+survival_time, 
                 data=T4,
                 method = "spearman",
                 #continuity = FALSE,
                 conf.level = 0.95,
                 p.value=T)
  Categoria[i]<-noquote(colnames(T4)[i+13])
  ValorCorrelacion[i]<-round(corr$estimate, 4)
  p.valor[i]<-round(corr$p.value,4)
  p.adjusted[i]<-p.adjust(p.valor, method = "holm", n=length(p.valor))
  T4spearman<-data.frame(Categoria, ValorCorrelacion, p.valor)
}
T4spearman$p.adj<-p.adjust(T4spearman$p.valor, method = "holm", n=length(T4spearman$p.valor))

#Spearman Test for N0
N0spearman<-data.frame()
Categoria<-vector()
ValorCorrelacion<-vector()
p.valor<-vector()
for (i in 1:25) {
  corr<-cor.test(~(N0[[i+13]])+survival_time, 
                 data=N0,
                 method = "spearman",
                 #continuity = FALSE,
                 conf.level = 0.95,
                 p.value=T)
  Categoria[i]<-noquote(colnames(N0)[i+13])
  ValorCorrelacion[i]<-round(corr$estimate, 4)
  p.valor[i]<-round(corr$p.value,4)
  N0spearman<-data.frame(Categoria, ValorCorrelacion, p.valor)
}
N0spearman$p.adj<-p.adjust(N0spearman$p.valor, method = "holm", n=length(N0spearman$p.valor))

#Spearman Test for N1
N1spearman<-data.frame()
Categoria<-vector()
ValorCorrelacion<-vector()
p.valor<-vector()
for (i in 1:25) {
  corr<-cor.test(~(N1[[i+13]])+survival_time, 
                 data=N1,
                 method = "spearman",
                 #continuity = FALSE,
                 conf.level = 0.95,
                 p.value=T)
  Categoria[i]<-noquote(colnames(N1)[i+13])
  ValorCorrelacion[i]<-round(corr$estimate, 4)
  p.valor[i]<-round(corr$p.value,4)
  N1spearman<-data.frame(Categoria, ValorCorrelacion, p.valor)
}
N1spearman$p.adj<-p.adjust(N1spearman$p.valor, method = "holm", n=length(N1spearman$p.valor))

#Spearman Test for N2
N2spearman<-data.frame()
Categoria<-vector()
ValorCorrelacion<-vector()
p.valor<-vector()
for (i in 1:25) {
  corr<-cor.test(~(N2[[i+13]])+survival_time, 
                 data=N2,
                 method = "spearman",
                 #continuity = FALSE,
                 conf.level = 0.95,
                 p.value=T)
  Categoria[i]<-noquote(colnames(N2)[i+13])
  ValorCorrelacion[i]<-round(corr$estimate, 4)
  p.valor[i]<-round(corr$p.value,4)
  N2spearman<-data.frame(Categoria, ValorCorrelacion, p.valor)
}
N2spearman$p.adj<-p.adjust(N2spearman$p.valor, method = "holm", n=length(N2spearman$p.valor))
p.adjust

#Spearman Test for N3
N3spearman<-data.frame()
Categoria<-vector()
ValorCorrelacion<-vector()
p.valor<-vector()
for (i in 1:25) {
  corr<-cor.test(~(N3[[i+13]])+survival_time, 
                 data=N3,
                 method = "spearman",
                 #continuity = FALSE,
                 conf.level = 0.95,
                 p.value=T)
  Categoria[i]<-noquote(colnames(N3)[i+13])
  ValorCorrelacion[i]<-round(corr$estimate, 4)
  p.valor[i]<-round(corr$p.value,4)
  N3spearman<-data.frame(Categoria, ValorCorrelacion, p.valor)
}
N3spearman$p.adj<-p.adjust(N3spearman$p.valor, method = "holm", n=length(N3spearman$p.valor))


#Spearman Test Results (Correlation value and p-values)
LCCspearman
SCCspearman
ADCspearman
NOSspearman
T1spearman
T2spearman
T3spearman
T4spearman
N0spearman
N1spearman
N2spearman
N3spearman
Ispearman
IIspearman
IIIaspearman
IIIbspearman


#HEATMAP

LCCspearman1<-LCCspearman %>% mutate(Survival=paste("LCC", paste("n=", nrow(LCC), sep="")))
SCCspearman1<-SCCspearman %>% mutate(Survival=paste("SCC", paste("n=", nrow(SCC), sep="")))
ADCspearman1<-ADCspearman %>% mutate(Survival=paste("ADC", paste("n=", nrow(ADC), sep="")))
NOSspearman1<-NOSspearman %>% mutate(Survival=paste("NOS", paste("n=", nrow(NOS), sep="")))
Ispearman1<-Ispearman %>% mutate(Survival=paste("I", paste("n=", nrow(I), sep="")))
IIspearman1<-IIspearman %>% mutate(Survival=paste("II", paste("n=", nrow(II), sep="")))
IIIaspearman1<-IIIaspearman %>% mutate(Survival=paste("IIIa", paste("n=", nrow(IIIa), sep="")))
IIIbspearman1<-IIIbspearman %>% mutate(Survival=paste("IIIb", paste("n=", nrow(IIIb), sep="")))
N0spearman1<-N0spearman %>% mutate(Survival=paste("N0", paste("n=", nrow(N0), sep="")))
N1spearman1<-N1spearman %>% mutate(Survival=paste("N1", paste("n=", nrow(N1), sep="")))
N2spearman1<-N2spearman %>% mutate(Survival=paste("N2", paste("n=", nrow(N2), sep="")))
N3spearman1<-N3spearman %>% mutate(Survival=paste("N3", paste("n=", nrow(N3), sep="")))
T1spearman1<-T1spearman %>% mutate(Survival=paste("T1", paste("n=", nrow(T1), sep="")))
T2spearman1<-T2spearman %>% mutate(Survival=paste("T2", paste("n=", nrow(T2), sep="")))
T3spearman1<-T3spearman %>% mutate(Survival=paste("T3", paste("n=", nrow(T3), sep="")))
T4spearman1<-T4spearman %>% mutate(Survival=paste("T4", paste("n=", nrow(T4), sep="")))

junto<-rbind(LCCspearman1,SCCspearman1, ADCspearman1,NOSspearman1,
             T1spearman1,T2spearman1,T3spearman1,T4spearman1,
             N0spearman1,N1spearman1,N2spearman1,N3spearman1,
             Ispearman1,IIspearman1,IIIaspearman1,IIIbspearman1) %>% mutate(N=1)

junto$Survival<-factor(junto$Survival)
junto$Survival<-factor(junto$Survival, levels(junto$Survival)[c(5:2,10:7,16:13,11,1,12,6)])
junto$Categoria<-factor(junto$Categoria, levels(junto$Categoria)[c(2,6,15,19,20,10,7,13,14,8,4,3,5,22,18,16,12,24,25,17,9,23,11,21,1)])
#junto$p.valor2<-if_else(junto$p.valor<0.05, junto$p.valor,NULL)
junto$p.adjust2<-if_else(junto$p.adj<0.05, junto$p.adj, NULL)
junto2<-junto %>% na.omit()


ggplot(data = junto, aes(x=Categoria, y=Survival)) + 
  geom_tile(aes(fill=ValorCorrelacion), color="white")+
  scale_fill_distiller(palette="RdYlBu", breaks=c(-0.5,0,0.5), limits=c(-0.5,0.5))+
  coord_equal()+
  labs(x=NULL, y=NULL, fill="")+
  ggtitle("Spearman's Rank Correlation")+
  geom_point(data=junto2, aes(x=Categoria, y=Survival), size=3, fill="black", colour="green", pch=21)+
  theme(plot.title=element_text(size=12, face="bold", hjust=0.5))+
  theme(legend.title=element_text(size=10), legend.title.align = 1, legend.text = element_text(size=9))+
  theme(axis.text.x=element_text(angle = 90, hjust=1))+
  guides(fill = guide_colourbar(ticks=F,barwidth = 1, barheight = 10))

#write_csv(junto, "CorrelacionSpearman_Uncensored_Sonia.csv")
