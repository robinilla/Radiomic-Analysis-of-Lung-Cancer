#Script created by Sonia Illanas
#Creation data: 01/01/2019
#Last actualization: 10/01/2019

library(readxl)         #load  the package for reading excel files
library(tidyverse)      #load  the package for data management
library(ggplot2)        #load  the package for making graphs
library(randomForest)   #Load the package for doing RANDOM FOREST
library(dismo)          #load SDM package
library(plotmo)         #plot models
library(viridis)        #colour palette


########################################################
#ANALISIS RANDOM FOREST: 
#It should be done for the groups that you're interested due to they shown significative Correlation, 
#and also to the other data which don't belong to the group.
########################################################
setwd("C:/Docs/San")    #set your working directory
rm(list=ls())           #clean your global environment
datos<-read_xlsx("jsonCompletoNodule_modificadoSonia_nombrescolumnasRF.xlsx") #load your data

#The following lines are for preparing your data as it's specified in the methodology
datos_uncensored<-datos %>%  filter(deadstatus_event==1) %>% mutate (survival_time2=survival_time) # If you want to do it with uncensored data (dead people)
datos_censored<-datos %>% filter(deadstatus_event==0) %>% mutate(survival_time2=(ifelse(survival_time<mean(datos_uncensored$survival_time), survival_time, mean(datos_uncensored$survival_time))))
datos<-rbind(datos_uncensored, datos_censored)      #All data with modified survival time for alive people, as indicated in the article
datos2<-lapply(datos[1:13], as.factor)
datos<-cbind(datos2, datos[14:39])
datos$survival_time<-datos$survival_time2
datos<-datos %>% mutate(EstadioM=if_else(clinical_m_stage=="0.0", "M0", if_else(clinical_m_stage=="1.0", "M1", "M3")))
datos$EstadioM<-as.factor(datos$EstadioM)
mediana_survival<-median(datos$survival_time) #Median of survival time for doing groups
borrar<-c("root_id", "deadstatus_event","patient_id", "histology", "gender", "clinical_m_stage","clinical_t_stage", "clinical_m_stage","clinical_n_stage")
datos<-select(datos, -borrar)
datos<-datos %>% mutate(presencia=if_else(survival_time2>mediana_survival | survival_time2==mediana_survival, 1,0))
datos<- datos %>% mutate(random=rnorm(nrow(datos), mean=0, sd=1))
rm(datos2, datos_censored, datos_uncensored, mediana_survival)

#Groups which have correlation with survival time
LCC<-datos %>% filter(NSCLC=="LCC")
T2<-datos %>% filter(EstadioT=="T2")
I<-datos %>% filter(overall_stage=="I")
N0<-datos %>% filter(EstadioN=="N0")


#For doing the analysis of Random Forest of non-groups of interest run the following lines inestead of the above ones.
#You don't need to change the name of the groups.
#LCC<-datos %>% filter(NSCLC!="LCC")
#T2<-datos %>% filter(EstadioT!="T2")
#I<-datos %>% filter(overall_stage!="I")
#N0<-datos %>% filter(EstadioN!="N0")



#FORMULA
nombres<-c("Age", "NSCLC", "EstadioT", "EstadioN", "EstadioM", "overall_stage")
formula.regresion<-as.formula(paste("presencia~", paste(nombres, collapse="+"), collapse=""))

nombres1<-as.vector(names(LCC)[c(2:30,32)])
formula.regresion1<-as.formula(paste("presencia~", paste(nombres1, collapse="+"), collapse=""))

nombres2<-as.vector(names(LCC)[c(2:30,32, 34)])
formula.regresion2<-as.formula(paste("presencia~", paste(nombres2, collapse="+"), collapse=""))


#SENSIBILITY ANALYSIS: Selection of nodesize.
#As in the paper didn't indicate the nodesize, we saw how AUC value changed respected the nodesize, 
#We select a value for Random Forest analysis taking into account the change of slope saw in this analysis
##########################################
#Run the code as many times as you need (according with your groups of interest)
#Remember: you just need to select the code, and change the word of the word written with the word of your group of interest!

nodesize.vector<-seq(10, 200, by=10)
auc.vector<-vector()
auc.vector1<-vector()
#iterate 
for (nodesize.valor in nodesize.vector){
  m.rf.temp<-randomForest(formula.regresion, data=I, ntree=500, nodesize=nodesize.valor) #adjust the model
  #evaluate the model and save AUC value in auc.vector
  auc.vector<-c(auc.vector, evaluate(
    p=I %>% filter(presencia==1) %>% select(nombres),
    a=I %>% filter(presencia==0) %>% select(nombres),
    model=m.rf.temp)@auc)
}#end of iterations

#iteramos sobre valores de regmult
for (nodesize.valor in nodesize.vector){
  m.rf.temp<-randomForest(formula.regresion1, data=I, ntree=500, nodesize=nodesize.valor)  #adjust the model
  #evaluate the model and save AUC value in auc.vector
  auc.vector1<-c(auc.vector1, evaluate(
    p=I %>% filter(presencia==1) %>% select(nombres1),
    a=I %>% filter(presencia==0) %>% select(nombres1),
    model=m.rf.temp)@auc)
}#end of iterations

#see the result
ggplot()+
  geom_line(aes(x=nodesize.vector, y=auc.vector, colour="I.TNM"), lwd=0.75)+
  geom_line(aes(x=nodesize.vector, y=auc.vector1, colour="I.combined"), lwd=0.75)+
  xlab("Nodesize")+ylab("AUC")+
  labs(fill="")+
  scale_y_continuous(limits = c(0.6, 1))+
  ggtitle("Nodesize election")+
  scale_colour_manual(name="", values=c(I.TNM="red", I.combined="blue"))+
  theme_minimal()+theme(legend.position=c(1,0.75), legend.justification=c(1,0))+
  theme(plot.title=element_text(size=12, face="bold"))


#MODELOS EVALUATION WITH K-FOLD
#(Nodesize selected:75).
###########################################################
#Run the code as many times as you need (according with your groups of interest)
#Remember: you just need to select the code, and change the word of the word written with the word of your group of interest!


set.seed(500)
presencia.kfold<-I #select your data (save it in other object)
k<-10                #generating 10 grupos
grupos<-kfold(I, k)
formula.modelo<-as.formula(paste("presencia~", paste(nombres, collapse="+"), 
                                 collapse="")) #formula of the model
formula.modelo1<-as.formula(paste("presencia~", paste(nombres1, collapse="+"), 
                                  collapse="")) #formula of the model
formula.modelo2<-as.formula(paste("presencia~", paste(nombres2, collapse="+"), 
                                  collapse=""))

all.response<-all.predictor <- aucs <- datos.plot.I<- c()
all.response1<-all.predictor1 <- aucs1 <- datos.plot.I.C<- c()

#for every grupo
for (grupo in 1:k) {
  #select data for train the model 
  learn<-presencia.kfold[grupos != grupo,]
  #select data for evaluate the model
  test<-presencia.kfold[grupos == grupo,]
  #adjust the model for the group TNM
  modelo.entrenado<-randomForest(formula.modelo, data=I, ntree=500, nodesize=75, importance=T)
  #predict the values with TNM data for evaluating the model 
  model.pred <- predict(modelo.entrenado, newdata=test)
  #see how good/bad my TNM model predict: TNM observed data (learn) vs predicted data (test)
  aucs <- c(aucs, roc(test$presencia, model.pred)$auc)
  all.response <- c(all.response, test$presencia)
  all.predictor <- c(all.predictor, model.pred)
  #Save the data for each curve to can plot it together
  datos.plot.I<-roc(all.response, all.predictor)
  
  #adjust the model for Combined
  modelo.entrenado1<-randomForest(formula.modelo1, data=I, ntree=500, nodesize=75, importance=T)
  #predict the values with TNM data for evaluating Combined model 
  model.pred1 <- predict(modelo.entrenado1, newdata=test)
  #see how good/bad my TNM model predict: TNM observed data (learn) vs predicted data (test)
  aucs1 <- c(aucs1, roc(test$presencia, model.pred1)$auc)
  all.response1 <- c(all.response1, test$presencia)
  all.predictor1 <- c(all.predictor1, model.pred1)
  #Save the data for each curve to can plot it together
  datos.plot.I.C<-roc(all.response1, all.predictor1)
  modelo.entrenado2<-randomForest(formula.modelo2, data=I, ntree=500, nodesize=75, importance=T)
} #end

auc.kfold.mean<-mean(aucs)
auc.kfold.sd<-sd(aucs)
auc.values<-data.frame(auc.kfold.mean, auc.kfold.sd, Grupo="I.TNM")

auc.kfold.mean1<-mean(aucs1)
auc.kfold.sd1<-sd(aucs1)
auc.values1<-data.frame(auc.kfold.mean=auc.kfold.mean1, auc.kfold.sd=auc.kfold.sd1, Grupo="I.Combined")

AUC.values<-rbind(AUC.values, 
                  auc.values, auc.values1)

#write.csv(AUC.values, file="/Docs/San/Resultados_RandomForest/AUC.values_20190110.csv")

#Figura 5A
###########################################################
ggroc(list("LCC TNM 77.83"=datos.plot.LCC, "T2 TNM: 68.20"=datos.plot.T2, 
           "N0 TNM 72.04"=datos.plot.N0,"TNM-I TNM 83.15"=datos.plot.I,
           "LCC combinado 84.85"=datos.plot.LCC.C, "T2 combinado 78.21"=datos.plot.T2.C, 
           "N0 combinado 83.80"=datos.plot.N0.C, "TNM-I combinado 93.33"=datos.plot.I.C),
      legacy.axes=T)+
  labs(title="A")+
  ylab("Sensibility")+
  xlab("1 - Specificity")+
  geom_abline(lty=3, col="grey")+
  scale_color_viridis(discrete=T, name="AUC %")+
  theme_bw()+
  theme(legend.position = c(0.79, 0.32),
        legend.background=element_rect(linetype="solid", colour ="black"),
        legend.text=element_text(size=8.5))+
  scale_x_continuous(expand = c(0, 0), breaks=c(0,0.2,0.4,0.6,0.8,1))+ 
  scale_y_continuous(expand = c(0, 0), breaks=c(0,0.2,0.4,0.6,0.8,1))
#scale_linetype_manual(values=rep(c("solid", "dotted"),4))



#Figura 6: ONLY for COMBINED models
###########################################################
feat_imp_df <- importance(modelo.entrenado1) %>% 
  data.frame() %>% 
  mutate(feature = row.names(.))
feat_imp_df <-feat_imp_df %>% mutate(Grupo=ifelse(feat_imp_df$feature=="NSCLC" | feat_imp_df$feature=="overall_stage" | feat_imp_df$feature=="EstadioM" | feat_imp_df$feature=="Age" | feat_imp_df$feature=="EstadioN"| feat_imp_df$feature=="EstadioT", "", 
                                                  ifelse(feat_imp_df$feature=="SurfaceArea" | feat_imp_df$feature=="Volume" | feat_imp_df$feature=="MajorAxisLength" | feat_imp_df$feature=="Eccentricity" | feat_imp_df$feature=="FractalDiffraction", "Shape", "Texture")))
feat_imp_df <- feat_imp_df %>% filter(Grupo=="Shape" | Grupo=="Texture") 

###Change tittle according to the figure you want to do!!
ggplot(feat_imp_df, aes(x = reorder(feature, X.IncMSE), y = X.IncMSE, fill=Grupo)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme_bw()+
  guides(fill=F)+
  labs(x="", y="Importance of the variable based in \n prediction error of the model", title = "LCC")+
  scale_fill_manual(name="",values=c(Texture="blue", Shape="red"))+
  theme(plot.title=element_text(size=12, face="bold"), 
        axis.title=element_text(size=8))



#You can do this graph adding a random variable 
#Then you can see how important is a variable (more or less than random)
#####################################################
#####################################################
feat_imp_df2<- importance(modelo.entrenado2) %>% 
  data.frame() %>% 
  mutate(feature = row.names(.))
feat_imp_df2<-feat_imp_df2 %>% mutate(Grupo=ifelse(feat_imp_df2$feature=="random", "random", "non-random"))

ggplot(feat_imp_df2, aes(x = reorder(feature, IncNodePurity), y = IncNodePurity, fill=Grupo)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme_classic() +
  scale_fill_viridis_d()+
  labs(x="", y="Variable importance",title = "TNM-I")
