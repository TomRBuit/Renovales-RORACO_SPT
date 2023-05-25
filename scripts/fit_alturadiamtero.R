library(MuMIn)
library(caret)
library(broom)
library(readxl)
library(tidyverse)


##cargamos la base de datos

df<- read_excel("D:/Github/Renovales-RORACO_SPT/datos/renovales_SPT.xlsx") %>%  filter(spp != "muerto", s != "MP")


##### Rodal DJuan  ####
##Raulí 
rodal1_dj_train <- read_excel("D:/Github/Renovales-RORACO_SPT/datos/renovales_SPT.xlsx") %>% 
                   filter(!is.na(ht), spp != "muerto", s != "MP")

# MODELOS A EVALUAR raulí en el rodal de Don Juan
#lineal simple
fit1_dj<-lm(ht~ dap,data = rodal1_dj_train )
fit2_dj<-lm(ht~dap+I(dap^2),data=rodal1_dj_train )
fit3_dj<-lm(ht~I(1/dap^2),data=rodal1_dj_train )
fit4_dj<-lm(I(log(ht))~I(log(dap)),data=rodal1_dj_train )
fit5_dj<-lm(I(log(ht))~-I(0.5*dap), data=rodal1_dj_train )
fit6_dj<-lm(I(log(ht))~I(1/sqrt(dap)),data = rodal1_dj_train )

##loop para meter los 6 modelos en un "tibble"

DF_dj<-tibble(Formula=rep(NA,6),Model=rep(NA,6),K=rep(NA,6),R_2_train=rep(NA,6), R_2_test=rep(NA,6), AICc=rep(NA,6))
DF_dj$Formula<-c("ht~dap","ht~dap+I(dap^2)","ht~I(1/dap)","I(log(ht))~I(log(dap))","I(log(ht))~I(0.5*dap)","I(log(ht))~I(1/sqrt(dap))")

for(i in 1:nrow(DF_dj)){
  DF_dj$Model[i]<-lm(as.formula(DF_dj$Formula[i]),data=rodal1_dj_train ) %>% list()
  DF_dj$R_2_train[i]<-DF_dj$Model[i][[1]] %>% glance() %>% pull(r.squared)
  DF_dj$K[i]<-DF_dj$Model[i][[1]] %>% glance() %>% pull(df)
  DF_dj$AICc[i]<-DF_dj$Model[i][[1]] %>% AICc
}

##filtro a la base de datos DJuan para predecir en función del fit2 para DAP >= 20

rodalDJuan_20a60 <- read_excel("D:/Github/Renovales-RORACO_SPT/datos/renovales_SPT.xlsx") %>% 
  filter(dap >= 20, spp != "muerto", s != "MP" ) 
rodalDJuan_20a60 <- rodalDJuan_20a60 %>% mutate(pred=predict(fit3_dj,newdata=rodalDJuan_20a60))

rodalDJuan_5a19 <- read_excel("D:/Github/Renovales-RORACO_SPT/datos/renovales_SPT.xlsx") %>% 
  filter(dap < 20, spp != "muerto", s != "MP" ) 
rodalDJuan_5a19 <- rodalDJuan_5a19 %>% mutate(pred=exp(predict(fit6_dj, newdata=rodalDJuan_5a19)))

##UNIR 2 BASES
renoval_SPT_con_H<-rbind(rodalDJuan_20a60,rodalDJuan_5a19)

#para guardar
write.xlsx(renoval_SPT_con_H,"D:/Github/Renovales-RORACO_SPT/datos/renovales_SPT_H.xlsx")

