library(tidyverse)
library(readxl)
library(glue)
library(ggtext)
library(datana)
library(openxlsx)
library(readr)

## tesina Emilio Saavedra 2023
## Calculo de parametros de rodal para el renoval frente a la casa de Don Juan Silva

renSPT <- read_excel("D:/Github/Renovales-RORACO_SPT/datos/renovales_SPT_H.xlsx")

# Tabla rodal
djuan <- renSPT %>%  
  drop_na(dap) %>%filter(rodal== "Djuan", dap>=5, spp != "muerto") %>% 
  mutate(clases=cut(dap,5*c(1:ceiling(max(dap)/5)),FALSE,include.lowest=TRUE),
         intervalos=cut(dap,5*c(1:ceiling(max(dap)/5)),include.lowest=TRUE),
         clases=as.numeric(clases)*5+2.5) %>% 
  group_by(clases,intervalos,spp) %>% 
  summarise(frequency = n()*(10000/1500)) %>% mutate(intervalos=as.character(levels(intervalos))[intervalos]) %>% 
  ungroup() %>% 
  select(clases,intervalos,spp,frequency) %>% 
  tidyr::spread(spp,frequency) %>% 
  mutate_all(~replace(., is.na(.), 0)) 
# Aqui a?adimos la sumatoria de las columnas 
total_djuan<-djuan %>% mutate(clases=as.character(clases)) %>% select_if(sapply(., is.numeric)) %>%
  colSums()
djuan <-bind_rows(djuan,total_djuan)
djuan[dim(djuan)[1],2]<-as.character("Total")
#para guardar
write.xlsx(djuan,"C:/Users/56979/Desktop/FONDECYT 1210147/TESISTAS/Emilio Saavedra_2023/tr_renovalDJuan.xlsx")

##Codigo 2-. Resumen de la variable "DAP", Area basal(m2/ha),densidad (arb/ha),"DMC", por parcela
## variables modificable DAP="DBH", variable parcela "plot", factor de expansi?n= 5 modificable l?nea 35.

resumen <- renSPT %>% 
  drop_na(dap) %>%filter(rodal == "Djuan", dap>=5, spp != "muerto", s != "MP") %>%
  mutate(clases=cut(dap,5*c(1:ceiling(max(dap)/5)),FALSE,include.lowest=TRUE),
         intervalos=cut(dap,5*c(1:ceiling(max(dap)/5)),include.lowest=TRUE),
         clases=as.numeric(clases)*5+2.5) %>% 
  mutate(AB=(pi/40000)*(clases*clases)) %>% 
  group_by(rodal,plot) %>%
  summarise(mediaDAP=mean(dap),Narb_ha=n()*20,  AB_ha=sum(AB)*20,DMC=sqrt((AB_ha/Narb_ha)*(40000/pi)), Vol_ha=sum(v)*20) %>%
  round(1) %>% view()

##codigo 3.- Resumen del rodal(con en total promediado por parcelas):Area basal(m2/ha),densidad (arb/ha),Valor importancia (%), DMC (cm)
## en la linea 131 es modificable el plot

Vi <- renSPT %>% 
  drop_na(dap) %>%filter(dap>=5, rodal == "ElHermoso", spp!="muerto", s!= "MP") %>% 
  mutate(clases=cut(dap,5*c(1:ceiling(max(dap)/5)),FALSE,include.lowest=TRUE),
         intervalos=cut(dap,5*c(1:ceiling(max(dap)/5)),include.lowest=TRUE),
         clases=as.numeric(clases)*5+2.5) %>% 
  group_by(clases, spp) %>% 
  summarise(frecuencia = n()) %>% 
  mutate(Nha=((frecuencia)),AB_ha=(pi/40000)*(clases*clases)*Nha) %>% 
  group_by(spp) %>% 
  summarise(Narb_ha=round(sum(Nha)*6.666667), AB_ha=round(sum(AB_ha)*6.666667,4)) %>% 
  mutate(VI=((Narb_ha/sum(Narb_ha))*100+(AB_ha/sum(AB_ha))*100)/2,DMC=sqrt((AB_ha/Narb_ha)*(40000/pi))) %>%  
  mutate(VI=(round(VI,3)),DMC=round(DMC,1)) %>% arrange(desc(VI)) 

##A?adir totales (suma densidad y area basal y media DMC) al resumen
Vi_totales<-Vi %>% summarise(spp="total",Narb_ha=round(sum(Narb_ha),1),AB_ha=round(sum(AB_ha),1),VI="100%",DMC=round(mean(DMC),1))
Vi<-rbind(Vi,Vi_totales)

write.xlsx(Vi,"C:/Users/56979/Desktop/FONDECYT 1210147/TESISTAS/Emilio Saavedra_2023/resumen_renovalVertederos.xlsx")
