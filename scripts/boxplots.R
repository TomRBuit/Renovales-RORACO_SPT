library(readxl)
library(tidyverse)

##boxplots de AB,N,V para la plantación de coihue y rauli

df <-  read_excel("D:/Github/Renovales-RORACO_SPT/datos/renovales_SPT_H.xlsx") %>% 
      group_by(rodal,plot) %>% 
      summarise(Densidad_ha=n()*20,AB_ha=(sum(ab)*20),Vol_ha=sum(v)*20)
#para guardar
write.xlsx(df,"D:/Github/Renovales-RORACO_SPT/datos/resumen_SPT.xlsx")

#boxplot área basal
ab<-ggplot(df,aes(x=rodal,y=AB_ha),colour=rodal)+
  stat_boxplot(geom ='errorbar',width=0.4,size=.08)+
  geom_boxplot(size=.3,width=.9,colour="black",outlier.shape = NA)+
  geom_point(shape =1,color="black",size = 1)+
  labs(x="Rodal",
       y=expression(paste("Área Basal (m "^2,"ha"^-1,")")))+
  theme_bw()+
  theme(axis.title.y=element_text(size=7,colour="black",family = "times"),
        axis.title.x=element_text(size=7,colour="black",family = "times"),
        axis.text.x = element_text(size=7,colour="black",family = "times"),
        axis.text.y = element_text(size=7,colour="black",family = "times"))+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,110))+
  scale_x_discrete(labels=c("Don Juan", "El Hermoso", "Vertederos"))

ab
ggsave('ab_ha.png',ab , device= "png",height = 5,width = 8,units = "cm", dpi = 300)

#boxplot densidad

den<- ggplot(df,aes(x=rodal,y=Densidad_ha),colour=rodal)+
  stat_boxplot(geom ='errorbar',width=0.4,size=.08)+
  geom_boxplot(size=.3,width=.9,colour="black",outlier.shape = NA)+
  geom_point(shape =1,color="black",size = 1)+
  labs(x="Rodal",
       y=expression(paste("Densidad (Árboles ","ha"^-1,")")))+
  theme_bw()+
  theme(axis.title.y=element_text(size=7,colour="black",family = "times"),
        axis.title.x=element_text(size=7,colour="black",family = "times"),
        axis.text.x = element_text(size=7,colour="black",family = "times"),
        axis.text.y = element_text(size=7,colour="black",family = "times"))+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,2400)) +
  scale_x_discrete(labels=c("Don Juan", "El Hermoso" , "Vertederos"))
den

ggsave('den_ha.png',den , device= "png",height = 5,width = 8,units = "cm", dpi = 300)

#boxplot volumen

vol<-ggplot(df,aes(x=rodal,y=Vol_ha),colour=rodal)+
  stat_boxplot(geom ='errorbar',width=0.4,size=.08)+
  geom_boxplot(size=.3,width=.9,colour="black",outlier.shape = NA)+
  geom_point(shape =1,color="black",size = 1)+
  labs(x="Rodal",
       y=expression(paste("Volumen (m "^3,"ha"^-1,")")))+
  theme_bw()+
  scale_y_continuous( breaks = seq(0,1200,100),limits = c(0,1000))+
  theme(axis.title.y=element_text(size=7,colour="black",family = "times"),
        axis.title.x=element_text(size=7,colour="black",family = "times"),
        axis.text.x = element_text(size=7,colour="black",family = "times"),
        axis.text.y = element_text(size=7,colour="black",family = "times"))+
  scale_x_discrete(labels=c("Don Juan", "El Hermoso" , "Vertederos"))
vol
ggsave('vol_ha.png',vol , device= "png",height = 5,width = 8,units = "cm", dpi = 300)

write_excel_csv2(df,"resumen")

###########-------------------------------###########################################

##boxplots de las 4 especies más importantes

DF2 <- read_excel("D:/Github/Renovales-RORACO_SPT/datos/renovales_SPT_H.xlsx") %>%
       filter(rodal =="Djuan") %>% 
       mutate(spp2=case_when(spp != "na" & spp!= "lp" & spp != "sc" ~  "Otras",
                            spp == "na" ~ "Raulí",
                            spp == "lp" ~ "Tepa",
                            spp == "sc" ~ "Mañío h.c.")) %>% 
  group_by(rodal,plot,spp2) %>% 
  summarise(Densidad_ha=n()*20,AB_ha=sum(ab)*20,Vol_ha=sum(v)*20,DMC=sqrt((AB_ha/Densidad_ha)*(40000/pi)))

DF3 <- read_excel("D:/Github/Renovales-RORACO_SPT/datos/renovales_SPT_H.xlsx") %>%
  filter(rodal =="ElHermoso") %>% 
  mutate(spp2=case_when(spp != "na" & spp!= "lp" & spp != "nd" ~  "Otras",
                        spp == "na" ~ "Raulí",
                        spp == "lp" ~ "Tepa",
                        spp == "nd" ~ "Coihue")) %>% 
  group_by(rodal,plot,spp2) %>% 
  summarise(Densidad_ha=n()*20,AB_ha=sum(ab)*20,Vol_ha=sum(v)*20,DMC=sqrt((AB_ha/Densidad_ha)*(40000/pi)))

DF4 <- read_excel("D:/Github/Renovales-RORACO_SPT/datos/renovales_SPT_H.xlsx") %>%
  filter(rodal =="Vertederos") %>% 
  mutate(spp2=case_when(spp != "na" & spp!= "lp" & spp != "no" ~  "Otras",
                        spp == "na" ~ "Raulí",
                        spp == "lp" ~ "Tepa",
                        spp == "no" ~ "Roble")) %>% 
  group_by(rodal,plot,spp2) %>% 
  summarise(Densidad_ha=n()*20,AB_ha=sum(ab)*20,Vol_ha=sum(v)*20,DMC=sqrt((AB_ha/Densidad_ha)*(40000/pi)))

##boxplot área basal (se modifica el nombre y base de datos de acuerdo con el rodal para no repetir script para cada boxplot)
  vert_ab_spp<-ggplot(DF4,aes(x=reorder(spp2,-AB_ha),y=AB_ha))+
  stat_boxplot(geom ='errorbar',width=0.2,size=.08)+
  geom_boxplot(size=.2,width=.5,colour="black",outlier.shape = NA)+
  geom_point(shape =1,color="black",size = 1)+
    labs(x="Especies principales",
         y=expression(paste("Área Basal (m "^2,"ha"^-1,")")))+
    theme_bw()+
    scale_y_continuous( breaks = seq(0,60,5),limits = c(0,60))+
    theme(axis.title.y=element_text(size=7,colour="black",family = "times"),
          axis.title.x=element_text(size=7,colour="black",family = "times"),
          axis.text.x = element_text(size=7,colour="black",family = "times"),
          axis.text.y = element_text(size=7,colour="black",family = "times"))
  dj_ab_spp
  eh_ab_spp
  vert_ab_spp

    ggsave('vert_ab_spp.png',vert_ab_spp , device= "png",height = 5,width = 7,units = "cm", dpi = 300)
  
##boxplot densidad 
  
  vert_n_spp<-ggplot(DF4,aes(x=reorder(spp2,-Densidad_ha),y=Densidad_ha))+
    stat_boxplot(geom ='errorbar',width=0.2,size=.08)+
    geom_boxplot(size=.2,width=.5,colour="black",outlier.shape = NA)+
    geom_point(shape =1,color="black",size = 1)+
    labs(x="Especies principales",
         y=expression(paste("Densidad (Árboles ","ha"^-1,")")))+
    theme_bw()+
    scale_y_continuous( breaks = seq(0,1000,100),limits = c(0,1000))+
    theme(axis.title.y=element_text(size=7,colour="black",family = "times"),
          axis.title.x=element_text(size=7,colour="black",family = "times"),
          axis.text.x = element_text(size=7,colour="black",family = "times"),
          axis.text.y = element_text(size=7,colour="black",family = "times"))
  
  dj_n_spp
  eh_n_spp
  vert_n_spp
  ggsave('vert_n_spp.png',vert_n_spp , device= "png",height = 5,width = 7,units = "cm", dpi = 300)
    

  ##boxplot volumen 
  
 vert_vol_spp<- ggplot(DF4,aes(x=reorder(spp2,-Vol_ha),y=Vol_ha))+
    stat_boxplot(geom ='errorbar',width=0.2,size=.08)+
    geom_boxplot(size=.2,width=.5,colour="black",outlier.shape = NA)+
    geom_point(shape =1,color="black",size = 1)+
    labs(x="Especies principales",
         y=expression(paste("Volumen (m "^3,"ha"^-1,")")))+
    theme_bw()+
    scale_y_continuous( breaks = seq(0,700,100),limits = c(0,700))+
    theme(axis.title.y=element_text(size=7,colour="black",family = "times"),
          axis.title.x=element_text(size=7,colour="black",family = "times"),
          axis.text.x = element_text(size=7,colour="black",family = "times"),
          axis.text.y = element_text(size=7,colour="black",family = "times"))
 
 dj_vol_spp
 eh_vol_spp
 vert_vol_spp
 
 
  ggsave(' vert_vol_spp.png', vert_vol_spp, device= "png",height = 5,width = 7,units = "cm", dpi = 300)

  ###boxplot dmc
  
  vert_dmc_spp<-ggplot(DF4,aes(x=reorder(spp2,-DMC),y=DMC))+
    stat_boxplot(geom ='errorbar',width=0.2,size=.08)+
    geom_boxplot(size=.2,width=.5,colour="black",outlier.shape = NA)+
    geom_point(shape =1,color="black",size = 1)+
    labs(x="Especies principales",
         y="DMC (cm)")+
    theme_bw()+
    scale_y_continuous( breaks = seq(0,40,10),limits = c(0,40))+
    theme(axis.title.y=element_text(size=7,colour="black",family = "times"),
          axis.title.x=element_text(size=7,colour="black",family = "times"),
          axis.text.x = element_text(size=7,colour="black",family = "times"),
          axis.text.y = element_text(size=7,colour="black",family = "times"))
  
  dj_dmc_spp
  eh_dmc_spp
  vert_dmc_spp  
  
  ggsave('vert_dmc_spp.png',  vert_dmc_spp, device= "png",height = 5,width = 7,units = "cm", dpi = 300)
    

    