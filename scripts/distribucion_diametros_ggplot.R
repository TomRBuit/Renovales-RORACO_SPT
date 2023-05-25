
#librarias necesarias
library(tidyverse)
library(readxl)

## cargamos la base de datos "renovales_SPT_H"

df <- read_excel("D:/Github/Renovales-RORACO_SPT/datos/renovales_SPT_H.xlsx")

## graficos de distribución de diamétros para djuan;

df_dj<- df %>%
  drop_na(dap) %>% filter(dap>=5) %>%
  filter(rodal=="ElHermoso")%>% 
  mutate(clases=cut(dap,5*c(1:ceiling(max(dap)/5)),FALSE,include.lowest=TRUE),
         intervalos=cut(dap,5*c(1:ceiling(max(dap)/5)),include.lowest=TRUE),
         clases=as.numeric(clases)*5+2.5) %>% 
  mutate(spp2=case_when(spp != "na" & spp!= "lp" & spp != "sc" ~  "Otras",
                        spp == "na" ~ "Raulí",
                        spp == "lp" ~ "Tepa",
                        spp == "sc" ~ "Mañío h.c.")) %>% 
             group_by(spp2,plot,clases,rodal) %>% 
              summarise(densidad=n()*(10000/1500)) %>% 
             group_by(spp2,clases) %>% 
             summarise(densidad_ha=sum(densidad)) 
##crear los niveles de la variable spp
df_dj$spp2<-factor(df_dj$spp2,levels = c("Raulí","Tepa","Mañío h.c.","Otras"))
## creamos la figura para dj
dj<-ggplot(df_dj,aes(x=clases,y=densidad_ha,pattern=spp2,fill=spp2))+
         geom_col(position = position_dodge2(reverse=FALSE,preserve = "single"),color="black",linewidth = .2,width = 4)+
  scale_x_continuous(expand = c(0,0), breaks = seq(5,75,5),limits = c(0,75))+
  scale_y_continuous(expand = c(0,0), breaks = seq(0,250,50),limits = c(0,250))+
  scale_fill_manual(values = c("#000000","#666666","#CCCCCC","#FFFFFF"))+
  labs(x="Clase diamétrica (cm)",
       y=expression(paste("Densidad (árb ","ha"^-1,")")))+
   guides(fill=guide_legend(title="Especie")) +
   theme_bw(base_line_size=.2)+
  theme(legend.position = c(0.85,0.6),
        legend.margin = margin(c(5, 5, 5, 0)),
        axis.text=element_text(size=6,colour="black",family = "times"),
        axis.title.x = element_text(size=7,colour="black",family = "times"),
        axis.title.y=element_text(size=7,colour="black",family = "times"),
        legend.text = element_text(size = 6,colour="black",family="times"),
        legend.title = element_text(size = 6,colour="black",family="times"),
        legend.key.size = unit(0.2,"cm"),
        axis.line = element_line(size = .1,colour="black"),
        axis.ticks = element_line(size=.3,colour="black"))
dj
##exportar figura
ggsave('dj.jpg',dj , device= "jpg",height = 5,width = 10,units = "cm", dpi = 300)

## Lo mismo para "el hermoso"

df_eh<- df %>%
  drop_na(dap) %>% filter(dap>=5) %>%
  filter(rodal=="ElHermoso")%>% 
  mutate(clases=cut(dap,5*c(1:ceiling(max(dap)/5)),FALSE,include.lowest=TRUE),
         intervalos=cut(dap,5*c(1:ceiling(max(dap)/5)),include.lowest=TRUE),
         clases=as.numeric(clases)*5+2.5) %>% 
  mutate(spp2=case_when(spp != "na" & spp!= "lp" & spp != "nd" ~  "Otras",
                        spp == "na" ~ "Raulí",
                        spp == "lp" ~ "Tepa",
                        spp == "nd" ~ "Coihue"))%>% 
  group_by(spp2,plot,clases,rodal) %>% 
  summarise(densidad=n()*(10000/1500)) %>% 
  group_by(spp2,clases) %>% 
  summarise(densidad_ha=sum(densidad)) 
##crear los niveles de la variable spp
df_eh$spp2<-factor(df_eh$spp2,levels = c("Raulí","Tepa","Coihue","Otras"))
## creamos la figura para dj
eh<-ggplot(df_eh,aes(x=clases,y=densidad_ha,pattern=spp2,fill=spp2))+
  geom_col(position = position_dodge2(reverse=FALSE,preserve = "single"),color="black",linewidth = .2,width = 4)+
  scale_x_continuous(expand = c(0,0), breaks = seq(5,85,5),limits = c(0,85))+
  scale_y_continuous(expand = c(0,0), breaks = seq(0,250,50),limits = c(0,250))+
  scale_fill_manual(values = c("#000000","#666666","#CCCCCC","#FFFFFF"))+
  labs(x="Clase diamétrica (cm)",
       y=expression(paste("Densidad (árb ","ha"^-1,")")))+
  guides(fill=guide_legend(title="Especie")) +
  theme_bw(base_line_size=.2)+
  theme(legend.position = c(0.85,0.6),
        legend.margin = margin(c(5, 5, 5, 0)),
        axis.text=element_text(size=6,colour="black",family = "times"),
        axis.title.x = element_text(size=7,colour="black",family = "times"),
        axis.title.y=element_text(size=7,colour="black",family = "times"),
        legend.text = element_text(size = 6,colour="black",family="times"),
        legend.title = element_text(size = 6,colour="black",family="times"),
        legend.key.size = unit(0.2,"cm"),
        axis.line = element_line(size = .1,colour="black"),
        axis.ticks = element_line(size=.3,colour="black"))
eh
##exportar figura
ggsave('eh.jpg',eh , device= "jpg",height = 5,width = 10,units = "cm", dpi = 300)

## Lo mismo para "Vertederos"

df_vert<- df %>%
  drop_na(dap) %>% filter(dap>=5) %>%
  filter(rodal=="Vertederos")%>% 
  mutate(clases=cut(dap,5*c(1:ceiling(max(dap)/5)),FALSE,include.lowest=TRUE),
         intervalos=cut(dap,5*c(1:ceiling(max(dap)/5)),include.lowest=TRUE),
         clases=as.numeric(clases)*5+2.5) %>% 
  mutate(spp2=case_when(spp != "na" & spp!= "lp" & spp != "no" ~  "Otras",
                        spp == "na" ~ "Raulí",
                        spp == "lp" ~ "Tepa",
                        spp == "no" ~ "Roble")) %>% 
  group_by(spp2,plot,clases,rodal) %>% 
  summarise(densidad=n()*(10000/2000)) %>% 
  group_by(spp2,clases) %>% 
  summarise(densidad_ha=sum(densidad)) 
##crear los niveles de la variable spp
df_vert$spp2<-factor(df_vert$spp2,levels = c("Raulí","Tepa","Roble","Otras"))
## creamos la figura para dj
vert<-ggplot(df_vert,aes(x=clases,y=densidad_ha,pattern=spp2,fill=spp2))+
  geom_col(position = position_dodge2(reverse=FALSE,preserve = "single"),color="black",linewidth = .2,width = 4)+
  scale_x_continuous(expand = c(0,0), breaks = seq(5,80,5),limits = c(0,80))+
  scale_y_continuous(expand = c(0,0), breaks = seq(0,450,50),limits = c(0,450))+
  scale_fill_manual(values = c("#000000","#666666","#CCCCCC","#FFFFFF"))+
  labs(x="Clase diamétrica (cm)",
       y=expression(paste("Densidad (árb ","ha"^-1,")")))+
  guides(fill=guide_legend(title="Especie")) +
  theme_bw(base_line_size=.2)+
  theme(legend.position = c(0.85,0.6),
        legend.margin = margin(c(5, 5, 5, 0)),
        axis.text=element_text(size=6,colour="black",family = "times"),
        axis.title.x = element_text(size=7,colour="black",family = "times"),
        axis.title.y=element_text(size=7,colour="black",family = "times"),
        legend.text = element_text(size = 6,colour="black",family="times"),
        legend.title = element_text(size = 6,colour="black",family="times"),
        legend.key.size = unit(0.2,"cm"),
        axis.line = element_line(size = .1,colour="black"),
        axis.ticks = element_line(size=.3,colour="black"))
vert
##exportar figura
ggsave('vert.jpg', vert , device= "jpg",height = 5,width = 10,units = "cm", dpi = 300)
