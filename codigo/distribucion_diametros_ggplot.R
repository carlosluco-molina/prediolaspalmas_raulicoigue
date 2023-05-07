
#librarias necesarias
library(tidyverse)
library(readxl)

## cargamos la base de datos "datos_plantacion"

df <- read_excel("C:/Users/carlo/Desktop/laspalmas_raulicoigue/prediolaspalmas_raulicoigue/datos/datos_plantacion.xlsx")

## graficos de distribución de diamétros para coigue;

df<- df %>%  mutate(ab=pi/40000*dap^2) %>% 
  drop_na(dap) %>% filter(dap>=5) %>%
  filter(is.na(obs)| obs!="muerto", plantacion=="Coihue")%>% 
  mutate(clases=cut(dap,5*c(1:ceiling(max(dap)/5)),FALSE,include.lowest=TRUE),
         intervalos=cut(dap,5*c(1:ceiling(max(dap)/5)),include.lowest=TRUE),
         clases=as.numeric(clases)*5+2.5) %>% 
  mutate(spp=case_when(spp != "Coihue" & spp!= "Avellano" & spp != "Olivillo" ~  "Otras",
                                spp == "Coihue" ~ "Coihue",
                                spp == "Avellano" ~ "Avellano",
                                spp == "Olivillo" ~ "Olivillo")) %>% 
             group_by(spp,plot,clases,plantacion) %>% 
              summarise(densidad=n()*(10000/500)) %>% 
             group_by(spp,clases) %>% 
             summarise(densidad_ha=sum(densidad)/5) 
##crear los niveles de la variable spp
df$spp<-factor(df$spp,levels = c("Coihue","Olivillo","Avellano","Otras"))
## creamos la figura para coigue
a<-ggplot(df,aes(x=clases,y=densidad_ha,pattern=spp,fill=spp))+
         geom_col(position = position_dodge2(reverse=FALSE,preserve = "single"),color="black",linewidth = .2,width = 4)+
  scale_x_continuous(expand = c(0,0), breaks = seq(5,55,5),limits = c(0,55))+
  scale_y_continuous(expand = c(0,0), breaks = seq(0,160,25),limits = c(0,160))+
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
##exportar figura
ggsave('a.jpg',a , device= "jpg",height = 4.5,width = 9,units = "cm", dpi = 300)

###########----------------#########################

## Lo mismo para rauli

df1<-read_excel("C:/Users/carlo/Desktop/laspalmas_raulicoigue/prediolaspalmas_raulicoigue/datos/datos_plantacion.xlsx") %>% 
  mutate(ab=pi/40000*dap^2) %>% 
  drop_na(dap) %>% filter(dap>=5) %>%
  filter(is.na(obs)| obs!="muerto", plantacion=="Rauli")%>% 
  mutate(clases=cut(dap,5*c(1:ceiling(max(dap)/5)),FALSE,include.lowest=TRUE),
         intervalos=cut(dap,5*c(1:ceiling(max(dap)/5)),include.lowest=TRUE),
         clases=as.numeric(clases)*5+2.5) %>% 
  mutate(spp=case_when(spp != "Rauli" & spp!= "Laurel" & spp != "Coihue" ~  "Otras",
                       spp == "Rauli" ~ "Rauli",
                       spp == "Laurel" ~ "Laurel",
                       spp == "Coihue" ~ "Coihue")) %>% 
  group_by(spp,plot,clases,plantacion) %>% 
  summarise(densidad=n()*(10000/500)) %>% 
  group_by(spp,clases) %>% 
  summarise(densidad_ha=sum(densidad)/5) 
##crear los niveles de la variable spp
df1$spp<-factor(df1$spp,levels = c("Rauli","Laurel","Coihue","Otras"))

##Figura para raulí

b<-ggplot(df1,aes(x=clases,y=densidad_ha,pattern=spp,fill=spp))+
  geom_col(position = position_dodge2(reverse=FALSE,preserve = "single"),color="black",linewidth = .2,width = 4)+
  scale_x_continuous(expand = c(0,0), breaks = seq(5,55,5),limits = c(0,55))+
  scale_y_continuous(expand = c(0,0), breaks = seq(0,170,20),limits = c(0,170))+
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
##exportar figura
ggsave('b.jpg',b , device= "jpg",height = 4.5,width = 9,units = "cm", dpi = 300)
