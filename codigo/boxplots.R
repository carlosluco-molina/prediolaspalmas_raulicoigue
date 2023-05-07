library(readxl)
library(tidyverse)

##boxplots de AB,N,V para la plantación de coihue y rauli

df <-  read_excel("C:/Users/carlo/Desktop/laspalmas_raulicoigue/prediolaspalmas_raulicoigue/datos/datos_con_volumen.xlsx") %>% 
      mutate(ab=pi/40000*dap^2) %>% 
      filter(is.na(obs) | obs!="muerto") #%>% 
      group_by(plantacion,plot) %>% 
      summarise(Densidad_ha=n()*20,AB_ha=sum(ab)*20,Vol_ha=sum(vol)*20)

#boxplot área basal
ab<-ggplot(df,aes(x=plantacion,y=AB_ha),colour=plantacion)+
  stat_boxplot(geom ='errorbar',width=0.4,size=.08)+
  geom_boxplot(size=.3,width=.9,colour="black",outlier.shape = NA)+
  geom_point(shape =1,color="black",size = 1)+
  labs(x="Plantación",
       y=expression(paste("Área Basal (m "^2,"ha"^-1,")")))+
  theme_bw()+
  theme(axis.title.y=element_text(size=7,colour="black",family = "times"),
        axis.title.x=element_text(size=7,colour="black",family = "times"),
        axis.text.x = element_text(size=7,colour="black",family = "times"),
        axis.text.y = element_text(size=7,colour="black",family = "times"))+
  scale_x_discrete(labels=c("Coihue", "Raulí"))
ggsave('ab_ha.png',ab , device= "png",height = 5,width = 5,units = "cm", dpi = 300)

#boxplot densidad

den<-ggplot(df,aes(x=plantacion,y=Densidad_ha),colour=plantacion)+
  stat_boxplot(geom ='errorbar',width=0.4,size=.08)+
  geom_boxplot(size=.3,width=.9,colour="black",outlier.shape = NA)+
  geom_point(shape =1,color="black",size = 1)+
  labs(x="Plantación",
       y=expression(paste("Densidad (Árboles ","ha"^-1,")")))+
  theme_bw()+
  theme(axis.title.y=element_text(size=7,colour="black",family = "times"),
        axis.title.x=element_text(size=7,colour="black",family = "times"),
        axis.text.x = element_text(size=7,colour="black",family = "times"),
        axis.text.y = element_text(size=7,colour="black",family = "times"))+
  scale_x_discrete(labels=c("Coihue", "Raulí"))
ggsave('den_ha.png',den , device= "png",height = 5,width = 5,units = "cm", dpi = 300)

#boxplot volumen

vol<-ggplot(df,aes(x=plantacion,y=Vol_ha),colour=plantacion)+
  stat_boxplot(geom ='errorbar',width=0.4,size=.08)+
  geom_boxplot(size=.3,width=.9,colour="black",outlier.shape = NA)+
  geom_point(shape =1,color="black",size = 1)+
  labs(x="Plantación",
       y=expression(paste("Volumen (m "^3,"ha"^-1,")")))+
  theme_bw()+
  scale_y_continuous( breaks = seq(300,700,100),limits = c(300,700))+
  theme(axis.title.y=element_text(size=7,colour="black",family = "times"),
        axis.title.x=element_text(size=7,colour="black",family = "times"),
        axis.text.x = element_text(size=7,colour="black",family = "times"),
        axis.text.y = element_text(size=7,colour="black",family = "times"))+
  scale_x_discrete(labels=c("Coihue", "Raulí"))
ggsave('vol_ha.png',vol , device= "png",height = 5,width = 5,units = "cm", dpi = 300)

write_excel_csv2(df,"resumen")

###########-------------------------------###########################################

##boxplot coihue pero de las 4 especies más importantes

DF2 <- read_excel("C:/Users/carlo/Desktop/laspalmas_raulicoigue/prediolaspalmas_raulicoigue/datos/datos_con_volumen.xlsx") %>%
       filter(plantacion=="Coihue", is.na(obs) | obs!="muerto") %>% 
       mutate(spp=case_when(spp != "Coihue" & spp!= "Avellano" & spp != "Olivillo" ~  "Otras",
                            spp == "Coihue" ~ "Coihue",
                            spp == "Avellano" ~ "Avellano",
                            spp == "Olivillo" ~ "Olivillo")) %>% 
  mutate(ab=pi/40000*dap^2) %>% 
group_by(plantacion,plot,spp) %>% 
  summarise(Densidad_ha=n()*20,AB_ha=sum(ab)*20,Vol_ha=sum(vol)*20,DMC=sqrt((AB_ha/Densidad_ha)*(40000/pi)))

##boxplot área basal coihue
  ab_spp<-ggplot(DF2,aes(x=reorder(spp,-AB_ha),y=AB_ha))+
  stat_boxplot(geom ='errorbar',width=0.2,size=.08)+
  geom_boxplot(size=.2,width=.5,colour="black",outlier.shape = NA)+
  geom_point(shape =1,color="black",size = 1)+
    labs(x="Especies principales",
         y=expression(paste("Área Basal (m "^2,"ha"^-1,")")))+
    theme_bw()+
    scale_y_continuous( breaks = seq(0,45,5),limits = c(0,45))+
    theme(axis.title.y=element_text(size=7,colour="black",family = "times"),
          axis.title.x=element_text(size=7,colour="black",family = "times"),
          axis.text.x = element_text(size=7,colour="black",family = "times"),
          axis.text.y = element_text(size=7,colour="black",family = "times"))
  ggsave('ab_spp.png',ab_spp , device= "png",height = 5,width = 7,units = "cm", dpi = 300)
  
##boxplot densidad coihue
  
  n_spp<-ggplot(DF2,aes(x=reorder(spp,-Densidad_ha),y=Densidad_ha))+
    stat_boxplot(geom ='errorbar',width=0.2,size=.08)+
    geom_boxplot(size=.2,width=.5,colour="black",outlier.shape = NA)+
    geom_point(shape =1,color="black",size = 1)+
    labs(x="Especies principales",
         y=expression(paste("Densidad (Árboles ","ha"^-1,")")))+
    theme_bw()+
    scale_y_continuous( breaks = seq(0,550,100),limits = c(0,550))+
    theme(axis.title.y=element_text(size=7,colour="black",family = "times"),
          axis.title.x=element_text(size=7,colour="black",family = "times"),
          axis.text.x = element_text(size=7,colour="black",family = "times"),
          axis.text.y = element_text(size=7,colour="black",family = "times"))
  ggsave('n_spp.png',n_spp , device= "png",height = 5,width = 7,units = "cm", dpi = 300)
    

  ##boxplot volumen coihue
  
 vol_spp<- ggplot(DF2,aes(x=reorder(spp,-Vol_ha),y=Vol_ha))+
    stat_boxplot(geom ='errorbar',width=0.2,size=.08)+
    geom_boxplot(size=.2,width=.5,colour="black",outlier.shape = NA)+
    geom_point(shape =1,color="black",size = 1)+
    labs(x="Especies principales",
         y=expression(paste("Volumen (m "^3,"ha"^-1,")")))+
    theme_bw()+
    scale_y_continuous( breaks = seq(0,550,100),limits = c(0,550))+
    theme(axis.title.y=element_text(size=7,colour="black",family = "times"),
          axis.title.x=element_text(size=7,colour="black",family = "times"),
          axis.text.x = element_text(size=7,colour="black",family = "times"),
          axis.text.y = element_text(size=7,colour="black",family = "times"))
  ggsave(' vol_spp.png', vol_spp, device= "png",height = 5,width = 7,units = "cm", dpi = 300)

  ###boxplot dmc coihue
  
  dmc_spp<-ggplot(DF2,aes(x=reorder(spp,-DMC),y=DMC))+
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
    ggsave('  dmc_spp.png',  dmc_spp, device= "png",height = 5,width = 7,units = "cm", dpi = 300)
    
#################------------------------#######################################################
    
##boxplots de las variables dasometricas de las 4 especies mas importantes de rauli 

    DF3 <- read_excel("C:/Users/carlo/Desktop/laspalmas_raulicoigue/prediolaspalmas_raulicoigue/datos/datos_con_volumen.xlsx") %>%
      filter(plantacion=="Rauli", is.na(obs) | obs!="muerto") %>% 
      mutate(spp=case_when(spp != "Rauli" & spp!= "Laurel" & spp != "Coihue" ~  "Otras",
                           spp == "Coihue" ~ "Coihue",
                           spp == "Rauli" ~ "Rauli",
                           spp == "Laurel" ~ "Laurel")) %>% 
      mutate(ab=pi/40000*dap^2) %>% 
      group_by(plantacion,plot,spp) %>% 
      summarise(Densidad_ha=n()*20,AB_ha=sum(ab)*20,Vol_ha=sum(vol)*20,DMC=sqrt((AB_ha/Densidad_ha)*(40000/pi)))
  
    ##boxplot área basal rauli
ab_rauli<-ggplot(DF3,aes(x=reorder(spp,-AB_ha),y=AB_ha))+
      stat_boxplot(geom ='errorbar',width=0.2,size=.08)+
      geom_boxplot(size=.2,width=.5,colour="black",outlier.shape = NA)+
      geom_point(shape =1,color="black",size = 1)+
      labs(x="Especies principales",
           y=expression(paste("Área Basal (m "^2,"ha"^-1,")")))+
      theme_bw()+
      scale_y_continuous( breaks = seq(0,45,5),limits = c(0,45))+
      theme(axis.title.y=element_text(size=7,colour="black",family = "times"),
            axis.title.x=element_text(size=7,colour="black",family = "times"),
            axis.text.x = element_text(size=7,colour="black",family = "times"),
            axis.text.y = element_text(size=7,colour="black",family = "times"))
    ggsave('ab_rauli.jpg',ab_rauli , device= "jpg",height = 5,width = 7,units = "cm", dpi = 300)
    
    ##boxplot densidad rauli
    
d_rauli<-ggplot(DF3,aes(x=reorder(spp,-Densidad_ha),y=Densidad_ha))+
      stat_boxplot(geom ='errorbar',width=0.2,size=.08)+
      geom_boxplot(size=.2,width=.5,colour="black",outlier.shape = NA)+
      geom_point(shape =1,color="black",size = 1)+
      labs(x="Especies principales",
           y=expression(paste("Densidad (Árboles ","ha"^-1,")")))+
      theme_bw()+
      scale_y_continuous( breaks = seq(0,750,100),limits = c(0,750))+
      theme(axis.title.y=element_text(size=7,colour="black",family = "times"),
            axis.title.x=element_text(size=7,colour="black",family = "times"),
            axis.text.x = element_text(size=7,colour="black",family = "times"),
            axis.text.y = element_text(size=7,colour="black",family = "times"))
ggsave('d_rauli.jpg',d_rauli, device= "jpg",height = 5 , width = 7,units = "cm", dpi = 300)
    
    
##boxplot volumen rauli
    
 vol_rauli<- ggplot(DF3,aes(x=reorder(spp,-Vol_ha),y=Vol_ha))+
      stat_boxplot(geom ='errorbar',width=0.2,size=.08)+
      geom_boxplot(size=.2,width=.5,colour="black",outlier.shape = NA)+
      geom_point(shape =1,color="black",size = 1)+
      labs(x="Especies principales",
           y=expression(paste("Volumen (m "^3,"ha"^-1,")")))+
      theme_bw()+
      scale_y_continuous( breaks = seq(0,550,100),limits = c(0,550))+
      theme(axis.title.y=element_text(size=7,colour="black",family = "times"),
            axis.title.x=element_text(size=7,colour="black",family = "times"),
            axis.text.x = element_text(size=7,colour="black",family = "times"),
            axis.text.y = element_text(size=7,colour="black",family = "times"))
    ggsave(' vol_rauli.jpg', vol_rauli, device= "jpg",height = 5,width = 7,units = "cm", dpi = 300)
    
    ###boxplot dmc rauli
    
    dmc_rauli<-ggplot(DF3,aes(x=reorder(spp,-DMC),y=DMC))+
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
    ggsave('dmc_rauli.jpg',   dmc_rauli, device= "jpg",height = 5,width = 7,units = "cm", dpi = 300)
########################################_---------------------------##############################################
    