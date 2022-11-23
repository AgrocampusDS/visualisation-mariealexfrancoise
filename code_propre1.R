
############ ---- LIBRARY ----------------

library(ggplot2)
library(tidyverse)
library(ggsci)
library(leaflet)
library(factoextra)
library(plotly)
library(Factoshiny)
library(FactoMineR)
library(egg)
library(ggpubr)
library(patchwork)
library(viridis)
library(wesanderson)




############ ---- IMPORTATION ET PRE TRAITEMENT ----------------

terrorism <- read.csv("terrorism_dataset.csv",sep=";") # 181 690 observations

# Supprimer NA sur latitude longitude
idx_NA_lat <- which(is.na(terrorism$latitude)==TRUE)
idx_NA_lon <- which(is.na(terrorism$longitude)==TRUE)
idx_NA <- unique(c(idx_NA_lat,idx_NA_lon))
terrorism <- terrorism[-idx_NA,] # 177 133 observations

# Mettre en facteur :
terrorism$attacktype1_txt <- as.factor(terrorism$attacktype1_txt)
levels(terrorism$attacktype1_txt)
terrorism$region_txt <- as.factor(terrorism$region_txt)
levels(terrorism$region_txt)
terrorism$iyear <- as.factor(terrorism$iyear)
levels(terrorism$iyear)
terrorism$country_txt <- as.factor(terrorism$country_txt)
levels(terrorism$country_txt)

# Garder uniquement l'Europe de l'Ouest
idx_WE <- which(terrorism$region_txt=="Western Europe")
length(idx_WE)
terrorism_eu <- terrorism[idx_WE,] # 21 599 observations





############ ---- GRAPHE 1 : MAP ----------------

# Evolution du terrorisme en europe de l'est depuis 1970 et visualisation des groupes à risque aujourd'hui

# différents jeux de données pour les plots :
# En vert : pas de kill
terrorism_eu_nonkill <- terrorism_eu[which(terrorism_eu$nkill==0),]
# En bleu : pas de blessés
terrorism_eu_nonwound <- terrorism_eu[which(terrorism_eu$nwound==0),]
# En noir : NA kill
terrorism_eu_NA_kill <- terrorism[which(is.na(terrorism$nkill)==TRUE),]
terrorism_eu_NA_kill <- terrorism_eu_NA_kill[which(terrorism_eu_NA_kill$region_txt=="Western Europe"),]
# En gris : NA wound
terrorism_eu_NA_wound <- terrorism[which(is.na(terrorism$nwound)==TRUE),]
terrorism_eu_NA_wound <- terrorism_eu_NA_wound[which(terrorism_eu_NA_kill$region_txt=="Western Europe"),]
# En rouge : kill
terrorism_eu_kill <- terrorism_eu[which(terrorism_eu$nkill>=1),]
# En jaune : blessés
terrorism_eu_wound <- terrorism_eu[which(terrorism_eu$nwound>=1),]

# MAP PLOT AVEC LES POINTS SUR MAP MONDE

Color_Assets <- as.factor(c("green", "blue", "red", "black"))
info <- as.factor(c("No victim","Wounded","Fatalities", "No information"))

#initialisation
longline_map <- leaflet(terrorism_eu) %>% 
  setView(lng = mean(terrorism_eu$longitude), lat = mean(terrorism_eu$latitude), zoom = 4) %>% 
  addTiles()
longline_map

longline_map %>% addCircles(lng = terrorism_eu_nonkill$longitude, lat = terrorism_eu_nonkill$latitude, weight =(1), radius = 1,
                            color="green", popup = paste("Number of fatalities =",terrorism_eu_nonkill$nkill," in ",terrorism_eu$iyear, " by ", terrorism_eu$attacktype1_txt)) %>%
  addCircles(lng = terrorism_eu_nonwound$longitude, lat = terrorism_eu_nonwound$latitude, weight =(1),radius = 1,
             color="green", popup = paste("Number of wounded =",terrorism_eu_nonwound$nwound," in ",terrorism_eu$iyear,  " by ", terrorism_eu$attacktype1_txt)) %>%
  addCircles(lng = terrorism_eu_NA_kill$longitude, lat = terrorism_eu_NA_kill$latitude, weight=(1),radius = 1,
             color="black", popup = paste("Number of fatalities =",terrorism_eu_NA_kill$nkill," in ",terrorism_eu$iyear, " by ", terrorism_eu$attacktype1_txt)) %>%
  addCircles(lng = terrorism_eu_NA_wound$longitude, lat = terrorism_eu_NA_wound$latitude, weight=(1),radius = 1,
             color="black", popup = paste("Number of wounded =",terrorism_eu_NA_wound$nwound," in ",terrorism_eu$iyear,  " by ", terrorism_eu$attacktype1_txt)) %>%
  addCircles(lng = terrorism_eu_wound$longitude, lat = terrorism_eu_wound$latitude, weight=1 ,radius = 150*terrorism_eu_wound$nwound,
             color= "blue", fillOpacity = 0.1, popup = paste("Number of wounded =",terrorism_eu_wound$nwound," in ",terrorism_eu$iyear,  " by ", terrorism_eu$attacktype1_txt)) %>%
  addCircles(lng = terrorism_eu_kill$longitude, lat = terrorism_eu_kill$latitude, weight=1, radius = 150*terrorism_eu_kill$nkill,
             color="red", popup = paste("Number of fatalities =",terrorism_eu_kill$nkill," in ",terrorism_eu$iyear, " by ", terrorism_eu$attacktype1_txt))  %>%
  addLegend("bottomleft", colors = Color_Assets, labels = info, title = "Terrorist attacks in Western Europe between 1970 and 2017", opacity = 1)





############ ---- GRAPHE 2 : VISION TEMPORELLE ----------------

# On ne garde que les 3 variables attaques, année et pays :
df1 <- data.frame(attaque=terrorism$eventid,annee=terrorism$iyear,country=terrorism$country_txt)

# On regroupe par année et pays et on compte
df3 <- df1%>%
  group_by(annee,country)%>%
  count(attaque)
df3

# On fait un data frame pour voir le nombre d'attentat par pays
df2 <- as.data.frame.matrix(table(df1$annee,df1$country))
sumjour<-colSums(df2);sumjour

# Et on garde les 4 pays avec le plus d'attentats En Europe:
idx_country <- which((df3$country=="France")|(df3$country=="Italy")|(df3$country=="Spain")|(df3$country=="United Kingdom"))
df3 <- df3[idx_country,] 

class(df3$annee)
df3$annee <- as.factor(df3$annee)

# GGPLOT

options(warn=-1)

color_chosen <- c("red","blue","orange","purple")
library(scales)
#display ggplot2 default hex color codes from 1 to 8
for(i in 1:8){
  print(hue_pal()(i))
}
# We find the defaut colors for 4 factors "#F8766D" "#7CAE00" "#00BFC4" "#C77CFF"

data_breaks <- data.frame(start = c("1970","1990","2010"),  # Create data with breaks
                          end = c("1980","2000","2017"),
                          colors = c("gray90","gray90","gray90"))

# Je définis un jeu de données pour l'évènement test
#df4 <- df3[which(df3$country=="Italy"),][c(6:14),]

graph2 <- ggplot() +
  
  geom_rect(data=data_breaks,aes(xmin = start,
                                 xmax = end,
                                 ymin = 0,
                                 ymax = 310,
                                 fill = colors),alpha = 0.5) +
  geom_line(data=df3, aes(x = annee, y = n, group = country, color = country),size=1) + 
  annotate("segment", x = "1991", xend = "2002",y = 305, yend = 305,
           arrow = arrow(ends = "both", angle = 90),colour = "#F8766D") + 
  geom_text(aes(x="1997",y=310),label="Groupe islamiste armé", nudge_x = 0.05, nudge_y = 0.05, check_overlap = T,col="#F8766D")+
  annotate("segment", x = "1972", xend = "1980",y = 315, yend = 315,
           arrow = arrow(ends = "both", angle = 90),colour = "#7CAE00") + 
  geom_text(aes(x="1976",y=320),label="Années de plombs", nudge_x = 0.05, nudge_y = 0.05, check_overlap = T,col="#7CAE00")+
  annotate("segment", x = "1970", xend = "1998",y = 325, yend = 325,
           arrow = arrow(ends = "both", angle = 90),colour = "#C77CFF") + 
  geom_text(aes(x="1984",y=330),label="Les troubles, conflit nord-irlandais", nudge_x = 0.05, nudge_y = 0.05, check_overlap = T,col="#C77CFF")+
  annotate("segment", x = "1970", xend = "2011",y = 335, yend = 335,
           arrow = arrow(ends = "both", angle = 90),colour = "#00BFC4") + 
  geom_text(aes(x="1991",y=340),label="Conflit Basque (ETA)", nudge_x = 0.05, nudge_y = 0.05, check_overlap = T,col="#00BFC4")+
  annotate("segment", x = "2009", xend = "2017",y = 305, yend = 305,
           arrow = arrow(ends = "both", angle = 90),colour = "grey30") + 
  geom_text(aes(x="2013",y=310),label="Groupe islamiste Daesh", nudge_x = 0.05, nudge_y = 0.05, check_overlap = T,col="grey30")+
  
  
  #scale_color_manual(values=color_chosen)+
  #scale_color_jco()+
  
  # then details for presentation
  labs(y = "Count of terrorist attacks",x="Year",title="Count of terrorist attacks in 4 Western Europe countries by year and country between 1970 and 2017")+
  scale_x_discrete(breaks=c("1970","1975","1980","1985","1990","1995","2000","2005","2010","2015")) +
  scale_fill_identity()+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5,size=9))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

graph2
ggplotly(graph2)


options(warn=0)

############ ---- GRAPHE 3 : Barplot ----------------

# On ne garde que les 3 variables attaques, année et pays :
df4 <- data.frame(attaque=terrorism$eventid,cible=terrorism$targtype1_txt,country=terrorism$country_txt,region=terrorism$region_txt,mort=terrorism$nkill,blesse=terrorism$nwound)

# On se concentre sur les 4 pays d'Europe
df4 <- df4[which(df4$region=="Western Europe"),]

df5 <- df4[which((df4$country=="France")|(df4$country=="Italy")|(df4$country=="Spain")|(df4$country=="United Kingdom")),]
df5 <- df5[,2:3]
#df6 <- cbind(count = 1, df5)
df5$cible <- as.factor(df5$cible)

df5[df5 == ""] <- "Other"
df5[df5 == "Abortion Related"] <- "Other"
df5[df5 == "Food or Water Supply"] <- "Other"
df5[df5 == "Maritime"] <- "Other"
df5[df5 == "NGO"] <- "Other"
df5[df5 == "Unknown"] <- "Other"
df5[df5 == "Violent Political Party"] <- "Other"


theme_set(theme_classic())

# Histogram on a Categorical variable
df5 <- within(df5, 
              cible <- factor(cible, 
                              levels=names(sort(table(cible), 
                                                increasing=TRUE))))
g <- ggplot(df5, aes(cible))
g + geom_bar(aes(fill=country), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  coord_flip()+
  labs(title="Histogramme du type de cibles en fonction du pays", 
       subtitle="On compte le nombre d'attaques par cibles", y = "Number of attacks",x="Type of target") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

