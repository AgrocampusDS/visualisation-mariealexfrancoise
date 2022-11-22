
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




############ ---- IMPORTATION ET PRE TRAITEMENT ----------------

terrorism <- read.csv("terrorism_dataset.csv",sep=";") # 181 690 observations
#terrorism2 <- terrorism

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
terrorism_eu_NA_kill <- terrorism_eu[which(is.na(terrorism$nkill)==TRUE),]
# En gris : NA wound
terrorism_eu_NA_wound <- terrorism_eu[which(is.na(terrorism$nwound)==TRUE),]
# En rouge : kill
terrorism_eu_kill <- terrorism_eu[which(terrorism_eu$nkill>1),]
# En jaune : blessés
terrorism_eu_wound <- terrorism_eu[which(terrorism_eu$nwound>1),]

# MAP PLOT AVEC LES POINTS SUR MAP MONDE

Color_Assets <- colorFactor(c("green", "blue", "red", "black"))
info <- as.factor(c("Pas de victimes","Des blessés","Des morts", "Pas d'informations"))

#initialisation
longline_map <- leaflet(terrorism_eu) %>% 
  setView(lng = mean(terrorism_eu$longitude), lat = mean(terrorism_eu$latitude), zoom = 4) %>% 
  addTiles()
longline_map

longline_map %>% addCircles(lng = terrorism_eu_nonkill$longitude, lat = terrorism_eu_nonkill$latitude, weight =(1), radius = 1,
                            color="green", popup = paste("Nombre de morts =",terrorism_eu_nonkill$nkill," en ",terrorism_eu$iyear, " par ", terrorism_eu$attacktype1_txt)) %>%
  addCircles(lng = terrorism_eu_nonwound$longitude, lat = terrorism_eu_nonwound$latitude, weight =(1),radius = 1,
             color="green", popup = paste("Nombre de blessés =",terrorism_eu_nonwound$nwound," en ",terrorism_eu$iyear,  " par ", terrorism_eu$attacktype1_txt)) %>%
  addCircles(lng = terrorism_eu_NA_kill$longitude, lat = terrorism_eu_NA_kill$latitude, weight=(1),radius = 1,
             color="black", popup = paste("Nombre de morts =",terrorism_eu_NA_kill$nkill," en ",terrorism_eu$iyear, " par ", terrorism_eu$attacktype1_txt)) %>%
  addCircles(lng = terrorism_eu_NA_wound$longitude, lat = terrorism_eu_NA_wound$latitude, weight=(1),radius = 1,
             color="black", popup = paste("Nombre de blessés =",terrorism_eu_NA_wound$nwound," en ",terrorism_eu$iyear,  " par ", terrorism_eu$attacktype1_txt)) %>%
  addCircles(lng = terrorism_eu_wound$longitude, lat = terrorism_eu_wound$latitude, weight=1 ,radius = 150*terrorism_eu_wound$nwound,
             color= "blue", fillOpacity = 0.1, popup = paste("Nombre de blessés =",terrorism_eu_wound$nwound," en ",terrorism_eu$iyear,  " par ", terrorism_eu$attacktype1_txt)) %>%
  addCircles(lng = terrorism_eu_kill$longitude, lat = terrorism_eu_kill$latitude, weight=1, radius = 150*terrorism_eu_kill$nkill,
             color="red", popup = paste("Nombre de morts =",terrorism_eu_kill$nkill," en ",terrorism_eu$iyear, " par ", terrorism_eu$attacktype1_txt))  %>%
  addLegend("topleft", colors = palette, labels = info, title = "Attentats en Europe de l'Ouest depuis 1970", opacity = 1)





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

# Et on garde les 4 pays avec le plus d'attentats :
idx_country <- which((df3$country=="France")|(df3$country=="Italy")|(df3$country=="Spain")|(df3$country=="United Kingdom"))
df3 <- df3[idx_country,] 

# GGPLOT

options(warn=-1)

color_chosen <- c("red","blue","yellow","gray")

data_breaks <- data.frame(start = c("1970","1990","2010"),  # Create data with breaks
                          end = c("1980","2000","2017"),
                          colors = c("gray90","gray90","gray90"))

# Je définis un jeu de données pour l'évènement test
df4 <- df3[which(df3$country=="Italy"),][c(6:14),]

graph2 <- ggplot() +
  
  geom_rect(data=data_breaks,aes(xmin = start,
                                 xmax = end,
                                 ymin = 0,
                                 ymax = 310,
                                 fill = colors),alpha = 0.5) +
  
  # On fait carrément un rectangle pour l'évènement
  # geom_rect(data=data_breaks,aes(xmin = "1975",
  #                                xmax = "1980",
  #                                ymin = 0,
  #                                ymax = 310,
  #                                fill = "lightblue"),alpha = 0.5) +
  
  # Ou alors on fait l'aire sous la courbe pour l'évènement
  geom_area(data=df4, aes(x = annee, y = n, group = country, fill = "lightblue"))+
  
  geom_line(data=df3, aes(x = annee, y = n, group = country, color = country),size=1) + 
  
  # If we want to put label or text :
  # https://r-graph-gallery.com/275-add-text-labels-with-ggplot2.html
  geom_text(aes(x="1977",y=320),label="test", nudge_x = 0.05, nudge_y = 0.05, check_overlap = T,col="lightblue")+
  #geom_label(aes(x="1996",y=280),label="1",  label.padding = unit(0.35, "lines"), 
  #label.size = 0.05,color = "black",fill="red")+
  
  scale_color_manual(values=color_chosen)+
  #scale_color_jco()+
  
  # then details for presentation
  xlab("Year") +
  scale_x_discrete(breaks=c("1970","1975","1980","1985","1990","1995","2000","2005","2010","2015")) +
  ylab("Count of terrorist attacks") +
  labs(title="Count of terrorist attacks by year and country")+
  scale_fill_identity()+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5,size=9))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

graph2

options(warn=0)




############ ---- GRAPHE 3 : ACM ----------------


# On prend les 4 pays
terrorism_ACM <- terrorism2
terrorism_ACM <- terrorism_ACM[which((terrorism_ACM$country=="France")|(terrorism_ACM$country=="Italy")|(terrorism_ACM$country=="Spain")|(terrorism_ACM$country=="United Kingdom")),]

# On regarde les NA dans succès et on les enlève
idx_NA_suc <- which(is.na(terrorism_ACM$success)==TRUE)
terrorism_ACM <- terrorism_ACM[-idx_NA_suc,]

# On convertie en facteurs
terrorism_ACM$country <- as.factor(terrorism_ACM$country)
terrorism_ACM$success <- as.factor(terrorism_ACM$success)
terrorism_ACM$attacktype1_txt <- as.factor(terrorism_ACM$attacktype1_txt)
terrorism_ACM$targtype1_txt <- as.factor(terrorism_ACM$targtype1_txt)
# terrorism_ACM$weaptype1_txt <- as.factor(terrorism_ACM$weaptype1_txt)

# On choisit 3 variables pour l'ACM et on fait l'ACM
terrorism_ACM_1 <- terrorism_ACM[,c(5,12,17)]
res.MCA<-MCA(terrorism_ACM_1,graph=FALSE)

# On va sur Factoshiny
#Factoshiny(terrorism_ACM_1)

#### 1) Graphe des variables #####################
plot_mca1 <- plot.MCA(res.MCA, choix='var',title="Variable graph",col.var=c(1,2,3))
plot_mca1


#### 2) Deuxième graphe des variables #####################
options(ggrepel.max.overlaps = Inf)
plot_mca2 <-plot.MCA(res.MCA,invisible= 'ind',col.var=c(1,1,1,1,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3),title="CMA graph",cex=0.6,cex.main=0.6,cex.axis=0.6,label =c('var'))
plot_mca2

ggplotly(plot_mca2)

test <- fviz_mca_var(res.MCA, labelsize = 3, repel = TRUE,col.var = c("country","country","country","country","success","success","target","target","target","target","target","target","target","target","target","target","target","target","target","target","target","target","target","target","target","target","target","target"))+
  theme(text = element_text(size = 7.5),
        axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 7.5))
test
ggplotly(test)

b <- fviz_mca_var(res.MCA, labelsize = 3, repel = TRUE) +
  theme(text = element_text(size = 7.5),
        axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 7.5))
plotly(b)
plotly(plot_mca2)


#### 3) Graphe des valeurs propres #####################
plot_mca3 <- fviz_eig(res.MCA, addlabels = TRUE, ylim = c(0, 6.5))+
                        xlab("Percentage of explained variances") +
                        ylab("Dimensions") + 
                        labs(title="Eigen values")+
                        scale_fill_identity()+
                        theme_bw() +
                        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                        theme(plot.title = element_text(hjust = 0.5,size=9))
plot_mca3


# On vérifie si l'inertie est suffisante :
permuteLigne <- function(v) {return(v[sample(1:length(v),replace=FALSE)])}
nind <- nrow(terrorism_ACM_1)
nvar <- ncol(terrorism_ACM_1)
print(nind)
print(nvar)
nbsimul <- 1000
iner <- NULL
for (i in 1:nbsimul){
  mat <- apply(terrorism_ACM_1,2,permuteLigne)
  iner <- c(iner,MCA(mat,graph=F)$eig[2,3])
}
# calcul de l'inertie du quantile (=8.45%)
a <- quantile(iner,0.95)
print(a)
# % l'inertie du jeux de donnees (=10.67%) est plus grand que le quantile 95%
b <- res.MCA$eig[2,3]
print(b)


#### 4) Ensemble des graphes #####################

ggarrange(plot_mca1,plot_mca2,plot_mca3,ncol=2,nrow=2) 

