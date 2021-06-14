setwd("C:/Users/Guillermo Sigrist/Desktop/bedu/r/proyecto")
install.packages("pool")
install.packages("dbplyr")
install.packages("DBI")
install.packages("rvest")
install.packages("TSA")
install.packages("ggplot2") 

library(pool)
library(rvest)
library(DBI)
library(dbplyr)
library(TSA)
library(ggplot2)

install.packages("ggpubr")
library(ggpubr)
theme_set(theme_pubr())
install.packages("scales")
library(scales)

tabla <- read.csv("https://raw.githubusercontent.com/ykevinsh/CursoR/master/Covid.csv")

summary(tabla$Ratio_Activos_Est)
View(tabla$est)






ggplot(tabla, aes(x = est,   y = Ratio_Activos_Est)) + 
  geom_line( color="blue") + 
  geom_point() +
  labs(x = "hh", 
       y = "Acumulado de casos confirmados",
       title = paste("Confirmados de COVID-19 en México:", 
                     format(Sys.time(), 
                            tz="America/Mexico_City", 
                            usetz=TRUE))) +
  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(face = "bold", color="#993333" , 
                                   size = 10, angle = 45, 
                                   hjust = 1),
        axis.text.y = element_text(face = "bold", color="#993333" , 
                                   size = 10, angle = 45, 
                                   hjust = 1)) 
ggplot(tabla, aes(x = sig,   y = Ratio_Activos_Est)) +
  geom_linerange(
    aes(x = sig, ymin = 0, ymax = Ratio_Activos_Est),
    color = "lightgray", size = 1.5)+
  geom_point(aes(color = sig), size = 1)+
  theme_pubclean()


install.packages("fmsb")
library(fmsb)

# Create data: note in High school for Jonathan:

data <-  as.data.frame(matrix(0,3,32))
colnames(data) <- c(tabla$sig)


# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data <- as.data.frame(rbind(rep(25,10) , 0 , tabla$Indice_Rec_Mue))
colnames(data) <- c(tabla$sig)
a1<-t(data)
a1<-as.data.frame(a1)
a2<-as.data.frame(a1$Letalidad)
colnames(a2)<-c("Lethalidad (%)")
rownames(a2)<-c(tabla$est)
# Check your data, it has to look like this!
# head(data)

# The default radar chart 
radarchart(data)

radarchart( data  , axistype=1 , 
            
          
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", caxislabels=seq(5,30,5), cglwd=1,
            
            #custom labels
            vlcex=1,
            title="Letalidad de Covid por Estado"
            
            
            
)

dataq <- as.data.frame(rbind(rep(2.5,10) , 0 , tabla$Ratio_Activos_Est))
colnames(dataq) <- c(tabla$sig)
a3<-t(dataq)
a3<-as.data.frame(a3)
colnames(a3)<-c("Q","W","RATIO PER CAPTIA")
rownames(a3)<-c(tabla$est)
A4<-as.data.frame(a3$`RATIO PER CAPTIA`)
radarchart(dataq)
colnames(A4)<-c("RATIO PER CAPTIA%")
rownames(A4)<-c(tabla$est)
radarchart( dataq  , axistype=1 , 
            
            
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", caxislabels=seq(0.5,2.5,.5), cglwd=1,
            
            #custom labels
            vlcex=1,
            title="Ratio de contagio Per capita"
            
            
            
)




install.packages("forecats")
library(forcats)


datat <- data.frame(tabla)
w <- data.frame(tabla$est, tabla$Total_Muertes)
y1 <- w[order(w$tabla.Total_Muertes),]

y2<-data.frame(y1)
colnames(y2) <- c("Estado","No.defunciones")

y1 %>%
  arrange(tabla.Total_Muertes) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(tabla.est=factor(tabla.est, levels=tabla.est)) %>%   # This trick update the factor levels
  ggplot( aes(x=tabla.est, y=tabla.Total_Muertes)) +
  geom_segment( aes(xend=tabla.est, yend=0)) +
  geom_point( size=4, color="orange") +
  coord_flip() +
  theme_bw() +
  xlab("")+
  labs(title = paste("Confirmados de COVID-19 en México:"))
  

library(treemap)
group <- c(tabla$Region)
length(group)
subgroup <- c(tabla$sig)
value <- c(tabla$Total_Muertes)
dataf <- data.frame(group,subgroup,value)

datat <- data.frame(tabla)
u <- data.frame(tabla$Region, tabla$Total_Muertes)

library(treemap)

by_cyl <- u %>% group_by(tabla.Region)
by_cyl
k<-by_cyl %>% summarise(
  No.FALLECIMIENTOS = sum(tabla.Total_Muertes)
)
k

# treemap
treemap(dataf,
        index=c("group","subgroup"),
        vSize=("value"),
        type="index",
        title="Defunciones por Region"
) 

library(treemap)
group1 <- c(tabla$Region)
length(group1)
subgroup1 <- c(tabla$sig)
value1 <- c(tabla$Indice_Rec_Mue)
dataw <- data.frame(group1,subgroup1,value1)

# treemap
treemap(dataw,
        index=c("group1","subgroup1"),
        vSize=("value1"),
        type="index",
        title="Letalidad del Covid por Region"
) 

library(treemap)
group2 <- c(tabla$Region)
length(group1)
subgroup2 <- c(tabla$sig)
value2 <- c(tabla$Ratio_muerte_Est)
datae <- data.frame(group2,subgroup2,value2)

# treemap
treemap(datae,
        index=c("group2","subgroup2"),
        vSize=("value2"),
        type="index",
        title="Fallecimientos per capita de Covid por Region"
) 

install.packages("hrbrthemes")  
library(hrbrthemes)

pop<-tabla$Total_Muertes
country<-tabla$sig
Letalidad<-tabla$Indice_Rec_Mue
lifeExp<-tabla$Ratio_Activos_Est
continent<-tabla$Region
mm<-tabla$No_Estado+400
datac<-data.frame(country,continent,lifeExp,pop,Letalidad,mm)


datac %>%
  mutate(country = factor(country, country)) %>%
  ggplot(aes(x=Letalidad, y=lifeExp, size=pop, color=country )) +
  geom_point(alpha=0.4) +
  geom_text(
    label=(country), 
    nudge_x = -0.3,
    nudge_y = 0.01,
    angle = 45,
    size = 4
  )+
  scale_size(range = c(1, 30), name="Defunsiones")+
  theme_ipsum() +
  ylab("Porcentaje de Contagiarte") +
  xlab("Porcentaje de Letalidad")

YY<-datac %>%
  mutate(country = factor(country, country)) %>%
  ggplot(aes(x=Letalidad, y=lifeExp, size=pop, color=country )) +
  geom_point(alpha=0.4) +
  geom_text(
    label=(country), 
    nudge_x = -0.3,
    nudge_y = 0.01,
    angle = 45,
    size = 4
  )+
  scale_size(range = c(1, 30), name="Defunsiones")+
  ylab("Porcentaje de Contagiarte") +
  xlab("Porcentaje de Letalidad")
YY

Y <- YY + 
  scale_color_manual(values = c(tabla$No_Estado))
Y
