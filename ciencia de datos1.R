library(knitr)
library(ggplot2)
library(readr)
library(gridExtra)
library(tidyr)
library(dplyr)
vinos<-read_csv("wine_quality.csv")
hist(vinos$quality, ylab="Frecuencia", xlab="Calidad del vino", main="Vino por calidad", col="blue")

plot1<-ggplot(data=vinos,aes(x=wine,y=vinos$density))+geom_boxplot()
plot2<-ggplot(data=vinos,aes(x=wine,y=vinos$density))+geom_boxplot(outlier.shape = NA)+scale_y_continuous(limits = quantile(vinos$density, c(0.05, 0.95)))
grid.arrange(plot1, plot2, ncol=2)

plot1<-ggplot(data=vinos,aes(x=wine,y=vinos$density))+geom_violin(stat="ydensity", draw_quantiles = c(0.05,0.95))
plot1


plot1<-ggplot(data=vinos,aes(x=wine,y=vinos$sulphates))+geom_boxplot()
plot2<-ggplot(data=vinos,aes(x=wine,y=vinos$sulphates))+geom_boxplot(outlier.shape = NA)+scale_y_continuous(limits = quantile(vinos$sulphates, c(0.05, 0.95)))
grid.arrange(plot1, plot2, ncol=2)

plot1<-ggplot(data=vinos,aes(x=wine,y=vinos$sulphates))+geom_violin(stat="ydensity", draw_quantiles = c(0.05,0.95))
plot1

plot1<-ggplot(data=vinos,aes(x=wine,y=vinos$`free sulfur dioxide`))+geom_boxplot()
plot2<-ggplot(data=vinos,aes(x=wine,y=vinos$`free sulfur dioxide`))+geom_boxplot(outlier.shape = NA)+scale_y_continuous(limits = quantile(vinos$`free sulfur dioxide`, c(0.05, 0.95)))
grid.arrange(plot1, plot2, ncol=2)

plot1<-ggplot(data=vinos,aes(x=wine,y=vinos$`free sulfur dioxide`))+geom_violin(stat="ydensity", draw_quantiles = c(0.05,0.95))
plot1


xx<-vinos %>% select(wine, quality) %>% group_by(wine,quality) %>% 
  summarize(total = n())
yy<-xx %>% spread(quality, total) %>% kable(caption="Total de vinos por calidad",format="latex")

rojo<- vinos %>% filter(wine=="red") 
blanco<-vinos %>% filter(wine=="white") 
rojo<-rojo%>% mutate(categ_qual=ifelse(rojo$quality<=4, "Mala",ifelse(rojo$quality>=5 & rojo$quality<7,"Regular","Excelente")))
blanco <- blanco %>%mutate(categ_qual=ifelse(blanco$quality<=4, "Mala",ifelse(blanco$quality>=5 & blanco$quality<7,"Regular","Excelente")))

plot1<-ggplot(data=rojo,aes(x=categ_qual))+geom_histogram(stat="count")+labs(x = "Categorias vino rojo")
plot2<-ggplot(data=blanco,aes(x=categ_qual))+geom_histogram(stat="count")+labs(x = "Categorias vino blanco")
grid.arrange(plot1, plot2, ncol=2)

plot1<-ggplot(data=rojo,aes(x=categ_qual,y=alcohol))+geom_boxplot()+labs(x = "Categorias vino rojo")
plot2<-ggplot(data=blanco,aes(x=categ_qual,y=alcohol))+geom_boxplot()+labs(x = "Categorias vino blanco")
grid.arrange(plot1, plot2, ncol=2)

plot1<-ggplot(data=rojo,aes(x=categ_qual,y=alcohol))+geom_violin(stat="ydensity", draw_quantiles = c(0.05,0.95))+xlab("Vino rojo")
plot2<-ggplot(data=blanco,aes(x=categ_qual,y=alcohol))+geom_violin(stat="ydensity", draw_quantiles = c(0.05,0.95))+xlab("Vino blanco")
grid.arrange(plot1, plot2, ncol=2)

plot1<-ggplot(data=rojo,aes(x=categ_qual,y=`volatile acidity`))+geom_boxplot()+labs(x = "Categorias vino rojo")
plot2<-ggplot(data=blanco,aes(x=categ_qual,y=`volatile acidity`))+geom_boxplot()+labs(x = "Categorias vino blanco")
grid.arrange(plot1, plot2, ncol=2)

plot1<-ggplot(data=rojo,aes(x=categ_qual,y=`volatile acidity`))+geom_violin(stat="ydensity", draw_quantiles = c(0.05,0.95))
plot2<-ggplot(data=blanco,aes(x=categ_qual,y=`volatile acidity`))+geom_violin(stat="ydensity", draw_quantiles = c(0.05,0.95))+xlab("Vino blanco")
grid.arrange(plot1, plot2, ncol=2)

plot1<-ggplot(data=rojo,aes(x=categ_qual,y=`citric acid`))+geom_boxplot()+labs(x = "Categorias vino rojo")
plot2<-ggplot(data=blanco,aes(x=categ_qual,y=`citric acid`))+geom_boxplot()+labs(x = "Categorias vino blanco")
grid.arrange(plot1, plot2, ncol=2)

plot1<-ggplot(data=rojo,aes(x=categ_qual,y=`citric acid`))+geom_violin(stat="ydensity", draw_quantiles = c(0.05,0.95))
plot2<-ggplot(data=blanco,aes(x=categ_qual,y=`citric acid`))+geom_violin(stat="ydensity", draw_quantiles = c(0.05,0.95))+xlab("Vino blanco")
grid.arrange(plot1, plot2, ncol=2)

plot1<-ggplot(data=rojo,aes(x=categ_qual,y=sulphates))+geom_boxplot()+labs(x = "Categorias vino rojo")
plot2<-ggplot(data=blanco,aes(x=categ_qual,y=sulphates))+geom_boxplot()+labs(x = "Categorias vino blanco")
grid.arrange(plot1, plot2, ncol=2)

plot1<-ggplot(data=rojo,aes(x=categ_qual,y=sulphates))+geom_violin(stat="ydensity", draw_quantiles = c(0.05,0.95))
plot2<-ggplot(data=blanco,aes(x=categ_qual,y=sulphates))+geom_violin(stat="ydensity", draw_quantiles = c(0.05,0.95))+xlab("Vino blanco")
grid.arrange(plot1, plot2, ncol=2)

plot1<-ggplot(data=rojo,aes(x=categ_qual,y=density))+geom_violin(stat="ydensity", draw_quantiles = c(0.05,0.95))
plot2<-ggplot(data=blanco,aes(x=categ_qual,y=density))+geom_violin(stat="ydensity", draw_quantiles = c(0.05,0.95))+xlab("Vino blanco")
grid.arrange(plot1, plot2, ncol=2)



#########################Ejercicio2 #####################################################3
library(ggbiplot)
salud<-read.csv("ushealth.csv")
region<-salud[,13]
salcausas<-c("acc","card","canc","pul","pneu","diab","liv")
saludpca<-prcomp(salud[,4:10], center = TRUE, scale=FALSE) 
print(saludpca) #explicar valores en las variables (absolutos)
summary(saludpca)

saludpca_scaled<-prcomp(salud[,4:10], center = TRUE, scale=TRUE) 
print(saludpca_scaled)
summary(saludpca_scaled)
par(mfrow=c(1,2))
plot(saludpca, type = "l")
plot(saludpca_scaled, type = "l")

g <- ggbiplot(saludpca, obs.scale = 1, var.scale = 1, 
              ellipse = TRUE, 
              groups=region,
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')


h <- ggbiplot(saludpca_scaled, obs.scale = 1, var.scale = 1, 
              ellipse = TRUE,
              groups=region,
              circle = TRUE)
h <- h + scale_color_discrete(name = '')
h <- h + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)
print(h)
