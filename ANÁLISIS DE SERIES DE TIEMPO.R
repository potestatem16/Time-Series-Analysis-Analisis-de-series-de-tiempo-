setwd("C:/Users/aleja/Documents/Universidad/Analisis de Riesgo 2020-2")

# CLASE SÁBADO 15 DE AGOSTO DE 2020
# ANÁLISIS DE SERIES DE TIEMPO
rm(list=ls())
dev.off()

#LIBRERIAS NECESARIAS
library(xlsx)
library(csv)
library(forecast)
library(tseries)
library(timeDate)
library(timeSeries)
library(gridExtra)
library(ggplot2)
library(ggthemes)
library(plotly)

#####################################################################

#RENDIMIENTOS TRIMESTRALES
# COMPORTAMIENTO ESTACIONARIO (CRECIMIENTO DE RENDIMIENTOS EN EL CUARTO TRIMESTRE)
rt<-c(0.3, 0.46, 0.345, 0.91, 0.33, 0.545, 0.44, 1.04, 0.495, 0.68, 0.545, 1.285, 0.55, 0.87, 0.66, 1.58, 0.59, 0.99, 0.83, 1.73, 0.61, 1.05, 0.92, 2.04, 0.7, 1.23, 1.06, 2.32, 0.82, 1.41, 1.25, 2.73)
length(rt)
plot.ts(rt,main="rendimientos")
plot(rt,type = "l",main = "Rendimientos de una serie estacionaria")

########################################################################

#ANÁLISIS PRECIO DE CIERRE Y RENDIMIENTOS SIMPLES DE LAS ACCIONES

# CARGAR LOS DATOS
# Periodicidad: 04 de junio de 2012 al 01 de junio de 2017
data1=read.csv(file = "acciones.csv", header = T,sep = ",")
head(data1)
tail(data1)
dim(data1) # 1257

#TRANSFORMAR EL FORMATO DE LAS FECHAS
data1$Date=as.Date(data1$Date,format = "%m/%d/%Y")
class(data1$Date)

#ORDENAR AÑO/MES/DÍA
data1$Date=format(data1$Date,"%Y-%m-%d")
head(data1)
class(data1$Date)

#CAMBIO EL FORMATO DE LA FECHA NUEVAMENTE
data1$Date=as.Date(data1$Date)
class(data1$Date)

#####################################################################
# ANÁLISIS DE LOS PRECIOS DE CIERRE DE APPLE, FACEBOOK, AMAZON Y GOOGLE
#####################################################################

#####################################################################
# INTRODUCCIÓN
#####################################################################

# El presente documento muestra el análisis de las series de tiempo
# de los precios diarios y los rendimientos simples de las acciones de Facebook,
# Apple, Amazon y Google. Estas acciones cotizan en la bolsa NASDAQ,
# en la cual se encuentran principalmente empresas de alta tecnología en
# electrónica, informática, telecomunicaciones y botecnología. El análisis
# se realiza para el periodo comprendindo entre el 04 de junio de 2012
# y el 01 de junio de 2017, es decir para 1257 observaciones.

#####################################################################
# ANÁLISIS DE LOS PRECIOS DE CIERRE
#####################################################################

summary(data1)

#GRAFICAR LA SERIE DE PRECIOS
windows()
par(mfrow=c(2,2))
plot(data1$Date,data1$AAPL,type = "l",main = "Precios Apple",col="orange",xlab="Fecha",ylab="Precio de cierre")
plot(data1$Date,data1$FB,type = "l",main = "Precios Facebook",col="blue",xlab="Fecha",ylab="Precio de cierre")
plot(data1$Date,data1$AMZN,type="l",main="Precios Amazon",col="red",xlab="Fecha",ylab="Precio de cierre")
plot(data1$Date,data1$GOOG,type="l",main="Precios Google",col="green",xlab="Fecha",ylab="Precio de cierre")

head(data1)

p1<-ggplot(data = data1, aes(x=Date, y=AAPL))+
        geom_line()+
        theme_tufte()+
        xlab("Apple") + ylab("Precio Cierre" )

p2<-ggplot(data = data1, aes(x=Date, y=FB))+
        geom_line()+
        theme_tufte()+
        xlab("Facebook") + ylab("Precio Cierre")

p3<-ggplot(data = data1, aes(x=Date, y=AMZN))+
        geom_line()+
        theme_tufte()+
        xlab("Amazon") + ylab("Precio Cierre")


p4<-ggplot(data = data1, aes(x=Date, y=GOOG))+
        geom_line()+
        theme_tufte()+
        xlab("Google") + ylab("Precio Cierre")

multiple_p<-grid.arrange(p1, p2, p3, p4, ncol=2, nrow=2)

subplot(p1, p2 , p3, p4,  nrows=2, margin = c(0.1, 0.01,0.01,0.01), 
        titleX = T, titleY = T)

        #ANÁLISIS:
# A partir de las gráficas de los precios de cierre de las acciones
# seleccionadas para el análisis, se observa que Apple, Facebook, Amazon
# y Google presentan una tendencia creciente en el periodo de tiempo definido,
# sin embargo, para algunas acciones se presentan cambios abruptos
# en los precios en determinados periodos, vale la pena resaltar los
# siguientes comportamientos:

# El precio de cierre mínimo de Facebook en el periodo analizado es
# de USD 17,73 (día 04 de septiembre de 2012), para Amazon el precio
# mínimo es de USD 213,2 (día 05 de Junio de 2012 ), para Google el
# precío mínimo es de USD 279,2 (día 14 de junio de 2012) y para Apple
# el precio mínimo es de USD 55,79 (día 19 de abril de 2013)

# El


#####################################################################
# ANÁLISIS DE LOS RENDIMIENTOS SIMPLES DE APPLE, FACEBOOK, AMAZON Y GOOGLE
#####################################################################

#CONVERTIR DATOS EN OBJETO DE SERIES DE TIEMPO PARA SACAR RENDIMIENTOS

#DATA NUEVA
data3=data.frame(DATE=data1$Date,FACEBOOK=data1$FB,APPLE=data1$AAPL,
                 AMAZON=data1$AMZN,GOOGLE=data1$GOOG)

head(data3)
precios_ts=as.timeSeries(data3)

windows()
plot(data3$DATE,precios_ts$FACEBOOK,type="l")
plot_ly(x=~data3$DATE, y=~precios_ts$FACEBOOK, mode="lines", 
        text= data3$DATE)->fig1
fig1 <- fig1 %>%  layout(title="Facebook", xaxis =list(title="Fecha"),
                         yaxis=list(title="precio"))

library(plotly)
library(ggthemes)

f1<-ggplot(data=data3, aes(x=DATE, y=precios_ts$FACEBOOK))
f1 +
     geom_line()+
     theme_tufte()+
     xlab("Fecha") + ylab("Precios")->f1



#ggplotly(f1)

plot(precios_ts$Dat)
head(precios_ts)

#SERIE DE RENDIMIENTOS DISCRETOS
retornos=returns(x = precios_ts,method="discrete")
retornos_df<-as.data.frame(retornos)
retornos_df$Date<- data1$Date[-1]
head(retornos_df$Date)



windows()
par(mfrow=c(2,2))
plot(data1$Date[-1],rent_fb,type="l",col="blue",main="Rentabilidad Simple Facebook",xlab="Fecha",ylab="Rentabilidad Simple")

r1 <- ggplot(data = retornos_df,  aes(x=Date, y=FACEBOOK))+
        geom_line()+
        theme_tufte()+
        xlab("Facebook") + ylab("Rentabilidad Simple")+
        ylim(-0.1,0.3)

r2 <- ggplot(data = retornos_df,  aes(x=Date, y=APPLE))+
        geom_line()+
        theme_tufte()+
        xlab("Apple") + ylab("Rentabilidad Simple")+
        ylim(-0.1,0.3); # r2

r3 <- ggplot(data = retornos_df,  aes(x=Date, y=AMAZON))+
        geom_line()+
        theme_tufte()+
        xlab("Amazon") + ylab("Rentabilidad Simple")+
        ylim(-0.1,0.3); # r3

r4 <- ggplot(data = retornos_df,  aes(x=Date, y=GOOGLE))+
        geom_line()+
        theme_tufte()+
        xlab("Google") + ylab("Rentabilidad Simple")+
        ylim(-0.1,0.3);# r4
        
multiple_p<-grid.arrange(r1,r2,r3,r4,ncol=2, nrow=2)
        
subplot(r1,r2,r3,r4, nrows=2, titleX = T, titleY = T, shareY = T,
        margin = c(0, 0,0.09,0))

plot(data1$Date[-1],rent_app,type="l",col="orange",main="Rentabilidad Simple Apple",xlab="Fecha",ylab="Rentabilidad Simple")
plot(data1$Date[-1],rent_amaz,type="l",col="red",main="Rentabilidad Simple Amazon",xlab="Fecha",ylab="Rentabilidad Simple")
plot(data1$Date[-1],rent_google,type="l",col="green",main="Rentabilidad Simple Google",xlab="Fecha",ylab="Rentabilidad Simple")

#####################################################################
# ANÁLISIS DE LOS RETORNOS SIMPLES
#####################################################################

# Naturalmente y como es comun encontrar en la mayoria de las series
# financieras se observa que los retornos simples oscilan alrededor de cero.
# El comportamiento de estos retornos indica que Apple y Amazon
# presentan mayor volatilidad que Google y Facebook, esto coincide con
# el compotramiento de los precios donde se evicenció que esta últimas dos
# acciones presentaban caídas mucho más pronunciadas en los valores de cierre
# Vale la pena resaltar los siguientes comportamientos

#- Facebook presenta picos de rentabilidad cercanos al 30%, siendo
# la acción con mayor valorización, mientras que los valores
# negativos de la rentablidad simple estan cercanos al 10%
# - Google presenta picos de rentabilidad cercanos al 15%, siendo
# la segunda acción con mayores rendimientos positivos mientras
# que sus valores negativos de la rentabilidad simple estan alrededor del 5%
# - Amazon presenta valores máximos y minimos de rentabilidad simple cercanos
# al 10 %
# - Apple presenta picos positivos en su rentabilidad cercanos al 5%
# y picos negativos cercanos al 10 %
# - Lo anterior inidica que las mejores opciones de inversion se encuentran
# en Facebook y Google, las cuales tienen los valores más altos de
# rentabilidad simple