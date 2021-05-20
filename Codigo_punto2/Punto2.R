# Optimizar es divertido, punto 2

library(spatstat.utils)
# Realizamos la lectura de los datos

lr_data<-read.csv("D:\\Documentos\\Rfiles\\Actividad3\\datos_clasificacion.csv", header=TRUE, sep = " ")
# <- read.csv("datos_clasificacion.csv", sep=" ")

#Procedimiento recomendado

#1. Dividir el rango de la variable explicativa (x) en 10 sub intervalos, entiendase variable explicativa como variable independiente, efectivamente x 

#Necesito tomar todos los puntos que estén dentro de (Xmax-Xmin)/10 [1] ... 
#Calcular la longitud de cada subintervalo
#Agrupar por subintervalos
#iterar sobre todos los datos
#mi incremento es la distancia del subintervalo, ya calculada
#punto de partida es Xmin, 10 iteraciones
#n_int es el número del subintervalo
#Hasta aquí ya tengo los n intervalos iguales

x_from<-min(lr_data$x)
d_int<-(max(lr_data$x) - x_from)/10
x_fin<-max(lr_data$x)-0.001
n_int<-1
res<-c()

while(x_from < x_fin) {
  count<-0
  #print(n_int)
  sRange<-c(x_from,x_from+d_int)
  if (n_int == 10){
    sRange<-c(x_from,max(lr_data$x))
  }
  #print(paste('Xfrom',sRange[1], 'Xto',sRange[2]))
  for (i in 1:100) {
    #Datos que se encuentren en el rango actual y que hayan ocurrido
    if (inside.range(lr_data$x[i], sRange) & (lr_data$y[i] == 1)){
      count<-count+1  
    }
  }
  res<-rbind(res, c(n_int,sRange[1],mean(c(sRange[1],sRange[2])),sRange[2],count))
  x_from <- x_from + d_int
  n_int<-n_int+1
}
colnames(res)<-c('nSubIntervalo','xFrom','xMean', 'xTo','Ocurrencias')
#En res se encuentra la información necesaria para el primer punto

#2. En el centro de cada subintervalo dibuje una línea vertical con la tasa de ocurrencias(unos) en ese subintervalo
dfRes<-data.frame(res)
plot(dfRes$xMean, dfRes$Ocurrencias, main= "OCURRENCIAS POR SUBINTERVALO", type="h", col=rep(c("red", "blue"), each=5))
grid(col = "darkgray", lwd = 1)

#3.1. Proponer un punto en el espacio de parametros (B1 y B2)
betas<-c()
plot(0, 0, col=NULL,xlim=c(-15, 15), ylim=c(-15, 15), main= "SELECCIONA UNA COMBINACIÓN DENTRO DEL ESPACIO DE PARAMETROS", xlab="B0", ylab="B1")
grid(col = "darkgray", lwd = 1)
my_point<-locator(1)
betas<-rbind(betas,c(my_point$x,my_point$y))
points(betas, pch=2, col=3)
colnames(betas)<-c('beta1','beta2')


#3.2. Grafique la curva logistica sobre los hiperparametros ingresados en el gráfico anterior

#Función de predicción de probabilidad
logreg_predict<- function(x, betas){
  y_prob<- 1/(1+exp(-(betas[1]+betas[2]*x)))
  return(y_prob)
}

#Solo la gráfica de la curva
curve(logreg_predict(x,betas),from=0, to=1, main='CURVA GENERADA')


#y_prob : Probabilidades de cada x con el beta ingresado
y_prob<-logreg_predict(lr_data$x, betas)

#Ajuste de la curva sobre los datos
x<-seq(0,1,0.01)
plot(x, logreg_predict(x,betas),
     main=paste("Regresión logistica de los parámetros ingresados", betas[1],betas[2]),
     ylab="Probabilidad Condicionada",
     type="l",
     col="blue",
     xlim=c(0,1),
     ylim=c(0,1))
points(lr_data)


#Curva ideal
opt_beta<-c(-9, 11)#Son los parámetros ideales
curve(logreg_predict(x,opt_beta),from=0, to=1, main='CURVA GENERADA')

x<-seq(0,1,0.01)
plot(x, logreg_predict(x,opt_beta),
     main=paste("AJUSTE DEL MODELO ÓPTIMO SOBRE LOS DATOS"),
     ylab="Probabilidad Condicionada",
     type="l",
     col="blue",
     xlim=c(0,1),
     ylim=c(0,1))
points(lr_data)


#4. Repetir hasta estar satisfecho con el resultado 

#Ahora a encontrar un buen ajuste a partir del espacio de parámetros

betas<-c()
for (i in 1:5){
  
  plot(0, 0, col=NULL,xlim=c(-15, 15), ylim=c(-15, 15), main= "SELECCIONA UNA COMBINACIÓN DENTRO DEL ESPACIO DE PARAMETROS", xlab="B0", ylab="B1")
  grid(col = "darkgray", lwd = 1)
  my_point<-locator(1)
  betas<-rbind(betas,c(my_point$x,my_point$y))
  points(betas[,1], betas[,2] , pch=2, col=c(1,2,3,4,5))
  #UNA VEZ SELECCIONO UNA COMBINACIÓN
  x<-seq(0,1,0.01)
  plot(x, logreg_predict(x,c(betas[i,][1],betas[i,][2])),
       main=paste("AJUSTE PARA ", 'B0=' ,round(betas[i,][1],2), ' B1=' ,round(betas[i,][2],2)),
       ylab="Probabilidad Condicionada",
       type="l",
       col=i,
       xlim=c(0,1),
       ylim=c(0,1))
  points(lr_data)
}
colnames(betas)<-c('beta1','beta2')

#5. Generar un gif con la evaluación del ajuste

##FIN - Aquí si lo logre


dir.create("D:\\Documentos\\Rfiles\\Actividad3\\Punto2")
setwd("D:\\Documentos\\Rfiles\\Actividad3\\Punto2")
## max_it<-5 ##Está inicializado arriba
png(file="logreg%02d.png",width=600, height=600)
#betas<-c()
x<-seq(0,1,0.01)
i<-1
for (i in 1:5){
  plot(x, logreg_predict(x,c(betas[i,][1],betas[i,][2])), 
       main=paste("AJUSTE PARA ", 'B0=' ,round(betas[i,][1],2), ' B1=' ,round(betas[i,][2],2)),
       ylab="Probabilidad Condicionada", type = 'l', col=i, xlim = c(0,1), ylim = c(0,1) )
  points(lr_data)
}
dev.off()
system("convert -delay 80 D:\\Documentos\\Rfiles\\Actividad3\\Punto2\\*.png logreg_33.gif")
file.remove(list.files(pattern=".png"))

