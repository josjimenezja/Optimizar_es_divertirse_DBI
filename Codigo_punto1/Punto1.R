# Optimizar es divertido
# ---------------
# | Ejercicio 1 |
# ---------------

# Se carga la libreria para la creacion del gif
library(animation)

# Realizamos la lectura de los datos
datos <- read.csv("datos_regresion.csv", sep=" ")

# Se grafican los datos
par(pty="s") #¿Que hace esta funcion?
plot(datos, las=1, main="x vs y")

# Utilizamos la funcion locator para marcar puntos con el mouse
# de manera interactiva en la grafica
puntos_xy <- locator()
puntos_x <- puntos_xy$x
puntos_y <- puntos_xy$y

# Se itera sobre un for para realizar un print de todas las rectas con base en los puntos
# que se marcaron, utilizando un contador para identificar cada grafica creada
counter1 <- 0
for(i in seq(from = 1, to = length(puntos_x), by = 2)){
  counter1 <- counter1 + 1
  p_x <- puntos_x[i:(i+1)]
  p_y <- puntos_y[i:(i+1)]
  
  line <- lm(p_y~p_x)
  
  # Con esta funcion guardabamos los graficos generados
  png(filename = gsub(" ", "", paste("Ejercicio1_Grafico", counter1,".png")),  width = 1280, height = 720)
  
  par(mfrow=c(1,2))
  plot(x = line$coefficients[1],
       y = line$coefficients[2],
       xlab = "Intercepto",
       ylab = "Pendiente",
       col = "red",
       pch = 2,
       cex = 2,
       lwd = 1,
       main = paste("Espacio de parámetros - #", counter1)
  )
  grid()
  plot(datos, las = 1, cex = 2, lwd = 1, main = paste("Variable x vs y - #", counter1))
  abline(line, col="red")
  grid()
  dev.off()
  
}

# Esta funcion se utiliza para crear al gif partiendo
# directamente de los datos introducidos sin tener
# en cuenta el espacio de parametros
counter2 <- 0
saveGIF({
  for(i in seq(from = 1, to = length(puntos_x), by = 2)){
    counter2 <- counter2 +1
    
    p_x <- puntos_x[i:(i+1)]
    p_y <- puntos_y[i:(i+1)]
    
    line <- lm(p_y~p_x)
    
    plot(datos, las=1, main = paste("Variable x vs y - #", counter2))
    abline(line, col="red")
    grid()
    
  }
}, movie.name = "Ejercicio1.gif")