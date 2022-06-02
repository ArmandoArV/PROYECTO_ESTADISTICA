(x = c(2, 5, 1, 3, 4, 1, 5, 3, 4, 2)); #No. comerciales Salida en consola
y = c(50, 57, 41, 54, 54, 38, 63, 48, 59, 46); #No. ventas

plot(x,y,pch=16, col="blue", main="Relación entre ventas y numero de comerciales", xlab="No. comerciales", ylab="No. ventas")  #Tipo de marcador (pch)
correlacion = cor(x, y, method="pearson")
test = cor.test(x, y) #


#¿La inteligencia de una persona está determinada por el tamaño del cerebro basado en el conteo obtenido a partir de escaneos por resonancia magnética, MRI (cuentas/10,000) o por el de su cuerpo (estatura-pulgadas, peso-libras)?*
inteligencia = read.table("iqsize.txt", header=T, row.names = 1); #row.names = etiquetas
inteligencia
attach(inteligencia)
names(inteligencia)
# skip = numeros de renglones a saltar
inteligencia$IQ
inteligencia[1,]


pairs(inteligencia, pch=20) #matriz de gráficos de dispersion
plot(Cerebro, IQ, pch=16, xlab="Tamaño cerebro", ylab="IQ")  #cambiar regresor x

plot(Altura, IQ, pch=16, xlab="Altura", ylab="IQ")

install.packages("psych")
library(psych)
install.packages("GGally")
library(GGally)

multi.hist(x=inteligencia, dcol=c("blue", "red"), dlty=c("dotted", "solid"), lwd=c(2, 1), main = c("IQ", "Cerebro", "Altura", "Peso"))
# curva roja es teórica
#curva en azul es la de los datos, real

ggpairs(inteligencia)
ggpairs(inteligencia, upper=list(continuous = "smooth"), lower = list(continuous = "blank"), diag = list(continuous = "densityDiag"))
#línea sombreada es la impresión del modelo
#Método de crecimiento para regresión líneal


install.packages("Hmisc")
library(Hmisc)
rcorr(as.matrix(inteligencia)) #matriz de correlación
# p matriz de prueba de hipotesis
library(corrplot)
corrplot(cor(inteligencia))

beta0=function(x,y){
  b1 = cov(x,y)/var(x)
  print(b1);
}

y = inteligencia$IQ
x = inteligencia $Cerebro
beta0(x,y)
lm(formula = y~x,data=inteligencia)
