# Calculo de variabilidad
coeffi = 0.56
variability = (coeffi)^2*100 # 
variability

# Obtener coeficiente de correlación
faltas = c(0,2,6,8,3)
Calif = c(95,100,75,60,85)
corre = cor(faltas,Calif)
corre

# Modelo de Regresion
lr = lm(Calif~faltas)
lr
summary(lr)

# Problema salud 
peso = 69
edad = 17
tiempo = 12
frecuencia_card = 129
y = (6+0.65*peso)-(0.04*edad)-(0.01*tiempo)-(0.02*frecuencia_card)
y

# Regresion lineal catalogos
ncat = c(5,1,5,6,2) # X
ordenes = c(442,437,440,450,426) # Y
ler = lm(ordenes~ncat )# Modelo siempre va Y contra X
ler
y = -3.283*10+232.811 # Se multiplica por diez porque los catalogos estan en cientos
y # Redondear a un numero entero
summary(ler)

# PREGUNTA 7 - Modelo de regresión múltiple, elección de 'mejor' modelo
ye = c(240,236,290,274,301,316,300,296,267,276,288,261)
x1 = c(25,31,45,60,65,72,80,84,75,60,50,38)
x2 = c(24,21,24,25,25,26,25,25,24,25,25,23)
x3 = c(91,90,88,87,91,94,87,86,88,91,90,89)
x4 = c(100,95,110,88,94,99,97,96,110,105,100,98)
multiple <- lm(ye~ x1 + x2 + x3 + x4)
step(object = multiple,direccion="both",trace=0.05)

# Pregunta 8 Correctos o incorrectos
investigador = read.table("yolanda.txt", header=T, sep = "\t")
lmi = lm(investigador)
summary(lmi)


# PREGUNTA 6 - Regresión lineal, pronosticar costo de mantenimiento para un camión con dos años de uso
x = c(5,1,5,6,2) # X
ye = c(442,437,440,450,426) # Y
regresionLineal <- lm(ye~x)
predict(regresionLineal,data.frame(x = 3))

