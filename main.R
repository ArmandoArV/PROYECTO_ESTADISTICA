# Main script
library("readxl")
library("skimr")
library("naniar")
library("Hmisc")
library("corrplot")
library("psych")
library("dplyr")

analyze = readxl::read_excel(sheet=1, path = "./datos.xlsx");
analyze2 = readxl::read_excel(sheet=4, path = "./datos.xlsx");
# getwd()
#ANALISIS HOJA 1
names(analyze)
limpios = analyze %>% select(-Mes,-"Piezas producidas A",-"Piezas Producidas B",-"Piezas Producidas C")
limpios
# limpios = analyze
skimr::skim(limpios)
pct_miss(limpios)
vis_miss(limpios) 
correlation = rcorr(as.matrix(limpios)) #matriz de correlación
CORRELATION_ANALYSYS = correlation[["r"]]

#ANALISIS HOJA 3
names(analyze2)
limpios2 = analyze2[-c(1),]
skimr::skim(limpios2)
pct_miss(limpios2)
vis_miss(limpios2) 

as.matrix(limpios2)
CORRELATION_ANALYSYS2 = correlation2[["r"]]
# Análisis
DEFECTUOSAS_A = as.numeric(limpios$A1)
DEFECTUOSAS_B = as.numeric(limpios$B1)
DEFECTUOSAS_C = as.numeric(limpios$C1)

DEF_A = na.omit(DEFECTUOSAS_A)
DEF_B = na.omit(DEFECTUOSAS_B)
DEF_C = na.omit(DEFECTUOSAS_C)

#PERCENTAGE
PER = as.numeric(limpios2$`%`)
PERCENTAGE = na.omit(PER)


#EXTRUSIÓN
EXTRU = as.numeric(limpios2$`m/s`)
EXTRUSION = na.omit(EXTRU)

#BAR


Correlacion = cor(EXTRUSION, PERCENTAGE, method="pearson")
test = cor.test(EXTRUSION, PERCENTAGE) #
test


# GRAFICAS DATAFRAME 1

ggpairs(limpios)
multi.hist(x=DEF_A, dcol=c("blue", "red"), dlty=c("dotted", "solid"), lwd=c(2, 1), main = c("DEFECTUOSAS PIEZA A"))
multi.hist(x=DEF_B, dcol=c("blue", "red"), dlty=c("dotted", "solid"), lwd=c(2, 1), main = c("DEFECTUOSAS PIEZA B"))
multi.hist(x=DEF_C, dcol=c("blue", "red"), dlty=c("dotted", "solid"), lwd=c(2, 1), main = c("DEFECTUOSAS PIEZA C"))
multi.hist(x=DEF_C, dcol=c("blue", "red"), dlty=c("dotted", "solid"), lwd=c(2, 1), main = c("EXTRUSION"))
cor_clean = cor(limpios)
corrplot(cor_clean,method = 'color',order="alphabet",addCoef.col = 'black')

# GRÁFICAS DATA FRAME 2
ggpairs(limpios2,columns = 1:6,cardinality_threshold=60,legend=NULL,params=NULL,xlab = NULL,ylab=NULL)
multi.hist(x=PERCENTAGE, dcol=c("blue", "red"), dlty=c("dotted", "solid"), lwd=c(2, 1), main = c("PORCENTAJE"))
cor_clean2 = cor(limpios2)
corrplot(cor_clean2,method = 'color',order="alphabet",addCoef.col = 'black')
