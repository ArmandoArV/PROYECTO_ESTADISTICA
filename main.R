# Main script
library("readxl")
library("skimr")
library("naniar")
library("Hmisc")
library("corrplot")
library("psych")
library("dplyr")
library("GGally")

analyze = readxl::read_excel(sheet=1, path = "./datos.xlsx");
analyze2 = readxl::read_excel(sheet=3, path = "./ORIGINAL.xlsx",skip=1);
# getwd()
#ANALISIS HOJA 1
names(analyze)
limpios = analyze %>% select(-Mes,-"Piezas producidas A",-"Piezas Producidas B",-"Piezas Producidas C")
colnames(limpios) <- c("DEFECTUOSAS A","DEFECTUOSAS B","DEFECTUOSAS C");
limpios
# limpios = analyze
skimr::skim(limpios)
pct_miss(limpios)
vis_miss(limpios) 
correlation = rcorr(as.matrix(limpios)) #matriz de correlación
CORRELATION_ANALYSYS = correlation[["r"]]
CORRELATION_ANALYSYS
#ANALISIS HOJA 3
names(analyze2)
colnames(analyze2) <- c("Pressure", "PlasticPumpTemperature", "PlasticMixerTemperature", "ScrewTemperature", "ScrewRPM", "BarrelTemperature", "ExtrusionVelocity", "CoolerTemperature", "RawMaterialType", "ErrorPercentage");
skimr::skim(analyze2)
pct_miss(analyze2)
vis_miss(analyze2) 
# Análisis
DEFECTUOSAS_A = as.numeric(limpios$`DEFECTUOSAS A`)
DEFECTUOSAS_B = as.numeric(limpios$`DEFECTUOSAS B`)
DEFECTUOSAS_C = as.numeric(limpios$`DEFECTUOSAS C`)

DEF_A = na.omit(DEFECTUOSAS_A)
DEF_B = na.omit(DEFECTUOSAS_B)
DEF_C = na.omit(DEFECTUOSAS_C)

#PERCENTAGE
PER = as.numeric(analyze2$ErrorPercentage)
PERCENTAGE = na.omit(PER)


#EXTRUSIÓN
EXTRU = as.numeric(analyze2$ExtrusionVelocity)
EXTRUSION = na.omit(EXTRU)

#BAR
Correlacion = cor(EXTRUSION, PERCENTAGE, method="pearson")
test = cor.test(EXTRUSION, PERCENTAGE) #
test

# Material
MATE = analyze2$RawMaterialType

# GRAFICAS DATAFRAME 1
pairs(limpios)
ggpairs(limpios)
multi.hist(x=DEF_A, dcol=c("blue", "red"), dlty=c("dotted", "solid"), lwd=c(2, 1), main = c("DEFECTUOSAS PIEZA A"))
multi.hist(x=DEF_B, dcol=c("blue", "red"), dlty=c("dotted", "solid"), lwd=c(2, 1), main = c("DEFECTUOSAS PIEZA B"))
multi.hist(x=DEF_C, dcol=c("blue", "red"), dlty=c("dotted", "solid"), lwd=c(2, 1), main = c("DEFECTUOSAS PIEZA C"))
multi.hist(x=DEF_C, dcol=c("blue", "red"), dlty=c("dotted", "solid"), lwd=c(2, 1), main = c("EXTRUSION"))
cor_clean = cor(limpios)
corrplot(cor_clean,method = 'color',order="alphabet",addCoef.col = 'black')

# GRÁFICAS DATA FRAME 2
pairs(analyze2)
ggpairs(analyze2,upper = list(continuous = wrap("cor", size = 2.5)))
multi.hist(x=PERCENTAGE, dcol=c("blue", "red"), dlty=c("dotted", "solid"), lwd=c(2, 1), main = c("PORCENTAJE"))
cor_clean2 = cor(analyze2)
corrplot(cor_clean2,method = 'color',order="alphabet",addCoef.col = 'black')
  

