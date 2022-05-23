# Main script
library("readxl")
library("skimr")
library("naniar")
library("Hmisc")
library("corrplot")
analyze = readxl::read_excel(sheet=1, path = "./datos.xlsx");
analyze2 = readxl::read_excel(sheet=4, path = "./datos.xlsx");
# getwd()
#ANALISIS HOJA 1
names(analyze)
# limpios = analyze[-c(1),]
limpios = analyze
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
correlation2 = rcorr(as.matrix(limpios2)) #matriz de correlación
CORRELATION_ANALYSYS2 = correlation2[["r"]]

