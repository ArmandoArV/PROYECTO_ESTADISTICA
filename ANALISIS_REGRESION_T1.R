#limpios = readxl::read_excel(sheet=3, path = paste(getwd(),"/ORIGINAL.xlsx", sep=""), skip=1);
corrMatrix = corrplot::corrplot(cor(limpios));
x1 = limpios$`DEFECTUOSAS B` 
y1 = limpios$`DEFECTUOSAS C` 
LRegression = lm(y1~x1) ;
summary(x1);
summary(y1);
LRegressionStatus = summary(LRegression);
print(LRegressionStatus);
EstCoeff = LRegressionStatus$coefficients;
print(paste("ExVel = (", round(EstCoeff[2, 1], 3), ")*(Pssure) + (", round(EstCoeff[1,1], 3), ')', sep = ''));
plot(x1, y1);
abline(LRegression);
correlation = cor(x1, y1);
determinationCoefficient = correlation^2;
print(paste(round(determinationCoefficient, 4)*100, "% de los datos especificados son explicados de forma correcta por el modelo de regresión lineal.", sep = ''));
plot(LRegression);
round(LRegressionStatus$r.squared, 4) == round(determinationCoefficient, 4);

if(EstCoeff[1, 4] >= 0.05){
  print(paste("Coeff: (", round(EstCoeff[1, 1], 4) , ") has to be deleted ", sep = ''));
  LRegression = lm(y1~x1-1);
}
if(EstCoeff[2, 4] >= 0.05){
  print(paste("Coeff: (", round(EstCoeff[2, 1], 4) , ") has to be deleted", sep = ''));
  LRegression = lm(y1-1~x1);
}
LRegressionStatus = summary(LRegression);
print(LRegressionStatus);
EstCoeff = LRegressionStatus$coefficients;
print(EstCoeff);
plot(LRegression);


