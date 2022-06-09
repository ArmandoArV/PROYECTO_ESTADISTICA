# analyze2 = readxl::read_excel(sheet=3, path = paste(getwd(),"/ORIGINAL.xlsx", sep=""), skip=1);
#colnames(analyze2) <- c("Pressure", "PlasticPumpTemperature", "PlasticMixerTemperature", "ScrewTemperature", "ScrewRPM", "BarrelTemperature", "ExtrusionVelocity", "CoolerTemperature", "RawMaterialType", "ErrorPercentage");
correlationMatrix = corrplot::corrplot(cor(analyze2));
x = analyze2$Pressure 
y = analyze2$ExtrusionVelocity 
linearRegression = lm(y~x);
summary(x);
summary(y);
linearRegressionStatus = summary(linearRegression);
print(linearRegressionStatus);
estimateCoefficients = linearRegressionStatus$coefficients;
print(paste("ExVel = (", round(estimateCoefficients[2, 1], 3), ")*(Pssure) + (", round(estimateCoefficients[1,1], 3), ')', sep = ''));
plot(x, y);
abline(linearRegression);
correlation = cor(x, y);
determinationCoefficient = correlation^2;
print(paste(round(determinationCoefficient, 4)*100, "% de los datos especificados son explicados de forma correcta por el modelo de regresión lineal.", sep = ''));
plot(linearRegression);
round(linearRegressionStatus$r.squared, 4) == round(determinationCoefficient, 4);

if(estimateCoefficients[1, 4] >= 0.05){
  print(paste("Coeff: (", round(estimateCoefficients[1, 1], 4) , ") has to be deleted ", sep = ''));
  linearRegression = lm(y~x-1);
}
if(estimateCoefficients[2, 4] >= 0.05){
  print(paste("Coeff: (", round(estimateCoefficients[2, 1], 4) , ") has to be deleted", sep = ''));
  linearRegression = lm(y-1~x);
}
linearRegressionStatus = summary(linearRegression);
print(linearRegressionStatus);
estimateCoefficients = linearRegressionStatus$coefficients;
print(estimateCoefficients);
plot(linearRegression);


