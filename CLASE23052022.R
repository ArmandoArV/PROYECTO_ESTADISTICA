x = c(2,5,1,3,4,1,5,3,4,2);
y = c(50,57,41,54,54,38,63,48,59,46);

plot(x,y,pch=16,col="blue",main="Relación entre ventas \n
     y número de comerciales",xlab="Núm. de comerciales",ylab="Ventas")
cor(x,y,method="pearson")
cor.test(x,y,method="pearson")
