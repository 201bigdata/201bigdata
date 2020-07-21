
datos<-read.delim('clipboard') # obtiene los datos copiados del excel

attach(datos) # sirve para descomponer los datos en columnas
View(datos) 

library("psych")
describeBy(tiempo,programa)
boxplot(tiempo~programa,xlab='Programas',ylab='Tiempo promedio',
        main="diagrama de cajas de los Programas",
        col= c("blue", "green", "yellow","red"))


# estableciendo el modelo
dca_m<-aov(tiempo~programa,data=datos)
dca_m
# prueba de normalidad con shapiro
shapiro.test(residuals(dca_m))

bartlett.test(tiempo~programa,data=datos)
summary(dca_m)
TukeyHSD(dca_m)
install.packages("agricolae")
library("agricolae")
HSD.test(tiempo,programa,16,44,alpha=0.01,group=TRUE,console=TRUE)


install.packages("multcomp",dependencies=TRUE)
library("multcomp")


