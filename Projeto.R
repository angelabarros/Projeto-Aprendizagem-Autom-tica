#PACKAGES
install.packages("faraway")
install.packages("tidyverse")
install.packages("ggcorrplot")


#LIBRARIES
require(ggplot2)
library(ggplot2)
library("faraway")
library(ggcorrplot)

data <- diabetes
?diabetes

class(diabetes) #data.frame

diabetes
#data.frame com 403 observações e 19 variavéis

names(diabetes)

cat("Number of missing value:", sum(is.na(diabetes)), "\n") #number of missing values

numeric.var <- sapply(diabetes, is.numeric)
corr.matrix <- cor(diabetes[,numeric.var])


lm(diabetes$glyhb ~ diabetes$weight + diabetes$height + diabetes$age + diabetes$chol + 
     diabetes$frame + diabetes$hdl + diabetes$waist + diabetes$hip + diabetes$id + diabetes$location + 
     diabetes$bp.1s + diabetes$bp.1d + diabetes$bp.2d + diabetes$bp.2s + diabetes$stab.glu + diabetes$ratio + 
     diabetes$gender + diabetes$time.ppn)

#criar categorias para as idades
diabetes$Age_Cat <- ifelse(diabetes$age < 20, "<20", 
                     ifelse((diabetes$age>=20) & (diabetes$age<=30), "20-30", 
                     ifelse((diabetes$age>30) & (diabetes$age<=40), "30-40",
                     ifelse((diabetes$age>40) & (diabetes$age<=50), "40-50",
                     ifelse((diabetes$age>50) & (diabetes$age<=60), "50-60",
                     ifelse((diabetes$age>60) & (diabetes$age<=70), "60-70",
                     ifelse((diabetes$age>70) & (diabetes$age<=80), "70-80",
                     ifelse((diabetes$age>80) & (diabetes$age<=90), "80-90",">90"))))))))
#data.frame com 403 observações e 20 variavéis

#nº de individuos que correspondem aos intervalos de idades criados
table(diabetes$Age_Cat)

class(diabetes$Age_Cat)


ggplot(aes(x = diabetes$age), data=diabetes) +
  geom_histogram( color='black', fill = "red") +
  scale_x_continuous(limits=c(20,90), breaks=seq(20,90,5)) +
  xlab("Age") +
  ylab("Number of people by age")

ggplot(aes(x = diabetes$Age_Cat), data = diabetes) +
  geom_bar(fill='steelblue') +
  xlab("Age group") +
  ylab("Number of people by age")
#a maioria dos individuos tem entre 30 a 40 anos




#correlação entre os atributos
corr<-round(cor(data),1) #erro
data
data2 <- data[,-gender]
