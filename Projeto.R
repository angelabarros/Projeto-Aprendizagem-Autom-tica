#PACKAGES
install.packages("faraway")
install.packages("tidyverse")
update.packages("lattice")
install.packages("ggcorrplot")
install.packages("caret")
install.packages("Amelia")
install.packages("tidyr")


#LIBRARIES
require(ggplot2)
require(dplyr)
library(ggplot2)
require(lattice)
library(Amelia)
library(tidyr)
library("faraway")
library(ggcorrplot)
library(reshape2)

library(caret)

data <- diabetes
?diabetes

class(diabetes) #data.frame
summary(data)
diabetes
#data.frame com 403 observações e 19 variavéis

names(diabetes)

cat("Number of missing value:", sum(is.na(diabetes)), "\n") #number of missing values


numeric.var <- sapply(diabetes, is.numeric)
corr.matrix <- cor(diabetes[,numeric.var])


linear_model <- lm(diabetes$glyhb ~ diabetes$weight + diabetes$height + diabetes$age + diabetes$chol + 
     diabetes$frame + diabetes$hdl + diabetes$waist + diabetes$hip + diabetes$id + diabetes$location + 
     diabetes$bp.1s + diabetes$bp.1d + diabetes$bp.2d + diabetes$bp.2s + diabetes$stab.glu + diabetes$ratio + 
     diabetes$gender + diabetes$time.ppn) #Adjusted R-squared: 0.7324




summary(linear_model)
plot(linear_model)




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
data

#nº de individuos que correspondem aos intervalos de idades criados
table(diabetes$Age_Cat)

class(diabetes$Age_Cat)

hist(diabetes$waist , main = paste(" Distribuição das medidas da cintura"), xlab = 'Cintura em inches', ylab = 'Frequência' ,col = 'blue') #NÃO GAUSSSIANO
hist(diabetes$age, main = paste(" Distribuição das idades"), xlab = 'Idade', ylab = 'Frequência' ,col = 'red') #NÃO GAUSSIANO
hist(diabetes$chol, main = paste(" Distribuição do colesterol"), xlab = 'Colesterol', ylab = 'Frequência' ,col = 'gray') #NÃO GAUSSIANO
hist(diabetes$glyhb, main = paste(" Distribuição da Hemoglobina Glycosolated"), xlab = 'Glyhb', ylab = 'Frequência' ,col = 'orange') #NÃO GAUSSIANO


diabetes$Age_Cat
#HISTOGRAMA COM AS IDADES
ggplot(aes(x = diabetes$age), data=diabetes) +
  geom_histogram( color='black', fill = "red") +
  scale_x_continuous(limits=c(20,90), breaks=seq(20,90,5)) +
  xlab("Age") +
  ylab("Number of people by age")

#HISTOGRAMA COM INTERVALO DE IDADES
ggplot(aes(x = diabetes$Age_Cat), data = diabetes) +
  geom_bar(fill='steelblue') +
  xlab("Age group") +
  ylab("Number of people by age")
#a maioria dos individuos tem entre 30 a 40 anos


#RELAÇÃO ENTRE IDADE E COMPRIMENTO DA CINTURA
ggplot(data,aes(x=cut(diabetes$age,breaks=5),y=diabetes$waist,fill=cut(diabetes$age,breaks=5)))+
  geom_boxplot()


#correlação entre os atributos
corr<-round(cor(data),1) #erro - é suposto
data
data2 <- data 
data2

#remover preditores categóricos + variável de resposta
data2$location <- NULL
data2$gender <- NULL
data2$frame <- NULL
data2$glyhb <- NULL #variável de resposta

corr<-round(cor(data2, use = "na.or.complete"),1) #NA's ?
corr
?cor

#correlação entre preditores
ggcorrplot(corr, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 2, 
           method="circle", 
           colors = c("red", "white", "blue"), 
           title="Diagrama de Correlação", 
           ggtheme=theme_bw)
attach(diabetes)

missmap(diabetes, main = "Missing values vs observed")


m1<- lm(diabetes$glyhb~. , data = diabetes)
summary(m1) #Adjusted R-squared: 0.849



#BIG P-VALUES --> REMOVE PREDICTORS
m2<- lm(diabetes$glyhb ~ diabetes$weight + diabetes$height + diabetes$age + diabetes$chol + 
     diabetes$frame + diabetes$hdl + diabetes$hip + diabetes$id + diabetes$location + 
     diabetes$bp.1s + diabetes$bp.1d + diabetes$bp.2d + diabetes$bp.2s + diabetes$stab.glu + diabetes$ratio + 
     diabetes$gender + diabetes$time.ppn)
summary(m2) #Adjusted R-squared: 0.7344

m3<- lm(diabetes$glyhb ~ diabetes$weight + diabetes$age + diabetes$chol + 
     diabetes$frame + diabetes$hdl + diabetes$hip + diabetes$id + diabetes$location + 
     diabetes$bp.1s + diabetes$bp.1d + diabetes$bp.2d + diabetes$bp.2s + diabetes$stab.glu + diabetes$ratio + 
     diabetes$gender + diabetes$time.ppn)
summary(m3) #Adjusted R-squared: 0.7367

m4<- lm(diabetes$glyhb ~ diabetes$weight + diabetes$age + diabetes$chol + 
     diabetes$frame + diabetes$hdl + diabetes$hip + diabetes$location + 
     diabetes$bp.1s + diabetes$bp.1d + diabetes$bp.2d + diabetes$bp.2s + diabetes$stab.glu + diabetes$ratio + 
     diabetes$gender + diabetes$time.ppn)
summary(m4) #Adjusted R-squared: 0.7391

m5<- lm(diabetes$glyhb ~ diabetes$weight + diabetes$age + diabetes$chol + 
     diabetes$frame + diabetes$hip + diabetes$location + 
     diabetes$bp.1s + diabetes$bp.1d + diabetes$bp.2d + diabetes$bp.2s + diabetes$stab.glu + diabetes$ratio + 
     diabetes$gender + diabetes$time.ppn)
summary(m5) #Adjusted R-squared: 0.7408
#lab3 -- predict (new data)

?glm
#LAB4
dim(diabetes)
diabetes$has_diabetes=rep(0,403)
diabetes$has_diabetes[diabetes$glyhb>7]=1

table(diabetes$has_diabetes)

#OBJETIVO: Obter o AIC mais baixo possível

#o glyhb dá erro --> variável de resposta <--- PERGUNTAR
glm.fit=glm(diabetes$has_diabetes~chol+stab.glu+hdl+ratio+location+age+gender+
              height+weight+frame+bp.1s+bp.1d+bp.2s+bp.2d+waist+hip+time.ppn, 
              data=diabetes, family=binomial)
summary(glm.fit) #AIC: 83.357  

glm.fit=glm(diabetes$has_diabetes~chol+stab.glu+hdl+age+height+weight+frame+
              bp.1s+bp.1d+bp.2s+bp.2d+waist+hip, data=diabetes, family=binomial)
summary(glm.fit) #AIC: 79.981 

glm.fit=glm(diabetes$has_diabetes~chol+stab.glu+hdl+age+gender+height+weight+
              frame+bp.1s+bp.1d+bp.2s+bp.2d+waist+hip, data=diabetes, family=binomial)
summary(glm.fit) #AIC: 79.67 

glm.fit=glm(diabetes$has_diabetes~chol+stab.glu+hdl+height+weight+frame+bp.1s+
              bp.1d+bp.2s+bp.2d+waist+hip, data=diabetes, family=binomial)
summary(glm.fit) #AIC: 79.614 

glm.fit=glm(diabetes$has_diabetes~chol+stab.glu+hdl+height+weight+bp.1s+
              bp.1d+bp.2s+bp.2d+waist+hip, data=diabetes, family=binomial)
summary(glm.fit) #AIC: 77.964

glm.fit=glm(diabetes$has_diabetes~chol+stab.glu+height+weight+bp.1s+
              bp.1d+bp.2s+bp.2d+hip, data=diabetes, family=binomial)
summary(glm.fit) #AIC 74.084

glm.fit=glm(diabetes$has_diabetes~chol+stab.glu+height+weight+bp.1s+bp.1d+
              bp.2d+hip, data=diabetes, family=binomial)
summary(glm.fit) #AIC 72.468

glm.fit=glm(diabetes$has_diabetes~chol+stab.glu+height+weight+bp.1d+
              bp.2d+hip, data=diabetes, family=binomial)
summary(glm.fit) #AIC 70.884    MELHOR

# exp(0.02225) - 1 aumento unitario de colesterol aumenta o risco de ter diabetes aumenta 2,2%

class(diabetes)
#DIVISÃO DADOS DE TREINO -- DADOS DE TESTE
n = 201
nr = nrow(diabetes)
train_data_aux = split(diabetes, rep(1:ceiling(nr/n), each=n, length.out=nr)) #DIVISÃO DE METADE DOS DADOS
train_data = train_data_aux$`1` #PRIMEIROS 201 REGISTOS
test_data = train_data_aux$`2` #RESTANTES

#treinar o meu melhor modelo com os dados de treino
#fazer predict sobre o resto da db

#TODOS OS DADOS
glm_all.probs_all_data=predict(glm.fit, type="response")             
glm_all.probs_all_data
glm_all.pred=rep(0,403)                                   
glm_all.pred[glm_all.probs_all_data>0.5]=1        
table(glm_all.pred, diabetes$has_diabetes)      

#DADOS DE TESTE
glm.probs_teste=predict(glm.fit, test_data, type="response")                   
glm.probs_teste
glm.pred=rep(0,201)                             
glm.pred[glm.probs_teste>0.5]=1                   
table(glm.pred, test_data$has_diabetes)          

#DADOS DE TREINO
glm2.probs_treino=predict(glm.fit, train_data,type="response")
glm2.probs_treino
glm2.pred=rep(0,201)
glm2.pred[glm2.probs_treino>0.5]=1  
table(glm2.pred, train_data$has_diabetes) 



##NEW ATTEMPT
glm_new.fit=glm(diabetes$has_diabetes~chol+stab.glu+height+weight+bp.1d+bp.2d+hip, data=diabetes, family=binomial)
summary(glm_new.fit)

PredictTrain <- predict(glm_new.fit, type = "response") #qd coloco os dados da erro
summary(PredictTrain)

teste_140 <- diabetes[1:140,]

empty_df = diabetes[FALSE,]

for (i in 1:nrow(diabetes)) {
  cholesterol_aux <- diabetes[2]$chol[i]
  stab.glu_aux    <- diabetes[3]$stab.glu[i]
  height_aux      <- diabetes[10]$height[i]
  weight_aux      <- diabetes[11]$weight[i]
  bp.1d_aux       <- diabetes[14]$bp.1d[i]
  bp.2d_aux       <- diabetes[16]$bp.2d[i]
  
  if (!is.na(cholesterol_aux) && !is.na(stab.glu_aux) && !is.na(height_aux)
      && !is.na(weight_aux) && !is.na(bp.1d_aux) && !is.na(bp.2d_aux)) {
    empty_df <- bind_rows(empty_df, diabetes[i,])
  }
  
  
  cholesterol_aux <- NA
  stab.glu_aux    <- NA
  height_aux      <- NA
  weight_aux      <- NA
  bp.1d_aux       <- NA
  bp.2d_aux       <- NA
  
}


auxiliar <- empty_df[1:140,]

#tapply(PredictTrain, teste_140$has_diabetes, mean)   
#tapply(PredictTrain, auxiliar$, mean) 


threshold_0.5 <- table(auxiliar$has_diabetes, PredictTrain > 0.5)
threshold_0.5

accuracy_0.5 <- round(sum(diag(threshold_0.5))/sum(threshold_0.5),2)
sprintf("Accuracy is %s",accuracy_0.5)


PredictTest <- predict(glm_new.fit, type = "response", newdata = test_data)
test_tab <- table(test_data$has_diabetes, PredictTest > 0.5)
test_tab

accuracy_test <- round(sum(diag(test_tab))/sum(test_tab),2)
sprintf("Accuracy on test set is %s", accuracy_test)


#########ANOVA#############
anova(glm.fit, test = "Chisq")
#A large p-value here indicates that the model without the variable explains 
#more or less the same amount of variation. 


table(glm.probs_teste)



################################################################################ 
glm_all.probs_all_data <- ifelse(glm_all.probs_all_data > 0.5,1,0)
misClasificError <- mean(glm_all.probs_all_data != auxiliar$has_diabetes)
print(paste('Accuracy',1-misClasificError)) #0,907 accuracy 


