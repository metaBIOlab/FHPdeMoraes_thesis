#-----------------------#
#   Projeto Alzheimer   #
#-----------------------#

#----Pacotes----
library(tidyverse) #para fácil manipulação e visualização de dados
library(caret) #para facilitar o fluxo de trabalho de aprendizado de máquina
library(glmnet) #para computação de regressão penalizada
library(mlbench) #para pegar os dados 
library(arm) #para o modelo glm bayesiano
library(MASS) #para o modelo stepwise
library(Epi)
library(ROCR)
library(pROC)

#Do modelo multinomial
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
library(stargazer)

#Do modelo Beta
require(betareg)

#----Abrindo o banco----
banco <- read.csv2("C:/Users/rebecca.osouza.REDEDOR/OneDrive - Rede D'Or/Agosto 2019/Bart - 29-08-19/CSF_IDOR.csv",dec=",",sep=";",header=T)
head(banco)
names(banco)
dim(banco)
banco <- banco[,-1]

#retirando as variaveis em vermelho e a idade
banco2<- banco[,-c(2,3,16:24,44:48)]

#tirando as correlacionadas
ex <- c(which(names(banco2)=="MIP1b"),
        which(names(banco2)=="IP10"),
        which(names(banco2)=="RANTES"),
        which(names(banco2)=="X5.HIAA"),
        which(names(banco2)=="HVA"),
        which(names(banco2)=="DOPAC.Dopamine"),
        which(names(banco2)=="Glutamate"),
        which(names(banco2)=="Glutamine.Glutamate"),
        which(names(banco2)=="GABA"))
banco2 <- banco2[,-ex]


#----Preparando os dados----
#Banco com apenas DA e Controle
banco.bin <- subset(banco2,DX=="Controle"|DX=="DA")
head(banco.bin)
names(banco.bin)
dim(banco.bin)
banco.bin$DX<- droplevels(banco.bin$DX)


#Dividindo os dados em treinamento (80%) e teste (20%)
table(banco.bin$DX)
#vamos colher uma Sample de 8 pacientes sendo 5 controle e 3 DA (amostragem estratificada porporcional)
#set.seed(10)
#training.samples <- banco.bin$DX %>% 
#  createDataPartition(p = 0.8, list = FALSE)
test.samples <- c(sample(which(banco.bin$DX=="DA"), 3),sample(which(banco.bin$DX=="Controle"), 5))
train.data  <- banco.bin[-test.samples, ]
test.data <- banco.bin[test.samples, ]


#----Verificando as correlacoes das covariaveis----
#correlacao entre as variaveis
#cor.x <- cor(train.data[,-c(1,49)])
#write.csv2(cor.x, "C:/Users/rebecca.osouza.REDEDOR/OneDrive - Rede D'Or/Bart - 29-08-19/Correlacao.csv")

#ind1<- c(8,12,15,35,33,34,34,33,34,39,41,39,40,36,43)
#ind2<- c(13,11,14,29,30,30,32,32,33,36,36,43,43,43,41)
#for(i in 1:15){
#  aux<-cor.test(train.data[,ind1[i]], train.data[,ind2[i]])
#  print(aux$p.value)
#}

#retirando as variaveis selecionadas
#names(train.data)
#names(test.data)
#ex <- c(which(names(train.data)=="MIP1b"),
#        which(names(train.data)=="IP10"),
#        which(names(train.data)=="RANTES"),
#        which(names(train.data)=="X5.HIAA"),
#        which(names(train.data)=="HVA"),
#        which(names(train.data)=="DOPAC.Dopamine"),
#        which(names(train.data)=="Glutamate"),
#        which(names(train.data)=="Glutamine.Glutamate"),
#        which(names(train.data)=="GABA"))
#train.data <- train.data[,-ex]
#test.data <- test.data[,-ex]

train.data$DX<-ifelse(train.data$DX == "DA", 1, 0)
test.data$DX<-ifelse(test.data$DX == "DA", 1, 0)


#---- Regressão logistica com LASSO ---- 

#matriz com as covariaveis
x <- model.matrix(DX~., train.data)[,-1] 
dim(x)

y <- train.data$DX

#Encontrar o melhor lambda usando a fun cross-validation
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
#plot(cv.lasso)

#Ajustar o modelo final para os dados de treinamento
model.lasso <- glmnet(x, y, alpha = 1, family = "binomial",
                      lambda = cv.lasso$lambda.min)

#Coeficientes da regressão
coef.l <- coef(model.lasso)

#quais variaveis ficam
fica <- which(coef.l != 0)-1 
fica <- c(fica[-1], 24) #tirando o intercepto e incluindo DX

#novo banco so com as variaveis selecionados pelo lasso
train.data.lasso <- train.data[, fica]
test.data.lasso <- test.data[, fica]

#novo modelo proposto pelo lasso
model.lasso2 <- bayesglm(DX ~. , data = train.data.lasso, family = binomial(link='logit'))
summary(model.lasso2)

#Previsoes sobre os dados de teste
probabilities.lasso <- model.lasso2 %>% predict(test.data.lasso, type = "response")
predicted.classes.lasso <- ifelse(probabilities.lasso > 0.5, 1, 0)

# Model accuracy
mean(predicted.classes.lasso == test.data$DX)
predicted.classes.lasso

#ROC
roc(test.data$DX, as.vector(predicted.classes.lasso), percent=F, ci.alpha=0.9, stratified=FALSE, plot=F, grid=TRUE, legacy.axes = TRUE, reuse.auc = TRUE,
    print.auc = TRUE, print.thres.col = "blue", ci=TRUE, ci.type="bars", print.thres.cex = 0.7,
    main ="ROC curve",
    ylab="Sensitivity (true positive rate)", xlab="1-Specificity (false positive rate)" )


#---- Logistico Stepwise ----
#rodando o modelo Bayesiano
model.step <- bayesglm(DX ~., data = train.data, family = binomial(link='logit')) %>%
  stepAIC(trace = FALSE)
#coef(model.step)
summary(model.step)


#Previsoes
probabilities.step <- predict(model.step, test.data, type = "response")
predicted.classes.step <- ifelse(probabilities.step > 0.5, 1, 0)

#Model accuracy
mean(predicted.classes.step == test.data$DX)
predicted.classes.step

#ROC
roc(test.data$DX, as.vector(predicted.classes.step), percent=F, ci.alpha=0.9, stratified=FALSE, plot=F, grid=TRUE, legacy.axes = TRUE, reuse.auc = TRUE,
    print.auc = TRUE, print.thres.col = "blue", ci=TRUE, ci.type="bars", print.thres.cex = 0.7,
    main ="ROC curve",
    ylab="Sensitivity (true positive rate)", xlab="1-Specificity (false positive rate)" )




#--------Gráficos---------
biomarcador<- names(banco.bin)[2:32] 
for(i in 1:length(biomarcador)){
  boxplot(banco.bin[,i] ~ banco.bin$DX, ylab=biomarcador[i])
}


#----------Modelo com oxitocina e tau/ab42------------
banco.bin2 <- subset(banco,DX=="Controle"|DX=="DA")
banco.bin2 <- banco.bin2[,-c(2,3,16:23,44:48)]
names(banco.bin2)

#Dividindo os dados em treinamento (80%) e teste (20%)
train.data2  <- banco.bin2[-test.samples, ]
test.data2 <- banco.bin2[test.samples, ]

train.data2$DX<-ifelse(train.data2$DX == "DA", 1, 0)
test.data2$DX<-ifelse(test.data2$DX == "DA", 1, 0)


#----Oxytocin
modeloxi <- bayesglm(DX ~ Oxytocin, data = train.data2, family = binomial(link='logit'))
summary(modeloxi)

#Previsoes
probabilities.oxi <- modeloxi %>% predict(test.data2, type = "response")
predicted.classes.oxi <- ifelse(probabilities.oxi > 0.5, 1, 0)

#Model accuracy
mean(predicted.classes.oxi == test.data2$DX)

#ROC
roc(test.data2$DX, as.vector(predicted.classes.oxi), percent=F, ci.alpha=0.9, stratified=FALSE, plot=TRUE, grid=TRUE, legacy.axes = TRUE, reuse.auc = TRUE,
    print.auc = TRUE, print.thres.col = "blue",
    ci=TRUE, ci.type="bars", print.thres.cex = 0.7, main ="ROC curve",
    ylab="Sensitivity (true positive rate)", xlab="1-Specificity (false positive rate)")

#----Tau/Ab42
modeltauab <- bayesglm(DX ~ Tau.Ab42, data = train.data2, family = binomial(link='logit'))
summary(modeltauab)

#Previsoes
probabilities.tauab <- modeltauab %>% predict(test.data2, type = "response")
predicted.classes.tauab <- ifelse(probabilities.tauab > 0.5, 1, 0)

#Model accuracy
mean(predicted.classes.tauab == test.data2$DX)

#ROC
roc(test.data2$DX, as.vector(predicted.classes.tauab), percent=F, ci.alpha=0.9, stratified=FALSE, plot=TRUE, grid=TRUE, legacy.axes = TRUE, reuse.auc = TRUE,
    print.auc = TRUE, print.thres.col = "blue",
    ci=TRUE, ci.type="bars", print.thres.cex = 0.7, main ="ROC curve",
    ylab="Sensitivity (true positive rate)", xlab="1-Specificity (false positive rate)")

#---- Ocytocin + Tau/Ab42
modeloxitau <- bayesglm(DX ~ Oxytocin + Tau.Ab42, data = train.data2, family = binomial(link='logit'))
summary(modeloxitau)

#Previsoes
probabilities.oxitau <- modeloxitau %>% predict(test.data2, type = "response")
predicted.classes.oxitau <- ifelse(probabilities.oxitau > 0.5, 1, 0)

#Model accuracy
mean(predicted.classes.oxitau == test.data2$DX)

#ROC
roc(test.data2$DX, as.vector(predicted.classes.oxitau), percent=F, ci.alpha=0.9, stratified=FALSE, plot=TRUE, grid=TRUE, legacy.axes = TRUE, reuse.auc = TRUE,
    print.auc = TRUE, print.thres.col = "blue",
    ci=TRUE, ci.type="bars", print.thres.cex = 0.7, main ="ROC curve",
    ylab="Sensitivity (true positive rate)", xlab="1-Specificity (false positive rate)")


#----------Oxytocin + todas as outras------------
dim(train.data)
v <- c(1:6,8:24)
accu <- NULL
for(i in v){
  data.aux <- train.data[,c(7,i,25)]
  modelaux <- bayesglm(DX ~ ., data = data.aux, family = binomial(link='logit'))
  resumo<- summary(modelaux)
  p.value<- resumo$coefficients[2:3,4]
  
  if(sum(which(p.value!=0))==3){ #se todas as var sao significativas
    #Previsoes
    probabilities.aux <- modelaux %>% predict(test.data, type = "response")
    predicted.classes.aux <- ifelse(probabilities.aux > 0.5, 1, 0)
    
    #Model accuracy
    accu[i] <- mean(predicted.classes.aux == test.data$DX)
  }
}
accu
names(train.data)[c(2,3)]


#-------------------------Modelo Multinomial-------------------
#tenho que determinar a categoria base
banco2$DX <- relevel(banco2$DX, ref = "Controle")

#Dividindo os dados em treinamento (80%) e teste (20%)
table(banco2$DX)
prop.table(table(banco2$DX))
12*prop.table(table(banco2$DX))
#vamos colher uma Sample de 12 pacientes sendo
#3 ccl, 6 controle e 3 DA (amostragem estratificada porporcional)

test.samples <- c(sample(which(banco2$DX=="DA"), 3),
                  sample(which(banco2$DX=="CCL"), 3),
                  sample(which(banco2$DX=="Controle"), 6))
train.data  <- banco2[-test.samples, ]
test.data <- banco2[test.samples, ]


#---- Regressão multinomial com LASSO ---- 


#matriz com as covariaveis
x <- model.matrix(DX~., train.data)[,-1] 
dim(x)

y <- train.data$DX

#Encontrar o melhor lambda usando a fun cross-validation
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "multinomial")
#plot(cv.lasso)

#Ajustar o modelo final para os dados de treinamento
multi.lasso <- glmnet(x, y, alpha = 1, family = "multinomial",
                      lambda = cv.lasso$lambda.min)

#Coeficientes da regressão
coef.l <- coef(multi.lasso)

#quais variaveis ficam
fica <- which(coef.l$Controle != 0 | coef.l$CCL != 0 | coef.l$DA != 0)-1 
fica <- c(fica[-1], 24) #tirando o intercepto e incluindo DX

#novo banco so com as variaveis selecionados pelo lasso
train.data.lasso <- train.data[, fica]
test.data.lasso <- test.data[, fica]

#novo modelo proposto pelo lasso
multi.lasso2 <- multinom(DX ~ ., data = train.data.lasso)

#summary(multi.lasso2)
stargazer(multi.lasso2, type="text", out="multi1.htm")

z <- summary(multi.lasso2)$coefficients/summary(multi.lasso2)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
t(p)

#Para facilitar a interpretação:
coef.multi.l = exp(coef(multi.lasso2))

#Previsoes
predicted.classes.multi.l <- multi.lasso2 %>% predict(test.data.lasso, type = "class")

#Model accuracy
mean(predicted.classes.multi.l == test.data.lasso$DX)
as.numeric(predicted.classes.multi.l)

#ROC
multiclass.roc(as.numeric(test.data.lasso$DX), as.numeric(predicted.classes.multi.l), percent=F, ci.alpha=0.9, stratified=FALSE, plot=F, grid=TRUE, legacy.axes = TRUE, reuse.auc = TRUE,
               print.auc = F, print.thres.col = "blue", ci.type="bars", print.thres.cex = 0.7, main ="ROC curve",
               ylab="Sensitivity (true positive rate)", xlab="1-Specificity (false positive rate)")


#-------------Regressão multinomial Stepwise---------
multi.step <- multinom(DX ~ ., data = train.data)%>%
  stepAIC(trace = FALSE)
summary(multi.step)
stargazer(multi.step, type="text", out="multi1.htm")

z <- summary(multi.step)$coefficients/summary(multi.step)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
t(p)

#Para facilitar a interpretação:
coef.multi.s = exp(coef(multi.step))

#Previsoes
predicted.classes.multi.s <- multi.step %>% predict(test.data, type = "class")

#Model accuracy
mean(predicted.classes.multi.s == test.data$DX)

#ROC
multiclass.roc(as.numeric(test.data$DX), as.numeric(predicted.classes.multi.s), percent=F, ci.alpha=0.9, stratified=FALSE, plot=F, grid=TRUE, legacy.axes = TRUE, reuse.auc = TRUE,
               print.auc = F, print.thres.col = "blue", ci.type="bars", print.thres.cex = 0.7, main ="ROC curve",
               ylab="Sensitivity (true positive rate)", xlab="1-Specificity (false positive rate)")

lrtest(multi.lasso2,multi.step)

#-----Multinomial  Oxytocin-----------

banco2.1<- banco2
banco2.1$Tau.Ab42<- banco$Tau.Ab42

#Dividindo os dados em treinamento (80%) e teste (20%)
train.data2  <- banco2.1[-test.samples, ]
test.data2 <- banco2.1[test.samples, ]

m.multi.nova <- multinom(DX ~ OxyTau.Ab42, data = train.data2)
stargazer(m.multi.nova, type="text", out="multi1.htm")

z <- summary(m.multi.nova)$coefficients/summary(m.multi.nova)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
t(p)

#Para facilitar a interpretação:
coef.multi = exp(coef(m.multi.nova))
t(coef.multi)
#Previsoes
predicted.classes.multi.nova <- m.multi.nova %>% predict(test.data2, type = "class")

#Model accuracy
mean(predicted.classes.multi.nova == test.data2$DX)

#ROC
multiclass.roc(as.numeric(test.data2$DX), as.numeric(predicted.classes.multi.nova), percent=F, ci.alpha=0.9, stratified=FALSE, plot=F, grid=TRUE, legacy.axes = TRUE, reuse.auc = TRUE,
               print.auc = F, print.thres.col = "blue", ci.type="bars", print.thres.cex = 0.7, main ="ROC curve",
               ylab="Sensitivity (true positive rate)", xlab="1-Specificity (false positive rate)")



#----------------Modelos para o MMSE----------------------
dim(banco)
dim(banco2)

banco3<- banco2[,-24]
banco3$MMSE<- banco$MMSE
dim(banco3)
names(banco3)

#---- Regressão poisson com LASSO ---- 

#matriz com as covariaveis
x <- model.matrix(MMSE~., banco3)[,-1] 
dim(x)

y <- banco3$MMSE

#Encontrar o melhor lambda usando a fun cross-validation
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "poisson")
plot(cv.lasso)

#Ajustar o modelo final para os dados de treinamento
poi.lasso <- glmnet(x, y, alpha = 1, family = "poisson",
                      lambda = cv.lasso$lambda.min)

#Coeficientes da regressão
coef.l <- coef(poi.lasso)

#quais variaveis ficam
fica <- which(coef.l != 0)-1 
fica <- c(fica[-1], 24) #tirando o intercepto e incluindo DX

#novo banco so com as variaveis selecionados pelo lasso
banco3.lasso <- banco3[, fica]

#novo modelo proposto pelo lasso
poi.lasso2 <- glm(MMSE ~. , data = banco3.lasso, family = poisson(link='log'))
summary(poi.lasso2)

#---- Regressão Poisson ----

#rodando o modelo Bayesiano
full.poi<- glm(MMSE ~., data = banco3, family = poisson(link='log'))
coef(full.poi)
summary(full.poi)


#---- Poisson Stepwise ----
#rodando o modelo Bayesiano
poi.step <- glm(MMSE ~., data = banco3, family = poisson(link='log')) %>%
  stepAIC(trace = FALSE)
coef(poi.step)
summary(poi.step)

anova(poi.lasso2, poi.step, test = "Chisq")

plot(banco3$MMSE~banco3$GABA..Glutamate)

#------------Modelo para o A7/A5 -------------------

banco4<- banco2[,-24]
banco4$A7A5<- banco$A7.A5
dim(banco4)
names(banco4)

banco4$A7A5<- ifelse(banco4$A7A5==1, banco4$A7A5-0.0000001, banco4$A7A5)
banco4$A7A5<- ifelse(banco4$A7A5==0, banco4$A7A5+0.0000001, banco4$A7A5)


#---- Regressão Poisson ----

#rodando o modelo Bayesiano
full.beta<- betareg(A7A5 ~., data = banco4)
summary(full.beta)

AIC(full.beta)


#---- Poisson Stepwise ----
#rodando o modelo Bayesiano
beta.step <- betareg(A7A5 ~., data = banco4) %>%
  stepAIC(trace = FALSE)
coef(beta.step)
summary(poi.step)

anova(poi.lasso2, poi.step, test = "Chisq")
