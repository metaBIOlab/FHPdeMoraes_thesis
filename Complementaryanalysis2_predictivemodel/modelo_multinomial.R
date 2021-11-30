#-------------------modelo multinomial-----------------

# INSTALAR O PACOTE VGAM:
install.packages("VGAM", repos = "http://cran.r-project.org")
require(VGAM)

#categorizando as variáveis 
childc <- NULL
childc[which(banco$CHILD==0)] <- "A"
childc[which(banco$CHILD==1)] <- "B"
childc[which(banco$CHILD==2)] <- "C"

#modelos
modelo1=vglm(childc ~ banco$Verde.PDR + banco$CCV + banco$CTOTAL,
             family=multinomial);summary(modelo1)

modeloverde=vglm(childc ~ banco$Verde.PDR,
                 family=multinomial);summary(modeloverde)

modeloccv=vglm(childc ~ banco$CCV,
               family=multinomial);summary(modeloccv)

modeloverdeccv=vglm(childc ~ banco$Verde.PDR + banco$CCV,
                    family=multinomial);summary(modeloverdeccv)


#coeficientes e OR
beta=coef(modeloccv);beta
OR=exp(beta);OR

#modelo nulo
modelo0=vglm(childc~1,family=multinomial);summary(modelo0)

#testes de comparação de modelos
lrtest(modelo0,modelo1) #prefiro o modelo 1
lrtest(modeloverde,modelo1) #prefiro o modelo 1
lrtest(modeloccv,modelo1) #prefiro o modelo ccv
lrtest(modeloverdeccv,modelo1) #prefiro o modelo verde ccv
lrtest(modeloccv,modeloverdeccv) #prefiro o modelo ccv
lrtest(modeloverde,modeloverdeccv) #prefiro o modelo verde ccv

#log-verossimilhança
lnLccv=logLik(modeloccv);lnLccv
lnLverde=logLik(modeloverde);lnLverde

#medida de comparação 
R2MF=1-(lnLccv/lnLverde);R2MF

#probabilidades
probest=fitted.values(modeloccv);probest

#probabilidades de cada categoria
prob_a=probest[,1];prob_a
prob_b=probest[,2];prob_b
prob_c=probest[,3];prob_c

#categorias estimadas
maxi<- NULL
estimado<- NULL
for(i in 1:60){
  maxi<- max(probest[i,])
  estimado[i] <- which(probest[i,]==maxi)
}
estimado<- estimado-1

#Graficos
plot(prob_a~banco$CCV , col=cores,pch=19, xlab="RLE Index", ylab="Probability of CHILD A")
legend("bottomright", c("Child A","Child B","Child C"), pch=19, col=c(3,4,2))
abline(v=76, lty=2)
plot(prob_b~banco$CCV , col=cores,pch=19, xlab="RLE Index", ylab="Probability of CHILD B")
legend("topright", c("Child A","Child B","Child C"), pch=19, col=c(3,4,2))
abline(v=76, lty=2)
plot(prob_c~banco$CCV , col=cores,pch=19, xlab="RLE Index", ylab="Probability of CHILD C")
legend("topright", c("Child A","Child B","Child C"), pch=19, col=c(3,4,2))
abline(v=76, lty=2)
