# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#
# * PROJECT INFO *
#   TITLE:
#   LEAD: 
#
# * THIS SCRIPT *
#   AIM: 
#   
#   AUTHORS: JOÃO VIDEIRA
#   CONTACT: joaoamvideira@gmail.com
#   
#   DATE: 
#
#   COPYRIGHT STATEMENT:
#
#   NOTES: 
# 
#   REFERENCES:
#
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# AJUSTES E CORRELAÇÕES ----------------------------------------------------------------
install_load("corrplot")
library(corrplot)
cor(log(basefinal3$`area natural`),basefinal3$altitude )
cor(log(basefinal3$`area natural`),log(basefinal3$corpodagua+1))

#matriz de correlação da base. Verificar possíveis multicolinearidades ( LINEAR!)
#quanto mais vermelho, mais negativa e azul o oposto. quanto maior o numero maior a correlação
#identificou-se multicolinearidade entre IFDM(2014)e IFDM (2015) e entre essas variáveis e tx popurbana
#correlação linear entre X e os Z é baixa 0.21 e 0.06.
corrplot(cor(basefinal2[,-c(2,3,4,5,8,17,18)]), method ='circle')

# GRÁFICOS ----------------------------------------------------------------
##Análise da relação entre X e Zs
#Corelação baixa e com sentido( negativo) não esperado
plot(log(basefinal2$`area natural`)~log(basefinal2$propcdagua), xlab = "log-Área Natural", ylab = "log-prop.C.de água")
cor(log(basefinal2$`area natural`),log(basefinal2$propcdagua+1))#-0.218

# parece haver, mesmo que fraca, uma relação não linear
plot(log(basefinal2$`area natural`)~basefinal2$altitude,xlab = "log-Área Natural", ylab = "Alitude" )
cor(log(basefinal2$`area natural`),basefinal2$altitude)#0.334

Basedig<-compmeans(log(basefinal2$`area natural`),basefinal2$` Tem Base Digitalizada`)
Basedig
CAR<-compmeans(log(basefinal2$`area natural`),basefinal2$`Tem CAR`)
CAR
#pela análise das tabelas e dos boxplots não parece haver correlação entre tais variáveis. Médias de AN
#não diferem muito entre catogorias das variaveis categoricas

##Analise da variável X
# histograma da variável independente
hist(basefinal2$`area natural`, col = "lightblue", breaks = 100,xlab = "Área Naural relativa",ylab = "Frequencia",main = "")
# distribuição assimetrica à esquerda, valores mais baixos mais frequentes
rug(basefinal2$`area natural`)
abline(v = 50, col = "magenta", lwd = 1)

# histograma d log da área natural. Se aproxima mais de uma dist. Normal
hist(log(basefinal2$`area natural`), col = "lightblue", breaks = 100,xlab = "Área Naural relativa",ylab = "Frequencia",main = "")

##Analise da variável Y
# histograma do IFDM
hist((basefinal2$IFDM ), col = "lightblue", breaks = 100,xlab = "IFDM-2016",ylab = "Frequencia",main = "")
hist(log(basefinal2$IFDM ), col = "lightblue", breaks = 100,xlab = "log-IFDM-2016",ylab = "Frequencia",main = "")

##Analise de outliers da variaveis x e y
boxplot(log(basefinal2$`area natural`), 
        names = "logAN", 
        col = "lightgreen")
##Analise de outliers da variaveis x e y
boxplot(basefinal2$IFDM, 
        names = "IFDM", 
        col = "lightblue")

summary(log(basefinal2$`area natural`))

logAN<-log(basefinal2$`area natural`)
basefinal2<-cbind(basefinal2,logAN)
basefinal3<-filter(basefinal2,logAN>-4)

hist(basefinal3$logAN, col = "lightblue", breaks = 100,xlab = "log-Área Natural",ylab = "Frequencia",main = "")

##Analise de outliers da variaveis x e y
boxplot(basefinal3$logAN, 
        names = "logAN", 
        col = "lightgreen")

## Grafico de dispersao y e x
plot(basefinal3$logAN ~basefinal3$IFDM ,xlab = "log-Área Natural", ylab = "IFDM-2016" )

# histograma do log Prop. corpo d´água. Se aproxima de uma dist. Normal
hist(log(basefinal3$propcdagua), col = "lightblue", breaks = 100,xlab = "Prop.Corpo d´agua",ylab = "Frequencia",main = "")

# histograma do altitude
hist(basefinal2$altitude, col = "lightblue", breaks = 100,xlab = "Altitude",ylab = "Frequencia",main = "")

summary(basefinal3)

##Histogramas covariáveis 
attach(basefinal3)
# histograma do IFDM-2014
hist(`IFDM-2014`, col = "lightblue", breaks = 100,xlab = "IFDM-2014",ylab = "Frequencia",main = "")

# histograma do IFDM-2015
hist(`IFDM-2015`, col = "lightblue", breaks = 100,xlab = "IFDM-2015",ylab = "Frequencia",main = "")

# histograma do log  Dendidade ppulacional
hist(log(denpop), col = "lightblue", breaks = 100,xlab = "log da densidade pop.",ylab = "Frequencia",main = "")

# histograma do log PIB per capita
hist(log(pibpercapita), col = "lightblue", breaks = 100,xlab = "log do PIB per capita.",ylab = "Frequencia",main = "")

# histograma do taxa de coleta de residuo domiciliar
hist(log(basefinal2$`tx de coleta de rdo`) , col = "lightblue", breaks = 100,xlab = "Log Coleta Residuo domiciliar",ylab = "Frequencia",main = "")

# histograma do tx. Pop. Urbana
hist( log(txpopurbana) , col = "lightblue", breaks = 100,xlab = "log taxa pop. urbana",ylab = "Frequencia",main = "")

# histograma do log Area agricola plantada
hist(log(areaagricola), col = "lightblue", breaks = 100,xlab = "log-Area Agricultura.",ylab = "Frequencia",main = "")

# histograma do rebanho or hectare
hist(Rebanhoporarea, col = "lightblue", breaks = 100,xlab = "Rebanho por hectare.",ylab = "Frequencia",main = "")

# histograma do log PIB per capita
hist(desppercapita, col = "lightblue", breaks = 100,xlab = "despesas munic per capita.",ylab = "Frequencia",main = "")

# Modelo Variáveis Instrumentais ----------------------------------------------------------------
# estimaçaõ do modelo
# estimaçaõ tres modelos 
attach(basefinal3)
m1 <- ivreg( basefinal3$IFDM~ basefinal3$logAN+ basefinal3$Rebanhoporarea+log(denpop+1)+desppercapita+log(areaagricola+1)+ log(basefinal3$`tx de coleta de rdo`+1)+estado+basefinal3$` Tem Base Digitalizada`+basefinal3$`Tem CAR`|basefinal3$` Tem Base Digitalizada`+basefinal3$`Tem CAR`+ log(basefinal3$altitude)+basefinal3$Rebanhoporarea+log(denpop+1)+desppercapita+log(areaagricola+1)+log(basefinal3$`tx de coleta de rdo`+1)+ estado)
m2 <- ivreg( basefinal3$IFDM~ basefinal3$logAN+ basefinal3$Rebanhoporarea+log(denpop+1)+desppercapita+log(areaagricola+1)+ log(basefinal3$`tx de coleta de rdo`+1)+estado+basefinal3$` Tem Base Digitalizada`+basefinal3$`Tem CAR`|log(basefinal3$propcdagua+1)+basefinal3$` Tem Base Digitalizada`+basefinal3$`Tem CAR`+basefinal3$Rebanhoporarea+log(denpop+1)+desppercapita+log(areaagricola+1)+log(basefinal3$`tx de coleta de rdo`+1)+ estado)
m3 <- ivreg( basefinal3$IFDM~ basefinal3$logAN+ basefinal3$Rebanhoporarea+log(denpop+1)+desppercapita+log(areaagricola+1)+ log(basefinal3$`tx de coleta de rdo`+1)+estado+basefinal3$` Tem Base Digitalizada`+basefinal3$`Tem CAR`|log(basefinal3$propcdagua+1)+basefinal3$` Tem Base Digitalizada`+basefinal3$`Tem CAR`+ log(basefinal3$altitude)+basefinal3$Rebanhoporarea+log(denpop+1)+desppercapita+log(areaagricola+1)+log(basefinal3$`tx de coleta de rdo`+1)+ estado)

m3 <- ivreg( basefinal3$IFDM~ basefinal3$logAN+ log(denpop+1)+log(Rebanhoporarea)+log(areaagricola)+desppercapita+log(basefinal3$txpopurbana)+log(basefinal3$txpopurbana)+basefinal3$`tx de coleta de rdo`|log(basefinal3$propcdagua+1)+log(basefinal3$altitude)+log(denpop+1)+log(Rebanhoporarea)+log(areaagricola)+desppercapita+log(basefinal3$txpopurbana)+basefinal3$`tx de coleta de rdo`)


# robust coefficient summary for 1.
coeftest(m1, vcov = vcovHC, type = "HC1")
coeftest(m2, vcov = vcovHC, type = "HC1")
coeftest(m3, vcov = vcovHC, type = "HC1")


# gather robust standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(m1, type = "HC1"))),
               sqrt(diag(vcovHC(m2, type = "HC1"))),
               sqrt(diag(vcovHC(m3, type = "HC1"))))

rob_se1 <- list(sqrt(diag(vcovHC(m3, type = "HC1"))))
summary(rob_se1)
rob_se1
summary(m3)

# generate table
library(stargazer)
stargazer(m3,
          header = FALSE,
          type = "text",
          omit.table.layout = "n",
          digits = 3,
          column.labels = c("IVs: Altitude, Prop. C.A."),
          dep.var.labels.include = FALSE,
          dep.var.caption = "LogAN")

# first-stage regressions
mod_relevance1 <- lm(`IFDM-2016` ~ `Reg Gov`+desppercapita)
mod_relevance2 <- lm(`IFDM-2016` ~ desppercapita)
mod_relevance3 <- lm(`IFDM-2016` ~ `Reg Gov`)

# check instrument relevance for model (2)
linearHypothesis(mod_relevance2,
                 "desppercapita = 0",
                 vcov = vcovHC, type = "HC1")

#verifica relevancia dos instrumentos no modelo
# first-stage regressions
mod_relevance1 <- lm(basefinal3$logAN ~ basefinal3$altitude)

linearHypothesis(mod_relevance1,
                 "basefinal3$altitude = 0",
                 vcov = vcovHC, type = "HC1")

#esse teste indicou que o instrumento: altitude é um instrumento relevante, ou seja
#há um correlação significativa entre altitude e o log da proporção de AN

round(coefficients(modelo1),3)

plot(log(basefinal3$IFDM)~basefinal3$logAN)
hist(log(basefinal3$IFDM), col = "lightblue", breaks = 100,xlab = "IFDM",ylab = "Frequencia",main = "")

plot(x = log(basefinal3$`area natural`),
     y = denpop,
     main = "",
     xlab = "Área Natural",
     ylab = "Den. populacional",
     pch = 20,
     ylim = c(0.0137,79.1407),
     cex.main = 0.85)

cor(basefinal3$txpopurbana, basefinal3$pibpercapita)

mod1 <- ivreg( basefinal3$IFDM~ basefinal3$logAN|log(basefinal3$altitude))
mod2 <- ivreg( basefinal3$IFDM~ basefinal3$logAN|log(basefinal3$propcdagua+1))
mod3 <- ivreg( basefinal3$IFDM~ basefinal3$logAN|log(basefinal3$propcdagua+1)+log(basefinal3$altitude))


# robust coefficient summary for 1.
coeftest(mod1, vcov = vcovHC, type = "HC1")
coeftest(mod2, vcov = vcovHC, type = "HC1")
coeftest(mod3, vcov = vcovHC, type = "HC1")


# gather robust standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(mod1, type = "HC1"))),
               sqrt(diag(vcovHC(mod2, type = "HC1"))),
               sqrt(diag(vcovHC(mod3, type = "HC1"))))

# generate table
library(stargazer)
stargazer(mod1,mod2,mod3,
          header = FALSE,
          type = "html",
          omit.table.layout = "n",
          digits = 3,
          column.labels = c("IVs:(1) Altitude;(2) Prop. C.A.;(3) Altitude, Prop. C.A."),
          dep.var.labels.include = FALSE,
          dep.var.caption = "LogAN",
          se = rob_se,out="table1.htm")

# first-stage regressions
mod_relevance1 <- lm(`IFDM-2016` ~ `Reg Gov`+desppercapita)
mod_relevance2 <- lm(`IFDM-2016` ~ desppercapita)
mod_relevance3 <- lm(`IFDM-2016` ~ `Reg Gov`)

# check instrument relevance for model (2)
linearHypothesis(mod_relevance2,
                 "desppercapita = 0",
                 vcov = vcovHC, type = "HC1")