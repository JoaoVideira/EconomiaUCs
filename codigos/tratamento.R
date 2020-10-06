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
# CONFIGURAÇÔES -----------------------------------------------------------
install.packages("tidyverse")

setwd("C:/Users/Joao/Desktop/EconomiaUCs/Base de dados")

library(dplyr)
library(tidyr)
library(AER)
library(plm)
library(stringr)
library(magrittr)
library(ggplot2)
library(stargazer)

# DADOS MAPBiomas -----------------------------------------------------------
# FONTE:https://mapbiomas.org/estatisticas?cama_set_language=pt-BR
#Uso e Cobertura da terra.Carregar tabela 

coberturanatural<-read.csv2("Cobertura da terra.csv", sep = ";", stringsAsFactors = FALSE) %>% 
  select(-cod.classe,-X1985:-X2004)%>%
  filter(estado=="MINAS GERAIS"|estado=="ESÍRITO SANTO"|estado=="RIO DE JANEIRO"|estado=="SÃO PAULO")%>%
  filter(nivel1=="Floresta"|nivel1=="Formação Natural não Florestal")%>%
  select(CODIBGE,municipio,"X2005":"X2016")%>%
  distinct(CODIBGE,.keep_all = TRUE)%>%
  gather(ano,area_nat,-CODIBGE,-municipio)%>%
  group_by(CODIBGE,municipio,ano)%>%
  summarise(total.AN = sum(area_nat))
 
coberturanatural$CODIBGE<-as.character(coberturanatural$CODIBGE)
coberturanatural$ano<-str_sub(string = coberturanatural$ano, start = 2)
coberturanatural$ano<-as.numeric(coberturanatural$ano)
coberturanatural$CODIBGE<-str_sub(string = coberturanatural$CODIBGE, end= 6)

#verificando se há alguma combinação de ano e coibge repetida.
any(table(coberturanatural$CODIBGE, coberturanatural$ano)!=1)

# DADOS sNIS -----------------------------------------------------------
# FONTE:http://app4.mdr.gov.br/serieHistorica/#
#Dados sobre Saneamento, água e Resíduos solidos

SNIS <- read.csv2("SNIS.csv", sep = ";", stringsAsFactors = FALSE)%>%
 select(1:4,13)%>%
 rename(CODIBGE=Código.do.Município,municipio=Município,ano=Ano.de.Referência)%>%
 filter(ano>"2004" & ano<"2017")
 
#verificando se há alguma combinação de ano e coibge repetida.
any(table(SNIS$CODIBGE, SNIS$ano)!=1)

SNIS$ano<-as.numeric(SNIS$ano)
SNIS$POP_TOT...População.total.do.município.do.ano.de.referência..Fonte..IBGE.. <-as.numeric(SNIS$POP_TOT...População.total.do.município.do.ano.de.referência..Fonte..IBGE..)
SNIS$Estado<-as.factor(SNIS$Estado)

# DADOS IFDM -----------------------------------------------------------
# FONTE:https://www.firjan.com.br/ifdm/downloads/
#Dados sobre Índice FIRJAN de Desenvolvimento Municipal.

IFDM<- read.csv2("IFDM.csv", sep = ";", stringsAsFactors = FALSE)%>%
       filter(Região=="Sudeste")%>%
       select(1,4,5,7,9,11,13,15,17,19,21,23,25,27)%>%
       distinct(Código,.keep_all = TRUE)%>%
       gather(ano,IFDM,-Código,-Município)%>%
       rename(CODIBGE=Código,municipio=Município)

#verificando se há alguma combinação de ano e coibge repetida.
any(table(IFDM$CODIBGE, IFDM$ano)!=1)

IFDM[,4] <- gsub("[,]", ".",IFDM[,4])
IFDM$ano<-str_sub(string = IFDM$ano, start = 2)
IFDM$ano<-as.numeric(IFDM$ano) 
IFDM$IFDM<-as.numeric(IFDM$IFDM) 

# DADOS Altitude-----------------------------------------------------------
# FONTE:ftp://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/localidades/Geomedia_MDB/
#Dados sobre Altitude.
altitude<- read.csv2("Dados Gerais.csv", sep = ";", stringsAsFactors = FALSE)%>%
           filter(NM_UF=="MINAS GERAIS"|NM_UF=="RIO DE JANEIRO"|NM_UF=="SÃO PAULO"|NM_UF=="ESPIRÍTO SANTO")%>%
           distinct(NM_MUNICIPIO,.keep_all = TRUE)%>%
           group_by(NM_MUNICIPIO)%>%
           summarise(Altitude = mean(ALT))

altitude$x2005<-altitude$Altitude
altitude$x2006<-altitude$Altitude
altitude$x2007<-altitude$Altitude
altitude$x2008<-altitude$Altitude
altitude$x2009<-altitude$Altitude
altitude$x2010<-altitude$Altitude
altitude$x2011<-altitude$Altitude
altitude$x2012<-altitude$Altitude
altitude$x2013<-altitude$Altitude
altitude$x2014<-altitude$Altitude
altitude$x2015<-altitude$Altitude
altitude$x2016<-altitude$Altitude

altitude<-select(altitude,-Altitude)
names(altitude)<-c("municipio","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")

altitude<-gather(altitude,ano,altitude,-municipio)

altitude$municipio<-str_to_title(altitude$municipio)
altitude$ano<-as.numeric(altitude$ano)

any(table(altitude$municipio, altitude$ano)!=1)

# DADOS área territorial (em km2)-----------------------------------------------------------
# FONTE:https://www.ibge.gov.br/geociencias/organizacao-do-territorio/estrutura-territorial/15761-areas-dos-municipios.html?=&t=downloads
#Dados sobre area.
area<- read.csv2("Area municipio.csv", sep = ";", stringsAsFactors = FALSE)%>%
       distinct(CD_GCMUN,.keep_all = TRUE)%>%
       filter(NM_UF_SIGLA=="MG"|NM_UF_SIGLA=="SP"|NM_UF_SIGLA=="RJ"|NM_UF_SIGLA=="ES")%>%
       select(-c(1:4))%>%
       rename(CODIBGE=CD_GCMUN, municipio=NM_MUN_2016,AREA=AR_MUN_2016)
       
area$municipio<-str_to_title(area$municipio)
area$CODIBGE<-as.character(area$CODIBGE)

area$x2005<-area$AREA
area$x2006<-area$AREA
area$x2007<-area$AREA
area$x2008<-area$AREA
area$x2009<-area$AREA
area$x2010<-area$AREA
area$x2011<-area$AREA
area$x2012<-area$AREA
area$x2013<-area$AREA
area$x2014<-area$AREA
area$x2015<-area$AREA
area$x2016<-area$AREA

area<-select(area,-AREA)
areapainel<-gather(area,ano,area,-CODIBGE,-municipio)

areapainel$ano<-str_sub(string = areapainel$ano, start = 2)
areapainel$CODIBGE <-str_sub(string = areapainel$CODIBGE, end = 6)
areapainel$ano<-as.numeric(areapainel$ano)

any(table(areapainel$ano, areapainel$CODIBGE)!=1)

# Juntando as bases
basepainel <- coberturanatural%>%
              inner_join(SNIS, by=(c('ano'='ano', 'CODIBGE'='CODIBGE')))%>%
              select(-5)

any(table(basepainel$ano, basepainel$CODIBGE)!=1)

basepainel<-basepainel%>%
            inner_join(IFDM, by=(c('ano'='ano', 'CODIBGE'='CODIBGE')))
    
any(table(basepainel$ano, basepainel$CODIBGE)!=1)

basepainel$municipio<-str_to_title(basepainel$municipio)
basepainel<-select(basepainel,-2)

basepainel<-altitude%>%
            inner_join(basepainel, by=(c('ano'='ano', 'municipio'='municipio')))

any(table(basepainel$ano, basepainel$CODIBGE)!=1)

basepainel<-basepainel%>%
            inner_join(areapainel, by=(c('ano'='ano', 'CODIBGE'='CODIBGE')))

any(table(basepainel$ano, basepainel$CODIBGE)!=1)

basepainel<-select(basepainel,-9)
summary(basepainel)


basepainel<-mutate(basepainel, area = basepainel$area*100)
basepainel<-mutate(basepainel, Prop.AN = total.AN/area)
basepainel<-mutate(basepainel,den.pop = basepainel$POP_TOT...População.total.do.município.do.ano.de.referência..Fonte..IBGE../area)

summary(basepainel)
basepainel<-rename(basepainel,ifdm=IFDM)

# Modelo Dados em Painel
library(plm)

# Set data as panel data
pbase <- pdata.frame(basepainel, index=c("CODIBGE","ano"))
         
any(table(pbase$ano, pbase$CODIBGE)!=1)

attach(pbase)

# Descriptive statistics
summary(ifdm)
summary(Prop.AN)
hist(ifdm)
hist(log(Prop.AN))
summary(Freq)

# Modelos com apenas x e y
# Pooled OLS estimator
pooling <- plm(ifdm ~ log(Prop.AN+0.001),data=pbase, model= "pooling")
summary(pooling)
coeftest(pooling, vcov. = vcovHC, type = "HC1")

# Between estimator
between <- plm(ifdm ~ log(Prop.AN+0.001),data=pbase, model= "between")
summary(between)

# First differences estimator
firstdiff <- plm(ifdm ~ log(Prop.AN+0.001),data=pbase, model= "fd")
summary(firstdiff)
coeftest(firstdiff, vcov. = vcovHC, type = "HC1")

# Fixed effects or within estimator
fixed <- plm(ifdm ~ log(Prop.AN+0.001),data=pbase, model= "within")
summary(fixed)
coeftest(fixed, vcov. = vcovHC, type = "HC1")

# Random effects estimator
random <- plm(ifdm ~ log(Prop.AN+0.001),data=pbase, model= "random")
summary(random)
coeftest(random, vcov. = vcovHC, type = "HC1")

# LM test for random effects versus OLS
plmtest(pooling)

# LM test for fixed effects versus OLS
pFtest(fixed, pooling)

# Hausman test for fixed versus random effects model
phtest(random, fixed)

# modelo com covariáveis
hist(log(den.pop))
hist(basepainel$altitude)

# Pooled OLS estimator
pooling1 <- plm(ifdm ~ log(Prop.AN+0.001)+ log(den.pop)+ Estado ,data=pbase, model= "pooling")
summary(pooling1)
coeftest(pooling1, vcov. = vcovHC, type = "HC1")

# Between estimator
between1 <- plm(ifdm ~ log(Prop.AN+0.001)+ log(den.pop)+ Estado,data=pbase, model= "between")
summary(between1)

# First differences estimator
firstdiff1 <- plm(ifdm ~ log(Prop.AN+0.001)+ log(den.pop)+ Estado,data=pbase, model= "fd")
summary(firstdiff1)
coeftest(firstdiff1, vcov. = vcovHC, type = "HC1")

# Fixed effects or within estimator
fixed1 <- plm(ifdm ~ log(Prop.AN+0.001),data=pbase, model= "within")
summary(fixed1)
coeftest(fixed1, vcov. = vcovHC, type = "HC1")

# Random effects estimator
random1 <- plm(ifdm ~ log(Prop.AN+0.001)+ log(den.pop)+ Estado,data=pbase, model= "random")
summary(random1)
coeftest(random1, vcov. = vcovHC, type = "HC1")
# LM test for random effects versus OLS
plmtest(pooling1)

# LM test for fixed effects versus OLS
pFtest(fixed1, pooling1)

# Hausman test for fixed versus random effects model
phtest(random1, fixed1)

# Normalidade dos resíduos
residuos<-between1$residuals
hist(residuos,col="orange")

stargazer(pooling1,between1,firstdiff1,fixed1,random1,
          header = FALSE,
          type = "html",
          omit.table.layout = "n",
          digits = 3,
          column.labels = c("(1)Pooling;(2)Between;(3)Firstdiff;(4)Fixed;(5)Random"),
          dep.var.labels.include = FALSE,
          dep.var.caption = "IFDM", out ="tabela3.htm")

iv<-lm(log(Prop.AN+0.001)~pbase$altitude)
summary(iv)
VI<-fitted(iv)

# Pooled OLS estimator
pooling2 <- plm(ifdm ~ VI+ Estado+log(den.pop),data=pbase, model= "pooling")
summary(pooling2)
coeftest(pooling2, vcov. = vcovHC, type = "HC1")

# Between estimator
between2 <- plm(ifdm ~ VI+ log(den.pop)+ Estado,data=pbase, model= "between")
summary(between2)

# First differences estimator
firstdiff2 <- plm(ifdm ~ VI+ log(den.pop)+ Estado,data=pbase, model= "fd")
summary(firstdiff2)
coeftest(firstdiff2, vcov. = vcovHC, type = "HC1")

# Fixed effects or within estimator
fixed2 <- plm(ifdm ~ VI+ log(den.pop), data=pbase, model= "within")
summary(fixed2)
coeftest(fixed2, vcov. = vcovHC, type = "HC1")

# Random effects estimator
random2 <- plm(ifdm ~ preditaVI+ log(den.pop)+ Estado,data=pbase, model= "random")
summary(random2)
coeftest(random2, vcov. = vcovHC, type = "HC1")
# LM test for random effects versus OLS
plmtest(pooling1)

# LM test for fixed effects versus OLS
pFtest(fixed1, pooling1)

# Hausman test for fixed versus random effects model
phtest(random1, fixed1)

# Normalidade dos resíduos
residuos<-between1$residuals
hist(residuos,col="orange")

kable(tidy(wage.pooled), digits=3, 
      caption="Pooled model")

# Modelos AN per capita como regressor
pbase<-mutate(pbase, ANpc = total.AN/POP_TOT...População.total.do.município.do.ano.de.referência..Fonte..IBGE..)
attach(pbase)
hist(log(ANpc))

# Pooled OLS estimator
pooling3 <- plm(ifdm ~ log(ANpc+0.001)+den.pop+Estado,data=pbase, model= "pooling")
summary(pooling3)
coeftest(pooling3, vcov. = vcovHC, type = "HC1")

# Between estimator
between3 <- plm(ifdm ~ log(ANpc+0.001)+den.pop+Estado,data=pbase, model= "between")
summary(between3)

# First differences estimator
firstdiff3 <- plm(ifdm ~ log(ANpc+0.001)+den.pop+Estado,data=pbase, model= "fd")
summary(firstdiff3)
coeftest(firstdiff3, vcov. = vcovHC, type = "HC1")

# Fixed effects or within estimator
fixed3 <- plm(ifdm ~ log(ANpc+0.001),data=pbase, model= "within")
summary(fixed3)
coeftest(fixed3, vcov. = vcovHC, type = "HC1")

# Random effects estimator
random3 <- plm(ifdm ~ log(ANpc+0.001),data=pbase, model= "random")
summary(random3)
coeftest(random3, vcov. = vcovHC, type = "HC1")

# LM test for random effects versus OLS
plmtest(pooling3)

# LM test for fixed effects versus OLS
pFtest(fixed3, pooling3)

# Hausman test for fixed versus random effects model
phtest(random3, fixed3)
