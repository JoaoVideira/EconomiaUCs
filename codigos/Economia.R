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

require(install.load)
install_load("descr")
install_load("foreign")
require(AER)
require(ggplot2)
library(dplyr)

setwd("C:/Users/Joao/Desktop/EconomiaUCs/Base de dados")

# DADOS MAPBiomas -----------------------------------------------------------
# FONTE:https://mapbiomas.org/estatisticas?cama_set_language=pt-BR
#Uso e Cobertura da terra.Carregar tabela 

coberturanatural<- read.csv2("Cobertura da terra.csv", sep = ";", stringsAsFactors = FALSE)

#Tratamento da base e criação da variável dependente % AN
#retirando colunas
coberturanatural<-coberturanatural[ ,-c(7:39,41,42)]

#retirando linhas
coberturanatural<-filter(coberturanatural,estado=="MINAS GERAIS"|estado=="ESÍRITO SANTO"|estado=="RIO DE JANEIRO"|estado=="SÃO PAULO")
AN<-filter(coberturanatural,nivel1=="Floresta"|nivel1=="Formação Natural não Florestal")

#Variável resposta: cobertura de área natural
AN<-aggregate(AN$X2016, by=list(AN$municipio), FUN=sum, na.rm=TRUE)

denominador<-aggregate(coberturanatural$X2016, by=list(coberturanatural$municipio),
              FUN=sum, na.rm=TRUE)

names(AN)<-c("municipio")
names(denominador)<-c("municipio")
base<-full_join(AN,denominador,by = "municipio")

#percentual de Área Natural
Y<-base$NA.x/base$NA.y
base<-cbind(base,Y)
base<-base[,-c(2,3)]

#covariável percentual corpos dágua
CAGUA<-coberturanatural[coberturanatural$nivel1=="Corpo D'água",c(2,3,4,7)]

# variáveis inclusas:corpo dagua,estado e Y. base será a base final.
base<-full_join(base,CAGUA,by= "municipio",na.rm=TRUE)
names(base)<-c("municipio","AN","estado","CODIBGE","Corpoagua")

# DADOS sNIS -----------------------------------------------------------
# FONTE:http://app4.mdr.gov.br/serieHistorica/#
#Dados sobre Saneamento, água e Resíduos solidos
library(readxl)

Saneamento <- read_excel("Saneamento.xlsx")

Saneamento <- Saneamento[,-c(4,5,6)]
names(Saneamento)<-c("CODIBGE","municipio","estado","Abastecimento agua","esgotamento","pop total","pop urbana","tx de coleta de rdo")

Saneamento<-replace(x = Saneamento, list = is.na(Saneamento), values = 0)

txabastecimento<-Saneamento$`Abastecimento agua`/Saneamento$`pop total`
txesgotamento<-Saneamento$esgotamento/Saneamento$`pop total`
txpopurbana<-Saneamento$`pop urbana`/Saneamento$`pop total`

Saneamento<-cbind(Saneamento,txesgotamento,txabastecimento,txpopurbana)
Saneamento <- Saneamento[,-c(4,5,6,7,9,10)]
Saneamento<-Saneamento[-1669,]

#alterando código dos municipios da base Saneamento
install.packages("stringr")
library(stringr)
CODIBGE<-str_sub(base$CODIBGE, end =6) # pegar apenas os seis primeiros caracteres
base<-cbind(base,CODIBGE)
Saneamento<-Saneamento[,-6]
CODIBGE<-as.character(Saneamento$CODIBGE)
base<-base[,-4]
Saneamento<-cbind(Saneamento,CODIBGE)
Saneamento<-Saneamento[,-1]

base1<-full_join(base,Saneamento,by= "CODIBGE",na.rm=TRUE)
base1<-base1[,-c(1,3)]
names(base1)<-c("area natural","corpodagua","CODIBGE","municipio","estado","tx de coleta de rdo","txpopurbana")

# DADOS FINBRA -----------------------------------------------------------
# FONTE:https://siconfi.tesouro.gov.br/siconfi/pages/public/consulta_finbra/finbra_list.jsf
#Dados sobre despesas dos municipios.

Finbra<- read.csv2("finbra.csv", sep = ";", stringsAsFactors = FALSE)

Finbra<-filter(Finbra,Coluna=="Despesas Pagas")
Finbra<-filter(Finbra,UF=="SP"|UF=="MG"|UF=="ES"|UF=="RJ")

despesas<-aggregate(Finbra$Valor, by=list(Finbra$Cod.IBGE), FUN=sum, na.rm=TRUE)
names(despesas)<-c("CODIBGE","Despesas")

CODIBGE<-str_sub(despesas$CODIBGE, end =6) # pegar apenas os seis primeiros caracteres
despesas<-cbind(despesas,CODIBGE)
despesas<-despesas[,-1]

base2<-full_join(base1,despesas,by= "CODIBGE",na.rm=TRUE)

#Retirando NA da variável resposta
base3 <- base2[!is.na(base2$`area natural`),]

# DADOS IFDM -----------------------------------------------------------
# FONTE:https://www.firjan.com.br/ifdm/downloads/
#Dados sobre Índice FIRJAN de Desenvolvimento Municipal.

IFDM<- read.csv2("IFDM.csv", sep = ";", stringsAsFactors = FALSE)
IFDM<-IFDM[-c(1,2),]
IFDM<-IFDM[ ,c(1:4,27)]

str_sub(string = IFDM, start = 1, end = 5)
IFDM<-filter(IFDM, X.1=="MG"|X.1=="SP"|X.1=="RJ"|X.1=="ES")
names(IFDM)<-c("CODIBGE","REGIAO","estado","municipio","IFDM")  
IFDM<-IFDM[,-2]

base4<-full_join(base3,IFDM,by= "CODIBGE",na.rm=TRUE)

#Retirando NA da base
base4 <- na.omit(base4)

# DADOS PIB-----------------------------------------------------------
# FONTE:https://www.ibge.gov.br/estatisticas/economicas/contas-nacionais/9088-produto-interno-bruto-dos-municipios.html?=&t=sobre
#Dados sobre PIB Municipal.
PIB<- read.csv2("PIB.csv", sep = ";", stringsAsFactors = FALSE)
PIB<-PIB[,c(1,13)]
PIB<-PIB[c(2245:3910),]
names(PIB)<-c("municipio","PIB")

municipio<-str_sub(PIB$municipio, end = -6)
View(municipio)
PIB<-cbind(PIB,municipio)
PIB<-PIB[,-1]


base5<-full_join(base4,PIB,by= "municipio",na.rm=TRUE)

#Retirando NA da base
base5 <- na.omit(base5)

# DADOS Área Plantada (Produção Agrícola Municipal)-----------------------------------------------------------
# FONTE:https://sidra.ibge.gov.br/tabela/5457
#Dados sobre Agricultura.
Agricultura<- read.csv2("Agricultura.csv", sep = ";", stringsAsFactors = FALSE)
Agricultura<-Agricultura[,-c(2:12,14)]
Agricultura<-Agricultura[c(2244:3903),]
names(Agricultura)<-c("municipio","agricultura")

municipio<-str_sub(Agricultura$municipio, end = -6)
View(municipio)
Agricultura<-cbind(Agricultura,municipio)
Agricultura<-Agricultura[,-1]

base6<-full_join(base5,Agricultura,by= "municipio",na.rm=TRUE)

#Retirando NA da base
base6 <- na.omit(base6)

# DADOS Rebanho (Produção Pecuária Municipal)-----------------------------------------------------------
# FONTE:https://sidra.ibge.gov.br/tabela/3939
#Dados sobre pecuária.
pecuaria<- read.csv2("Rebanho.csv", sep = ";", stringsAsFactors = FALSE)
pecuaria<-pecuaria[,c(1,2)]
pecuaria<-pecuaria[c(2245:3907),]
names(pecuaria)<-c("municipio","pecuaria")

municipio<-str_sub(pecuaria$municipio, end = -6)
View(municipio)
pecuaria<-cbind(pecuaria,municipio)
pecuaria<-pecuaria[,-1]

base7<-full_join(base6,pecuaria,by= "municipio",na.rm=TRUE)

#Retirando NA da base
base7 <- na.omit(base7)

# DADOS Altitude-----------------------------------------------------------
# FONTE:ftp://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/localidades/Geomedia_MDB/
#Dados sobre Altitude.
altitude<- read.csv2("Dados Gerais.csv", sep = ";", stringsAsFactors = FALSE)
altitude<-filter(altitude,altitude$NM_UF=="MINAS GERAIS"|altitude$NM_UF=="RIO DE JANEIRO"|altitude$NM_UF=="SÃO PAULO"|altitude$NM_UF=="ESPIRÍTO SANTO")

#Variável altitude média do municipio
altitudemedia<-aggregate(altitude$ALT, by=list(altitude$NM_MUNICIPIO), FUN=mean, na.rm=TRUE)

names(altitudemedia)<-c("municipio","altitude")
altitudemedia$municipio<- str_to_title(altitudemedia$municipio)

base7$municipio<- str_to_title(base7$municipio)
base8<-full_join(base7,altitudemedia,by= "municipio",na.rm=TRUE)

#Retirando NA da base
base8 <- na.omit(base8)

# DADOS população (estimativas da população)-----------------------------------------------------------
# FONTE:https://www.ibge.gov.br/estatisticas/sociais/populacao/9103-estimativas-de-populacao.html?=&t=downloads
#Dados sobre população.
populacao<- read.csv2("Populaçao.csv", sep = ";", stringsAsFactors = FALSE)

populacao<-filter(populacao, UF=="MG"|UF=="SP"|UF=="RJ"|UF=="ES")

populacao<-populacao[,c(4,5)]

names(populacao)<-c("municipio","populacao")

populacao$municipio<- str_to_title(populacao$municipio)

base9<-full_join(base8,populacao,by= "municipio",na.rm=TRUE)

#Retirando NA da base
base9 <- na.omit(base9)

base10<-base9 %>% distinct(municipio, .keep_all = TRUE)

# DADOS área territorial (em km2)-----------------------------------------------------------
# FONTE:https://www.ibge.gov.br/geociencias/organizacao-do-territorio/estrutura-territorial/15761-areas-dos-municipios.html?=&t=downloads
#Dados sobre area.
area<- read.csv2("Area municipio.csv", sep = ";", stringsAsFactors = FALSE)

area<-filter(area, NM_UF_SIGLA=="MG"|NM_UF_SIGLA=="SP"|NM_UF_SIGLA=="RJ"|NM_UF_SIGLA=="ES")

area<-area[,c(6,7)]

names(area)<-c("municipio","area")

area$municipio<- str_to_title(area$municipio)

base11<-full_join(base10,area,by= "municipio",na.rm=TRUE)

#Retirando NA da base
base11 <- na.omit(base11)

base11<-base11 %>% distinct(municipio, .keep_all = TRUE)

# Tratamento variaveis da Base final-----------------------------------------------------------
#Criando variável densidade populacional
base11[,14] <- gsub("[.]", "",base11[,14])
base11$populacao<-as.numeric(base11$populacao)

base11$denpop<-base11$populacao/base11$area
base11$denpop<-base11$populacao/(base11$area*100)

#criando variavel rebanho por hectare
# Retirando da variável agricultura e rebanho (...) que significa "Valor não disponível"
base12<-filter(base11,base11$pecuaria!="...")
base12<-filter(base12,base12$agricultura!="...")
base12$pecuaria<-as.numeric(base12$pecuaria)

base12$Rebanhoporarea<-base12$pecuaria/(base12$area*100)

#criando variável percentual de área agricola(area plantada) no municipio
base12$agricultura <-as.numeric(base12$agricultura)
base12$areaagricola<-base12$agricultura/(base12$area*100)

#criando variável PIB per capita
base12$pibpercapita<-(base12$PIB*1000)/(base12$populacao)

#Despesas pagas per capita
base12$desppercapita<-(base12$Despesas )/(base12$populacao)

#tratamento variável IFDM
base12[,9] <- gsub("[,]", "",base12[,9])
base12$IFDM <-as.numeric(base12$IFDM)
base12$IFDM<-base12$IFDM/10000 

#tratamento variável estado 
base12$estado<-as.factor(base12$estado)

#Adicionando variáveis defasadas do IFDM (2015,2014)
#FONTE:https://www.firjan.com.br/ifdm/downloads/
#Dados sobre Índice FIRJAN de Desenvolvimento Municipal.
IFDM<- read.csv2("IFDM.csv", sep = ";", stringsAsFactors = FALSE)
IFDM<-IFDM[-c(1,2),]
IFDM<-IFDM[ ,c(1:4,23,25)]

str_sub(string = IFDM$Informações.do.Município , start = 1, end = 5)
IFDM<-filter(IFDM, X.1=="MG"|X.1=="SP"|X.1=="RJ"|X.1=="ES")
names(IFDM)<-c("CODIBGE","REGIAO","estado","municipio","IFDM-2014","IFDM-2015")  
IFDM<-IFDM[,-2]
IFDM<-IFDM[,-c(2,3)]

basefinal<-full_join(base12,IFDM,by= "CODIBGE",na.rm=TRUE)

#Retirando NA da base
basefinal <- na.omit(basefinal)
basefinal<-basefinal %>% distinct(municipio, .keep_all = TRUE)

#tratamento variável IFDM
basefinal[,21] <- gsub("[,]", ".",basefinal[,21])
basefinal[,22] <- gsub("[,]", ".",basefinal[,22])

basefinal$`IFDM-2014` <-as.numeric(basefinal$`IFDM-2014`)
basefinal$`IFDM-2015` <-as.numeric(basefinal$`IFDM-2015`)

#Adicionando base MUNIC -2015
#Adicionando variáveis Base digitalizada (sim ou nao); tem CAR ( sim ou não)
#FONTE:https://www.ibge.gov.br/estatisticas/sociais/saude/10586-pesquisa-de-informacoes-basicas-municipais.html?=&t=downloads
#Dados sobre MUNIC.
MUNIC<- read.csv2("MUNIC Ambiental 2015.csv", sep = ";", stringsAsFactors = FALSE)
MUNIC<-MUNIC[,-3]

MUNIC<-filter(MUNIC, Codigouf=="31"|Codigouf=="32"|Codigouf=="33"|Codigouf=="35")

MUNIC$A1<-as.character(MUNIC$A1)
MUNIC$A1<-str_sub(string = MUNIC$A1 , start = 1, end = 6)
names(MUNIC)<-c("CODIBGE","CODUF","municipio"," Tem Base Digitalizada","Tem CAR")  
MUNIC<-MUNIC[,-c(2,3)]
MUNIC$`Tem CAR`<-as.factor(MUNIC$`Tem CAR`)
MUNIC$` Tem Base Digitalizada` <-as.factor(MUNIC$` Tem Base Digitalizada`)

basefinal1<-full_join(basefinal,MUNIC,by= "CODIBGE",na.rm=TRUE)

#Retirando NA da base
basefinal1 <- na.omit(basefinal1)
basefinal1<-basefinal1 %>% distinct(municipio, .keep_all = TRUE)

basefinal2<-basefinal1[ ,-c(8,10,11,12,14,15)]

