rm(list=ls(all=TRUE)) # Limpar memória do R 
library(readxl)       # Lê planílias do Excel
library(ggplot2)      # Pacote gráfico
library(mondate)      # Manipula datas
library(rgdal)        # Fazer mapas
library(tmap)         # Plota mapas
library(igraph)       # Análise de redes
library(plyr)         # Possui a função join para juntar BDs
options(scipen=999)   # Desabilita notação científica

## ------------------- ##
## Abrindo BDs         ##
## Ocorrências letais  ##
## ------------------- ##
setwd('C:/Users/Vinicius/Desktop/TextoTese/Cap04-Macro/FBSP-Dados')  

# -------------------------------------------------------------------------------------
# Homicídio doloso, latrocínio, mortes violentas intencionais, CVLI, mortes de policiais, MDIP e outras ocorrências
# Fonte: Forum Brasileiro de Segurança Pública
# Em http://www.forumseguranca.org.br/estatisticas/introducao/

library(readr)
OcorreciasLetais <- read_delim("OcorreciasLetais.csv", 
                               "\t", escape_double = FALSE, locale = locale(date_names = "pt", 
                                                                            decimal_mark = ","), trim_ws = TRUE)
View(OcorreciasLetais)

# Taxas de ocorrências letais -------------------------------------------------------------------------------------
# Análise Brasil
br = OcorreciasLetais[OcorreciasLetais[,'UF']=='Brasil' & OcorreciasLetais[,'Nomes de medida']!='Números absolutos', ]
br = as.data.frame(br)
ggplot(data = br, aes(x = as.factor(Ano), y = `Valores de medida`)) +
  geom_point(color="red",size=3) + theme_minimal()

# Estados 2013:2014
estadostx = OcorreciasLetais[OcorreciasLetais[,'UF']!='Brasil' & OcorreciasLetais[,'Nomes de medida']!='Números absolutos', ]
ggplot(data = estadostx, aes(x=as.factor(`Ano`), y=`Valores de medida`)) +  
  labs(x='Ano', y='Taxa de homicídio') +
  geom_boxplot() + geom_text(aes(label=UF, color=`Região`), size=4, position=position_jitter(0.15)) +
  geom_point(data=br, aes(x = as.factor(Ano), y = `Valores de medida`), color='red',size=3) + 
  theme_minimal()
# Números absolutos de ocorrências letais -------------------------------------------------------------------------------------
# Análise Brasil
br = OcorreciasLetais[OcorreciasLetais[,'UF']=='Brasil' & OcorreciasLetais[,'Nomes de medida']=='Números absolutos', ]
br = as.data.frame(br)
ggplot(data = br, aes(x = as.factor(`Ano`), y = `Valores de medida`)) +
  geom_point(color="blue",size=3)+ theme_minimal()

# Estados 2013:2014
estadostx = OcorreciasLetais[OcorreciasLetais[,'UF']!='Brasil' & OcorreciasLetais[,'Nomes de medida']=='Números absolutos', ]
ggplot(data = estadostx, aes(x = as.factor(`Ano`), y = `Valores de medida`)) + 
  labs(x='Ano', y='Números Absolutos') +
  geom_boxplot() + geom_text(aes(label=UF, color=`Região`), size=4, position=position_jitter(0.15))+ 
  theme_minimal() 


## --------------------- ##
## Abrindo BDs           ##
## Crimes Patrimoniais   ##
## --------------------- ##
setwd('C:/Users/Vinicius/Desktop/TextoTese/Cap04-Macro/FBSP-Dados')  
library(readr)
Patrimoniais <- read_delim("CrimesPatrimoniais.csv", 
                                 "\t", escape_double = FALSE, locale = locale(date_names = "pt", 
                                                                              decimal_mark = ","), trim_ws = TRUE)
View(Patrimoniais)

# Taxas de ocorrências roubo a veículos -------------------------------------------------------------------------------------
# Análise Brasil
br = Patrimoniais[Patrimoniais[,'UF']=='Brasil' & Patrimoniais[,'Nomes de medida']!='Números absolutos', ]
br = as.data.frame(br)
ggplot(data = br, aes(x = as.factor(`Ano`), y = `Valores de medida`)) +
  geom_point(color="red",size=3) +theme_minimal() 

# Estados 2007:2016
estadostx =Patrimoniais[Patrimoniais[,'UF']!='Brasil' & Patrimoniais[,'Nomes de medida']!='Números absolutos', ]
ggplot(data = estadostx, aes(x = as.factor(`Ano`), y = `Valores de medida`)) +  
  labs(x='Ano', y='Taxas de roubos  a veículos por 100 mil veículos') +
  geom_boxplot() + geom_text(aes(label=UF, color=`Região`), size=4, position=position_jitter(0.15)) +
  geom_point(data=br, aes(x = as.factor(`Ano`), y = `Valores de medida`), color="red",size=3)+ 
  theme_minimal() 

# Números absolutos de roubo a veículos -------------------------------------------------------------------------------------
# Análise Brasil
br = Patrimoniais[Patrimoniais[,'UF']=='Brasil' & Patrimoniais[,'Nomes de medida']=='Números absolutos', ]
br = as.data.frame(br)
ggplot(data = br, aes(x = as.factor(`Ano`), y = `Valores de medida`)) +
  geom_point(color="red",size=3)+ theme_minimal() 

# Estados 2007:2016
estadostx = Patrimoniais[Patrimoniais[,'UF']!='Brasil' & Patrimoniais[,'Nomes de medida']=='Números absolutos', ]
ggplot(data = estadostx, aes(x = as.factor(`Ano`), y = `Valores de medida`)) + 
  labs(x='Ano', y='Roubos a veículos') +
  geom_boxplot() + geom_text(aes(label=UF, color=`Região`), size=4, position=position_jitter(0.15))+ 
  theme_minimal()  


## ---------------------------- ##
## Taxas de homicídio nacional  ##
## ---------------------------- ##
# Atlas da Violência - IPEA
# http://www.ipea.gov.br/atlasviolencia/
# MS/SVS/CGIAE - Sistema de Informações sobre Mortalidade - SIM
# Considera os códigos CIDs 10: X85-Y09 (agressão) e Y35,Y36 (intervenção legal)
# Óbitos por residência.
# Elaboração Diest/Ipea.
setwd('C:/Users/Vinicius/Desktop/TextoTese/Cap04-Macro/AtlasViolencia-Dados')  

library(readr)
homtx <- read_delim("HomicidiosTaxasEstados.csv",";", escape_double = FALSE, locale = locale(date_names = "pt", 
                    decimal_mark = ","), trim_ws = TRUE)
View(homtx)
homtx = as.data.frame(homtx)
boxplot(homtx[,2:dim(homtx)[2]]) # Por ano 1996:2016
boxplot(t(homtx[,2:dim(homtx)[2]]), names=homtx$UF) # Por estado

# Taxas de homicídios no brasil
br = cbind(1996:2016, c(27.80, 24.78, 25.39, 25.94, 26.20, 27.35, 27.86, 28.53, 29.14, 26.94, 26.13, 26.61,
                        26.20, 26.72, 27.18, 27.45, 29.41, 28.55, 29.82, 28.89, 30.33))
colnames(br)=c('Ano','Valores de medida')
br = as.data.frame(br)

homtx = reshape(homtx, idvar="UF", times=as.character(1996:2016), varying = list(2:22), v.names="TxHom", direction = "long")
homtx$`Região` = 'NE'
homtx[homtx$UF %in% c('RJ','SP','ES','MG'),'Região'] = 'SE'
homtx[homtx$UF %in% c('RS','SC','PR'),'Região'] = 'S'
homtx[homtx$UF %in% c('MS','MT','GO','DF'),'Região'] = 'CO'
homtx[homtx$UF %in% c('AM','RO','RR','AC','TO','PA'),'Região'] = 'N'
rownames(homtx)=NULL
homtx = homtx[,c(2,1,4,3)]
colnames(homtx)[c(1,4)] = c('Ano','Valores de medida')

# Estados 1996:2016
plt = ggplot(data = homtx, aes(x = as.factor(Ano), y = `Valores de medida`)) + 
  labs(x='Ano', y='Taxa de homicídios') +
  geom_boxplot() + geom_text(aes(label=UF, color=`Região`), size=3, position=position_jitter(0.25)) 
plt + theme_minimal()
plt + geom_point(data=br, aes(x = as.factor(Ano), y = `Valores de medida`), color="red",size=3)+ theme_minimal()


## ---------------------------- ##
## Evolução da população presa  ##
## ---------------------------- ##
# Dados do DEPEN
# Em http://depen.gov.br/DEPEN/noticias-1/noticias/infopen-levantamento-nacional-de-informacoes-penitenciarias-2016/relatorio_2016_22111.pdf
# Acessado em 01/03/2019
# Segundo o Relatório:
# "Com exceção do ano de 2002, em que foi produzido apenas relatório referente ao primeiro semestre do ano, e do ano de
# 2016, que se refere a Junho, os demais dados referem-se ao mês de dezembro de cada ano. Não há dados disponíveis para os
# anos de 1996 e 1998. Os dados disponíveis em cada ano incluem as pessoas privadas de liberdade que se encontram no Sistema
# Penitenciário Federal."

Ano = 1990:2016
Encarceramento = c(90,    NA,    114.3,126.2, 129.2, 148.8, NA, 170.6, NA, 194.1, 232.8, 
                   233.9, 239.3, 308.3, 336.4, 361.4, 401.2, 422.4, 451.4, 473.6,  496.3, 
                   514.6, 549.8, 581.5, 622.2, 698.6, 726.7)
TxEncarceramento = c(rep(NA, 10), 137.1, 135.7, 137.1, 174.3, 185.2, 196.2, 214.8, 229.6,
                     238.1,247.3, 260.2, 267.5, 283.5, 289.3, 306.2, 341.7, 352.8)
TxHomicidio = c(rep(NA, 6), 27.80, 24.78, 25.39, 25.94, 26.20, 27.35, 27.86, 28.53, 29.14, 26.94, 26.13, 26.61,
                   26.20, 26.72, 27.18, 27.45, 29.41, 28.55, 29.82, 28.89, 30.33)
TxHomicidio = 10*TxHomicidio  # Por milhão  
TxRouboVeiculo=c(rep(NA,17), 237.0799, 268.0645, 263.7494, 234.0123, 243.3533, 
                265.9646, 267.1450, 279.9592, 268.5723, 291.0841)

br = cbind(Ano, Encarceramento, TxEncarceramento, TxHomicidio, TxRouboVeiculo)
br = as.data.frame(br)

# Aumento do número de presos
ggplot(data=br, aes(x=as.factor(Ano), y=Encarceramento)) +
  geom_bar(stat = "identity") + labs(x='Ano', y='Presos (mil)')+
  geom_text(aes(label=Encarceramento), vjust=-0.3, size=3)+
  theme_minimal()

# Taxas
txbr = reshape(br, idvar="Ano", times=c('Encarceramento (por 100 mil hab.)','Homicídio (por milhão de hab.)', 'Roubo a veículo (por 100 mil veículos)'), varying = list(3:5), v.names="Valores", direction = "long")
colnames(txbr)[3] = 'Taxas'

txbr = subset(txbr, txbr$Ano %in% 1996:2016)
ggplot(data = txbr, aes(x=Ano, y=Valores, color=Taxas)) +
  geom_line(size=2) +
  theme_bw() +
  theme(legend.position="bottom")+ labs(x='Ano', y='Taxas') 
  
# Encarceramento por ano
plot(Encarceramento~Ano, data=br)
abline(lm(Encarceramento~Ano, data=br))
summary(lm(Encarceramento~Ano, data=br))

# Taxa de Encarceramento por ano
plot(TxEncarceramento~Ano, data=br)
abline(lm(TxEncarceramento~Ano, data=br))
summary(lm(TxEncarceramento~Ano, data=br))


