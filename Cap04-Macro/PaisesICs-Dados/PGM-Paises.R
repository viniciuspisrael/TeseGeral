rm(list=ls(all=TRUE)) # Limpar memória do R 
library(readxl)       # Lê planílias do Excel
library(ggplot2)      # Pacote gráfico
library(plyr)         # Possui a função join para juntar BDs
library(raster)       # Fazer escala no gráfico
library(car)          # Biblioteca de diagnóstico
library(plotly)       # Plot em 3d
library(ggpubr)       # Múltiplos gráficos
library(arm)          # Regressão bayesiana
library(lattice)      # Permite fazer gráficos em 3D
options(scipen=999)   # Desabilita notação científica

## -------------------------------------------- ##
## Abrindo BDs                                  ##
## Banco de Dados produzido pelos alunos de IC  ##
## -------------------------------------------- ##
setwd('C:/Users/Vinicius/Desktop/TextoTese/Cap04-Macro/PaisesICs-Dados')  

# -------------------------------------------------------------------------------------
# PNUD 2016 - IDH
# Banco Mundial - Renda per capita, homicídios e GINI
## http://hdr.undp.org/en/composite/HDI e 
## http://hdr.undp.org/en/content/human-development-index-hdi
# World Prison Brief - População carcerária e taxa de encarceramento
# GINI(últimos 10 anos)
# Fraser Institute(economic freedom-2015) 

library(readxl)
PoP <- read_excel("PoP.xlsx")
na.omit(PoP$Prisonrate)
View(PoP)

# Alguns acertos no BD ------------------------------------------
PoP[PoP=='NA'] = NA
# Taxa de encarceramento por continentes
PoP$Continents[PoP$Continents=='Africa'] = 'África'
PoP$Continents[PoP$Continents=='America'] = 'América'
PoP$Continents[PoP$Continents=='Asia'] = 'Ásia'
PoP$Continents[PoP$Continents=='Europe'] = 'Europa'
PoP$Continents[PoP$Continents=='Oceania'] = 'Oceania'
PoP$Region[PoP$Region=='EU/West'] = 'Europa Ocidental'
PoP$Region[PoP$Region=='EX-USSR/East Europe'] = 'Ex-URSS/Europa Oriental'
PoP$Region[PoP$Region=='Latin America'] = 'América Latina'
PoP$Region[PoP$Region=='Middle East/North Africa'] = 'Oriente Médio/Norte da África'
PoP$Region[PoP$Region=='North America'] = 'América do Norte'
PoP$Region[PoP$Region=='Southeast Asia'] = 'Sudeste da Ásia'
PoP$Region[PoP$Region=='Sub-saharan Africa'] = 'África Subsariana'
# Gini ente zero e um.
PoP$Gini = PoP$Gini/100
# Democracy Index - Transformar em numérico
PoP$DemocracyIndex = as.numeric(PoP$DemocracyIndex)
PoP$FunGog = as.numeric(PoP$FunGog)
PoP$Pculture = as.numeric(PoP$Pculture)
PoP$ElectoralProcess = as.numeric(PoP$ElectoralProcess)
PoP$Pparticipation = as.numeric(PoP$Pparticipation)
PoP$Civilliberties = as.numeric(PoP$Civilliberties)
# Retirando a variável "Dummie AL"
PoP$`Dummie AL` = NULL
# Definindo os fatores
PoP$IDHO = as.factor(PoP$IDHO) 
PoP$Continents = as.factor(PoP$Continents)
PoP$UNregion = as.factor(PoP$UNregion)
PoP$Region = as.factor(PoP$Region)
# Transformando os anos de estudo em numeric
PoP$Yearsscholling = as.numeric(PoP$Yearsscholling) 

## --- ##
## AED ##
## --- ##

# Taxa de encarceramento ----------------------------------------
summary(PoP$Prisonrate)

df = subset(PoP, select=c('Countries','Prisonrate','Continents','Region' ))
df = na.omit(df)

p1 = ggplot(PoP, aes(x=Prisonrate)) +
  geom_histogram(aes(y=..density..), fill="grey", color="black",  position='identity') +
  geom_density(stat="density", position="identity", alpha=0.2,fill='blue')  + 
  xlab('Taxa de encarceramento') + ylab('Densidade') + theme_minimal()
p2 = ggplot(PoP, aes(x=log(Prisonrate))) +
  geom_histogram(binwidth=.3,aes(y=..density..), fill="grey", color="black",  position='identity') +
  geom_density(stat="density", position="identity", alpha=0.2,fill='blue')  + 
  xlab('log(Taxa de encarceramento)') + ylab('Densidade') + theme_minimal()
ggarrange(p1, p2, ncol = 2, nrow = 1)

ggplot(PoP, aes(x=Prisonrate, fill=Continents))+
  geom_density(color='black')+ 
  xlab('Taxa de encarceramento') + ylab('Densidade') + 
  labs(fill='Continentes') + 
  theme_minimal()

ggplot(df, aes(x=Continents, y=Prisonrate))+
  geom_boxplot()+ 
  xlab('Continents') + ylab('Taxa de encarceramento') + 
  theme_minimal()

ggplot(df, aes(y=Prisonrate, fill=Region))+
  geom_boxplot()+ 
  xlab('Região') + ylab('Taxa de encarceramento') + 
  labs(fill='Região') + 
  theme_minimal()

# IDH ----------------------------------------------------
summary(PoP$IDH)

df = subset(PoP, select=c('Countries','Prisonrate','IDH','Continents','Region' ))
df = na.omit(df)

ggplot(df, aes(x=IDH))+
  geom_density(color="darkblue", fill="lightblue") + 
  xlab('IDH') + ylab('Densidade') + theme_minimal()

ggplot(df, aes(x=Continents, y=IDH))+
  geom_boxplot()+ 
  xlab('Continentes') + ylab('IDH') + 
  theme_minimal()

ggplot(df, aes(y=IDH, fill=Region))+
  geom_boxplot()+ 
  theme(axis.text.x = element_blank())+
  xlab('Região') + ylab('IDH') + 
  labs(fill='Região') + 
  theme_minimal()

# GINI -------------------------------------------------------
summary(PoP$Gini)

df = subset(PoP, select=c('Countries','Prisonrate','Gini','Continents','Region'))
df = na.omit(df)

ggplot(df, aes(x=Gini))+
  geom_density(color="darkblue", fill="lightblue") + 
  xlab('Gini') + ylab('Densidade') + theme_minimal()

ggplot(df, aes(x=Continents, y=Gini))+
  geom_boxplot()+ 
  xlab('Continentes') + ylab('Gini') + 
  theme_minimal()

ggplot(df, aes(y=Gini, fill=Region))+
  geom_boxplot()+ 
  theme(axis.text.x = element_blank())+
  xlab('Região') + ylab('Gini') + 
  labs(fill='Região') + 
  theme_minimal()

# Democracy Index -------------------------------------------------------
summary(PoP$DemocracyIndex)

df = subset(PoP, select=c('Countries','Prisonrate','DemocracyIndex','Continents','Region' ))
df = na.omit(df)

ggplot(df, aes(x=DemocracyIndex)) +
  geom_density(color="darkblue", fill="lightblue") + 
  xlab('Índice de democracia') + ylab('Densidade') + theme_minimal()

ggplot(df, aes(x=Continents, y=DemocracyIndex))+
  geom_boxplot()+ 
  xlab('Continentes') + ylab('Índice de democracia') + 
  theme_minimal()

ggplot(df, aes(y=DemocracyIndex, fill=Region))+
  geom_boxplot()+ 
  theme(axis.text.x = element_blank())+
  xlab('Região') + ylab('ìndice de democracia') + 
  labs(fill='Região') + 
  theme_minimal()

# Freedom Index -------------------------------------------------------
summary(PoP$EconomicF)

df = subset(PoP, select=c('Countries','Prisonrate','EconomicF','Continents','Region' ))
df = na.omit(df)

ggplot(df, aes(x=EconomicF))+
  geom_density(color="darkblue", fill="lightblue") + 
  xlab('Índice liberdade econômica') + ylab('Densidade') + theme_minimal()

ggplot(df, aes(x=Continents, y=EconomicF))+
  geom_boxplot()+ 
  xlab('Continentes') + ylab('Índice de liberdade econômica') + 
  theme_minimal()

ggplot(df, aes(y=EconomicF, fill=Region))+
  geom_boxplot()+ 
  theme(axis.text.x = element_blank())+
  xlab('Região') + ylab('ìndice de liberdade econômica') + 
  labs(fill='Região') + 
  theme_minimal()


## ------------------ ##
## Cruzando variáveis ##
## ------------------ ##
# Correlação entre todas as variáveis --------------------
nm = names(PoP)[c(2:14,16:24)]
df = subset(PoP, select=nm)
df = na.omit(df)
df = data.frame(df)
vif(lm(Prisonrate ~ GDP+Young+Homicide+Prisonpop+Gini+DemocracyIndex+FunGog+                                
                    Pculture+ElectoralProcess+Pparticipation+Civilliberties+                        
                    IDH+Lifeexpec+Yearsscholling+Gross.national.income..GNI..per.capita+
                    EconomicF+Sizegov+Legalsystem+Soundmoney+Freedomtrade+Regulation,
       data=df))

cr = round(cor(df),2)
par(mar=c(10,10,4,5)+.1)
image(cr, axes=F)
axis(1, at=seq(0,1,by=1/21), labels=nm, las=3)
axis(2, at=seq(0,1,by=1/21), labels=nm, las=1)

# Democracia Index ---------------------------
nm = names(PoP)[c(8:13)]
df = subset(PoP, select=nm)
df = na.omit(df)
df = data.frame(df)

cr = as.matrix(round(cor(df),2))
cr
par(mar=c(10,10,4,5)+.1)
image(cr, axes=F)
axis(1, at=seq(0,1,by=1/5), labels=nm, las=3)
axis(2, at=seq(0,1,by=1/5), labels=nm, las=1)
# Mesmo gráfico com o pacote lattice
levelplot(cr)

# EconomicF = Economic Freedom --------------
nm = names(PoP)[c(19:24)]
df = subset(PoP, select=nm)
df = na.omit(df)
df = data.frame(df)

cr = as.matrix(round(cor(df),2))
cr
par(mar=c(10,10,4,5)+.1)
image(cr, axes=F)
axis(1, at=seq(0,1,by=1/5), labels=nm, las=3)
axis(2, at=seq(0,1,by=1/5), labels=nm, las=1)
# Mesmo gráfico com o pacote lattice
levelplot(cr)

# Cruzando IDH, 1-Gini e Taxa de encarceramento ---------------------------------------
df = subset(PoP, select=c('Countries','Prisonrate','IDH','Gini','Continents','Region' ))
df = na.omit(df)
nm = names(df)[c(2,3,4)]

# Correlação
cr = as.matrix(round(cor(df[,c(2,3,4)]),2))
cr
par(mar=c(10,10,4,5)+.1)
image(cr, axes=F)
axis(1, at=seq(0,1,by=1/2), labels=nm, las=3)
axis(2, at=seq(0,1,by=1/2), labels=nm, las=1)
# Mesmo gráfico com o pacote lattice
levelplot(cr)


# Scatterplot3d
plot_ly(x=df$IDH, y=1-df$Gini, z=df$Prisonrate, type="scatter3d", mode="markers", color=df$Region)

plot_ly(x=df$IDH, y=1-df$Gini, z=df$Prisonrate) %>%
add_markers(color=df$Region) %>%
  layout(scene = list(xaxis = list(title = 'IDH'),
                      yaxis = list(title = '1-Gini'),
                      zaxis = list(title = 'Taxa')))

## ------------------------------ ##
## Modelos de regressão bayesiana ##
## ------------------------------ ##
# Modelo Saturado
nm = names(PoP)[c(2:14,16:27)]
df = subset(PoP, select=nm)
#df = na.omit(df)
df = data.frame(df)
colnames(df)[16] = 'GNIpc'
bs = bayesglm(log(Prisonrate)~IDH+Lifeexpec+Yearsscholling+GNIpc+
        DemocracyIndex+FunGog+Pculture+ElectoralProcess+Pparticipation+Civilliberties+
        EconomicF+Sizegov+Legalsystem+Soundmoney+Freedomtrade+Regulation+
        Young+Homicide+Gini,
        data=df, family=gaussian())
summary(bs)
vif(bs)
# Modelo com os índices agregados
bsa = bayesglm(log(Prisonrate)~IDH+DemocracyIndex+EconomicF+
        Young+Homicide+Gini,
        data=df, family=gaussian())
summary(bsa)
vif(bsa)
# Modelo com os índices desagregados
bsd = bayesglm(log(Prisonrate)~Lifeexpec+Yearsscholling+GNIpc+
                 FunGog+Pculture+ElectoralProcess+Pparticipation+Civilliberties+
                 Sizegov+Legalsystem+Soundmoney+Freedomtrade+Regulation+
                 Young+Homicide+Gini,
               data=df, family=gaussian())
summary(bsd)
vif(bsd)

data.frame(list(Modelo=c('Saturado','Agregado','Desagregado')),
                AIC=c(bs$aic,bsa$aic,bsd$aic))

# Análise do melhor modelo via step
step(bs)
mm = bayesglm(formula = log(Prisonrate)~IDH+GNIpc+DemocracyIndex+Pculture+Freedomtrade+ 
                Young + Homicide + Gini, 
              family = gaussian(), data = df)
summary(mm) # Melhor modelo com menor aic
vif(mm)
# P.S. Ao adicionar a variável Região os modelos ficam com aic mais baixo,
# mas o outilier EUA influencia a região América do Norte.

# Testando o modelo somente com IDH, GINI, DEM, FREEDON
t=bayesglm(formula = log(Prisonrate)~IDH+Gini+
        DemocracyIndex+EconomicF, 
         family = gaussian(), data = df)
summary(t) # Melhor modelo com menor aic
vif(t)

