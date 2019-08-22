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
# GINI (últimos 10 anos)
# Fraser Institute (Economic Freedom-2015)
# The Economist Intelligence Unit (Democracy Index)

library(readxl)
PoP <- read_excel("PoP.xlsx")
View(PoP)

# Alguns acertos no BD ------------------------------------------
PoP[PoP=='NA'] = NA
# Taxa de encarceramento por continentes
PoP$Continents[PoP$Continents=='Africa'] = 'África'
PoP$Continents[PoP$Continents=='America'] = 'América'
PoP$Continents[PoP$Continents=='Asia'] = 'Ásia'
PoP$Continents[PoP$Continents=='Europe'] = 'Europa'
PoP$Continents[PoP$Continents=='Oceania'] = 'Oceania'
PoP$Region[PoP$Region=='EU/West Europe'] = 'Europa Ocidental'
PoP$Region[PoP$Region=='EX-USSR/East Europe'] = 'Ex-URSS/Europa Oriental'
PoP$Region[PoP$Region=='Latin America'] = 'América Latina'
PoP$Region[PoP$Region=='Middle East/North Africa'] = 'Oriente Médio/Norte da África'
PoP$Region[PoP$Region=='North America'] = 'América do Norte'
PoP$Region[PoP$Region=='Southeast Asia'] = 'Sudeste da Ásia'
PoP$Region[PoP$Region=='Sub-saharan Africa'] = 'África Subsariana'
# Gini ente zero e um.
PoP$Gini = PoP$Gini/100
# Retirando a variável "Dummie AL"
PoP$`Dummie AL` = NULL
# Definindo os fatores
PoP$IDHO = as.factor(PoP$IDHO)
PoP$Continents = as.factor(PoP$Continents)
PoP$UNregion = as.factor(PoP$UNregion)
PoP$Region = as.factor(PoP$Region)

## ----------------------------- ##
## Análise Exploratória de Dados ##
## ----------------------------- ##

# Taxa de encarceramento ----------------------------------------
summary(PoP$Prisonpop); sd(na.omit(PoP$Prisonpop));
summary(PoP$Prisonrate); sd(na.omit(PoP$Prisonrate));
summary(PoP$IDH);  sd(na.omit(PoP$IDH));
summary(PoP$`Gross national income (GNI) per capita`);  sd(na.omit(PoP$`Gross national income (GNI) per capita`));
summary(PoP$Yearsscholling);  sd(na.omit(PoP$Yearsscholling));
summary(PoP$Lifeexpec);  sd(na.omit(PoP$Lifeexpec));
summary(PoP$Gini);  sd(na.omit(PoP$Gini));
summary(PoP$GDP);  sd(na.omit(PoP$GDP));
summary(PoP$Homicide);  sd(na.omit(PoP$Homicide));
summary(PoP$Young);  sd(na.omit(PoP$Young));
summary(PoP$DemocracyIndex);  sd(na.omit(PoP$DemocracyIndex));
summary(PoP$FunGog);  sd(na.omit(PoP$FunGog));
summary(PoP$Pculture);  sd(na.omit(PoP$Pculture));
summary(PoP$ElectoralProcess);  sd(na.omit(PoP$ElectoralProcess));
summary(PoP$Pparticipation);  sd(na.omit(PoP$Pparticipation));
summary(PoP$Civilliberties);  sd(na.omit(PoP$Civilliberties));
summary(PoP$EconomicF);  sd(na.omit(PoP$EconomicF));
summary(PoP$Sizegov);  sd(na.omit(PoP$Sizegov));
summary(PoP$Legalsystem);  sd(na.omit(PoP$Legalsystem));
summary(PoP$Soundmoney);  sd(na.omit(PoP$Soundmoney));
summary(PoP$Freedomtrade);  sd(na.omit(PoP$Freedomtrade));
summary(PoP$Regulation);  sd(na.omit(PoP$Regulation));

df = subset(PoP, select=c('Countries','Prisonrate','Continents','Region'))
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
df = na.omit(df)
#df = data.frame(df)
colnames(df)[16] = 'PIBpc'
bs = bayesglm(log(Prisonrate)~IDH+Lifeexpec+Yearsscholling+PIBpc+
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
bsd = bayesglm(log(Prisonrate)~Lifeexpec+Yearsscholling+PIBpc+
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
mm = bayesglm(formula = log(Prisonrate)~IDH+PIBpc+DemocracyIndex+EconomicF+
                Sizegov+Soundmoney+Regulation+Homicide+Gini,
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


## -------------------------------------- ##
## Análise Exploratória de Dados Espacial ##
## -------------------------------------- ##

# Plotando dados em 3D e superfície ------------------------
# Cruzando IDH, 1-Gini e Taxa
df = subset(PoP, select=c('Countries','Prisonrate','IDH','Gini','Continents','Region'))
df = na.omit(df)
nm = names(df)[c(2,3,4)]
# Taxa
p1p=ggplot(df, aes(x=IDH, y=1-Gini)) +
  geom_point(aes(size=Prisonrate)) + geom_smooth() +
  labs(fill='Taxa') + theme_minimal()
p2p=ggplot(df, aes(x=IDH, y=Prisonrate)) + geom_smooth() +
  geom_point(aes(size=1-Gini)) + ylab('Taxa') +
  theme_minimal()
p3p=ggplot(df, aes(x=1-Gini, y=Prisonrate)) +
  geom_point(aes(size=IDH)) + geom_smooth() +
  ylab('Taxa')+ theme_minimal()
p4p=ggplot(df, aes(x=Prisonrate))+
  geom_histogram(aes(y=..density..),color='black',fill='white') +
  geom_density(alpha=.2, fill='blue') + ylab('Taxa') +
  theme_minimal()
ggarrange(p1p, p2p, p3p, p4p, ncol = 2, nrow = 2)
# log Taxa
p1lp=ggplot(df, aes(x=IDH, y=1-Gini)) +
  geom_point(aes(size=log(Prisonrate))) + geom_smooth() +
  labs(fill='Taxa') + theme_minimal()
p2lp=ggplot(df, aes(x=IDH, y=log(Prisonrate))) + geom_smooth() +
  geom_point(aes(size=1-Gini)) + ylab('Taxa') +
  theme_minimal()
p3lp=ggplot(df, aes(x=1-Gini, y=log(Prisonrate))) +
  geom_point(aes(size=IDH)) + geom_smooth() +
  ylab('Taxa')+ theme_minimal()
p4lp=ggplot(df, aes(x=log(Prisonrate)))+
  geom_histogram(aes(y=..density..),color='black',fill='white')+
  geom_density(alpha=.2, fill='blue') + ylab('log(Taxa)')+
  theme_minimal()
ggarrange(p1lp, p2lp, p3lp, p4lp, ncol = 2, nrow = 2)

# Plot 3D
plot_ly(x=df$IDH, y=1-df$Gini, z=df$Prisonrate, type="scatter3d", mode="markers", color=df$Region)

plot_ly(x=df$IDH, y=1-df$Gini, z=df$Prisonrate,
        text = ~paste('País: ', df$Countries)) %>%
  add_markers(color=df$Region) %>%
  layout(scene = list(xaxis = list(title = 'IDH'),
                      yaxis = list(title = '1-Gini'),
                      zaxis = list(title = 'Taxa')))

plot_ly(x=df$IDH, y=1-df$Gini, z=df$Prisonrate, type="scatter3d", mode="markers",
        marker = list(color=df$Prisonrate, colorscale=c('#FFE1A1','#683531'), showscale=TRUE),
        text = ~paste('País: ', df$Countries)) %>%
  layout(scene = list(xaxis = list(title = 'IDH'),
                      yaxis = list(title = '1-Gini'),
                      zaxis = list(title = 'Taxa')))

plot_ly(x=df$IDH, y=1-df$Gini, z=log(df$Prisonrate), type="scatter3d", mode="markers",
        marker=list(color=log(df$Prisonrate),colorscale=c('#FFE1A1','#683531'), showscale=TRUE),
        text = ~paste('País: ', df$Countries)) %>%
  layout(scene = list(xaxis = list(title = 'IDH'),
                      yaxis = list(title = '1-Gini'),
                      zaxis = list(title = 'log(Taxa)')))

# ------------------------------------------------------- #
# Diggle e Ribeiro 2007, Capítulo 7 - Bayesian Inference  #
# ------------------------------------------------------- #
library(geoR)
nm = names(PoP)[c(2,7,8,14,19,4,5)] # IDH, 1-GINI, PRISONRATE, DEMIDEX, EFINDEX, YOUNG, HOMICIDES
df = subset(PoP, select=nm)
df = na.omit(df)

gdt = as.geodata(cbind(df$IDH, 1-df$Gini, log(df$Prisonrate)))
gdt$covariate = as.data.frame(cbind(df$DemocracyIndex, df$EconomicF, df$Young, df$Homicide))
colnames(gdt$covariate) = c('DemIndex','FreedIndex','TxJovens','TxHomicidio')
colnames(gdt$coords) = c('IDH', '1-Gini')
names(gdt)
class(gdt)
summary(gdt)
plot(gdt)

# ------------------------------------- #
# Fazendo o Ajuste Bayesiano e Clássico #
# ------------------------------------- #

# Comandos comuns
PC <- prior.control(beta.prior=c("flat"),
                    phi.discrete = seq(0, 6, l=41),
                    phi.prior = "reciprocal",
                    sigmasq.prior = "reciprocal",
                    tausq.rel.prior = "unif",
                    tausq.rel.discrete = seq(0, 3, l=31))
OC <- output.control(n.post = 5000, moments = T)
# Escolhendo o kappa
kp = 0.5
kp = 1
kp = 1.5
# ------------------------------------
# Modelo 0: mu constante
MC <- model.control(trend.d = "cte", kappa = kp)
m0b <- krige.bayes(gdt, model=MC, prior=PC, output=OC)
m0 = likfit(gdt, ini=c(3000, 2), cov.model="matern",kappa=kp)
summary(m0)
plot(gdt, trend = "cte", qt.col = 1)
plot(variog(gdt, trend="cte"), xlab='Distância', ylab='Semivariograma')
# -----------------------------
# Modelo 1: "1st"
MC <- model.control(trend.d = "1st", kappa = kp)
m1skp15 <- krige.bayes(gdt, model=MC, prior=PC, output=OC)
m01 = likfit(gdt, trend = "1st", ini=c(3000, 2), cov.model="matern",kappa=kp)
summary(m01) # Melhor modelo e melhor ajuste dos resíduos
plot(gdt, trend = "1st", qt.col = 1)
plot(variog(gdt, trend="1st"), xlab='Distância', ylab='Semivariograma')
# -----------------------------
# Modelo 2: "2nd"
MC <- model.control(trend.d = "2nd", kappa = kp)
m2skp15 <- krige.bayes(gdt, model=MC, prior=PC, output=OC)
m02 = likfit(gdt, trend = "2nd", ini=c(3000, 2), cov.model="matern",kappa=kp)
summary(m02)
plot(gdt, trend = "2nd", qt.col = 1)
plot(variog(gdt, trend="2nd"), xlab='Distância', ylab='Semivariograma')
# -----------------------------
# Modelo 3:  DemIndex + FreedIndex
f = ~covariate[,1]+~covariate[,2]
MC <- model.control(trend.d = f, kappa=kp)
m3skp15 <- krige.bayes(gdt, model=MC, prior=PC, output=OC)
m1 = likfit(gdt, trend=f, ini=c(3000, 2), cov.model="matern",kappa=kp)
summary(m1)
plot(gdt, trend=f, qt.col = 1)
plot(variog(gdt, trend=f), xlab='Distância', ylab='Semivariograma')
# -----------------------------------------------------
# Modelo 4: "1st" + DemIndex + FreedIndex
f = ~coords[,1]+~coords[,2]+~covariate[,1]+~covariate[,2]
MC <- model.control(trend.d = f, kappa=kp)
m4skp15 <- krige.bayes(gdt, model=MC, prior=PC, output=OC)
m2 = likfit(gdt, trend=f, ini=c(3000, 2), cov.model="matern",kappa=kp)
summary(m2)                     # Melhor Modelo
plot(gdt, trend=f, qt.col = 1)
plot(variog(gdt, trend=f), xlab='Distância', ylab='Semivariograma')
# -----------------------------------------------------
# Modelo 5: "1st" + Democracy Index
f = ~coords[,1]+~coords[,2]+~covariate[,1]
MC <- model.control(trend.d = f, kappa=kp)
m5skp15 <- krige.bayes(gdt, model=MC, prior=PC, output=OC)
m3 = likfit(gdt, trend=f, ini=c(3000, 2), cov.model="matern",kappa=kp)
summary(m3)
plot(gdt, trend=f, qt.col = 1)
plot(variog(gdt, trend=f), xlab='Distância', ylab='Semivariograma')
# -----------------------------------------------------
# Modelo 6: "1st" + Economic Freedom Index
f = ~coords[,1]+~coords[,2]+~covariate[,2]
MC <- model.control(trend.d = f, kappa=kp)
m6skp15 <- krige.bayes(gdt, model=MC, prior=PC, output=OC)
m4 = likfit(gdt, trend=f, ini=c(3000, 2), cov.model="matern",kappa=kp)
summary(m4)
plot(gdt, trend=f, qt.col = 1)
plot(variog(gdt, trend=f), xlab='Distância', ylab='Semivariograma')
# -----------------------------------------------------
# Modelo 7: Democracy Index
f = ~covariate[,1]
MC <- model.control(trend.d=f, kappa=kp)
m7skp15 <- krige.bayes(gdt, model=MC, prior=PC, output=OC)
m5 = likfit(gdt, trend=f, ini=c(3000, 2), cov.model="matern",kappa=kp)
summary(m5)
plot(gdt, trend=f, qt.col = 1)
plot(variog(gdt, trend=f), xlab='Distância', ylab='Semivariograma')
# -----------------------------------------------------
# Modelo 8: Economic Freedom Index
f = ~covariate[,2]
MC <- model.control(trend.d = f, kappa=kp)
m8skp15 <- krige.bayes(gdt, model=MC, prior=PC, output=OC)
m6 = likfit(gdt, trend=f, ini=c(3000, 2), cov.model="matern",kappa=kp)
summary(m6)
plot(gdt, trend=f, qt.col = 1)
plot(variog(gdt, trend=f), xlab='Distância', ylab='Semivariograma')
# -----------------------------------------------------
# Modelo 9: "1st" + DemInd + EconFreed + Young + Homicidio
f = ~coords[,1]+coords[,2]+covariate[,1]+covariate[,2]+covariate[,3]+covariate[,4]
MC <- model.control(trend.d = f, kappa=kp)
m9skp15 <- krige.bayes(gdt, model=MC, prior=PC, output=OC)
m7 = likfit(gdt, trend=f, ini=c(3000, 2), cov.model="matern",kappa=kp)
summary(m7)
plot(gdt, trend=f, qt.col = 1)
plot(variog(gdt, trend=f), xlab='Distância', ylab='Semivariograma')

# Comparando os modelos
ltkp =list('Modelo'=c('Sem trend', 'Trend 1st', 'Trend 2nd',
                      'Trend DemI + EcFrI','Trend coord e covariáveis',
                      'Trend coord e DemI', 'Trend coord + DemI + EcFrI',
                      'Trend DemI', 'Trend EcFrI', 'Saturado'),
         'AICespacial'= c(m0$AIC,m01$AIC,m02$AIC,m1$AIC,m2$AIC,m3$AIC,m4$AIC,m5$AIC,m6$AIC, m7$AIC),
         'AICnespacial'=c(m0$nospatial$AIC.ns,m01$nospatial$AIC.ns,m02$nospatial$AIC.ns,
                          m1$nospatial$AIC.ns,m2$nospatial$AIC.ns,
                          m3$nospatial$AIC.ns,m4$nospatial$AIC.ns,
                          m5$nospatial$AIC.ns,m6$nospatial$AIC.ns,
                          m7$nospatial$AIC.ns),
         'BICespacial'= c(m0$BIC,m01$BIC,m02$BIC,m1$BIC,m2$BIC,m3$BIC,m4$BIC,m5$BIC,m6$BIC,m7$BIC),
         'BICnespacial'=c(m0$nospatial$BIC.ns,m01$nospatial$BIC.ns,m02$nospatial$BIC.ns,
                          m1$nospatial$BIC.ns,m2$nospatial$BIC.ns,
                          m3$nospatial$BIC.ns,m4$nospatial$BIC.ns,
                          m5$nospatial$BIC.ns,m6$nospatial$BIC.ns,
                          m7$nospatial$BIC.ns))
as.data.frame(ltkp)
write.csv(as.data.frame(ltkp), paste0('EspacialModeloComparakp',kp))

# Qual modelo bayesiano?
md = m9skp15
f = ~coords[,1]+coords[,2]+covariate[,1]+covariate[,2]+covariate[,3]+covariate[,4]
# Análise dos parâmetros a posteriori
par(mfrow=c(1,2))
plot(md, ylab='Densidade', ylab='Densidade')
par(mfrow=c(1,1))
plot(variog(gdt, trend=f), xlab='Distância', ylab='Semivariograma')
eyefit(variog(gdt, trend=f))

n = dim(md$posterior$sample)[2]
xl = c(expression(beta[0]),expression(beta[1]),expression(beta[2]),
       expression(beta[3]),expression(beta[4]),expression(beta[5]),
       expression(beta[6]),expression(sigma^2),expression(phi),
       expression(nu^2))
op = par(mfrow=c(3,4))
for (i in 1:(dim(md$posterior$sample)[2])){
#  plot(md$posterior$sample[,i], ylab=names(md$posterior$sample)[i])
  hist(md$posterior$sample[,i],prob=T, main='', xlab=xl[i],ylab='Densidade')
  lines(density(md$posterior$sample[,i]), col='blue')
  abline(v=quantile(md$posterior$sample[,i],probs=c(0.025,0.5,0.975)), col='red',lty=2)
}
par(op)
summary(md$posterior$sample)

# Análise do variograma
plot(variog(gdt, option = "cloud"), xlab = "u", ylab = "V(u)")
plot(variog(gdt, uvec = seq(0, 0.65, by = 0.05)), xlab = "u", ylab = "V(u)")
plot(variog(gdt, trend = "2nd", max.dist = 6.5), xlab = "u", ylab = "V(u)")

# Ajustando o variograma
# ν2 = τ2/σ2
tau2 = abs(md$posterior$sample[n]*summary(md$posterior$sample[,n-2]))
summary(tau2)
tau2 = median(tau2[,1])
tau2
sig2 = median(md$posterior$sample[,n-2])
sig2
phi = median(md$posterior$sample[,n-1])
phi

gdt.v <- variog(gdt, max.dist=1, trend=f)
plot(gdt.v, ylab='Semivariograma', xlab='Distância')
eyefit(variog(gdt, trend=f))
lines.variomodel(seq(0, 0.6, l = 100),cov.pars=c(sig2, phi),
                 cov.model="matern", kappa=0.5, nugget = tau2, col='blue')

# ----------------------------------
# Suavizando curva "2nd"
locs <- pred_grid(c(0,1), c(0,1), by=0.05)
# kappa = 0.5 - Modelo 2: "2nd"
MC <- model.control(trend.d = "2nd", trend.l = "2nd", kappa = 0.5)
ms <- krige.bayes(gdt, loc = locs, model=MC, prior=PC, output=OC)

md = ms
# Fazendo a predição
op=par(mfrow=c(2,2))
  image(md, loc = locs, main = "predicted", col=gray(seq(1,0.1,l=30)))
  image(md, val ="variance", loc = locs,
      main = "prediction variance", col=gray(seq(1,0.1,l=30)))
  image(md, val = "simulation", number.col = 1, loc = locs,
      main = "a simulation from the predictive distribution", col=gray(seq(1,0.1,l=30)))
  image(md, val = "simulation", number.col = 2,loc = locs,
      main = "another simulation from \n the predictive distribution", col=gray(seq(1,0.1,l=30)))
par(op)
# Usando o plotly
plot_ly(x=locs[,1], y=locs[,2], z=md$predictive$mean, type="scatter3d", mode="markers",
        marker=list(color=md$predictive$mean,colorscale=c('#FFE1A1','#683531'), showscale=TRUE)) %>%
  layout(scene = list(xaxis = list(title = 'IDH'),
                      yaxis = list(title = '1-Gini'),
                      zaxis = list(title = 'log(Predito)')))


Sys.setenv("plotly_username"="vinicius.israel")
Sys.setenv("plotly_api_key"="tRaDlFjEdthdjfzPwOLT")
# Construindo o grid
z = t(matrix(md$predictive$mean, nrow=21, ncol=21))
p <- plot_ly(z = ~z, colorscale=c('#FFE1A1','#683531')) %>%
  add_surface(contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  )) %>%
  layout(scene = list(xaxis = list(title = 'IDH'),
                      yaxis = list(title = '1-Gini'),
                      zaxis = list(title = 'log(Predito)')))

chart_link = api_create(p, filename="superficie")
chart_link

# -------------------------------------------------------------------
# Previsão da taxa de encarceramento para o Brasil pelo melhor modelo
md = m9skp15
# IDH, 1-GINI, PRISONRATE, DEMIDEX, EFINDEX, YOUNG, HOMICIDES
Xbr = array(c(1,PoP[24,"IDH"],1-PoP[24,"Gini"],PoP[24,"DemocracyIndex"],PoP[24,"EconomicF"],PoP[24,"Young"],PoP[24,"Homicide"]))
Xbr = as.numeric(Xbr)
beta = as.matrix(md$posterior$sample[,1:7])
tx = beta%*%Xbr
quantile(exp(tx), probs=c(0.025, 0.5, 0.975))
PoP[24,"Prisonrate"]

quantile(na.omit(PoP$Prisonrate), probs=c(0.025, 0.5, 0.975))
PoP$Prisonrate[24]
hist(PoP$Prisonrate)


# Citando o GeoR
# http://leg.ufpr.br/geoR/geoRdoc/geoRintro.pdf
# RIBEIRO Jr., P.J. & DIGGLE, P.J. (2001) geoR: A
# package for geostatistical analysis. R-NEWS, Vol 1,
# No 2, 15-18. ISSN 1609-3631.
# @Article{,
# title = {{geoR}: a package for geostatistical analysis},
# author = {Ribeiro Jr., P.J. and Diggle, P.J.},
# journal = {R-NEWS},
# year = {2001},
# volume = {1},
# number = {2},
# pages = {15--18},
# issn = {1609-3631},
# url = {http://cran.R-project.org/doc/Rnews}
#}


