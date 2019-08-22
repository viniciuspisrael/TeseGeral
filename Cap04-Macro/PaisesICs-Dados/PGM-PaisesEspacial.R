rm(list=ls(all=TRUE)) # Limpar memória do R 
library(readxl)       # Lê planílias do Excel
library(ggplot2)      # Pacote gráfico
library(plyr)         # Possui a função join para juntar BDs
library(raster)       # Fazer escala no gráfico
library(car)          # Biblioteca de diagnóstico
library(plotly)       # Plot em 3d
library(ggpubr)       # Múltiplos gráficos
library(arm)          # Regressão bayesiana
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
# Retirando a variável "Dummie AL"
PoP$`Dummie AL` = NULL
# Definindo os fatores
PoP$IDHO = as.factor(PoP$IDHO) 
PoP$Continents = as.factor(PoP$Continents)
PoP$UNregion = as.factor(PoP$UNregion)
PoP$Region = as.factor(PoP$Region)

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

plot_ly(x=df$IDH, y=1-df$Gini, z=df$Prisonrate) %>%
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
# ------------------------------------
# kappa = 1.5 - Modelo 0: mu constante
MC <- model.control(trend.d = "cte", kappa = 1.5)
m0b <- krige.bayes(gdt, model=MC, prior=PC, output=OC)
m0 = likfit(gdt, ini=c(3000, 2), cov.model="matern",kappa=1.5)
summary(m0)
plot(gdt, trend = "cte", qt.col = 1)
plot(variog(gdt, trend="cte"))
# -----------------------------
# kappa = 1.5 - Modelo 1: "1st"
MC <- model.control(trend.d = "1st", kappa = 1.5)
m1skp15 <- krige.bayes(gdt, model=MC, prior=PC, output=OC)
m01 = likfit(gdt, trend = "1st", ini=c(3000, 2), cov.model="matern",kappa=1.5)
summary(m01) # Melhor modelo e melhor ajuste dos resíduos
plot(gdt, trend = "1st", qt.col = 1)
plot(variog(gdt, trend="1st"))
# -----------------------------
# kappa = 1.5 - Modelo 2: "2nd"
MC <- model.control(trend.d = "2nd", kappa = 1.5)
m2skp15 <- krige.bayes(gdt, model=MC, prior=PC, output=OC)
m02 = likfit(gdt, trend = "2nd", ini=c(3000, 2), cov.model="matern",kappa=1.5)
summary(m02)
plot(gdt, trend = "2nd", qt.col = 1)
plot(variog(gdt, trend="2nd"))
# -----------------------------
# kappa = 1.5 - Modelo 3:  DemIndex + FreedIndex
f = ~covariate[,1]+~covariate[,2]
MC <- model.control(trend.d = f, kappa = 1.5)
m3skp15 <- krige.bayes(gdt, model=MC, prior=PC, output=OC)
m1 = likfit(gdt, trend=f, ini=c(3000, 2), cov.model="matern",kappa=1.5)
summary(m1)
plot(gdt, trend=f, qt.col = 1)
plot(variog(gdt, trend=f))
# -----------------------------------------------------
# kappa = 1.5 - Modelo 4: "1st" + DemIndex + FreedIndex
f = ~coords[,1]+~coords[,2]+~covariate[,1]+~covariate[,2]
MC <- model.control(trend.d = f, kappa = 1.5)
m4skp15 <- krige.bayes(gdt, model=MC, prior=PC, output=OC)
m2 = likfit(gdt, trend=f, ini=c(3000, 2), cov.model="matern",kappa=1.5)
summary(m2)                     # Melhor Modelo
plot(gdt, trend=f, qt.col = 1)
plot(variog(gdt, trend=f))
# -----------------------------------------------------
# kappa = 1.5 - Modelo 5: "1st" + Democracy Index
f = ~coords[,1]+~coords[,2]+~covariate[,1]
MC <- model.control(trend.d = f, kappa = 1.5)
m5skp15 <- krige.bayes(gdt, model=MC, prior=PC, output=OC)
m3 = likfit(gdt, trend=f, ini=c(3000, 2), cov.model="matern",kappa=1.5)
summary(m3)
plot(gdt, trend=f, qt.col = 1)
plot(variog(gdt, trend=f))
# -----------------------------------------------------
# kappa = 1.5 - Modelo 6: "1st" + Economic Freedom Index
f = ~coords[,1]+~coords[,2]+~covariate[,2]
MC <- model.control(trend.d = f, kappa = 1.5)
m6skp15 <- krige.bayes(gdt, model=MC, prior=PC, output=OC)
m4 = likfit(gdt, trend=f, ini=c(3000, 2), cov.model="matern",kappa=1.5)
summary(m4)
plot(gdt, trend=f, qt.col = 1)
plot(variog(gdt, trend=f))
# -----------------------------------------------------
# kappa = 1.5 - Modelo 7: Democracy Index
f = ~covariate[,1]
MC <- model.control(trend.d=f, kappa = 1.5)
m7skp15 <- krige.bayes(gdt, model=MC, prior=PC, output=OC)
m5 = likfit(gdt, trend=f, ini=c(3000, 2), cov.model="matern",kappa=1.5)
summary(m5)
plot(gdt, trend=f, qt.col = 1)
plot(variog(gdt, trend=f))
# -----------------------------------------------------
# kappa = 1.5 - Modelo 8: Economic Freedom Index
f = ~covariate[,2]
MC <- model.control(trend.d = f, kappa = 1.5)
m8skp15 <- krige.bayes(gdt, model=MC, prior=PC, output=OC)
m6 = likfit(gdt, trend=f, ini=c(3000, 2), cov.model="matern",kappa=1.5)
summary(m6)
plot(gdt, trend=f, qt.col = 1)
plot(variog(gdt, trend=f))
# -----------------------------------------------------
# kappa = 1.5 - Modelo 9: "1st" + DemInd + EconFreed + Young + Homicidio
f = ~coords[,1]+~coords[,2]+~covariate[,1]+~covariate[,2]+~covariate[,3]+~covariate[,4]
MC <- model.control(trend.d = f, kappa = 1.5)
m9skp15 <- krige.bayes(gdt, model=MC, prior=PC, output=OC)
m7 = likfit(gdt, trend=f, ini=c(3000, 2), cov.model="matern",kappa=1.5)
summary(m7)
plot(gdt, trend=f, qt.col = 1)
plot(variog(gdt, trend=f))

# Comparando os modelos
lt =list('Modelo'=c('Sem trend', 'Trend 1st', 'Trend 2nd', 
                    'Trend covariáveis','Trend coord e covariáveis',
                    'Trend coord e DemorcarcyI', 'Trend coord e EconomicFreedI',
                    'Trend DI', 'Trend FI', 'Saturado'),
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
as.data.frame(lt)
write.csv(as.data.frame(lt), 'EspacialModeloCompara')


# Qual modelo bayesiano?
md = m4skp15
f = ~coords[,1]+~coords[,2]+~covariate[,1]+~covariate[,2]
# Análise dos parâmetros a posteriori
par(mfrow=c(1,2))
plot(md)
par(mfrow=c(1,1))
plot(variog(gdt, trend=f))

n = dim(md$posterior$sample)[2]
op = par(mfrow=c((n+1)/2, 4))
for (i in 1:dim(md$posterior$sample)[2]){
  plot(md$posterior$sample[,i], ylab=names(md$posterior$sample)[i])
  hist(md$posterior$sample[,i],prob=T, main='', xlab=names(md$posterior$sample)[i],ylab='Densidade')
  lines(density(md$posterior$sample[,i]), col='blue')
  abline(v=quantile(md$posterior$sample[,i],probs=c(0.025,0.5,0.975)), col='red',lty=2)
}
par(op)  
summary(md$posterior$sample)


# ----------------------------------
# Suavizando curva "2nd"
locs <- pred_grid(c(0,1), c(0,1), by=0.05)
# kappa = 1.5 - Modelo 2: "2nd"
MC <- model.control(trend.d = "2nd", trend.l = "2nd", kappa = 1.5)
b2skp15 <- krige.bayes(gdt, loc = locs, model=MC, prior=PC, output=OC)

md = b2skp15
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
# Construindo o grid
z = matrix(md$predictive$mean, nrow=21, ncol=21)
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
