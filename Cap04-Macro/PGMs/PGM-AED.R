library("rstudio")
library("arm")
library("scatterplot3d")
library("geoR")

# -----------------#
# Presos no Mundo  #
#------------------#
# Leitura dos Dados
setwd("C:/Users/Vinicius/Desktop/TextoTese/Cap04-Macro/PGMs/Dados")
hdi.vh= read.csv('HDI-VeryHigh.csv',dec=",",sep=";",header=T, fill=TRUE)
hdi.h = read.csv('HDI-High.csv',dec=",",sep=";",header=T, fill=TRUE)
hdi.m = read.csv('HDI-Medium.csv',dec=",",sep=";",header=T, fill=TRUE)
hdi.l = read.csv('HDI-Low.csv',dec=",",sep=";",header=T, fill=TRUE)
paises= rbind(hdi.vh, hdi.h, hdi.m, hdi.l)
g7 = hdi.vh[c(5,6,8,14,17,20,26),]
g20= paises[c(2,5,6,8,14,15,17,20,16,40,49,57,69,71,79,91,108,118,135),]
BR = read.csv('EstadosBR.csv',dec=",",sep=";",header=T, fill=TRUE)

ID = menu(c('IDH muito alto', 'IDH alto', 'IDH médio', 'IDH baixo', 'G7', 'G20', 'Todos'), 
          title='IDH', graphics = TRUE)
if(ID==1) Dados = hdi.vh 
if(ID==2) Dados = hdi.h
if(ID==3) Dados = hdi.m
if(ID==4) Dados = hdi.l 
if(ID==5) Dados = g7  
if(ID==6) Dados= g20
if(ID==7) Dados = rbind(hdi.vh, hdi.h, hdi.m, hdi.l)

# ------------------------------ #
# Análise Exploratória dos Dados #
# ------------------------------ #
# Figura - Normalidade
op=par(mfrow=c(3,3))
   qqnorm(log(hdi.vh[,6]));qqline(log(hdi.vh[,6])); # IDH muito alto
   qqnorm(log(hdi.h[,6]));qqline(log(hdi.h[,6]));   # IDH alto
   qqnorm(log(hdi.m[,6]));qqline(log(hdi.m[,6]));   # IDH médio
   qqnorm(log(hdi.l[,6]));qqline(log(hdi.l[,6]));   # IDH baixo
   qqnorm(log(g20[,6]));qqline(log(g20[,6]));       # g20
   qqnorm(log(g7[,6]));qqline(log(g7[,6]));         # g7
   qqnorm(log(paises[,6]));qqline(log(paises[,6])); # Todos os países
   qqnorm(log(BR[,9]));qqline(log(BR[,9]));         # Estados brasileiros
par(op)

# FIGURA - TX encarceramento / IDH / GINI
lista = list('G7'=g7[,6], 'G20'=g20[,6], 'IDH-MuitoAlto'=hdi.vh[,6], 'IDH-Alto'=hdi.h[,6],
             'IDH-Médio'=hdi.m[,6], 'IDH-Baixo'=hdi.l[,6], 'Países'=Dados[,6], 'BR'=BR[,9])
lista1 = list('G7'=g7[,3], 'G20'=g20[,3], 'IDH-MuitoAlto'=hdi.vh[,3], 'IDH-Alto'=hdi.h[,3],
              'IDH-Médio'=hdi.m[,3], 'IDH-Baixo'=hdi.l[,3], 'Países'=Dados[,3], 'BR'=BR[,2])
lista2 = list('G7'=g7[,4], 'G20'=g20[,4], 'IDH-MuitoAlto'=hdi.vh[,4], 'IDH-Alto'=hdi.h[,4],
              'IDH-Médio'=hdi.m[,4], 'IDH-Baixo'=hdi.l[,4], 'Países'=Dados[,4], 'BR'=100*BR[,3])

op=par(mfrow=c(1,3))
  boxplot(lista,  ylab='Taxa de encarceramento por 100mil hab', main='2013-2014')
  text(x=1, y= 30, "Japão")
  text(x=1, y=680, "EUA")
  text(x=2, y=680, "EUA")
  text(x=2, y=450, "Rússia")
  text(x=3, y=680, "EUA")
  text(x=3, y=480, "Cuba")
  text(x=5, y=715, "São Cristóvão e Neves")
  text(x=4.5,y=690, "Seychelles")
  text(x=5, y=400, "El Salvador")
  text(x=6, y=450, "Ruanda")
  text(x=6, y=260, "Suazilândia")
  ##
  boxplot(lista1, ylab='IDH', main='2012-2014')
  ##
  boxplot(lista2, ylab='Índice GINI', main='Média 2008-2012')
par(op)


# Figura - Categorias de Gini - Países e BR
op=par(mfrow=c(2,3))
   ### Países
   Dados = na.omit(Dados)
   GINI.h = Dados[Dados[,4]>=quantile(Dados[,4],.75),]
   GINI.m = Dados[Dados[,4]>= quantile(Dados[,4],.25) & Dados[,4]<quantile(Dados[,4],.75),]
   GINI.l = Dados[Dados[,4]< quantile(Dados[,4],.25),]
   ###
   plot(Dados[,3], log(Dados[,6]), lwd=2, ylab='Log(Taxa de encarceramento)', 
        xlab='IDH',xlim=c(0,1),ylim=c(3,7))
   par(new=T)
   plot(g20[,3], log(g20[,6]), lwd=2, col='blue', ylab='', xlab='',xlim=c(0,1),ylim=c(3,7))
   legend("topleft", legend = c("Países","G20"), title='GINI', lty = 1:2, col=c('black','blue') )  
   ###
   plot(GINI.h[,3], log(GINI.h[,6]), lty=1, lwd=2, col='red', 
     ylab='Log(taxa de encarceramento)', xlab='IDH',xlim=c(0,1),ylim=c(3,7))
   abline(lm(log(GINI.h[,6])~GINI.h[,3]), lty=1, lwd=2, col='red')
   par(new=T)
   plot(GINI.m[,3], log(GINI.m[,6]), lty=2, lwd=2, col='gray',
     ylab='', xlab='',xlim=c(0,1),ylim=c(3,7))
   abline(lm(log(GINI.m[,6])~GINI.m[,3]), lty=2, col='gray', lwd=2)
   par(new=T)
   plot(GINI.l[,3], log(GINI.l[,6]), lty=3, lwd=2, col='green',
     ylab='', xlab='',xlim=c(0,1),ylim=c(3,7))
   abline(lm(log(GINI.l[,6])~GINI.l[,3]), lty=3, col='green', lwd=2)
  legend("topleft", legend = c("Maiores","Intermediários","Menores"),
       title='GINI', lty = 1:3, col=c('red','gray','green') )  
   ###
   boxplot(list('Menores' = log(GINI.l[,6]), 'Intermediários'=log(GINI.m[,6]),'Maiores'=log(GINI.h[,6])), 
           col=c('green','gray','red'), ylab='Log(tx encarceramento)', xlab='GINI',ylim=c(3,7))  
   ### ESTADOS BR
   GINI.h = BR[BR[,3]>=quantile(BR[,3],.75),]
   GINI.m = BR[BR[,3]>= quantile(BR[,3],.25) & BR[,3]<quantile(BR[,3],.75),]
   GINI.l = BR[BR[,3]< quantile(BR[,3],.25),]
   plot(BR[,2], log(BR[,9]), lwd=2, ylab='log(taxa de encarceramento por 100mil hab)', 
     xlab='IDH',xlim=c(0,1),ylim=c(3,7))
   abline(lm(log(BR[,9])~BR[,2]), lty=1, lwd=2, col='black')
   ###
   plot(GINI.h[,2], log(GINI.h[,9]), lty=1, lwd=2, col='red', 
     ylab='log(taxa de encarceramento)', xlab='IDH',
     xlim=c(0,1),ylim=c(3,7))
   abline(lm(log(GINI.h[,9])~GINI.h[,2]), lty=1, lwd=2, col='red')
   par(new=T)
   plot(GINI.m[,2], log(GINI.m[,9]), lty=2, lwd=2, col='gray',
     ylab='', xlab='',xlim=c(0,1),ylim=c(3,7))
   abline(lm(log(GINI.m[,9])~GINI.m[,2]), lty=2, col='gray', lwd=2)
   par(new=T)
   plot(GINI.l[,2], log(GINI.l[,9]), lty=3, lwd=2, col='green',
     ylab='', xlab='',xlim=c(0,1),ylim=c(3,7))
   abline(lm(log(GINI.l[,9])~GINI.l[,2]), lty=3, col='green', lwd=2)
   legend("topleft", legend = c("Maiores","Intermediários","Menores"),
       title='GINI', lty = 1:3, col=c('red','gray','green') )  
   ###
   boxplot(list('Menores' = log(GINI.l[,9]), 'Intermediários'=log(GINI.m[,9]), 'Maiores'=log(GINI.h[,9])), 
        col=c('green','gray','red'), ylab='Log(tx encarceramento por 100 mil hab)', xlab='GINI',ylim=c(3,7))  
par(op)


# Figura 3
OrdAbsoluto = Dados[order(Dados[,5],na.last=NA,decreasing=T),]
OrdRelativo = Dados[order(Dados[,6],na.last=NA,decreasing=T),]
op=par(mfrow=c(2,2))
  boxplot(OrdAbsoluto[,5], main ='Tx Absoluta')
  boxplot(OrdRelativo[,6], main ='TxRelativa')
  barplot(OrdAbsoluto[,5], names.arg= OrdAbsoluto[,2],main ='Tx Absoluta')
  barplot(OrdRelativo[,6], names.arg= OrdAbsoluto[,2],main ='Tx Relativa')
par(op)

# Figura 4
op=par(mfrow=c(3,1))
  sort.tx  = Dados[order(Dados[,6],decreasing=T,na.last=NA),]
  barplot(sort.tx[,6], names.arg= sort.tx[,2], main ='Tx Relativa')
  sort.idh = Dados[order(Dados[,3],decreasing=T,na.last=T),]
  barplot(sort.idh[,3], names.arg= sort.idh[,2], main ='IDH')
  sort.gini= Dados[order(Dados[,4],decreasing=T,na.last=T),] 
  barplot(sort.gini[,4], names.arg= sort.gini[,2], main ='GINI')
par(op)

