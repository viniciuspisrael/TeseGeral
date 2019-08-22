##################################################
## PGM - Seminário Internacional sobre Prisões  ##
## Dados de Encarceramento no BR                ##
## Última atualização: 28/12/2018               ##
##################################################
rm(list=ls(all=TRUE)) #Limpar memória do R 
library(foreign); library(descr); library(car);
library(ggplot2); library(arm); 

# -------------------------
# Boxplots encarceramento
# Leitura dos Dados
setwd('C:/Users/Vinicius/Desktop/TextoTese/Cap04-Macro/PGMs/Dados')
hdi.vh= read.csv('HDI-VeryHigh.csv',dec=",",sep=";",header=T, fill=TRUE)
hdi.h = read.csv('HDI-High.csv',dec=",",sep=";",header=T, fill=TRUE)
hdi.m = read.csv('HDI-Medium.csv',dec=",",sep=";",header=T, fill=TRUE)
hdi.l = read.csv('HDI-Low.csv',dec=",",sep=";",header=T, fill=TRUE)
paises= rbind(hdi.vh, hdi.h, hdi.m, hdi.l)
g7 = hdi.vh[c(5,6,8,14,17,20,26),]
g20= paises[c(2,5,6,8,14,15,17,20,16,40,49,57,69,71,79,91,108,118,135),]
Dados = rbind(hdi.vh, hdi.h, hdi.m, hdi.l)

dc = 'C:/Users/Vinicius/Desktop/TextoTese/Cap04-Macro/PGM-SemPrisões/BD/EstadosBR.csv'
BR = read.csv(dc,dec=",",sep=";",header=T, fill=TRUE)
# América Latina
alcode = c('Argentina', 'Bolivia (Plurinational State of)',
           'Brazil','Chile','Colombia','Costa Rica','Cuba',
           'Ecuador','El Salvador','Guatemala','Haiti',
           'Honduras','Mexico','Nicaragua','Panama', 'Paraguay',
           'Peru','Dominican Republic', 'Uruguay',
           'Venezuela (Bolivarian Republic of)')
AL = paises[paises$Country %in% alcode,]

# FIGURA - TX encarceramento / IDH / GINI
lista = list('G7'=g7[,6], 'G20'=g20[,6], 'IDH-MuitoAlto'=hdi.vh[,6], 'IDH-Alto'=hdi.h[,6],
             'IDH-Médio'=hdi.m[,6], 'IDH-Baixo'=hdi.l[,6],'Am. Lat.'= AL[,6], 'Países'=Dados[,6], 'BR'=BR[,11])

boxplot(lista, ylab='Taxa de encarceramento por 100mil hab', 
        main='2013-2014', cex=1.5)
text(x=1, y= 30, "Japão",col='red',cex=1.2)
text(x=1, y=680, "EUA",col='red',cex=1.2)
text(x=2, y=680, "EUA",col='red',cex=1.2)
text(x=2, y=450, "Rússia",col='red',cex=1.2)
text(x=3, y=680, "EUA",col='red',cex=1.2)
text(x=3, y=480, "Cuba",col='red',cex=1.2)
text(x=5, y=715, "São Cristóvão e Neves",col='red',cex=1.2)
text(x=4.5,y=690, "Seychelles",col='red',cex=1.2)
text(x=5, y=400, "El Salvador",col='red',cex=1.2)
text(x=6, y=450, "Ruanda",col='red',cex=1.2)
text(x=6, y=260, "Suazilândia",col='red',cex=1.2)
text(x=7, y=480, "Cuba",col='red',cex=1.2)
text(x=9, y=540, "MS",col='red',cex=1.2)



# ----------------------------------------
# Banco de dados - TCC Fernanda Mencarelli
# Dados sobre encarceramento no Brasil - 2014 
bd = read.csv2("EstadosBR.csv", header = T) 
summary(bd)
bd$UF = c('AC', 'AL', 'AP', 'AM', 'BH', 'CE', 'DF', 'ES', 'GO', 
              'MA', 'MT', 'MS', 'MG', 'PA', 'PB', 'BR', 'PE', 'PI',
              'RJ', 'RN', 'RS', 'RO', 'RR', 'SC', 'SP', 'SE', 'TO')

# Gráficos
ord = order(bd$TAXA.2014, decreasing = T)
aux = bd[ord,]
aux$UF = factor(aux$UF, levels= aux$UF)
# Barplot
g = ggplot(aux, aes(UF, TAXA.2014, fill=REGIAO))
g + geom_bar(stat="identity")+
geom_text(aes(label=TAXA.2014), vjust=-0.3, size=3.5)+
theme_minimal()

# Boxplot
g + geom_boxplot(aes(REGIAO, TAXA.2014))

# Histograma
g = ggplot(bd, aes(TAXA.2014))
g + geom_histogram(breaks=seq(50, 600, by=50)) +
  xlab('Taxa.2014')+ylab("Frequência")

# Dados 2004 -------------------------------------
# http://www.ibge.gov.br/apps/populacao/projecao/
bd$Pop2004 = c(646548, 3049431, 577786, 3170740, 14075570, 8042368, 2278824,
                3397224, 5534201, 6135099, 2763537, 2273874, 19037702, 6904392,
                3612078, 10128262, 8484308, 3007333, 15374696, 3062933, 10628806,
                1515151, 392392, 5801932, 39824526, 1950985, 1285028)
summary(bd$Pop2004)

# Site do Ministério da Justiça - Pop Prisional referente a dez 2014.
# http://www.justica.gov.br/seus-direitos/politica-penal/transparencia-institucional/estatisticas-prisional/relatorios-estatisticos-sinteticos
bd$Presos2004 = c(2548, 2541, 1574, 3012, 7144, 10116, 7299, 5221, 6226,
                   2964, 7891, 6289, 7221, 6076, 6118, 10817, 15817, 1785,
                   23054, 2243, 22621, 4124, 972,  9570, 120601, 2142, 933)
bd$TAXA.2004 = (bd$Presos2004/bd$Pop2004)*100000
summary(bd$TAXA.2004)

# Gráfico de dispersão
g = ggplot(bd, aes(x=TAXA.2004, y=TAXA.2014))
g + geom_point(aes(col=REGIAO), size=4) + 
geom_text(data=bd, mapping=aes(x=TAXA.2004, y=TAXA.2014+20, label=UF), size=4, vjust=3, hjust=0.5) +
geom_abline() +xlab('Taxa2004') + ylab('Taxa2014')

# Taxa de aumento 2014 vs 2004
bd$TAXA.1404 = bd$TAXA.2014/bd$TAXA.2004
ord = order(bd$TAXA.1404, decreasing = T)
aux = bd[ord,]
aux$UF = factor(aux$UF, levels= aux$UF)

# Barplot
g = ggplot(aux, aes(UF, TAXA.1404, fill=REGIAO))
g + geom_bar(stat="identity") + ylab('Taxa de Aumento') +
  theme_minimal()

# Boxplot
g + geom_boxplot(aes(REGIAO, TAXA.1404))+
  xlab('Região')+ylab('Taxa de Aumento')+
  ylim(c(0,8))

# Histograma
g = ggplot(bd, aes(TAXA.1404))
g + geom_histogram(breaks=seq(0, 8, by=0.25)) +
  xlab('Taxa de Aumento')+ylab("Frequência")





