library("scatterplot3d")
library("geoR")
library("animation")

# -----------------------------#
# Presos no Brasil e no Mundo  #
#------------------------------#
# Leitura dos Dados
setwd("C:/Users/vinicius/Desktop/Encarceramento/Dados")
hdi.vh= read.csv('HDI-VeryHigh.csv',dec=",",sep=";",header=T, fill=TRUE)
hdi.h = read.csv('HDI-High.csv',dec=",",sep=";",header=T, fill=TRUE)
hdi.m = read.csv('HDI-Medium.csv',dec=",",sep=";",header=T, fill=TRUE)
hdi.l = read.csv('HDI-Low.csv',dec=",",sep=";",header=T, fill=TRUE)
paises= rbind(hdi.vh, hdi.h, hdi.m, hdi.l)
g7 = hdi.vh[c(5,6,8,14,17,20,26),]
g20= paises[c(2,5,6,8,14,15,17,20,16,40,49,57,69,71,79,91,108,118,135),]
BR = read.csv('EstadosBR.csv',dec=",",sep=";",header=T, fill=TRUE)
usa =read.csv('USAstates.csv',dec=",",sep=";",header=T, fill=TRUE)

ID = menu(c('IDH muito alto', 'IDH alto', 'IDH médio', 'IDH baixo', 'G7', 'G20', 'Países','Brasil'), 
          title='IDH', graphics = TRUE)

if(ID==1) Dados = hdi.vh 
if(ID==2) Dados = hdi.h
if(ID==3) Dados = hdi.m
if(ID==4) Dados = hdi.l 
if(ID==5) Dados = g7  
if(ID==6) Dados= g20
if(ID==7) Dados = rbind(hdi.vh, hdi.h, hdi.m, hdi.l)
if(ID==8) Dados = BR

# ---------------------------------------------------------------
# class(Dados); dim(Dados); names(Dados); str(Dados); head(Dados);
if(ID==8){p1=9; p2=2; p3=3;dv=1;}else{p1=6; p2=3; p3=4;dv=100}
Dados = na.omit(Dados[,c(p1,p2,p3)])
y   = as.numeric(Dados[,1])
IDH = as.numeric(Dados[,2])      # IDH
GINI= as.numeric(Dados[,3])/dv   # GINI

op=par(mfrow=c(2,2))
  hist(y, main='Tx de encarceramento por 100 mil hab')
  hist(log(y), main='Ln tx de encarceramento por 100 mil hab')
  qqnorm(y);qqline(y);
  qqnorm(log(y)); qqline(log(y));
par(op)

op=par(mfrow=c(2,2))
  plot(GINI, IDH, xlab='GINI', ylab='IDH', main='Pa?ses',xlim=c(0,1),ylim=c(0,1))
  scatterplot3d(GINI, IDH, log(y), xlab='GINI', ylab='IDH',xlim=c(0,1),ylim=c(0,1))
  plot(IDH, log(y), xlab='IDH',xlim=c(0,1),ylim=c(0,7))
  abline(lm(log(y)~IDH), col='red', lwd=2)
  plot(GINI, log(y), xlab='GINI',xlim=c(0,1),ylim=c(0,7))
  abline(lm(log(y)~GINI), col='red', lwd=2)
par(op)

oopt = ani.options(interval = 0.1, nmax = 1000)
for (i in 1:ani.options("nmax")) {
  scatterplot3d(GINI, IDH, log(y), xlab='GINI', ylab='IDH',xlim=c(0.1,0.8),ylim=c(0.5,1), angle = 2*i/10)
  ani.pause()  ## pause for a while ('interval')
}
ani.options(oopt)


# --------------------#
# Regressão Bayesiana #
# --------------------#
# y = X beta + e,  e ~ N(0, sig2)
Y = log(y)
X = cbind(IDH-mean(IDH), GINI-mean(GINI))
X = as.matrix(cbind(rep(1,nrow(X)), X))

# Análise para priori não informativa
post1 = MRLB1(Y,X)
# Análise conjugada
p = dim(X)
A=3; B=9; BBeta=rep(0,p[2]); Sigma=diag(rep(2, p[2]));
post2 = MRLB2(Y, X, A, B, BBeta, Sigma)

# Análise do ajuste
# Gráfico da preditiva
b.p = post2[c(p[2]),c(1,3,4,5)]
y.p.m  = X%*%b.p[,1]
y.p.1q = X%*%b.p[,2]
y.p.md = X%*%b.p[,3]
y.p.3q = X%*%b.p[,4]

op=par(mfrow=c(2,2))
   qqplot(Y, y.p, main='Observado versus ajustado')
   abline(line(Y, y.p), lwd=2)
   ##
   yl=c(2,8)
   plot(Y, main='Observado versus Intervalo de credibilidade', xlab='Dados', ylim=yl)
   par(new=T)
   plot(y.p.1q, axes=F, ylim=yl, type='l', xlab='')
   par(new=T)
   plot(y.p.md, axes=F, ylim=yl, type='l', xlab='')
   par(new=T)
   plot(y.p.3q, axes=F, ylim=yl, type='l', xlab='')
   ##
   hist((log(y)-y.p.m), main='Res?duos', xlab='Dados')
par(op)


# -------------------#
# Regressão Clássica #
# -------------------#
x1=(IDH-mean(IDH); post1=lm(log(y)~x1);
summary(post1)
post2= lm(log(y)~GINI)
summary(post2)
x1= IDH-mean(IDH); x2= GINI-mean(GINI); post3 =lm(log(y)~x1+GINI);
summary(post3)
post4 =lm(log(y)~IDH+GINI+IDH*GINI)
summary(post4)
IDH2 = IDH*IDH
post5=lm(log(y)~IDH+IDH2)
summary(post5)
x=seq(0, 1, by=.01)
y.h = post5$coef[1] + post5$coef[2]*x +post5$coef[3]*x^2
plot(IDH, log(y), xlab='IDH',xlim=c(0.3,1), ylim=c(0,7))
par(new=T)
plot(x,y.h, xlim=c(0.3,1), ylim=c(0,7),type='l',lwd=3, col='red')
post6=lm(log(y)~IDH+IDH2+GINI)
summary(post6)


#----------------------------------#
# Análise Baysiana - WinBUGS to R  #
#----------------------------------#
library("R2WinBUGS")
BugsDIR = "C:/Users/Vinicius/Desktop/WinBUGS14"
#BugsDIR = "C:/Program Files/WinBugs14"
Dir = "C:/Users/Vinicius/Desktop/Encarceramento"


# Modelo 00 - Fully Pooled with GINI as covariate
Modelo  = "Modelo00.txt"
data  <- list('y'=log(y),'x1'=IDH,'x2'=GINI,'n'=length(IDH))
inits <- list(list(alpha=0,beta1=0,beta2=0,is.2=.01),
              list(alpha=3,beta1=3,beta2=3,is.2=.10))   
parameters = c("alpha","beta1","beta2","s.2")


# Modelo 0 - Fully Pooled
Modelo  = "Modelo0.txt"
data  <- list('y'=log(y),'x1'=IDH,'n'=length(IDH))
inits <- list(list(alpha=0,beta1=0,is.2=.01),
              list(alpha=3,beta1=3,is.2=.10))   
parameters = c("alpha","beta1","s.2")

#----------------
# Modelo 1, 1b, 2, 2b,  3 e 3b
GINI.h = Dados[Dados[,3]>=quantile(Dados[,3],.75),]
GINI.m = Dados[Dados[,3]>= quantile(Dados[,3],.25) & Dados[,3]<quantile(Dados[,3],.75),]
GINI.l = Dados[Dados[,3]< quantile(Dados[,3],.25),]

y1 = log(GINI.h[,1]); y2 = log(GINI.m[,1]); y3 = log(GINI.l[,1]);
x1 = GINI.h[,2]; x2 = GINI.m[,2]; x3 = GINI.l[,2];
n1=length(y1); n2=length(y2); n3=length(y3);

# Modelo 1 - Hierárquia no intercepto (Random Intercept)
Modelo  = "Modelo1.txt"
data <- list ('y1'=y1,'y2'=y2,'y3'=y3,'x1'=x1,'x2'=x2,'x3'=x3,'n1'=n1,'n2'=n2,'n3'=n3)
inits <- list(list(alpha=0,beta=0,alpha1=0,alpha2=0,alpha3=0,is2.alpha1=1,is2.alpha2=1,is2.alpha3=1,is2=.01),
              list(alpha=3,beta=3,alpha1=3,alpha2=3,alpha3=3,is2.alpha1=.1,is2.alpha2=.1,is2.alpha3=.1,is2=.1))   
parameters = c("alpha","beta","alpha1","alpha2","alpha3","s2.alpha1","s2.alpha2","s2.alpha3","s2")

# Modelo 1b - Hierarquia no intercepto (Random Intercept)
Modelo  = "Modelo1b.txt"
data <- list ('y1'=y1,'y2'=y2,'y3'=y3,'x1'=x1,'x2'=x2,'x3'=x3,'n1'=n1,'n2'=n2,'n3'=n3)
inits <- list(list(alpha=0,beta=0,alpha1=0,alpha2=0,alpha3=0,is2.alpha=1,is2=.01),
              list(alpha=3,beta=3,alpha1=3,alpha2=3,alpha3=3,is2.alpha=.1,is2=.1))   
parameters = c("alpha","beta","alpha1","alpha2","alpha3","s2.alpha","s2")


# Modelo 2 - Hierarquia na inclinação (Random Slope)
Modelo  = "Modelo2.txt"
data <- list ('y1'=y1,'y2'=y2,'y3'=y3,'x1'=x1,'x2'=x2,'x3'=x3,'n1'=n1,'n2'=n2,'n3'=n3)
inits <- list(list(alpha=0,beta=0,beta1=0,beta2=0,beta3=0,is2.beta1=1,is2.beta2=1,is2.beta3=1,is2=.01),
              list(alpha=3,beta=3,beta1=3,beta2=3,beta3=3,is2.beta1=.1,is2.beta2=.1,is2.beta3=.1,is2=.1))   
parameters = c("alpha","beta","beta1","beta2","beta3","s2.beta1","s2.beta2","s2.beta3","s2")

# Modelo 2b - Hierarquia na inclinação (Random Slope)
Modelo  = "Modelo2b.txt"
data <- list ('y1'=y1,'y2'=y2,'y3'=y3,'x1'=x1,'x2'=x2,'x3'=x3,'n1'=n1,'n2'=n2,'n3'=n3)
inits <- list(list(alpha=0,beta=0,beta1=0,beta2=0,beta3=0,is2.beta=1,is2=.01),
              list(alpha=3,beta=3,beta1=3,beta2=3,beta3=3,is2.beta=.1,is2=.1))   
parameters = c("alpha","beta","beta1","beta2","beta3","s2.beta","s2")


# Modelo 3 - Hierarquia no intercepto e na inclinação (Random Intercept and Slope)
Modelo  = "Modelo3.txt"
data <- list ('y1'=y1,'y2'=y2,'y3'=y3,'x1'=x1,'x2'=x2,'x3'=x3,'n1'=n1,'n2'=n2,'n3'=n3)
inits <- list(list(alpha=0,beta=0,alpha1=0,alpha2=0,alpha3=0,beta1=0,beta2=0,beta3=0,
                   is2.alpha1=1, is2.alpha2=1, is2.alpha3=1, is2.beta1=1, is2.beta2=1, is2.beta3=1, is2=.01),
              list(alpha=3,beta=3,alpha1=3,alpha2=3,alpha3=3,beta1=3,beta2=3,beta3=3,
                   is2.alpha1=.1, is2.alpha2=.1, is2.alpha3=.1, is2.beta1=.1, is2.beta2=.1,is2.beta3=.1, is2=.01))   
parameters = c("alpha","beta","alpha1","alpha2","alpha3","beta1","beta2","beta3",
               "s2.alpha1","s2.alpha2","s2.alpha3","s2.beta1","s2.beta2","s2.beta3","s2")

# Modelo 3b - Hierarquia no intercepto e na inclinação (Random Intercept and Slope)
Modelo  = "Modelo3b.txt"
data <- list ('y1'=y1,'y2'=y2,'y3'=y3,'x1'=x1,'x2'=x2,'x3'=x3,'n1'=n1,'n2'=n2,'n3'=n3)
inits <- list(list(alpha=0,beta=0,alpha1=0,alpha2=0,alpha3=0,beta1=0,beta2=0,beta3=0,
                   is2.alpha=1, is2.beta=1, is2=.01),
              list(alpha=3,beta=3,alpha1=3,alpha2=3,alpha3=3,beta1=3,beta2=3,beta3=3,
                   is2.alpha=.1, is2.beta=.1, is2=.01))   
parameters = c("alpha","beta","alpha1","alpha2","alpha3","beta1","beta2","beta3", "s2.alpha","s2.beta","s2")


# Modelo 4 - Fully Unpooled
Modelo  = "Modelo4.txt"
data <- list ('y1'=y1,'y2'=y2,'y3'=y3,'x1'=x1,'x2'=x2,'x3'=x3,'n1'=n1,'n2'=n2,'n3'=n3)
inits <- list(list(alpha1=0,alpha2=0,alpha3=0,beta1=0,beta2=0,beta3=0,is2.1=1, is2.2=1, is2.3=1),
              list(alpha1=3,alpha2=3,alpha3=3,beta1=3,beta2=3,beta3=3,is2.1=.1,is2.2=.1,is2.3=.1))   
parameters = c("alpha1","alpha2","alpha3","beta1","beta2","beta3","s2.1","s2.2","s2.3")


# Rodando MCMC pelo R ---------------------------------------------------
theta.post <- bugs(data, inits, parameters, model.file=Modelo, n.chains=2, 
                   n.iter=300000, n.burnin=100000, n.thin=100,
                   digits=5, codaPkg=FALSE, DIC = TRUE, bugs.directory=BugsDIR,
                   program=c("WinBUGS", "OpenBUGS", "winbugs", "openbugs"),
                   working.directory=Dir, clearWD=FALSE, debug=TRUE)
theta.post
plot(theta.post)

# Países
ll = c(266.58, 276.46, 275.62, 280.49, 301.75)
np = c( 11,     7,       7,      4,      3)
aic = ll+2*np
bic = ll+np*log(148)

# Estados
ll = c(23.74, 25.4, 34.25, 24.69, 31.11)
np = c( 11,     7,       7,      4,      3)
aic = ll+2*np
bic = ll+np*log(27)

# Gráficos para apresentação - ABCP 2016

ID = menu(c('Países', 'Estados', graphics = TRUE)
if(ID == 1){          
fl = "C:/Users/Vinicius/Desktop/Encarceramento/OutputMundo/M3b"
}else{
fl = "C:/Users/Vinicius/Desktop/Encarceramento/OutputBRpriorisvagas/M3b"
}
setwd(fl)
thid = read.table('codaIndex.txt')
th00 = read.table('coda1.txt')
th01 = read.table('coda2.txt')

a1=rbind(th00[2001:4000,2],th01[2001:4000,2])
a2=rbind(th01[6001:8000,2],th01[6001:8000,2])
xl=c(mean(c(a1,a2))-3*sd(c(a1,a2)),mean(c(a1,a2))+3*sd(c(a1,a2))); 
yl=c(0,5);
plot(density(a1),xlim=xl, ylim=yl, col='red',lwd=3,lty=1, 
     main='Intercepto IDH x log TX',
     xlab ='intercepto', ylab='densidade')
lines(density(a2), col='blue',lwd=3,lty=2)
text(x=mean(a1),y=4.5,labels='GINI alto')
text(x=mean(a2),y=4.5,labels='GINI baixo')


b1=rbind(th00[10001:12000,2],th01[10001:12000,2])
b2=rbind(th01[14001:16000,2],th01[14001:16000,2])
xl=c(mean(c(b1,b2))-3*sd(c(b1,b2)),mean(c(b1,b2))+3*sd(c(b1,b2))); 
yl=c(0,0.5);
plot(density(b1),xlim=xl, ylim=yl, col='red',lwd=3,lty=1, 
     main='Inclinação IDH x log TX',
     xlab ='taxa', ylab='densidade')
lines(density(b2), col='blue',lwd=3,lty=2)
text(x=mean(b1)+.5,y=0.3,labels='GINI alto')
text(x=mean(b2)-.5,y=0.3,labels='GINI baixo')


# -----------------#
# Análise Espacial #
# -----------------#
if(ID==8){p1=9; p2=2; p3=3;dv=1;}else{p1=6; p2=3; p3=4;dv=100}
y   = as.numeric(Dados[,p1])
IDH = as.numeric(Dados[,p2])     # IDH
GINI= as.numeric(Dados[,p3])/dv  # GINI

geo = as.geodata(cbind(log(y),IDH,GINI))
summary(geo)  
plot(geo)
geo.b = krige.bayes(geo)
summary(geo.b)



