library(ggplot2)
# modélisation évolution de Perf valide -> para ---------------------------

ds <- read.csv2("data/exemple_rupture.csv")

ds <- ds[which(ds$Cat=="Mandy Francois-Elie"| ds$Cat=="Mandy Francois-Para"),]
# ds <- ds[which(ds$Cat=="Mandy Francois-Para"),]
# ds <- ds[which(ds$Cat=="Record Valide"),]

d <- ggplot(ds, aes(x=Age, y=Perf)) 
d + stat_summary(fun.data = "mean_cl_boot", colour = "red") + ylab("Perf") + xlab("Age")

## 1/ modele lineaire
model=lm(Perf~Age,data=ds)
summary(model)
plot(density(model$residuals))
qqnorm(model$residuals)
qqline(model$residuals)
shapiro.test(model$residuals)

# representation graphique
plot(ds$Age,ds$Perf, pch=16, col="black")
predictions<-predict(model,data.frame(Age=ds$Age)) # calcul des valeurs predites par le modele
lines(ds$Age,predictions, col="red", lwd=2)


## 2/ modele polynomial
modelpoly=lm(Perf~Age+I(Age^2),data=ds)
summary(modelpoly)

# representation graphique
plot(ds$Age,ds$Perf, pch=16, col="black")
predictions<-predict(modelpoly,data.frame(Age=ds$Age)) # calcul des valeurs predites par le modele
lines(ds$Age,predictions, col="red", lwd=2)

#########################################
#########################################
#########################################
#########################################
MMC=function(age,Perf,methode,nbpara,precision=10,borne=-Inf, initial=runif(nbpara)){
  
  SQ_methode=function(p){ #Cette fonction permet de calculer la somme carre des residus            #theta sont les parametre qu 
    sum((Perf-methode(age,p))^2) }
  
  o=rep(10^6,precision) #o permet de stocker la fonction objectif qu'on voudra le plus bas
  r=rep(0,precision) #r permet de sotcker le R?
  para=rep(10^6,precision*(nbpara+1)) #pour stocker tout les parametres
  for (i in seq(nbpara+1,precision*(nbpara+1),nbpara+1)){ 
    sol=nlminb(start=initial + rnorm(nbpara,0,sqrt(abs(initial))),SQ_methode) 
    res= nlminb(start = sol$par,SQ_methode,lower=borne) 
    para[i:(i+nbpara-1)]=res$par #ici"
    o[i/(nbpara+1)]=res$objective}
  compt=which.min(o) 
  parametre=rep(0,nbpara)
  parametre=para[((nbpara+1)*compt):(((nbpara+1)*compt)+(nbpara-1))] #on stock les meilleurs parametres
  fit=methode(age,parametre) #regression 
  R2=sum((fit-mean(Perf))^2)/sum((Perf-mean(Perf))^2) #R? 
  peak=age[methode(sort(age),parametre)==max(methode(sort(age),parametre))]
  return(list(parametre=parametre,objectif=min(o),R2=R2,peak=peak))}
#########################################
#########################################
#########################################
#########################################

## 3/ modele de Moore

# ecriture de la fonction
f=function(x,p){
  p[1]*(1-exp(p[2]*x))+p[3]*(1-exp(p[4]*x))
}

# estimation des parametres via la fonction MMC
# MMC( x, y, fonction a appliquer aux donnees, nb de parametres de la fonction, nb d'iterations)

##########################################################
##dans notre cas,                                       ##
##                x = ds$Age                    ##
##                y = ds$Perf                       ##
##                fonction a appliquer aux donnees = f  ##
##                nb de parametres de la fonction = 4   ##
##                nb d'iterations = mettons 500         ##
##########################################################

modelmoore=MMC(ds$Age,ds$Perf,f,4,precision=500)
summary(modelmoore)


# representation graphique
plot(ds$Age,ds$Perf, pch=16, col="black")
predictions<-f(ds$Age,modelmoore$parametre) # calcul des valeurs predites par le modele
lines(ds$Age,predictions, col="red", lwd=2)



################ representation graphique de tous les modeles
plot(ds$Age,ds$Perf, pch=16, xlab="Age", ylab="Perf", main="800m feminin")
lines(ds$Age,predict(model,data.frame(Age=ds$Age)),col="red", lwd=2)
lines(ds$Age,predict(modelpoly,data.frame(Age=ds$Age)),col="blue", lwd=2)
lines(ds$Age,f(ds$Age,modelmoore$parametre),col="green", lwd=2)
legend(x=22, y=6.4, legend=c("Lineaire", "Polynomiale", "Moore"),
       col=c("red", "blue", "green"), pch=16, cex=0.7)



## 4/ modele IMAP1

# ecriture de la fonction
IMAP1=function(t,p){
  p[1]*exp((p[2]/p[3])*(exp(-p[3]*t)))*(1-exp(p[4]*(t-p[5])))}


# estimation des parametres via la fonction MMC
modelimap=MMC(ds$Age,ds$Perf,IMAP1,5,precision=500)
summary(modelimap)


# representation graphique
plot(ds$Age,ds$Perf, pch=16, col="black")
predictions<-IMAP1(ds$Age,modelimap$parametre) # calcul des valeurs predites par le modele
lines(ds$Age,predictions, col="red", lwd=2)


################ representation graphique de tous les modeles
plot(ds$Age,ds$Perf, pch=16, xlab="Age", ylab="Perf", main="")
lines(ds$Age,predict(model,data.frame(Age=ds$Age)),col="red", lwd=2)
lines(ds$Age,predict(modelpoly,data.frame(Age=ds$Age)),col="blue", lwd=2)
lines(ds$Age,f(ds$Age,modelmoore$parametre),col="green", lwd=2)
lines(seq(min(ds$Age), max(ds$Age),.1),
      f(seq(min(ds$Age), max(ds$Age),.1),modelmoore$parametre),col="green", lwd=2)
lines(ds$Age,IMAP1(ds$Age,modelimap$parametre),col="gold", lwd=2)
lines(seq(min(ds$Age), max(ds$Age),.1),
      IMAP1(seq(min(ds$Age), max(ds$Age),.1),modelimap$parametre),col="gold", lwd=2)
legend(x=40, y=7.5, legend=c("Lineaire", "Polynomiale", "Moore", "IMAP1"),
       col=c("red", "blue", "green", "gold"), pch=16, cex=0.7)






