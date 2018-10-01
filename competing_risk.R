vignette("Tutorial", package="mstate")
library(mstate)
library("colorspace")
data(aidssi)
si <- aidssi # Just a shorter name
table(si$status)
tmat <- trans.comprisk(2, names = c("event-free", "AIDS", "SI"))
tmat

?msfit
si$stat1 <- as.numeric(si$status == 1)
si$stat2 <- as.numeric(si$status == 2)
dim(aidssi)[1]
#add a continious variable
#ccr5 category variable with 2 levels one dummy variable ww as reference
#radom continuous variable
si$radom<-rnorm(dim(aidssi)[1],0,1)
silong <- msprep(time = c(NA, "time", "time"), 
                   status = c(NA, "stat1", "stat2"), data = si, keep = c("ccr5","radom"), trans = tmat)
events(silong)
covs<-c("ccr5","radom")
silong <- expand.covs(silong, covs)
c1 <- coxph(Surv(time, status) ~ 1, data = silong, subset = (trans ==1), method = "breslow")
#a non-parametric model
c0<-coxph(Surv(time, status) ~ strata(trans), data = silong,method = "breslow")
msf0 <- msfit(object = c0, vartype = "greenwood", trans = tmat)
plot(msf0, las = 1, lty = rep(1:2, c(8, 4)),
      xlab = "Years since event-free")
#stacked plot of probability of transition
pt0<-probtrans(msf0,predt=0,method="greenwood")
summary(pt0,from=1)
plot(pt0, ord=c(1,2,3), lwd=2,
     xlab = "Years since event-free",
     ylab="Prediction probabilities"
     )
#stacked plot with heat map
statecols<-heat_hcl(3,c=c(80,30),l=c(30,90),power=c(1/5,2))[c(1,2,3)]
ord<-c(1,2,3)
plot(pt0,ord=ord,xlab = "Years since transplantation",
      las = 1, ylim=c(-0.1,1.1),type = "filled", col = statecols[ord])

#from 1?? it seems computing risk can only be from 1 and state 2 and 3 are absorbing state
#hard coding stacked plot
pt01<-pt0[[1]]
head(pt01)
tail(pt01)
plot(pt01$time,pt01$pstate1,lwd=1,
     xlab = "Years since event-free",
     ylab="Prediction probabilities",
     col="red"
     )
lines(pt01$time,pt01$pstate1+pt01$pstate2,lwd=1,col="blue")
lines(pt01$time,pt01$pstate1+pt01$pstate2+pt01$pstate3,lwd=1,col="green")
legend("topleft",c("event-free","AID","SI"),lwd = 2, col = c("red","blue","green"), bty = "n")

#legend=c("SI","AIDS")
#transition 1 event-free to AID 2 event-free to SI     

#check to see if we need trasition specific covariate
coxph(Surv(time, status) ~ ccr5*factor(trans)+strata(trans), data = silong,method = "breslow")
#significant interaction
coxph(Surv(time, status) ~ ccr5WM.1+ccr5WM.2+strata(trans), data = silong,method = "breslow")

coxph(Surv(time, status) ~ radom*factor(trans)+strata(trans), data = silong,method = "breslow")
#random covariate not significant, interaction not significant
coxph(Surv(time, status) ~ radom.1+radom.2+strata(trans), data = silong,method = "breslow")

#full model
cfull<-coxph(Surv(time, status) ~ ccr5WM.1+ccr5WM.2+radom.1+radom.2+strata(trans), data = silong,method = "breslow")

silong[silong$id==1,]

#prediction for a observation with WW feature
WW<-data.frame(ccr5WM.1=c(0,0),ccr5WM.2=c(0,0),trans=c(1,2),
               strata=c(1,2),radom.1=c(0.5,0),radom.2=c(0,0.5))
msf.WW <- msfit(cfull, WW, trans = tmat)
pt.WW <- probtrans(msf.WW, 0)[[1]]
#prediction for a observation with WM feature
WM<-data.frame(ccr5WM.1=c(1,0),ccr5WM.2=c(0,1),trans=c(1,2),
               strata=c(1,2),radom.1=c(0.5,0),radom.2=c(0,0.5))
msf.WM <- msfit(cfull, WM, trans = tmat)
pt.WM <- probtrans(msf.WM, 0)[[1]]

idx1 <- (pt.WW$time < 13)
idx2 <- (pt.WM$time < 13)
plot(c(0, pt.WW$time[idx1]), c(0, pt.WW$pstate2[idx1]), type = "s",
        ylim = c(0, 0.5), xlab = "Years from HIV infection", ylab = "Probability",
        lwd = 2,col="red")
lines(c(0, pt.WM$time[idx2]), c(0, pt.WM$pstate2[idx2]), type = "s",
         lwd = 2, col = "blue")
title(main = "AIDS")
text(9.2, 0.345, "WW", adj = 0, cex = 0.75)
text(9.2, 0.125, "WM", adj = 0, cex = 0.75)

plot(c(0, pt.WW$time[idx1]), c(0, pt.WW$pstate3[idx1]), type = "s",
       ylim = c(0, 0.5), xlab = "Years from HIV infection", ylab = "Probability",
        lwd = 2,col="red")
lines(c(0, pt.WM$time[idx2]), c(0, pt.WM$pstate3[idx2]), type = "s",
         lwd = 2, col = "blue")
title(main = "SI appearance")
text(7.5, 0.31, "WW", adj = 0, cex = 0.75)
text(7.5, 0.245, "WM", adj = 0, cex = 0.75)

#another way to compare there two observations
ptWW<-probtrans(msf.WW,predt=0)
ptWM<-probtrans(msf.WM,predt=0)
summary(ptWW,from=1)
statecols<-heat_hcl(3,c=c(80,30),l=c(30,90),power=c(1/5,2))[c(1,2,3)]
ord<-c(1,2,3)
par(mfrow = c(1,2))
plot(ptWW,ord=ord,xlab = "Years since transplantation",
     las = 1, ylim=c(-0.1,1.1),type = "filled", col = statecols[ord],main="WW")


plot(ptWM,ord=ord,xlab = "Years since transplantation",
     las = 1, ylim=c(-0.1,1.1),type = "filled", col = statecols[ord],main="WM")