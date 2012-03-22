library(effects)
library(ggplot2)
library(car)
library(arm)

setwd("c:/documents and settings/dbarron/my documents/financial cost of caring")
load(.RData)

dta2 <- read.table("dta2.txt")
sect.lvs <- levels(dta2$jbsect)

PrivateSect <- factor(ifelse(dta2$jbsect==sect.lvs[1],"Private",
                      ifelse(dta2$jbsect==sect.lvs[6]|dta2$jbsect==sect.lvs[7]|dta2$jbsect==sect.lvs[8],NA,"Public")))
table(dta2$jbsect,PrivateSect,useNA="a")

dta2$PrivateSect <- PrivateSect

PS.sex <- factor(ifelse(PrivateSect=="Public"& dta2$sex == "male", "Public, male",
              ifelse(PrivateSect=="Public"& dta2$sex == "female", "Public, female",
                     ifelse(PrivateSect=="Private"& dta2$sex == "male", "Private, male",
                            ifelse(PrivateSect=="Private"& dta2$sex == "female", "Private, female",NA)))))
dta2$PS.sex <- PS.sex

ss <- dta3$hrwage > 0
seg <- dta3$jbseg
lv.seg <- levels(seg)

rm(PS.sex,PrivateSect)

quals.lv <- levels(dta2$qfachi)
quals <- ifelse(dta2$qfachi==quals.lv[1]|dta2$qfachi==quals.lv[2]|dta2$qfachi==quals.lv[3],"Tertiary",
                 ifelse(dta2$qfachi==quals.lv[4]|dta2$qfachi==quals.lv[5]|dta2$qfachi==quals.lv[6],"Secondary",
                   ifelse(dta2$qfachi==quals.lv[7],"None",NA)))
quals <- relevel(factor(quals),"None")
dta2$quals <- quals

# Proportion female by SOC
tab.f <- xtabs(~jbsoc+sex,dta2)
prop.f <- tab.f[,1]/rowSums(tab.f)
prop.f.df <- data.frame(jbsoc=as.numeric(rownames(tab.f)),prop.female=prop.f)

tab.tu <- xtabs(~jbsoc + orgmb,dta2, exclude=NA, na.action=na.pass)
tab.tu[,2] <- NA
tab.tu[,4] <- NA
tab.tu[,5] <- NA

prop.tu <- ifelse(tab.tu[,1]==0,0,tab.tu[,1]/rowSums(tab.tu,na.rm=TRUE))
prop.tu.df <- data.frame(jbsoc=as.numeric(rownames(tab.tu)),prop.tu=prop.tu)

dta.tmp <- merge(prop.f.df,prop.tu.df,by="jbsoc",all=TRUE)

dta3 <- merge(dta2,dta.tmp,by="jbsoc",all.x=TRUE)
write.table(dta3,"dta3.txt")

ix <- (PrivateSect=="Public"|PrivateSect=="Private")&!is.na(PS.sex)
ix <- (PrivateSect=="Public"|PrivateSect=="Private")&!is.na(PrivateSect)

g <- ggplot(data=dta2[ss&ix,],aes(x=lhrwage)) + opts(axis.text.x=theme_text(size=15),axis.text.y=theme_text(size=15),axis.title.x=theme_text(size=15,vjust=0),axis.title.y=theme_text(size=15,vjust=0.25,angle=90))
g  + geom_density(aes(colour=PrivateSect[,drop=TRUE])) + opts(legend.title=theme_blank()) + labs(x="Log hourly wage",y="Density") + xlim(c(0,5)) + geom_hline(yintercept=0,colour="white")
g  + geom_histogram(aes(y=..density..,fill=PrivateSect[,drop=TRUE]),binwidth=.02) + opts(legend.title=theme_blank()) + labs(x="Log hourly wage",y="Density") + xlim(c(0,5)) + geom_hline(yintercept=0,colour="white") 

xtabs(~PrivateSect + sex, dta2, subset=ss)

library(descr)
with(dta2[ss,],CrossTable(PrivateSect,sex))
with(dta2[ss,],CrossTable(PrivateSect,quals))

# Basic premium
display(lm(lhrwage ~ PrivateSect,data=dta2,subset=ss&wave==17))

display(basic <- lmer(real.lhrwage ~ PrivateSect + (1|pid), data=dta2, subset=ss))
base.b <- fixef(basic)
(exp(sum(base.b)) - exp(base.b[1]))/exp(base.b[1])
 base.re <- lmer(lhrwage ~ jbseg + factor(wave) + (1|pid), data=dta2, subset=ss)

display(base.re)
seg.re1 <- lmer(lhrwage ~ PrivateSect * factor(wave) + (1|pid) , data=dta2, subset=ss)

display(seg.re1)
b0 <- fixef(seg.re1)
b1 <- fixef(seg.re1)
prvt <- exp(b1[1] + c(0,b1[3:19]))
pblc <- exp(b1[1] + b1[2] + (c(0,b1[3:19])+c(0,b1[20:36])))
pc.diff <- (pblc - prvt)/pblc
year <- 1:18 + 1990

plot(year,pc.diff, type="l")
plot(year,prvt, type="l")
lines(year,pblc,col="red")
seg.lm <- lm(paygu ~ sector+sex  , data=dta3, subset=ss)

diff.dta <- data.frame(pc.diff,Year=year)
gd <-ggplot(data=diff.dta,aes(x=Year,y=pc.diff)) + opts(axis.text.x=theme_text(size=15),axis.text.y=theme_text(size=15),axis.title.x=theme_text(size=15,vjust=0),axis.title.y=theme_text(size=15,vjust=0.25,angle=90))
gd + geom_line() + scale_y_continuous(name="Public sector premium",formatter="percent")

display(ch1 <- lmer(real.lhrwage ~ jbseg + PrivateSect * factor(wave) + (1|pid),data=dta3, subset=ss))
b1 <- fixef(ch1)
pubs <- b1[17]
tm <- c(0,b1[34:49])
plot(year,pubs+tm)
summary(aov(real.lhrwage ~ PrivateSect + jbseg, data=dta3, subset=ss))

#segs <- c(6:11,14,15,17:19)

ix.2 <- dta3$jbseg %in% lv.seg[segs]
dta.tmp <- dta3[ix.2,c("real.lhrwage","jbseg")]
dta.tmp$jbseg <- dta.tmp$jbseg[,drop=TRUE]
ss.tmp <- dta.tmp$real.lhrwage>0
dta.tmp <- dta.tmp[ss.tmp,]
g <- ggplot(data=dta.tmp,aes(x=real.lhrwage))
g + geom_density(aes(colour=jbseg),size=1) + opts(legend.title=theme_blank()) + labs(x="Log hourly wage",y="Density")

xtabs(~PrivateSect, dta2,subset=ss)
seg.re <- lmer(lhrwage ~  PrivateSect + factor(wave) + (1|pid) , data=dta2, subset=ss&(seg==lv.seg[15]))
display(seg.re)

xtabs(~PrivateSect,dta2)
  

dta3 <- read.table("dta3.txt")
dta3$wave <- factor(dta3$wave)
ss <- dta3$hrwage > 0
seg <- dta3$jbseg
lv.seg <- levels(seg)

#segs <- c(6:11,14,15,17:19)
#segs <- c(10,14,9,13,12,11,4,7,2,6)
segs <- c(14,19,9,17,18,6,11,8,10,15)
#segs <- sort(segs)
zz <- textConnection("PublicResultsBase2","w")
sink(zz)
op <- matrix(NA, ncol=3,nrow=length(segs))
rownames(op) <- lv.seg[segs]
colnames(op) <- c("Private","Public","Public premium (%)")

for (i in 1:length(segs)){
      seg6 <- dta3$jbseg == lv.seg[segs[i]]
      seg.re <- lmer(lhrwage ~ PrivateSect + wave + (1|pid) , data=dta3, subset=ss&seg6)
      cat("\n",date(),lv.seg[segs[i]],"\n",sep="\n")
#      display(seg.re, digits=3)
      b <- fixef(seg.re)
      op[i,1] <- exp(b[1])
      op[i,2] <- exp(b[1]) + exp(b[2])
      op[i,3] <- exp(b[2])/exp(b[1])
}

sink()
close(zz)
capture.output(cat(PublicResultsBase2,sep="\n"),file="PublicResultsBase2.txt")


zz <- textConnection("PublicResults","w")
sink(zz)
lv.seg <- levels(dta3$jbseg)
op2 <- matrix(NA, ncol=6,nrow=length(segs))
rownames(op2) <- lv.seg[segs]
colnames(op2) <- c("Private","Public","Male","Public x male","Prem male","Prem female")
effs.all <- matrix(, ncol=6)
colnames(effs.all) <- c("lhrwage", "lower",   "upper",   "Sex",     "Sector",  "SEG" )

for (i in 1:length(segs)){
      seg6 <- dta3$jbseg == lv.seg[segs[i]]
      seg.re <- lmer(lhrwage ~ PrivateSect * sex + age + agesq.k + quals + jobsen + prop.female + prop.tu + wave + (1|pid) , data=dta3, subset=ss&seg6)
      cat("\n",date(),i,lv.seg[segs[i]],"\n",sep="\n")
      display(seg.re, digits=3)
      b <- fixef(seg.re)
      op2[i,1] <- exp(b[1])
      op2[i,2] <- exp(b[2])
      op2[i,3] <- exp(b[3])
      op2[i,4] <- exp(b[length(b)])
      op2[i,5] <- sum(op2[i,1:4])/(op2[i,1]+op2[i,3])
      op2[i,6] <- (op2[i,1] + op2[i,2])/op2[i,1]
      efft <- effect("PrivateSect:sex",seg.re, transformation=list(link=log,inverse=exp),
              xlevels=list(age=40,agesq.k=1.6,quals="Tertiary",jobsen=5,wave=18))
      tmp <- data.frame(lhrwage=efft$fit,lower=efft$lower,upper=efft$upper,Sex=gl(2,2,labels=c("Female","Male")),Sector=gl(2,1,4,labels=c("Private","Public")),SEG=lv.seg[segs[i]])
      effs.all <- rbind(data.frame(effs.all),tmp)
}

sink()
close(zz)
capture.output(cat(PublicResults,sep="\n"),file="PublicResults.txt")
effs.all <- effs.all[-1,]

ggplot(data=effs.all,aes(x=Sector,y=exp(lhrwage),ymin=exp(lower),ymax=exp(upper),colour=Sex)) + facet_wrap(~SEG,scales="free")+ geom_pointrange(size=1) + labs(x="Sector",y="Hourly wage")

zz <- textConnection("PublicResultsNoTU","w")
sink(zz)
op3 <- matrix(NA, ncol=6,nrow=length(segs))
rownames(op3) <- lv.seg[segs]
colnames(op3) <- c("Private","Public","Male","Public x male","Prem male","Prem female")
effs <- matrix(, ncol=6)
colnames(effs) <- c("lhrwage", "lower",   "upper",   "Sex",     "Sector",  "SEG" )

for (i in 1:length(segs)){
      seg6 <- dta3$jbseg == lv.seg[segs[i]]
      seg.re <- lmer(lhrwage ~ PrivateSect * sex + age + agesq.k + quals + jobsen  + wave + (1|pid) , data=dta3, subset=ss&seg6)
      cat("\n",date(),lv.seg[segs[i]],"\n",sep="\n")
      display(seg.re, digits=3)
      b <- fixef(seg.re)
      op3[i,1] <- exp(b[1])
      op3[i,2] <- exp(b[2])
      op3[i,3] <- exp(b[3])
      op3[i,4] <- exp(b[length(b)])
      op3[i,5] <- exp(sum(b[1:3],b[length(b)]))/exp(sum(b[1],b[3]))
      op3[i,6] <- exp(sum(b[1:2]))/exp(b[1])
      efft <- effect("PrivateSect:sex",seg.re, transformation=list(link=log,inverse=exp),
              xlevels=list(age=40,agesq.k=1.6,quals="Tertiary",jobsen=5,wave=18))
      tmp <- data.frame(lhrwage=efft$fit,lower=efft$lower,upper=efft$upper,Sex=gl(2,2,labels=c("Female","Male")),Sector=gl(2,1,4,labels=c("Private","Public")),SEG=lv.seg[segs[i]])
      effs <- rbind(data.frame(effs),tmp)
}

sink()
close(zz)
capture.output(cat(PublicResultsNoTU,sep="\n"),file="PublicResultsNoTU.txt")
effs <- effs[-1,]

ggplot(data=effs,aes(x=Sector,y=exp(lhrwage),ymin=exp(lower),ymax=exp(upper),colour=Sex)) + facet_wrap(~SEG,scales="free")+ geom_pointrange(size=1) + labs(x="Sector",y="Hourly wage")


zz <- textConnection("PublicResultsF","w")
sink(zz)
op4 <- matrix(NA, ncol=6,nrow=length(segs))
rownames(op4) <- lv.seg[segs]
colnames(op4) <- c("Private","Public","Male","Public x male","Prem male","Prem female")
effs.f <- matrix(, ncol=6)
colnames(effs.f) <- c("lhrwage", "lower",   "upper",   "Sex",     "Sector",  "SEG" )

for (i in 1:length(segs)){
      seg6 <- dta3$jbseg == lv.seg[segs[i]]
      seg.re <- lmer(lhrwage ~ PrivateSect * sex + age + agesq.k + quals + jobsen  + prop.female + wave + (1|pid) , data=dta3, subset=ss&seg6)
      cat("\n",date(),lv.seg[segs[i]],"\n",sep="\n")
      display(seg.re, digits=3)
      b <- fixef(seg.re)
      op4[i,1] <- exp(b[1])
      op4[i,2] <- exp(b[2])
      op4[i,3] <- exp(b[3])
      op4[i,4] <- exp(b[length(b)])
      op4[i,5] <- exp(sum(b[1:3],b[length(b)]))/exp(sum(b[1],b[3]))
      op4[i,6] <- exp(sum(b[1:2]))/exp(b[1])
      efft <- effect("PrivateSect:sex",seg.re, transformation=list(link=log,inverse=exp),
              xlevels=list(age=40,agesq.k=1.6,quals="Tertiary",jobsen=5,wave=18))
      tmp <- data.frame(lhrwage=efft$fit,lower=efft$lower,upper=efft$upper,Sex=gl(2,2,labels=c("Female","Male")),Sector=gl(2,1,4,labels=c("Private","Public")),SEG=lv.seg[segs[i]])
      effs.f <- rbind(data.frame(effs.f),tmp)
}

sink()
close(zz)
capture.output(cat(PublicResultsF,sep="\n"),file="PublicResultsF.txt")
effs.f <- effs.f[-1,]

ggplot(data=effs.f,aes(x=Sector,y=exp(lhrwage),ymin=exp(lower),ymax=exp(upper),colour=Sex)) + facet_wrap(~SEG,scales="free")+ geom_pointrange(size=1) + labs(x="Sector",y="Hourly wage")



rawdiff <- lm(real.lhrwage ~ PrivateSect,data=dta3,subset=ss)
timediff <- with(dta3[ss,],by(real.lhrwage,list(PrivateSect,wave),mean,na.rm=TRUE))
pvtw <- exp(timediff[2,])
pubw <- exp(timediff[3,])
pcdiff <- (pubw-pvtw)/pvtw
plot(pcdiff)

st <- lmer(jbsat ~ PrivateSect * sex + age + agesq.k + quals + jobsen + factor(wave)  + (1|pid), data=dta3,subset=ss&PrivateSect!="Other")
display(st)

ef <- effect("PrivateSect",seg.re)
seg.re.glm <- effects:::mer.to.glm(seg.re)
efft <- effect("PrivateSect:sex",seg.re, transformation=list(link=log,inverse=exp),
              xlevels=list(age=40,agesq.k=1.6,quals="Tertiary",jobsen=5,wave=18))
plot(efft)
