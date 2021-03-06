library(effects)
library(ggplot2)
library(car)
library(arm)


setwd("c:/documents and settings/dbarron/my documents/financial cost of caring")
load(".RData")

dta3 <- read.table("dta3.txt")
dta3 <- read.table("dta4.txt")
sect.lvs <- levels(dta3$jbsect)

PrivateSect <- factor(ifelse(dta3$jbsect=="private firm/company","Private",
                      ifelse(dta3$jbsect=="non-profit orgs."|dta3$jbsect=="other"|dta3$jbsect==sect.lvs[8],NA,"Public")))
table(dta3$jbsect,PrivateSect,useNA="a")

dta3$PrivateSect <- PrivateSect

PS.sex <- factor(ifelse(PrivateSect=="Public"& dta3$sex == "male", "Public, male",
              ifelse(PrivateSect=="Public"& dta3$sex == "female", "Public, female",
                     ifelse(PrivateSect=="Private"& dta3$sex == "male", "Private, male",
                            ifelse(PrivateSect=="Private"& dta3$sex == "female", "Private, female",NA)))))
dta3$PS.sex <- PS.sex

ss <- dta3$hrwage > 0
seg <- dta3$jbseg
lv.seg <- levels(seg)

rm(PS.sex,PrivateSect)

quals.lv <- levels(dta3$qfachi)
quals <- ifelse(dta3$qfachi=="1st degree"|dta3$qfachi=="higher degree"|dta3$qfachi=="hnd,hnc,teaching","Tertiary",
                 ifelse(dta3$qfachi=="a level"|dta3$qfachi=="cse"|dta3$qfachi=="o level","Secondary",
                   ifelse(dta3$qfachi=="none of these","None",NA)))
quals <- relevel(factor(quals),"None")
dta3$quals <- quals

# Proportion female by SOC
tab.f <- xtabs(~jbsoc+sex,dta3)
prop.f <- tab.f[,1]/rowSums(tab.f)
prop.f.df <- data.frame(jbsoc=as.numeric(rownames(tab.f)),prop.female=prop.f)

tab.tu <- xtabs(~jbsoc + orgmb,dta3, exclude=NA, na.action=na.pass)
tab.tu[,2] <- NA
tab.tu[,4] <- NA
tab.tu[,5] <- NA

prop.tu <- ifelse(tab.tu[,1]==0,0,tab.tu[,1]/rowSums(tab.tu,na.rm=TRUE))
prop.tu.df <- data.frame(jbsoc=as.numeric(rownames(tab.tu)),prop.tu=prop.tu)

dta.tmp <- merge(prop.f.df,prop.tu.df,by="jbsoc",all=TRUE)

dta4 <- merge(dta3,dta.tmp,by="jbsoc",all.x=TRUE)
write.table(dta3,"dta4.txt")

ix <- (dta3$PrivateSect=="Public"|dta3$PrivateSect=="Private")&!is.na(dta3$PS.sex)
ix <- (dta3$PrivateSect=="Public"|dta3$PrivateSect=="Private")&!is.na(dta3$PrivateSect)

g <- ggplot(data=dta3[ix,],aes(x=lhrwage)) + opts(axis.text.x=theme_text(size=15),axis.text.y=theme_text(size=15),axis.title.x=theme_text(size=15,vjust=0),axis.title.y=theme_text(size=15,vjust=0.25,angle=90))
g  + geom_density(aes(colour=PS.sex[,drop=TRUE]))  + labs(x="Log hourly wage",y="Density") + xlim(c(0,5)) + geom_hline(yintercept=0,colour="white") + theme_bw() + opts(legend.title=theme_blank())
g  + geom_histogram(aes(y=..density..,fill=PrivateSect[,drop=TRUE]),binwidth=.02) + opts(legend.title=theme_blank()) + labs(x="Log hourly wage",y="Density") + xlim(c(0,5)) + geom_hline(yintercept=0,colour="white") 

xtabs(~PrivateSect + sex, dta2, subset=ss)

library(descr)
with(dta2[ss,],CrossTable(PrivateSect,sex))
with(dta2[ss,],CrossTable(PrivateSect,quals))

# Basic premium
display(lm(lhrwage ~ PrivateSect,data=dta3,subset=wave==18))

display(basic <- lmer(lhrwage ~ PrivateSect + (1|pid), data=dta3))
base.b <- fixef(basic)
(exp(sum(base.b)) - exp(base.b[1]))/exp(base.b[1])
base.re <- lmer(lhrwage ~ PrivateSect + factor(wave) + (1|pid), data=dta3)

display(base.re)
base.re.b <- fixef(base.re)

(exp((base.re.b[1]+base.re.b[2])) - exp(base.re.b[1]))/exp(base.re.b[1])
seg.re1 <- lmer(lhrwage ~ PrivateSect * factor(wave) + (1|pid) , data=dta3)

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
gd + geom_line() + scale_y_continuous(name="Public sector premium",formatter="percent") + theme_bw()

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
effs.b <- matrix(, ncol=6)
colnames(effs.b) <-  c("lhrwage", "lower",   "upper",   "Sex",     "Sector",  "SEG" )

for (i in 1:length(segs)){
      seg6 <- dta3$jbseg == lv.seg[segs[i]]
      seg.re <- lmer(lhrwage ~ PrivateSect * sex + wave + (1|pid) , data=dta3, subset=ss&seg6)
      cat("\n",date(),lv.seg[segs[i]],"\n",sep="\n")
      display(seg.re, digits=3)
#      b <- fixef(seg.re)
#      op[i,1] <- exp(b[1])
#      op[i,2] <- exp(b[1] + b[2])
#      op[i,3] <- exp(b[2])/exp(b[1])
#      efft <- effect("PrivateSect:sex",seg.re, transformation=list(link=log,inverse=exp),
#              xlevels=list(wave=18))
#      tmp <- data.frame(lhrwage=efft$fit,lower=efft$lower,upper=efft$upper,Sex=gl(2,2,labels=c("Female","Male")),Sector=gl(2,1,4,labels=c("Private","Public")),SEG=lv.seg[segs[i]])
#      effs.b <- rbind(data.frame(effs.b),tmp)
}

sink()
close(zz)
capture.output(cat(PublicResultsBase2,sep="\n"),file="PublicResultsBase2.txt")

effs.b <- effs.b[-1,]

ggplot(data=effs.b,aes(x=Sector,y=exp(lhrwage),ymin=exp(lower),ymax=exp(upper))) + facet_wrap(~SEG,scales="free")+ geom_pointrange(size=1) + labs(x="Sector",y="Hourly wage")
dta3$wave <- factor(dta3$wave)

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
      seg.re <- lmer(lhrwage ~ PrivateSect * sex + age + agesq.k + quals + jobsen  + wave + (1|pid) , data=dta3, subset=seg6)
      cat("\n",date(),i,lv.seg[segs[i]],"\n",sep="\n")
      display(seg.re, digits=3)
     b <- fixef(seg.re)
     op2[i,1] <- exp(b[1])
     op2[i,2] <- exp(b[2])
     op2[i,3] <- exp(b[3])
     op2[i,4] <- exp(b[length(b)])
     op2[i,5] <- sum(op2[i,1:4])/(op2[i,1]+op2[i,3])
     op2[i,6] <- (op2[i,1] + op2[i,2])/op2[i,1]
#     efft <- effect("PrivateSect:sex",seg.re, transformation=list(link=log,inverse=exp),
#             xlevels=list(age=40,agesq.k=1.6,quals="Tertiary",jobsen=5,wave=18))
#     tmp <- data.frame(lhrwage=efft$fit,lower=efft$lower,upper=efft$upper,Sex=gl(2,2,labels=c("Female","Male")),Sector=gl(2,1,4,labels=c("Private","Public")),SEG=lv.seg[segs[i]])
#     effs.all <- rbind(data.frame(effs.all),tmp)
}

sink()
close(zz)
capture.output(cat(PublicResults,sep="\n"),file="PublicResultsNew.txt")
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
      seg.re <- lmer(lhrwage ~ PrivateSect * sex + age + agesq.k + PrivateSect*white + quals + jobsen  + wave + (1|pid) , data=dta3, subset=ss&seg6)
      cat("\n",date(),lv.seg[segs[i]],"\n",sep="\n")
      display(seg.re, digits=3)
#       b <- fixef(seg.re)
#       op3[i,1] <- exp(b[1])
#       op3[i,2] <- exp(b[2])
#       op3[i,3] <- exp(b[3])
#       op3[i,4] <- exp(b[length(b)])
#       op3[i,5] <- exp(sum(b[1:3],b[length(b)]))/exp(sum(b[1],b[3]))
#       op3[i,6] <- exp(sum(b[1:2]))/exp(b[1])
#       efft <- effect("PrivateSect:sex",seg.re, transformation=list(link=log,inverse=exp),
#               xlevels=list(age=40,agesq.k=1.6,quals="Tertiary",jobsen=5,wave=18))
#       tmp <- data.frame(lhrwage=efft$fit,lower=efft$lower,upper=efft$upper,Sex=gl(2,2,labels=c("Female","Male")),Sector=gl(2,1,4,labels=c("Private","Public")),SEG=lv.seg[segs[i]])
#       effs <- rbind(data.frame(effs),tmp)
}

sink()
close(zz)
capture.output(cat(PublicResultsNoTU,sep="\n"),file="PublicResultsNoTURace.txt")
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

#### Time varying premium?
dta3$wave <- factor(dta3$wave)
i <- 1
seg6 <- dta3$jbseg == lv.seg[segs[i]]
seg.re <- lmer(lhrwage ~ PrivateSect * sex + age + agesq.k + PrivateSect*wave + quals + jobsen + (1|pid) , data=dta3, subset=seg6)
b <- fixef(seg.re)
men.prvt <- b[1] + b[3]
men.pub <- b[1] + b[2] + b[3] + b[26]
wom.pvt <- b[1]
wom.pub <- b[1] + b[2]
pvt.wv <- c(0,b[6:22])
pub.wv <- c(0,(b[6:22]+b[27:43]))
      men.pv.wv <- exp(men.prvt + pvt.wv)
      men.pub.wv <- exp(men.pub + pub.wv)
      wom.pv.wv <- exp(wom.pvt + pvt.wv)
      wom.pub.wv <- exp(wom.pub + pub.wv)
      men.pub.prem <- (men.pub.wv - men.pv.wv)/men.pv.wv
      wom.pub.prem <- (wom.pub.wv - wom.pv.wv)/wom.pv.wv
tmp.p <- c(men.pub.prem,wom.pub.prem)
tmp <- c(men.pv.wv,men.pub.wv,wom.pv.wv,wom.pub.wv)
sex <- gl(2,36,labels=c("Men","Women"))
sector <- gl(2,18,72,labels=c("Private","Public"))
sex.sector <- gl(4,18,labels=c("Men, Private","Men, Public","Women, Private","Women, Public"))
wave <- rep(1:18,4)
tmp.d <- data.frame(SEG=lv.seg[segs[1]],wave=wave,sex,sector,sex.sector,wage=tmp)
tmp.d.p <- data.frame(SEG=lv.seg[segs[i]],wave=rep(1:18,2),
            Sex=gl(2,18,labels=c("Men","Women")),premium=tmp.p)
dta.wv <- tmp.d
dta.p <- tmp.d.p

for (i in 2:length(segs)){
      seg6 <- dta3$jbseg == lv.seg[segs[i]]
      seg.re <- lmer(lhrwage ~ PrivateSect * sex + age + agesq.k + PrivateSect*wave + quals + jobsen + (1|pid) , data=dta3, subset=seg6)
      cat("\n",date(),lv.seg[segs[i]],"\n",sep="\n")
      b <- fixef(seg.re)
      men.prvt <- b[1] + b[3]
      men.pub <- b[1] + b[2] + b[3] + b[26]
      wom.pvt <- b[1]
      wom.pub <- b[1] + b[2]
      pvt.wv <- c(0,b[6:22])
      pub.wv <- c(0,(b[6:22]+b[27:43]))
      men.pv.wv <- exp(men.prvt + pvt.wv)
      men.pub.wv <- exp(men.pub + pub.wv)
      wom.pv.wv <- exp(wom.pvt + pvt.wv)
      wom.pub.wv <- exp(wom.pub + pub.wv)
      men.pub.prem <- (men.pub.wv - men.pv.wv)/men.pv.wv
      wom.pub.prem <- (wom.pub.wv - wom.pv.wv)/wom.pv.wv
      tmp <- c(men.pv.wv,men.pub.wv,wom.pv.wv,wom.pub.wv)
      tmp.p <- c(men.pub.prem,wom.pub.prem)
      sex <- gl(2,36,labels=c("Men","Women"))
      sector <- gl(2,18,72,labels=c("Private","Public"))
      sex.sector <- gl(4,18,labels=c("Men, Private","Men, Public","Women, Private","Women, Public"))
      wave <- rep(1:18,4)
      tmp.d <- data.frame(SEG=lv.seg[segs[i]],wave=wave,sex,sector,sex.sector,wage=tmp)
      tmp.d.p <- data.frame(SEG=lv.seg[segs[i]],wave=rep(1:18,2),
            Sex=gl(2,18,labels=c("Men","Women")),premium=tmp.p)
      dta.wv <- rbind(dta.wv,tmp.d)
      dta.p <- rbind(dta.p,tmp.d.p)
}

g <- ggplot(data=dta.p,aes(x=wave+1990,y=premium*100,colour=Sex)) + geom_line() + facet_wrap(~SEG,scales="fixed")
g + labs(x="Year",y="Public sector premium (%)") + theme_bw() + geom_hline(aes(yintercept=0),colour="red")

      seg6 <- dta3$jbseg == lv.seg[segs[3]]
      seg.re <- lmer(lhrwage ~ PrivateSect * sex + age + agesq.k + PrivateSect*wave + quals + jobsen + (1|pid) , data=dta3, subset=seg6)
u0 <- ranef(seg.re, postVar = TRUE) 

u0se <- sqrt(attr(u0[[1]], "postVar")[1, , ])
pid <- as.numeric(rownames(u0[[1]]))

u0tab <- cbind(pid, u0[[1]], u0se)

colnames(u0tab) <- c("id","u0","u0se")

u0tab <- u0tab[order(u0tab$u0), ]

u0tab <- cbind(u0tab, c(1:dim(u0tab)[1]))

colnames(u0tab)[4] <- "u0rank"

u0tab <- u0tab[order(u0tab$id), ]

u0tab[1:10, ]

plot(u0tab$u0rank, u0tab$u0, type = "n", xlab = "u_rank", ylab = "conditional modes of r.e. for school_id:_cons")

segments(u0tab$u0rank, u0tab$u0 - 1.96*u0tab$u0se, u0tab$u0rank, u0tab$u0 + 1.96*u0tab$u0se)

points(u0tab$u0rank, u0tab$u0, col = "blue")

abline(h = 0, col = "red")
