# Analysis for Public Private Pay paper

#library(effects)
library(xtable)
library(ggplot2)
library(car)
library(arm)
library(effects)
#setwd("c:/documents and settings/dbarron/my documents/financial cost of caring")
#load(".RData")

#dta3 <- read.table("dta3.txt")

#dta3 <- read.table("dta3.txt")

dta3 <- read.table("dta4.txt")
#dta3$sec.rc <- sec.rc 
#write.table(dta3,"dta4.txt")

xtable(xtabs(~sec.rc,dta3),digits=0)

secs <- levels(dta3$sec.rc)

wgs <- tapply(dta3$hrwage,dta3$sec.rc,mean,na.rm=TRUE)
ix <- order(wgs)
wgs[secs]
xtable(data.frame(wgs),digits=2)

##########################################################
# This section creates the Public/Private sector factor
# Non-profit and Other coded as NA, so not included in later analysis
# Armed forces, NHS, higher ed  included in Public Sector
###########################################################

sect.lvs <- levels(dta3$jbsect)

PrivateSect <- factor(ifelse(dta3$jbsect=="private firm/company","Private",
                      ifelse(dta3$jbsect=="non-profit orgs."|dta3$jbsect=="other"|dta3$jbsect==sect.lvs[8],NA,"Public")))
table(dta3$jbsect,dta3$PrivateSect,useNA="a")

dta3$PrivateSect <- PrivateSect

###############
# Creates Publc/private x Sex factor, useful for graphics
#############################################################

PS.sex <- factor(ifelse(PrivateSect=="Public"& dta3$sex == "male", "Public, male",
              ifelse(PrivateSect=="Public"& dta3$sex == "female", "Public, female",
                     ifelse(PrivateSect=="Private"& dta3$sex == "male", "Private, male",
                            ifelse(PrivateSect=="Private"& dta3$sex == "female", "Private, female",NA)))))
dta3$PS.sex <- PS.sex

#########
# Create some selections useful for looping later
###############

ss <- dta3$hrwage > 0
sec <- dta3$sec.rc
lv.sec <- levels(sec)

rm(PS.sex,PrivateSect)

###########################
## Create three-level educational qualifications factor
## Tertiary = first or higher degree or HND, HNC, or teaching qual
## Secondary = A-levels, Highers, Standard, O-levels, CSE or GCSE
## None = everyone else
###################################################################

quals.lv <- levels(dta3$qfachi)
quals <- ifelse(dta3$qfachi=="1st degree"|dta3$qfachi=="higher degree"|dta3$qfachi=="hnd,hnc,teaching","Tertiary",
                 ifelse(dta3$qfachi=="a level"|dta3$qfachi=="cse"|dta3$qfachi=="o level","Secondary",
                   ifelse(dta3$qfachi=="none of these","None",NA)))
quals <- relevel(factor(quals),"None")
dta3$quals <- quals

#########################################
# Proportion of employees that are female by SOC (occupation)
###############################################

tab.f <- xtabs(~jbsoc+sex,dta3)
prop.f <- tab.f[,1]/rowSums(tab.f)
prop.f.df <- data.frame(jbsoc=as.numeric(rownames(tab.f)),prop.female=prop.f)
mw <- tapply(dta3$hrwage,dta3$jbsoc,median,na.rm=T)
all.equal(as.numeric(rownames(tab.f)),as.numeric(rownames(mw)))
cor(prop.f,mw,use="c")
########################
# Proportion in each occupation that are trade union members
# variable orgmb identifies TU membership
# Columns 2,4 and 5 are types of missing
# Column 3 = "not mentioned", assume this means not a TU member
##################################################################

tab.tu <- xtabs(~jbsoc + orgmb,dta3, exclude=NA, na.action=na.pass)
tab.tu[,2] <- NA
tab.tu[,4] <- NA
tab.tu[,5] <- NA

prop.tu <- ifelse(tab.tu[,1]==0,0,tab.tu[,1]/rowSums(tab.tu,na.rm=TRUE))
prop.tu.df <- data.frame(jbsoc=as.numeric(rownames(tab.tu)),prop.tu=prop.tu)

dta.tmp <- merge(prop.f.df,prop.tu.df,by="jbsoc",all=TRUE)

dta4 <- merge(dta3,dta.tmp,by="jbsoc",all.x=TRUE)
write.table(dta4,"dta4.txt")

###########################################
### Density plot of public and private sector wages by sex
###########################################################

ix <- (dta3$PrivateSect=="Public"|dta3$PrivateSect=="Private")&!is.na(dta3$PS.sex)
ix <- (dta3$PrivateSect=="Public"|dta3$PrivateSect=="Private")&!is.na(dta3$PrivateSect)

g <- ggplot(data=dta3[ix,],aes(x=lhrwage)) + opts(axis.text.x=theme_text(size=15),axis.text.y=theme_text(size=15),axis.title.x=theme_text(size=15,vjust=0),axis.title.y=theme_text(size=15,vjust=0.25,angle=90))
g  + geom_density(aes(colour=PS.sex[,drop=TRUE]))  + labs(x="Log hourly wage",y="Density") + xlim(c(0,5)) + geom_hline(yintercept=0,colour="white") + theme_bw() + opts(legend.title=theme_blank())

## Also tried histograms, but don't look as good
g  + geom_histogram(aes(y=..density..,fill=PrivateSect[,drop=TRUE]),binwidth=.02) + opts(legend.title=theme_blank()) + labs(x="Log hourly wage",y="Density") + xlim(c(0,5)) + geom_hline(yintercept=0,colour="white") 


############################
## Crosstabs of sector with sex and education
#############################
library(descr)
with(dta3,CrossTable(PrivateSect,sex))
with(dta3,CrossTable(PrivateSect,quals))
summary(aov(prop.female~PrivateSect,dta3))
by(dta3$prop.female,dta3$PrivateSect,mean,na.rm=T)
fem.re <- lmer(lhrwage ~ prop.female + (1|pid), dta3)
###################################################################
# REGRESSION MODELS
######################################################################
## Make wave a factor
dta3$fwave <- factor(dta3$wave)

# These are the SEGs that we analyse (could remove others from data?)

# Basic premium

base.re <- lmer(lhrwage ~ PrivateSect + fwave + (1|pid), data=dta3)

display(base.re)
base.re.b <- fixef(base.re)

(exp((base.re.b[1]+base.re.b[2])) - exp(base.re.b[1]))/exp(base.re.b[1])

##### Relative size of sector and SEG effects on pay
ix <- dta3$jbseg %in% lv.seg[segs]
seg.re1 <- lmer(lhrwage ~ PrivateSect + fwave + jbseg + (1|pid) , data=dta3,subset=ix)
base.re.sub <- update(seg.re1, .~.-jbseg)
anova(base.re.sub,seg.re1) # chisq=8164, df=9, p << .001

display(seg.re1)

# Look at variation in public sector premium over time
# Use interacton of wave with sector
# Wave as factor or integer?

seg.re2 <- lmer(lhrwage ~ PrivateSect * fwave + (1|pid) , data=dta3)
display(seg.re2)

b1 <- fixef(seg.re2)
prvt <- exp(b1[1] + c(0,b1[3:19]))
pblc <- exp(b1[1] + b1[2] + (c(0,b1[3:19])+c(0,b1[20:36])))
pc.diff <- (pblc - prvt)/pblc
year <- 1:18 + 1990

### Standard plot
plot(year,pc.diff, type="l")
plot(year,prvt, type="l")
lines(year,pblc,col="red")

#### ggplot version
diff.dta <- data.frame(pc.diff,Year=year)
gd <-ggplot(data=diff.dta,aes(x=Year,y=pc.diff*100)) + opts(axis.text.x=theme_text(size=15),axis.text.y=theme_text(size=15),axis.title.x=theme_text(size=15,vjust=0),axis.title.y=theme_text(size=15,vjust=0.25,angle=90))
gd + geom_line() + scale_y_continuous(name="Public sector premium (%)") + theme_bw()


##############
## REGRESSIONS in the paper
#####################################################

##########################
## Results in Table 2 and 3
################################
zz <- textConnection("PublicResultsBase7.8.12","w")
sink(zz)
op <- matrix(NA, ncol=4,nrow=length(secs))
rownames(op) <- secs
colnames(op) <- c("Constant","Public","Male","Male x male")
#effs.b <- matrix(, ncol=6)
#colnames(effs.b) <-  c("lhrwage", "lower",   "upper",   "Sex",     "Sector",  "SEG" )

for (i in 1:length(secs)){
      seg6 <- dta3$sec.rc == secs[i]
      seg.re <- lmer(lhrwage ~ PrivateSect * sex + fwave + (1|pid) , data=dta3, subset=ss&seg6)
      cat("\n",date(),secs[i],"\n",sep="\n")
      display(seg.re, digits=3)
      b <- fixef(seg.re)
     op[i,] <- b[c(1:3,21)]
#     efft <- effect("PrivateSect:sex",seg.re, transformation=list(link=log,inverse=exp),
#             xlevels=list(wave=18))
#     tmp <- data.frame(lhrwage=efft$fit,lower=efft$lower,upper=efft$upper,Sex=gl(2,2,labels=c("Female","Male")),Sector=gl(2,1,4,labels=c("Private","Public")),SEG=lv.seg[segs[i]])
#     effs.b <- rbind(data.frame(effs.b),tmp)
}

sink()
close(zz)
capture.output(cat(PublicResultsBase7.8.12,sep="\n"),file="PublicResultsBase7.8.12.txt")

################
## Calculate percentage pay premiums for table 3
#################################
men.pv <- exp(op[,1] + op[,3])
men.pub <- exp(op[,1] + op[,3] + op[,2] + op[,4])
women.pv <- exp(op[,1] )
women.pub <- exp(op[,1] + op[,2] )
men.prem <- matrix(100*(men.pub-men.pv)/men.pv,ncol=1)
rownames(men.prem) <- lv.seg[segs]
wom.prem <- matrix(100*(women.pub-women.pv)/women.pv,ncol=1)
rownames(wom.prem) <- rownames(men.prem)
prem <- gdata::interleave(men.prem,wom.prem)
sex <- gl(2,1,20,labels=c("Men","Women"))
prems <- data.frame(rownames(prem),sex,prem)
xtable(prems,digits=2)


effs.b <- effs.b[-1,]

ggplot(data=effs.b,aes(x=Sector,y=exp(lhrwage),ymin=exp(lower),ymax=exp(upper))) + facet_wrap(~SEG,scales="free")+ geom_pointrange(size=1) + labs(x="Sector",y="Hourly wage")
dta3$wave <- factor(dta3$wave)

zz <- textConnection("PublicResults7.8.12","w")
sink(zz)

op2 <- matrix(NA, ncol=6,nrow=length(secs))
rownames(op2) <- secs
colnames(op2) <- c("Private","Public","Male","Public x male","Prem male","Prem female")
effs.all <- matrix(, ncol=6)
colnames(effs.all) <- c("lhrwage", "lower",   "upper",   "Sex",     "Sector",  "SEG" )

for (i in 1:length(secs)){
      seg6 <- dta3$sec.rc == secs[i]
      seg.re <- lmer(lhrwage ~ PrivateSect * sex + age + agesq.k + quals + jobsen  + fwave + (1|pid) , data=dta3, subset=seg6)
      cat("\n",date(),i,secs[i],"\n",sep="\n")
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
capture.output(cat(PublicResults7.8.12,sep="\n"),file="PublicResultsFull7.8.12.txt")
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

seg.r <- lmer(jbsat ~ PrivateSect * sex * wave + age + agesq.k + quals + jobsen +  (1|pid), data=dta3,subset=ss&PrivateSect!="Other")
display(st)

ef <- effect("PrivateSect",seg.re)
seg.re.glm <- effects:::mer.to.glm(seg.re)
efft <- effect("PrivateSect:sex",seg.re, transformation=list(link=log,inverse=exp),
              xlevels=list(age=40,agesq.k=1.6,quals="Tertiary",jobsen=5,wave=18))
plot(efft)

#### Time varying premium?
dta3$wave <- factor(dta3$wave)
i <- 1
seg6 <- dta3$sec.rc == secs[i]
seg.re <- lmer(lhrwage ~ PrivateSect * sex + PrivateSect * wave + age + agesq.k +  quals + jobsen + (1|pid) , data=dta3, subset=seg6)
seg.reb <- update(seg.re,.~ . - PrivateSect:wave)
aa <- anova(seg.re,seg.reb)
anov.dta <- data.frame(SEC=secs[i],Chisq=aa[2,5],p=aa[2,7])
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
tmp.d <- data.frame(SEC=secs[1],wave=wave,sex,sector,sex.sector,wage=tmp)
tmp.d.p <- data.frame(SEC=secs[i],wave=rep(1:18,2),
            Sex=gl(2,18,labels=c("Men","Women")),premium=tmp.p)
dta.wv <- tmp.d
dta.p <- tmp.d.p

for (i in 2:length(secs)){
      seg6 <- dta3$sec.rc == secs[i]
      seg.re <- lmer(lhrwage ~ PrivateSect * sex + age + agesq.k + PrivateSect*wave + quals + jobsen + (1|pid) , data=dta3, subset=seg6)
      cat("\n",date(),secs[i],"\n",sep="\n")

      seg.reb <- update(seg.re,.~ . - PrivateSect:wave)
      aa <- anova(seg.re,seg.reb)
      anov.tmp <- data.frame(SEC=secs[i],Chisq=aa[2,5],p=aa[2,7])
      anov.dta <- rbind(anov.dta,anov.tmp)
      
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
      tmp.d <- data.frame(SEC=secs[i],wave=wave,sex,sector,sex.sector,wage=tmp)
      tmp.d.p <- data.frame(SEC=secs[i],wave=rep(1:18,2),
            Sex=gl(2,18,labels=c("Men","Women")),premium=tmp.p)
      dta.wv <- rbind(dta.wv,tmp.d)
      dta.p <- rbind(dta.p,tmp.d.p)
}

print(xtable(anov.dta,digits=2),type="latex")

library(plyr)
dta.p2 <- ddply(dta.p,c("SEC","Sex"),transform,meanprem=mean(premium))

g <- ggplot(data=dta.p2,aes(x=wave+1990,y=premium*100,colour=Sex)) + geom_line() + facet_wrap(~SEC,scales="fixed")
g + labs(x="Year",y="Public sector premium (%)") + theme_bw() + geom_hline(aes(yintercept=0),colour="grey") + ylim(c(-20,35)) + geom_smooth(aes(colour=Sex),method='lm',se=FALSE,linetype="dashed")

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

## Add prop.female to model

zz <- textConnection("PublicResultsFem","w")
sink(zz)
lv.seg <- levels(dta3$jbseg)
op3 <- matrix(NA, ncol=6,nrow=length(segs))
rownames(op3) <- lv.seg[segs]
colnames(op3) <- c("Private","Public","Male","Public x male","Prem male","Prem female")

for (i in 1:length(segs)){
  seg6 <- dta3$jbseg == lv.seg[segs[i]]
  seg.re <- lmer(lhrwage ~ PrivateSect * sex + age + agesq.k + quals + jobsen  + prop.female + fwave + (1|pid) , data=dta3, subset=seg6)
  cat("\n",date(),i,lv.seg[segs[i]],"\n",sep="\n")
  display(seg.re, digits=3)
  b <- fixef(seg.re)
  op3[i,1] <- exp(b[1])
  op3[i,2] <- exp(b[2])
  op3[i,3] <- exp(b[3])
  op3[i,4] <- exp(b[length(b)])
  op3[i,5] <- sum(op3[i,1:4])/(op3[i,1]+op3[i,3])
  op3[i,6] <- (op3[i,1] + op3[i,2])/op3[i,1]
}

sink()
close(zz)
capture.output(cat(PublicResultsFem,sep="\n"),file="PublicResultsFem.txt")


##########################
## Figure 1
##############################

prop.table(xtabs(~PrivateSect,dta3,subset=jbsoc==211)) # Mechanical engineers
prop.table(xtabs(~PrivateSect,dta3,subset=jbsoc==233)) # Secondary school teachers
prop.table(xtabs(~PrivateSect,dta3,subset=jbsoc==340)) # Nurses
prop.table(xtabs(~PrivateSect,dta3,subset=jbsoc==242)) # Solicitors
prop.table(xtabs(~PrivateSect,dta3,subset=jbsoc==532)) # Plumbers
prop.table(xtabs(~PrivateSect,dta3,subset=jbsoc==620)) # Chefs, cooks
prop.table(xtabs(~PrivateSect,dta3,subset=jbsoc==640)) # Assistant nurses
prop.table(xtabs(~PrivateSect,dta3,subset=jbsoc==952)) # Kitchen porters

occs <- gl(8,2,labels=c('Mechanical engineers','Secondary school teachers','Nurses',
                        'Solicitors','Plumbers','Chefs, cooks','Assistant nurses',
                        'Kitchen porters'))
sect <- gl(2,1,16,labels=c('Private','Public'))
prop <- c(.86,.14,.066,.93,.17,.83,.87,.13,.90,.098,.75,.25,.15,.85,.81,.19)

fig1.d <- data.frame(Occupations=occs,Sector=sect,Percentage=prop*100)
fig1 <- ggplot(data=fig1.d,aes(x=Occupations,y=Percentage,fill=Sector)) + geom_bar(position='dodge',stat='identity') + theme_bw() + coord_flip()
fig1

xtabs(~sec.rc + jbsoc, dta3)

####################
### Prop. female ##############
##########################
zz <- textConnection("PublicResults.female","w")
sink(zz)

op3 <- matrix(NA, ncol=6,nrow=length(secs))
rownames(op3) <- secs
colnames(op3) <- c("Private","Public","Male","Public x male","Prem male","Prem female")
effs.all <- matrix(, ncol=6)
colnames(effs.all) <- c("lhrwage", "lower",   "upper",   "Sex",     "Sector",  "SEG" )

for (i in 1:length(secs)){
  seg6 <- dta3$sec.rc == secs[i]
  seg.re <- lmer(lhrwage ~ PrivateSect * sex + age + agesq.k + quals + jobsen  + fwave + prop.female +  (1|pid) , data=dta3, subset=seg6)
  cat("\n",date(),i,secs[i],"\n",sep="\n")
  display(seg.re, digits=3)
  b <- fixef(seg.re)
  op3[i,1] <- exp(b[1])
  op3[i,2] <- exp(b[2])
  op3[i,3] <- exp(b[3])
  op3[i,4] <- exp(b[length(b)])
  op3[i,5] <- sum(op3[i,1:4])/(op3[i,1]+op3[i,3])
  op3[i,6] <- (op3[i,1] + op3[i,2])/op3[i,1]
  #     efft <- effect("PrivateSect:sex",seg.re, transformation=list(link=log,inverse=exp),
  #             xlevels=list(age=40,agesq.k=1.6,quals="Tertiary",jobsen=5,wave=18))
  #     tmp <- data.frame(lhrwage=efft$fit,lower=efft$lower,upper=efft$upper,Sex=gl(2,2,labels=c("Female","Male")),Sector=gl(2,1,4,labels=c("Private","Public")),SEG=lv.seg[segs[i]])
  #     effs.all <- rbind(data.frame(effs.all),tmp)
}

sink()
close(zz)
capture.output(cat(PublicResults.female,sep="\n"),file="PublicResultsfemale.txt")
effs.all <- effs.all[-1,]

ggplot(data=effs.all,aes(x=Sector,y=exp(lhrwage),ymin=exp(lower),ymax=exp(upper),colour=Sex)) + facet_wrap(~SEG,scales="free")+ geom_pointrange(size=1) + labs(x="Sector",y="Hourly wage")

#########
## Is premium related to proportion women in occupation?
#####################
fwave <- factor(dta3$wave)
zz <- textConnection("PublicResults.femaleocc","w")
sink(zz)

op3 <- matrix(NA, ncol=6,nrow=length(secs))
rownames(op3) <- secs
colnames(op3) <- c("Private","Public","Male","Public x male","Prem male","Prem female")
effs.all <- matrix(, ncol=6)
colnames(effs.all) <- c("lhrwage", "lower",   "upper",   "Sex",     "Sector",  "SEG" )

for (i in 1:length(secs)){
  seg6 <- dta3$sec.rc == secs[i]
  seg.re <- lmer(lhrwage ~ PrivateSect * sex + prop.female*sex + age + agesq.k + quals + jobsen  + fwave + prop.female +  (1|pid) , data=dta3, subset=seg6)
  cat("\n",date(),i,secs[i],"\n",sep="\n")
  display(seg.re, digits=3)
  b <- fixef(seg.re)
  op3[i,1] <- exp(b[1])
  op3[i,2] <- exp(b[2])
  op3[i,3] <- exp(b[3])
  op3[i,4] <- exp(b[length(b)])
  op3[i,5] <- sum(op3[i,1:4])/(op3[i,1]+op3[i,3])
  op3[i,6] <- (op3[i,1] + op3[i,2])/op3[i,1]
  #     efft <- effect("PrivateSect:sex",seg.re, transformation=list(link=log,inverse=exp),
  #             xlevels=list(age=40,agesq.k=1.6,quals="Tertiary",jobsen=5,wave=18))
  #     tmp <- data.frame(lhrwage=efft$fit,lower=efft$lower,upper=efft$upper,Sex=gl(2,2,labels=c("Female","Male")),Sector=gl(2,1,4,labels=c("Private","Public")),SEG=lv.seg[segs[i]])
  #     effs.all <- rbind(data.frame(effs.all),tmp)
}

sink()
close(zz)
capture.output(cat(PublicResults.femaleocc,sep="\n"),file="PublicResultsfemaleocc.txt")
effs.all <- effs.all[-1,]

ggplot(data=effs.all,aes(x=Sector,y=exp(lhrwage),ymin=exp(lower),ymax=exp(upper),colour=Sex)) + facet_wrap(~SEG,scales="free")+ geom_pointrange(size=1) + labs(x="Sector",y="Hourly wage")


####################
### Prop. TU ##############
##########################
zz <- textConnection("PublicResults.tu","w")
sink(zz)

op3 <- matrix(NA, ncol=6,nrow=length(secs))
rownames(op3) <- secs
colnames(op3) <- c("Private","Public","Male","Public x male","Prem male","Prem female")
effs.all <- matrix(, ncol=6)
colnames(effs.all) <- c("lhrwage", "lower",   "upper",   "Sex",     "Sector",  "SEG" )

for (i in 1:length(secs)){
  seg6 <- dta3$sec.rc == secs[i]
  seg.re <- lmer(lhrwage ~ PrivateSect * sex + age + agesq.k + quals + jobsen  + fwave + prop.tu +  (1|pid) , data=dta3, subset=seg6)
  cat("\n",date(),i,secs[i],"\n",sep="\n")
  display(seg.re, digits=3)
  b <- fixef(seg.re)
  op3[i,1] <- exp(b[1])
  op3[i,2] <- exp(b[2])
  op3[i,3] <- exp(b[3])
  op3[i,4] <- exp(b[length(b)])
  op3[i,5] <- sum(op3[i,1:4])/(op3[i,1]+op3[i,3])
  op3[i,6] <- (op3[i,1] + op3[i,2])/op3[i,1]
#       efft <- effect("PrivateSect:sex",seg.re, transformation=list(link=log,inverse=exp),
#               xlevels=list(age=40,agesq.k=1.6,quals="Tertiary",jobsen=5,wave=18))
#       tmp <- data.frame(lhrwage=efft$fit,lower=efft$lower,upper=efft$upper,Sex=gl(2,2,labels=c("Female","Male")),Sector=gl(2,1,4,labels=c("Private","Public")),SEG=lv.seg[segs[i]])
#       effs.all <- rbind(data.frame(effs.all),tmp)
}

sink()
close(zz)
capture.output(cat(PublicResults.tu,sep="\n"),file="PublicResultstu.txt")
effs.all <- effs.all[-1,]

ggplot(data=effs.all,aes(x=Sector,y=exp(lhrwage),ymin=exp(lower),ymax=exp(upper),colour=Sex)) + facet_wrap(~SEG,scales="free")+ geom_pointrange(size=1) + labs(x="Sector",y="Hourly wage")
