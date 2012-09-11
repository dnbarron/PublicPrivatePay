library(xtable)
library(arm)
library(ggplot2)

################################
### Tables 3 and figure 6
#################################

dta3 <- read.table("dta4.txt")
dta3$fwave <- factor(dta3$wave)

prem.b <- matrix(, ncol=6)
vn <- c("SEC","Premium","Diff", "Sex", "SEprem", "PremPct" )
colnames(prem.b) <- vn
prem.b <- data.frame(prem.b)
segs <- c(14,19,9,17,18,6,11,8,10,15)
sec.rc <- dta3$sec.rc
secs <- levels(sec.rc)
ss <- dta3$hrwage > 0

for (i in 1:length(secs)){
      seg6 <- dta3$sec.rc == secs[i]
      seg.re <- lmer(lhrwage ~ PrivateSect*sex + fwave + (1|pid) , data=dta3, subset=seg6)
      cat("\n",date(),secs[i],"\n",sep="\n")
#      display(seg.re, digits=3)
      b <- fixef(seg.re)
      vc <- vcov(seg.re)
      k <- length(b)
      b0 <- b[1]
      b1 <- b[2]
      men <- b[3]
      vb1 <- vc[2,2]
      b3 <- b[k]
      vb3 <- vc[k,k]
      cb1b3 <- vc[2,k]
# Public sector premium men and women (log scale)      
      p.men <- b1 + b3
      p.women <- b1
# Differences between public and private men and women      
      d.men <- exp(b0 + men + b1 + b3) - exp(b0 + men)
      d.women <- exp(b0 + b1) - exp(b0)
# Percentage public sector premium men and women      
      p.men.pct <- exp(p.men+b0 + men)/exp(b0 + men) - 1
      p.women.pct <- exp(p.women + b0)/exp(b0) - 1
# Standard error of pub sect premium (log scale) men and women
      se.p.men <- sqrt(vb1 + vb3 + 2*cb1b3)
      se.p.women <- sqrt(vb1)
      tmp <- data.frame(secs[i],p.men,d.men,"Men", se.p.men, p.men.pct)
      tmp2 <- data.frame(secs[i], p.women,d.women, "Women",se.p.women, p.women.pct )
      names(tmp) <- vn
      names(tmp2) <- vn
      prem.b <- rbind(prem.b,tmp,tmp2)
}
prem.b <- prem.b[-1,]

#g <- ggplot(data=prem.b, aes(x=Sex,y=Premium,ymin=Premium-2*SEprem,ymax=Premium+2*SEprem)) + facet_wrap(~SEG) + geom_pointrange(size=1,colour="blue") + labs(x=NULL,y="Log hourly wages in pounds") + geom_hline(aes(yintercept=0),colour="red") + theme_bw()
#gg <- ggplot(data=prem.b, aes(x=Sex,y=PremPct))  + geom_point(aes(colour=SEG),size=2)# + facet_wrap(~SEG)
#############
## Figure 6

g <- ggplot(data=prem.b, aes(x=Sex,y=Premium,ymin=Premium-2*SEprem,ymax=Premium+2*SEprem)) + facet_wrap(~SEC) + geom_pointrange(size=1,colour="blue") + labs(x=NULL,y="Log hourly wage premium in pounds") + geom_hline(aes(yintercept=0),colour="red") + theme_bw()
pdf("BasePremium.pdf",paper="a4r")
g
dev.off()

##  Table 3
xtable(data.frame(prem.b[,c(1,4)],prem.b[,6]*100))
########


############
#### Table 5 and figure 7
#######################

prem.f <- matrix(, ncol=7)
vn <- c("SEC","Premium","Diff","Hat", "Sex", "SEprem", "PremPct" )
colnames(prem.f) <- vn
prem.f <- data.frame(prem.f)

for (i in 1:length(secs)){
      seg6 <- dta3$sec.rc == secs[i]
      seg.re <- lmer(lhrwage ~ PrivateSect*sex + age + agesq.k + quals + jobsen + fwave + (1|pid) , data=dta3, subset=seg6)
      cat("\n",date(),secs[i],"\n",sep="\n")
#      display(seg.re, digits=3)
      b <- fixef(seg.re)
      vc <- vcov(seg.re)
      k <- length(b)
      b0 <- b[1]
      b1 <- b[2]
      men <- b[3]
      vb1 <- vc[2,2]
      b3 <- b[k]
      vb3 <- vc[k,k]
      cb1b3 <- vc[2,k]
      p.men <- b1 + b3
      p.women <- b1
      d.men <- exp(b0 + b1 + men + b3) - exp(b0 + men)
      d.women <- exp(b0 + b1) - exp(b0)
      hat.pub.men <- exp(b0+b1+men+b3 + b[4]*40 + b[5]*(40^2/1000) + b[6] + b[8]*2 + b[k-1])
      hat.pvt.men <- exp(b0+men + b[4]*40 + b[5]*(40^2/1000) + b[6] + b[8]*2 + b[k-1])
      d.hat.men <- hat.pub.men - hat.pvt.men
      hat.pub.women <- exp(b0+b1+ b[4]*40 + b[5]*(40^2/1000) + b[6] + b[8]*2 + b[k-1])
      hat.pvt.women <- exp(b0 + b[4]*40 + b[5]*(40^2/1000) + b[6] + b[8]*2 + b[k-1])
      d.hat.women <- hat.pub.women - hat.pvt.women
#      p.men.pct <- d.men/exp(b0 + men) 
#      p.women.pct <- d.women/exp(b0) 
      p.men.pct <- hat.pub.men/hat.pvt.men - 1
      p.women.pct <- hat.pub.women/hat.pvt.women - 1
      se.p.men <- sqrt(vb1 + vb3 + 2*cb1b3)
      se.p.women <- sqrt(vb1)
      tmp <- data.frame(secs[i],p.men,d.men,d.hat.men,"Men", se.p.men, p.men.pct)
      tmp2 <- data.frame(secs[i], p.women,d.women,d.hat.women, "Women",se.p.women, p.women.pct )
      names(tmp) <- vn
      names(tmp2) <- vn
      prem.f <- rbind(prem.f,tmp,tmp2)
}
prem.f <- prem.f[-1,]

g <- ggplot(data=prem.f, aes(x=Sex,y=Premium,ymin=Premium-2*SEprem,ymax=Premium+2*SEprem)) + facet_wrap(~SEC) + geom_pointrange(size=1,colour="blue") + labs(x=NULL,y="Log hourly wage premiums in pounds") + geom_hline(aes(yintercept=0),colour="red") + theme_bw()
pdf("FullPremium.pdf", paper="a4r")
g
dev.off()

#gg <- ggplot(data=prem.f, aes(x=Sex,y=PremPct))  + geom_point(aes(colour=SEG),size=2)# + facet_wrap(~SEG)

xtable(data.frame(prem.f[,c(1,5)],prem.f[,7]*100))



##############################
###########
#### Table 5 and figure 7
#######################

prem.f <- matrix(, ncol=7)
vn <- c("SEC","Premium","Diff","Hat", "Sex", "SEprem", "PremPct")
colnames(prem.f) <- vn
prem.f <- data.frame(prem.f)
wages <- matrix(, ncol=4)
colnames(wages) <- c('SEC','Sector','Sex','Wage')
wages <- data.frame(wages)

for (i in 1:length(secs)){
  seg6 <- dta3$sec.rc == secs[i]
  seg.re <- lmer(lhrwage ~ PrivateSect*sex + age + agesq.k + quals + jobsen + fwave + prop.female + (1|pid) , data=dta3, subset=seg6)
  cat("\n",date(),secs[i],"\n",sep="\n")
  #      display(seg.re, digits=3)
  b <- fixef(seg.re)
  vc <- vcov(seg.re)
  k <- length(b)
  b0 <- b[1]
  b1 <- b[2]
  men <- b[3]
  vb1 <- vc[2,2]
  b3 <- b[k]
  vb3 <- vc[k,k]
  cb1b3 <- vc[2,k]
  p.men <- b1 + b3
  p.women <- b1
  d.men <- exp(b0 + b1 + men + b3) - exp(b0 + men)
  d.women <- exp(b0 + b1) - exp(b0)
  hat.pub.men <- exp(b0+b1+men+b3 + b[4]*40 + b[5]*(40^2/1000) + b[6] + b[8]*2 + b[k-1])
  hat.pvt.men <- exp(b0+men + b[4]*40 + b[5]*(40^2/1000) + b[6] + b[8]*2 + b[k-1])
  d.hat.men <- hat.pub.men - hat.pvt.men
  hat.pub.women <- exp(b0+b1+ b[4]*40 + b[5]*(40^2/1000) + b[6] + b[8]*2 + b[k-1])
  hat.pvt.women <- exp(b0 + b[4]*40 + b[5]*(40^2/1000) + b[6] + b[8]*2 + b[k-1])
  d.hat.women <- hat.pub.women - hat.pvt.women
  #      p.men.pct <- d.men/exp(b0 + men) 
  #      p.women.pct <- d.women/exp(b0) 
  p.men.pct <- hat.pub.men/hat.pvt.men - 1
  p.women.pct <- hat.pub.women/hat.pvt.women - 1
  se.p.men <- sqrt(vb1 + vb3 + 2*cb1b3)
  se.p.women <- sqrt(vb1)
  tmp <- data.frame(secs[i],p.men,d.men,d.hat.men,"Men", se.p.men, p.men.pct)
  tmp2 <- data.frame(secs[i], p.women,d.women,d.hat.women, "Women",se.p.women, p.women.pct )
  names(tmp) <- vn
  names(tmp2) <- vn
  prem.f <- rbind(prem.f,tmp,tmp2)
  tmp.w <- data.frame(rep(secs[i],4),c('Public','Public','Private','Private'),c('Men','Women','Men','Women'),c(hat.pub.men,hat.pub.women,hat.pvt.men,hat.pvt.women))
  colnames(tmp.w) <- c('SEC','Sector','Sex','Wage')
  wages <- rbind(wages,tmp.w)

}
prem.f <- prem.f[-1,]
wages <- wages[-1,]

g <- ggplot(data=wages, aes(x=Sex,y=Wage,colour=Sector)) + facet_wrap(~SEC) + geom_point(size=3) + labs(x=NULL,y="Log hourly wage in pounds") + theme_bw()
#pdf("FullPremium.pdf", paper="a4r")
g
  dev.off()

#gg <- ggplot(data=prem.f, aes(x=Sex,y=PremPct))  + geom_point(aes(colour=SEG),size=2)# + facet_wrap(~SEG)

xtable(data.frame(prem.f[,c(1,5)],prem.f[,7]*100))


### Prop tu

prem.f <- matrix(, ncol=7)
vn <- c("SEC","Premium","Diff","Hat", "Sex", "SEprem", "PremPct" )
colnames(prem.f) <- vn
prem.f <- data.frame(prem.f)

for (i in 1:length(secs)){
  seg6 <- dta3$sec.rc == secs[i]
  seg.re <- lmer(lhrwage ~ PrivateSect*sex + age + agesq.k + quals + jobsen + fwave + prop.tu + (1|pid) , data=dta3, subset=seg6)
  cat("\n",date(),secs[i],"\n",sep="\n")
  #      display(seg.re, digits=3)
  b <- fixef(seg.re)
  vc <- vcov(seg.re)
  k <- length(b)
  b0 <- b[1]
  b1 <- b[2]
  men <- b[3]
  vb1 <- vc[2,2]
  b3 <- b[k]
  vb3 <- vc[k,k]
  cb1b3 <- vc[2,k]
  p.men <- b1 + b3
  p.women <- b1
  d.men <- exp(b0 + b1 + men + b3) - exp(b0 + men)
  d.women <- exp(b0 + b1) - exp(b0)
  hat.pub.men <- exp(b0+b1+men+b3 + b[4]*40 + b[5]*(40^2/1000) + b[6] + b[8]*2 + b[k-1])
  hat.pvt.men <- exp(b0+men + b[4]*40 + b[5]*(40^2/1000) + b[6] + b[8]*2 + b[k-1])
  d.hat.men <- hat.pub.men - hat.pvt.men
  hat.pub.women <- exp(b0+b1+ b[4]*40 + b[5]*(40^2/1000) + b[6] + b[8]*2 + b[k-1])
  hat.pvt.women <- exp(b0 + b[4]*40 + b[5]*(40^2/1000) + b[6] + b[8]*2 + b[k-1])
  d.hat.women <- hat.pub.women - hat.pvt.women
  #      p.men.pct <- d.men/exp(b0 + men) 
  #      p.women.pct <- d.women/exp(b0) 
  p.men.pct <- hat.pub.men/hat.pvt.men - 1
  p.women.pct <- hat.pub.women/hat.pvt.women - 1
  se.p.men <- sqrt(vb1 + vb3 + 2*cb1b3)
  se.p.women <- sqrt(vb1)
  tmp <- data.frame(secs[i],p.men,d.men,d.hat.men,"Men", se.p.men, p.men.pct)
  tmp2 <- data.frame(secs[i], p.women,d.women,d.hat.women, "Women",se.p.women, p.women.pct )
  names(tmp) <- vn
  names(tmp2) <- vn
  prem.f <- rbind(prem.f,tmp,tmp2)
}
prem.f <- prem.f[-1,]

g <- ggplot(data=prem.f, aes(x=Sex,y=Premium,ymin=Premium-2*SEprem,ymax=Premium+2*SEprem)) + facet_wrap(~SEC) + geom_pointrange(size=1,colour="blue") + labs(x=NULL,y="Log hourly wage premiums in pounds") + geom_hline(aes(yintercept=0),colour="red") + theme_bw()
#pdf("FullPremium.pdf", paper="a4r")
g
#dev.off()


### Pay levels, not premiums