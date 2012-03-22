dta3 <- read.table("dta4.txt")
prem.b <- matrix(, ncol=5)
vn <- c("SEG","Premium", "Sex", "SEprem", "PremPct" )
colnames(prem.b) <- vn
prem.b <- data.frame(prem.b)
segs <- c(14,19,9,17,18,6,11,8,10,15)
seg <- dta3$jbseg
lv.seg <- levels(seg)
ss <- dta3$hrwage > 0

for (i in 1:length(segs)){
      seg6 <- dta3$jbseg == lv.seg[segs[i]]
      seg.re <- lmer(lhrwage ~ PrivateSect*sex + wave + (1|pid) , data=dta3, subset=seg6)
      cat("\n",date(),lv.seg[segs[i]],"\n",sep="\n")
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
      p.men.pct <- exp(p.men+b0 + men)/exp(b0 + men) - 1
      p.women.pct <- exp(p.women + b0)/exp(b0) - 1
      se.p.men <- sqrt(vb1 + vb3 + 2*cb1b3)
      se.p.women <- sqrt(vb1)
      tmp <- data.frame(lv.seg[segs[i]],p.men,"Men", se.p.men, p.men.pct)
      tmp2 <- data.frame(lv.seg[segs[i]], p.women, "Women",se.p.women, p.women.pct )
      names(tmp) <- vn
      names(tmp2) <- vn
      prem.b <- rbind(prem.b,tmp,tmp2)
}
prem.b <- prem.b[-1,]
g <- ggplot(data=prem.b, aes(x=Sex,y=Premium,ymin=Premium-2*SEprem,ymax=Premium+2*SEprem)) + facet_wrap(~SEG) + geom_pointrange(size=1,colour="blue") + labs(x=NULL,y="Log hourly wages in pounds") + geom_hline(aes(yintercept=0),colour="red") + theme_bw()
gg <- ggplot(data=prem.b, aes(x=Sex,y=PremPct))  + geom_point(aes(colour=SEG),size=2)# + facet_wrap(~SEG)

g <- ggplot(data=prem.b, aes(x=Sex,y=Premium,ymin=Premium-2*SEprem,ymax=Premium+2*SEprem)) + facet_wrap(~SEG) + geom_pointrange(size=1,colour="blue") + labs(x=NULL,y="Log hourly wages in pounds") + geom_hline(aes(yintercept=0),colour="red") + theme_bw()
g

xtable(data.frame(prem.b[,c(1,3)],prem.b[,5]*100))
########

prem.f <- matrix(, ncol=5)
vn <- c("SEG","Premium", "Sex", "SEprem", "PremPct" )
colnames(prem.f) <- vn
prem.f <- data.frame(prem.f)

for (i in 1:length(segs)){
      seg6 <- dta3$jbseg == lv.seg[segs[i]]
      seg.re <- lmer(lhrwage ~ PrivateSect*sex + age + agesq.k + quals + jobsen + wave + (1|pid) , data=dta3, subset=seg6)
      cat("\n",date(),lv.seg[segs[i]],"\n",sep="\n")
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
      p.men.pct <- exp(p.men+b0 + men)/exp(b0 + men) - 1
      p.women.pct <- exp(p.women + b0)/exp(b0) - 1
      se.p.men <- sqrt(vb1 + vb3 + 2*cb1b3)
      se.p.women <- sqrt(vb1)
      tmp <- data.frame(lv.seg[segs[i]],p.men,"Men", se.p.men, p.men.pct)
      tmp2 <- data.frame(lv.seg[segs[i]], p.women, "Women",se.p.women, p.women.pct )
      names(tmp) <- vn
      names(tmp2) <- vn
      prem.f <- rbind(prem.f,tmp,tmp2)
}
prem.f <- prem.f[-1,]



g <- ggplot(data=prem.f, aes(x=Sex,y=Premium,ymin=Premium-2*SEprem,ymax=Premium+2*SEprem)) + facet_wrap(~SEG) + geom_pointrange(size=1,colour="blue") + labs(x=NULL,y="Log hourly wages in pounds") + geom_hline(aes(yintercept=0),colour="red") + theme_bw()
g
gg <- ggplot(data=prem.f, aes(x=Sex,y=PremPct))  + geom_point(aes(colour=SEG),size=2)# + facet_wrap(~SEG)

xtable(data.frame(prem.f[,c(1,3)],prem.f[,5]*100))