library(car)
library(lme4)

setwd("c:/documents and settings/dbarron/my documents/financial cost of caring")
dta <- read.csv("dta1.18.csv")
xtabs(~jbsect,dta)
#jbsect
#        armed forces civil srv/cntrl govt local govt/town hall 
#                 660                 5121                17361 
# natnalised industry   nhs or higher educ     non-profit orgs. 
#                  826                 9655                 3970 
#                other private firm/company 
#                 1144                84105 

vars <- names(dta)
# Deal with missing data

lv <- levels(dta$mlstat)  
# levels 2, 5, 7, 9, 10 are missing
ix <- dta$mlstat %in% lv[c(2,5,7,9,10)]
dta$mlstat[ix] <- NA
dta$mlstat <- dta$mlstat[drop=TRUE]
with(dta,table(mlstat,useNA="a"))

lv <- levels(dta$qfachi)  
# levels 6,7,10,11 are missing
ix <- dta$qfachi %in% lv[c(6,7,10,11)]
dta$qfachi[ix] <- NA
dta$qfachi <- dta$qfachi[drop=TRUE]
with(dta,table(qfachi, useNA="a"))

ix <- dta$feend <= 0
dta$feend[ix] <- NA
with(dta, summary(feend))

ix <- dta$scend <= 0
dta$scend[ix] <- NA
with(dta, summary(scend))

lv <- levels(dta$jbtime)
# levels 5,8,9,11,14,15 are missing
ix <- dta$jbtime %in% lv[c(5,6,9,11,14,15)]
dta$jbtime[ix] <- NA
dta$jbtime <- dta$jbtime[, drop=TRUE]

ix <- dta$jbhrs <= 0
dta$jbhrs[ix]  <-  NA

ix <- dta$jshrs <= 0
dta$jshrs[ix]  <-  NA

ix <- dta$jbhgs <= 0
dta$jbhgs[ix] <- NA

ix <- dta$jbsoc < 0
dta$jbsoc[ix] <- NA

lv <- levels(dta$j2has)
# Levels 1,3,4,5,6 are missing
ix <- dta$j2has %in% lv[c(1,3:6)]
dta$j2has[ix] <- NA
dta$j2has <- dta$j2has[drop=TRUE]
with(dta, table(j2has, useNA="a"))

ix <- dta$jbbgd <= 0
dta$jbbgd[ix] <- NA

lv <- levels(dta$jbbgm)
#Levels 5,7,13,16,17,18 are missing
ix <- dta$jbbgm %in% lv[c(5,7,13,16:18)]
dta$jbbgm[ix] <- NA
dta$jbbgm <- dta$jbbgm[drop=TRUE]
with(dta, table(jbbgm, useNA="a"))  

ix <- dta$jbbgy4 <= 0
dta$jbbgy4[ix] <- NA

lv <- levels(dta$jbft)
# Levels 2,3,5 are missing
ix <- dta$jbft %in% lv[c(2,3,5)]
dta$jbft[ix] <- NA
dta$jbft <- dta$jbft[, drop=TRUE]
with(dta, table(jbft, useNA="a"))

lv <- levels(dta$jbsemp)
#Levels 1,3,4,5,6 are missing
ix <- dta$jbsemp %in% lv[c(1,3:6)]
dta$jbsemp[ix] <- NA
dta$jbsemp <- dta$jbsemp[drop=TRUE]
with(dta, table(jbsemp, useNA="a"))

lv <- levels(dta$jbstat)
# Levels 1,12,13,16,17 are missing
ix <- dta$jbstat %in% lv[c(1,12,13,16,17)]
dta$jbstat[ix] <- NA
dta$jbstat <- dta$jbstat[drop=TRUE]

lv <- levels(dta$jbterm)
# Levels 2,3,4,5,8,9 missing
ix <- dta$jbterm %in% lv[c(2:5,8,9)]
dta$jbterm[ix] <- NA
dta$jbterm <- dta$jbterm[, drop=TRUE]
with(dta, table(jbterm, useNA="a"))

ix <- dta$cjsten <= 0
dta$cjsten[ix] <- NA

lv <- levels(dta$jbmngr)
#Levels 1,3,5,6,8 missing
ix <- dta$jbmngr %in% lv[c(1,3,5,6,8)]
dta$jbmngr[ix] <- NA
dta$jbmngr <- dta$jbmngr[, drop=TRUE]
with(dta, table(jbmngr, useNA="a"))

lv <- levels(dta$jbhad)
# Levels 1,2,4-6 missing
ix <- dta$jbhad %in% lv[c(1,2,4:6)]
dta$jbhad[ix] <- NA
dta$jbhad <- dta$jbhad[drop=TRUE]
with(dta, table(jbhad, useNA="a"))

ix <- dta$njbs < 0
dta$njbs[ix] <- NA
dta$njbs <- as.integer(dta$njbs)

lv <- levels(dta$jbgold)
# Levels 2,5,6,7,8 are missing
ix <- dta$jbgold %in% lv[c(2,5:8)]
dta$jbgold[ix] <- NA
dta$jbgold <- dta$jbgold[drop=TRUE]
with(dta, table(jbgold, useNA="a"))

ix <- dta$jbsat <= 0
dta$jbsat[ix] <- NA

lv <- levels(dta$jbsect)
#Levels 3,4,6,10,13 are missing
ix <- dta$jbsect %in% lv[c(3,4,6,10,13)]
dta$jbsect[ix] <- NA
dta$jbsect <- dta$jbsect[, drop=TRUE]
with(dta, table(jbsect, useNA="a"))

lv <- levels(dta$jbrgsc)
#Levels 2,3,5 are missing
ix <- dta$jbrgsc %in% lv[c(2,3,5)]
dta$jbrgsc[ix] <- NA
dta$jbrgsc <- dta$jbrgsc[drop=TRUE]
with(dta, table(jbrgsc, useNA="a"))

lv <- levels(dta$jbseg)
# Levels 2,8,15,16,17 missing
ix <- dta$jbseg %in% lv[c(2,8,15:17)]
dta$jbseg[ix] <- NA
dta$jbseg <- dta$jbseg[, drop=TRUE]
with(dta, table(jbseg, useNA="a"))

ix <- dta$paygu <= 0
dta$paygu[ix] <- NA

hrwage <- dta$paygu/(4*dta$jbhrs)
lhrwage <- log(hrwage)
dta$hrwage <- hrwage
dta$lhrwage <- lhrwage

plot(density(lhrwage,na.rm=TRUE))

jobsen <- (dta$wave+1990) - dta$jbbgy4
jobsen[jobsen<0] <- 0   
dta$jobsen <- jobsen

agesq.k <- dta$age^2/1000
dta$agesq.k <- agesq.k

lvs <- levels(dta$mlstat)
    
sing <- recode(dta$mlstat,"'married'='married';'in a civil partnership'='married';'separated'='single';'divorced'='single';
    'widowed'='single';'never married'='single';'have a dissolved civit partnership'='single';
               else=NA")
dta$sing <- sing    

lv <- levels(dta$jbmngr)
manager <- recode(dta$jbmngr,"'manager'='Manager';'foreman/supervisor'='Manager';
  'not mnger/supervisor'='Not manager';else=NA")
with(dta, table(jbmngr,manager, useNA="a"))
dta$manager <- manager

lv <- levels(dta$jbstat)
in.emp <- ifelse(dta$jbstat==lv[1]|dta$jbstat==lv[7]|dta$jbstat==lv[11]|
  dta$jbstat==lv[10],TRUE,FALSE)
in.emp <- in.emp[drop=TRUE]
dta$in.emp <- in.emp
with(dta, table(in.emp, useNA="a"))

prob1 <- glm(in.emp ~ sex*sing + age + agesq.k + nchild  + qfachi , family=binomial(link="probit"), data=dta, na.action=na.exclude)


lp <- naresid(prob1$na.action,prob1$linear.predictors)

imr <- dnorm(lp)/pnorm(lp)
dta$imr <- imr      
      
cpi <- data.frame(year=c(1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,
2005,2006,2007,2008,2009),
cpi=c(63.5,66.8 ,71.5 ,76.8 ,80.1 ,82.1 ,83.8 ,86 ,88.1 ,89.7 ,91.1 ,92.3 ,93.1 ,94.2 ,95.4 ,96.7 ,98 ,
100 ,102.3,104.7,108.5,110.8))

dta2 <- merge(dta,cpi,by.x="doiy4",by.y="year",all.x=TRUE)

lv <- levels(dta2$sex)
# Levels 3 are missing
ix <- dta2$sex %in% lv[c(3)]
dta2$sex[ix] <- NA
dta2$sex <- dta2$sex[drop=TRUE]
with(dta2,table(sex, useNA="a"))

dta2$real.hrwage <- exp(dta2$lhrwage)/dta2$cpi
dta2$real.lhrwage <- log(dta2$real.hrwage)

tmp <- dta2[,c("pid","race","wave")]
ixo <- order(tmp[,1],tmp[,3])
tmp <- tmp[ixo,]
lv <- levels(tmp[,2])
ix <- tmp$race %in% lv[c(12,13,15,16,26:28)]
tmp$race[ix] <- NA
repNA <- function(x){
    if(any(is.na(x))){
      if(!all(is.na(x))){
        t <- x[complete.cases(x)]
        t[1] 
        }
      else NA
    }
    else x[1]
}
rc <- tapply(tmp$race,tmp$pid,repNA)
rc <- factor(rc, labels=lv[-c(12,13,15,16,26:28)])
rc2 <- data.frame(pid=names(rc),race.new=rc)
dta3 <- merge(dta2,rc2,by="pid",all=TRUE)


with(dta3, table(race.new, useNA="a"))
lv <- levels(dta3$race.new)
library(stringr)
ix <- str_detect(lv,"white")
ix <- ix[-1]
white.ix <- dta3$race.new %in% lv[ix]
white <- ifelse(white.ix,"White","Other")
white[is.na(dta3$race.new)] <- NA
dta3$white <- white

write.table(dta3,"dta3.txt")

rm(sex,lv,ix,real.hrwage,real.lhrwage)
rm(agesq.k,cpi,dta,ended,hrwage,imr,in.emp)
rm(jobsen,jobsen.p,lhrwage,log.jobsen.p,lp,lvs,manager,nurse,prob1,schlteacher,sector,semp,sing,teacher,vars,welfare)
rm(ix,lv,real.hrwage,real.lhrwage,sex)

with(dta2,table(sex, useNA="always"))
with(dta2,table(jbsect, useNA="always"))
table(PrivateSect, useNA="always")
with(dta2,table(PrivateSect,sex, useNA="always"))
table(PS.sex,useNA="always")
with(dta2,summary(paygu))
with(dta2,summary(hrwage))
with(dta2,summary(lhrwage))
with(dta2, table(qfachi, useNA="always"))
with(dta2, table(quals, useNA="always"))
with(dta2, summary(jobsen))
with(dta2, table(jbbgy4, useNA="always"))
with(dta2, table(PrivateSect,jbsoc, useNA="a"))
