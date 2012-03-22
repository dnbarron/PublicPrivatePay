library(foreign)
#Rprof(memory.profiling=TRUE)
keep.vars <- function(varnms,datanm){
    vartab <- names(datanm)
    ix <- match(varnms,vartab)
    if (any(is.na(ix))) stop(varnms[is.na(ix)], " not found")
    datanm[,ix]
}
setwd("c:/users/dbarron/my documents/my dropbox/bhps")

vn <- c("hid","pno","age","sex","mlstat","qfachi","feend","scend",
"jbtime","jbhrs","jshrs","jbhgs","jbisco","jbsic","jbsoc","j2has","jbbgd",
"jbbgm","jbbgy4","jbft","jbsemp","jbstat","jbterm","cjsten","jbmngr","jbhad",
"njbs","orgmb","nchild","jbgold","jbsat","jbsect","jbrgsc","jbseg","paygu")

# Wave 1

wave1 <- read.dta("aindresp.dta")
vars <- c("pid","ahid","apno","aage","asex","amlstat","aqfachi","afeend","ascend",
"ajbtime","ajbhrs","ajshrs","ajbhgs","ajbisco","ajbsic","ajbsoc","aj2has","ajbbgd",
"ajbbgm","ajbbgy4","ajbft","ajbsemp","ajbstat","ajbterm","acjsten","ajbmngr","ajbhad",
"anjbs","aorgmb","anchild","ajbgold","ajbsat","ajbsect","ajbrgsc","ajbseg","apaygu","arace")

#vars <- c("pid",paste("a",vn,sep=""))

awave <- keep.vars(vars,wave1)
awave <- data.frame(awave,year=1991)
names(awave)[length(names(awave))] <- "adoiy4"
rm(wave1)

# Wave 2
wave2 <- read.dta("bindresp.dta")
vars <- c("pid","bhid","bpno","bage","bsex","bmlstat","bqfachi","bfeend","bscend",
"bjbtime","bjbhrs","bjshrs","bjbhgs","bjbisco","bjbsic","bjbsoc","bj2has","bjbbgd",
"bjbbgm","bjbbgy4","bjbft","bjbsemp","bjbstat","bjbterm","bcjsten","bjbmngr","bjbhad",
"bnjbs","borgmb","bnchild","bjbgold","bjbsat","bjbsect","bjbrgsc","bjbseg","bpaygu","brace","bdoiy4")

bwave <- keep.vars(vars,wave2)

rm(wave2)

# Wave 3

wave3 <- read.dta("cindresp.dta")
vars <- c("pid","chid","cpno","cage","csex","cmlstat","cqfachi","cfeend","cscend",
"cjbtime","cjbhrs","cjshrs","cjbhgs","cjbisco","cjbsic","cjbsoc","cj2has","cjbbgd",
"cjbbgm","cjbbgy4","cjbft","cjbsemp","cjbstat","cjbterm","ccjsten","cjbmngr","cjbhad",
"cnjbs","corgmb","cnchild","cjbgold","cjbsat","cjbsect","cjbrgsc","cjbseg","cpaygu","crace","cdoiy4")

cwave <- keep.vars(vars,wave3)

rm(wave3)

# Wave 4

wave4 <- read.dta("dindresp.dta")
vars <- c("pid","dhid","dpno","dage","dsex","dmlstat","dqfachi","dfeend","dscend",
"djbtime","djbhrs","djshrs","djbhgs","djbisco","djbsic","djbsoc","dj2has","djbbgd",
"djbbgm","djbbgy4","djbft","djbsemp","djbstat","djbterm","dcjsten","djbmngr","djbhad",
"dnjbs","dorgmb","dnchild","djbgold","djbsat","djbsect","djbrgsc","djbseg","dpaygu","drace","ddoiy4")

dwave <- keep.vars(vars,wave4)

rm(wave4)

# Wave 5

wave5 <- read.dta("eindresp.dta")
vars <- c("pid","ehid","epno","eage","esex","emlstat","eqfachi","efeend","escend",
"ejbtime","ejbhrs","ejshrs","ejbhgs","ejbisco","ejbsic","ejbsoc","ej2has","ejbbgd",
"ejbbgm","ejbbgy4","ejbft","ejbsemp","ejbstat","ejbterm","ecjsten","ejbmngr","ejbhad",
"enjbs","eorgmb","enchild","ejbgold","ejbsat","ejbsect","ejbrgsc","ejbseg","epaygu","erace","edoiy4")

ewave <- keep.vars(vars,wave5)

rm(wave5)

# Wave 6

wave6 <- read.dta("findresp.dta")
vars <- c("pid","fhid","fpno","fage","fsex","fmlstat","fqfachi","ffeend","fscend",
"fjbtime","fjbhrs","fjshrs","fjbhgs","fjbisco","fjbsic","fjbsoc","fj2has","fjbbgd",
"fjbbgm","fjbbgy4","fjbft","fjbsemp","fjbstat","fjbterm","fcjsten","fjbmngr","fjbhad",
"fnjbs","fnchild","fjbgold","fjbsat","fjbsect","fjbrgsc","fjbseg","fpaygu","frace","fdoiy4")

fwave <- keep.vars(vars,wave6)

rm(wave6)

nc <- dim(fwave)[2]

fwave <- cbind(fwave[,1:28],NA,fwave[,29:nc])
names(fwave)[29] <- "forgmb"

# Wave 7

wave7 <- read.dta("gindresp.dta")
vars <- c("pid","ghid","gpno","gage","gsex","gmlstat","gqfachi","gfeend","gscend",
"gjbtime","gjbhrs","gjshrs","gjbhgs","gjbisco","gjbsic","gjbsoc","gj2has","gjbbgd",
"gjbbgm","gjbbgy4","gjbft","gjbsemp","gjbstat","gjbterm","gcjsten","gjbmngr","gjbhad",
"gnjbs","gorgmb","gnchild","gjbgold","gjbsat","gjbsect","gjbrgsc","gjbseg","gpaygu","grace","gdoiy4")

gwave <- keep.vars(vars,wave7)

rm(wave7)


# Wave 8

wave8 <- read.dta("hindresp.dta")
vars <- c("pid","hhid","hpno","hage","hsex","hmlstat","hqfachi","hfeend","hscend",
"hjbtime","hjbhrs","hjshrs","hjbhgs","hjbisco","hjbsic","hjbsoc","hj2has","hjbbgd",
"hjbbgm","hjbbgy4","hjbft","hjbsemp","hjbstat","hjbterm","hcjsten","hjbmngr","hjbhad",
"hnjbs","hnchild","hjbgold","hjbsat","hjbsect","hjbrgsc","hjbseg","hpaygu","hrace","hdoiy4")

hwave <- keep.vars(vars,wave8)

rm(wave8)

nc <- dim(hwave)[2]

hwave <- cbind(hwave[,1:28],NA,hwave[,29:nc])
names(hwave)[29] <- "horgmb"


# Wave 9

wave9 <- read.dta("iindresp.dta")
vars <- c("pid","ihid","ipno","iage","isex","imlstat","iqfachi","ifeend","iscend",
"ijbtime","ijbhrs","ijshrs","ijbhgs","ijbisco","ijbsic","ijbsoc","ij2has","ijbbgd",
"ijbbgm","ijbbgy4","ijbft","ijbsemp","ijbstat","ijbterm1","icjsten","ijbmngr","ijbhad",
"injbs","iorgmb","inchild","ijbgold","ijbsat","ijbsect","ijbrgsc","ijbseg","ipaygu","irace","idoiy4")

iwave <- keep.vars(vars,wave9)

rm(wave9)

# Wave 10

wave10 <- read.dta("jindresp.dta")
vars <- c("pid","jhid","jpno","jage","jsex","jmlstat","jqfachi","jfeend","jscend",
"jjbtime","jjbhrs","jjshrs","jjbhgs","jjbisco","jjbsic","jjbsoc","jj2has","jjbbgd",
"jjbbgm","jjbbgy4","jjbft","jjbsemp","jjbstat","jjbterm1","jcjsten","jjbmngr","jjbhad",
"jnjbs","jnchild","jjbgold","jjbsat","jjbsect","jjbrgsc","jjbseg","jpaygu","jrace","jdoiy4")

jwave <- keep.vars(vars,wave10)

rm(wave10)

nc <- dim(jwave)[2]

jwave <- cbind(jwave[,1:28],NA,jwave[,29:nc])
names(jwave)[29] <- "jorgmb"


# Wave 11

wave11 <- read.dta("kindresp.dta")
vars <- c("pid","khid","kpno","kage","ksex","kmlstat","kqfachi","kfeend","kscend",
"kjbtime","kjbhrs","kjshrs","kjbhgs","kjbisco","kjbsic","kjbsoc","kj2has","kjbbgd",
"kjbbgm","kjbbgy4","kjbft","kjbsemp","kjbstat","kjbterm1","kcjsten","kjbmngr","kjbhad",
"knjbs","korgmb","knchild","kjbgold","kjbsat","kjbsect","kjbrgsc","kjbseg","kpaygu","krace","kdoiy4")

kwave <- keep.vars(vars,wave11)

rm(wave11)

# Wave 12

wave12 <- read.dta("lindresp.dta")
vars <- c("pid","lhid","lpno","lage","lsex","lmlstat","lqfachi","lfeend","lscend",
"ljbtime","ljbhrs","ljshrs","ljbhgs","ljbisco","ljbsic92","ljbsoc","lj2has","ljbbgd",
"ljbbgm","ljbbgy4","ljbft","ljbsemp","ljbstat","ljbterm1","lcjsten","ljbmngr","ljbhad",
"lnjbs","lnchild","ljbgold","ljbsat","ljbsect","ljbrgsc","ljbseg","lpaygu","lrace","ldoiy4")

lwave <- keep.vars(vars,wave12)

rm(wave12)

nc <- dim(lwave)[2]

lwave <- cbind(lwave[,1:28],NA,lwave[,29:nc])
names(lwave)[29] <- "lorgmb"
names(lwave)[15] <- "ljbsic"

# Wave 13

wave13 <- read.dta("mindresp.dta")
vars <- c("pid","mhid","mpno","mage","msex","mmlstat","mqfachi","mfeend","mscend",
"mjbhrs","mjshrs","mjbhgs","mjbisco","mjbsoc","mj2has","mjbbgd",
"mjbbgm","mjbbgy4","mjbft","mjbsemp","mjbstat","mjbterm1","mcjsten","mjbmngr","mjbhad",
"mnjbs","morgmb","mnchild","mjbgold","mjbsat","mjbsect","mjbrgsc","mjbseg","mpaygu","mracel","mdoiy4")

mwave <- keep.vars(vars,wave13)

rm(wave13)

nc <- dim(mwave)[2]

mwave <- cbind(mwave[,1:9],NA,mwave[,10:13],NA,mwave[,14:nc])
names(mwave)[10] <- "mjbtime"
names(mwave)[15] <- "mjbsic"
names(mwave)[37] <- "mrace"

# Wave 14

wave14 <- read.dta("nindresp.dta")
# jbsic, njbterm, norgmb not in this data, positions 15, 24, 29
vars <- c("pid","nhid","npno","nage","nsex","nmlstat","nqfachi","nfeend","nscend",
"njbtime","njbhrs","njshrs","njbhgs","njbisco","njbsoc","nj2has","njbbgd",
"njbbgm","njbbgy4","njbft","njbsemp","njbstat","ncjsten","njbmngr","njbhad",
"nnjbs","nnchild","njbgold","njbsat","njbsect","njbrgsc","njbseg","npaygu","nracel","ndoiy4")

nwave <- keep.vars(vars,wave14)

rm(wave14)

nc <- dim(nwave)[2]

nwave <- cbind(nwave[,1:14],NA,nwave[,15:nc])
names(nwave)[15] <- "njbsic"

nc <- dim(nwave)[2]
nwave <- cbind(nwave[,1:23],NA,nwave[,24:nc])
names(nwave)[24] <- "njbterm"

nc <- dim(nwave)[2]
nwave <- cbind(nwave[,1:28],NA,nwave[,29:nc])
names(nwave)[29] <- "norgmb"
names(nwave)[37] <- "nrace"

# Wave 15

wave15 <- read.dta("oindresp.dta")

vars <- c("pid","ohid","opno","oage","osex","omlstat","oqfachi","ofeend","oscend",
"ojbtime","ojbhrs","ojshrs","ojbhgs","ojbisco","ojbsoc","oj2has","ojbbgd",
"ojbbgm","ojbbgy4","ojbft","ojbsemp","ojbstat","ocjsten","ojbmngr","ojbhad",
"onjbs","oorgmb","onchild","ojbgold","ojbsat","ojbsect","ojbrgsc","ojbseg","opaygu","oracel","odoiy4")

owave <- keep.vars(vars,wave15)

rm(wave15)

nc <- dim(owave)[2]

owave <- cbind(owave[,1:14],NA,owave[,15:nc])
names(owave)[15] <- "ojbsic"

nc <- dim(owave)[2]
owave <- cbind(owave[,1:23],NA,owave[,24:nc])
names(owave)[24] <- "ojbterm"
names(owave)[37] <- "orace"

# Wave 16

wave16 <- read.dta("pindresp.dta")

vars <- c("pid","phid","ppno","page","psex","pmlstat","pqfachi","pfeend","pscend",
"pjbtime","pjbhrs","pjshrs","pjbhgs","pjbisco","pjbsoc","pj2has","pjbbgd",
"pjbbgm","pjbbgy4","pjbft","pjbsemp","pjbstat","pcjsten","pjbmngr","pjbhad",
"pnchild","pjbgold","pjbsat","pjbsect","pjbrgsc","pjbseg","ppaygu","pracel","pdoiy4")

pwave <- keep.vars(vars,wave16)

rm(wave16)

nc <- dim(pwave)[2]

pwave <- cbind(pwave[,1:14],NA,pwave[,15:nc])
names(pwave)[15] <- "pjbsic"

nc <- dim(pwave)[2]
pwave <- cbind(pwave[,1:23],NA,pwave[,24:nc])
names(pwave)[24] <- "pjbterm"

nc <- dim(pwave)[2]
pwave <- cbind(pwave[,1:27],NA,pwave[,28:nc])
names(pwave)[28] <- "pnjbs"

nc <- dim(pwave)[2]
pwave <- cbind(pwave[,1:28],NA,pwave[,29:nc])
names(pwave)[29] <- "porgmb"
names(pwave)[37] <- "prace"


# Wave 17

wave17 <- read.dta("qindresp.dta")
vars <- c("pid","qhid","qpno","qage","qsex","qmlstat","qqfachi","qfeend","qscend",
"qjbtime","qjbhrs","qjshrs","qjbhgs","qjbisco","qjbsoc","qj2has","qjbbgd",
"qjbbgm","qjbbgy4","qjbft","qjbsemp","qjbstat","qcjsten","qjbmngr","qjbhad",
"qorgmb","qnchild","qjbgold","qjbsat","qjbsect","qjbrgsc","qjbseg","qpaygu","qracel","qdoiy4")

qwave <- keep.vars(vars,wave17)

rm(wave17)

nc <- dim(qwave)[2]
qwave <- cbind(qwave[,1:14],NA,qwave[,15:nc])
names(qwave)[15] <- "qjbsic"

nc <- dim(qwave)[2]
qwave <- cbind(qwave[,1:23],NA,qwave[,24:nc])
names(qwave)[24] <- "qjbterm"

nc <- dim(qwave)[2]
qwave <- cbind(qwave[,1:27],NA,qwave[,28:nc])
names(qwave)[26] <- "qnjbs"
names(qwave)[37] <- "qrace"

# Wave 18

wave18 <- read.dta("rindresp.dta")
vars <- c("pid","rhid","rpno","rage","rsex","rmlstat","rqfachi","rfeend","rscend",
"rjbtime","rjbhrs","rjshrs","rjbhgs","rjbisco","rjbsoc","rj2has","rjbbgd",
"rjbbgm","rjbbgy4","rjbft","rjbsemp","rjbstat","rcjsten","rjbmngr","rjbhad",
"rnchild","rjbgold","rjbsat","rjbsect","rjbrgsc","rjbseg","rpaygu","rracel","rdoiy4")

rwave <- keep.vars(vars,wave18)

rm(wave18)

nc <- dim(rwave)[2]
rwave <- cbind(rwave[,1:14],NA,rwave[,15:nc])
names(rwave)[15] <- "rjbsic"

nc <- dim(rwave)[2]
rwave <- cbind(rwave[,1:23],NA,rwave[,24:nc])
names(rwave)[24] <- "rjbterm"

nc <- dim(rwave)[2]
rwave <- cbind(rwave[,1:27],NA,rwave[,28:nc])
names(rwave)[26] <- "rnjbs"
          
nc <- dim(rwave)[2]
rwave <- cbind(rwave[,1:28],NA,rwave[,29:nc])
names(rwave)[29] <- "rorgmb"
names(rwave)[37] <- "rrace"


awave <- cbind(awave,1)
bwave <- cbind(bwave,2)
cwave <- cbind(cwave,3)
dwave <- cbind(dwave,4)
ewave <- cbind(ewave,5)
fwave <- cbind(fwave,6)
gwave <- cbind(gwave,7)
hwave <- cbind(hwave,8)
iwave <- cbind(iwave,9)
jwave <- cbind(jwave,10)
kwave <- cbind(kwave,11)
lwave <- cbind(lwave,12)
mwave <- cbind(mwave,13)
nwave <- cbind(nwave,14)
owave <- cbind(owave,15)
pwave <- cbind(pwave,16)
qwave <- cbind(qwave,17)
rwave <- cbind(rwave,18)

newnms <- substring(names(fwave),2)
nc <- length(newnms)
newnms[1] <- "pid"
newnms[nc] <- "wave"

stk <- function(d1,d2,vn){
    names(d1) <- vn
    names(d2) <- vn
    rbind(d1,d2)
}

dta <- stk(awave,bwave,newnms)
dta <- stk(dta,cwave,newnms)
dta <- stk(dta,dwave,newnms)
dta <- stk(dta,ewave,newnms)
dta <- stk(dta,fwave,newnms)
dta <- stk(dta,gwave,newnms)
dta <- stk(dta,hwave,newnms)
dta <- stk(dta,iwave,newnms)
dta <- stk(dta,jwave,newnms)
dta <- stk(dta,kwave,newnms)
dta <- stk(dta,lwave,newnms)
dta <- stk(dta,mwave,newnms)
dta <- stk(dta,nwave,newnms)
dta <- stk(dta,owave,newnms)
dta <- stk(dta,pwave,newnms)
dta <- stk(dta,qwave,newnms)
dta <- stk(dta,rwave,newnms)

#by(dta$hrwage,dta$jbsoc,mean,na.rm=TRUE)

rm(awave,bwave,cwave,dwave,ewave,fwave,gwave,hwave,iwave,jwave,kwave,lwave,mwave,nwave,owave,pwave,qwave,rwave)
rm(keep.vars,newnms,stk,vars,nc,vn)

write.csv(dta,"dta1.18.csv")