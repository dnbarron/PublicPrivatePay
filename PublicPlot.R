pl.segs <- c("Personal service","Unskilled manual","Semi-skilled, manual",
"Junior non-manual","Skilled manual","Int. non-man, foreman",
"Foreman manual","Managers, small","Int. non-manual",
"Professional empolyees","Managers, large")

b0 <- c(0.115,0.093,0.081,0.033,0.021,0.157,0.049,0.031,0.016,-0.114,0.002)
b0.se <- c(0.017,0.019,0.013,0.01,0.02,0.022,0.022,0.025,0.012,0.025,0.016)

const <- c(0.955,1.142,1.067,1.181,0.843,1.368,1.25,1.17,1.202,1.456,1.38)
public <- c(0.118,0.116,0.18,0.024,0.097,0.154,0.162,0.061,0.04,-0.1,0.058)
male <- c(0.061,0.087,0.173,0.079,0.244,0.144,0.258,0.202,0.148,0.073,0.193)
int <- c(-0.031,-0.077,-0.192,0.041,-0.085,0.008,-0.154,-0.072,-0.068,-0.021,-0.108)

pvt.fem <- const
pub.fem <- const + public
pvt.male <- const + male
pub.male <- const + public + male + int
ll <- gl(4,11,labels=c("Private, Female","Public, Female","Private, Male","Public, Male"))
seg <- gl(11,k=1,length=44,labels=pl.segs)
plot.data <- data.frame(SEG=seg,Type=ll,Prediction=c(pvt.fem,pub.fem,pvt.male,pub.male))
ix <- order(plot.data$Prediction)
plot.data <- plot.data[ix,]
nn <- ggplot(data=plot.data,aes(x=SEG))
nn + geom_point(aes(y=Prediction,shape=Type)) + coord_flip()

plt.dta <- data.frame(SEG=as.character(pl.segs),Public0=b0,Public0se=b0.se,Public1=b0.1,
  Public1se=b0.1se,Male=male,Malese=male.se,Interaction=intact,Interactionse=intact.se)

plt.dta2 <- transform(plt.dta,pred=Public1+Male+Interaction)


pp <- ggplot(data=plt.dta2, aes(x=SEG))
pp + geom_pointrange(aes(y=Public0,ymin=Public0-2*Public0se,ymax=Public0+2*Public0se))
pp + geom_point(aes(y=Public1)) + geom_point(aes(y=pred),colour="red")

pp + geom_pointrange(aes(y=Public1))


pvt <- c(0.068,0.042,0.115,0.247,0.142,0.043,0.054,0.071,0.078,0.041,0.100)
pblc <- c(0.095,0.041,0.072,0.192,0.025,0.038,0.024,0.036,0.306,0.064,0.108)

bar.dta <- data.frame(Employees=c(pvt,pblc),Sector=gl(2,11,labels=c("Private","Public")),
      SEG=gl(11,k=1,length=22,labels=pl.segs))
bb <- ggplot(data=bar.dta,aes(x=SEG)) + opts(axis.text.x=theme_text(size=15),axis.text.y=theme_text(size=15),axis.title.x=theme_text(size=15,vjust=0)) + theme_bw()

bb + geom_bar(aes(y=Employees,fill=Sector),position="dodge") + coord_flip() + xlab("") + scale_y_continuous(name="Employees",formatter="percent")

xtabs(~jbseg+PrivateSect,data=dta3,subset=wave==17&ss)

#### baseline

b0 <- c(1.16,1.28,1.43,1.46,1.61,1.78,1.82,1.85,2.04,2.13)
b1 <- c(.171,.074,.137,.095,.037,.046,.126,.042,.065,-.045)

(b1)/b0

levels(dta3$jbseg) <- str_trim(dta3$jbseg)
ss.pl <- pl.segs %in% dta3$jbseg

pl.segs <- c("personal service wks","unskilled manual","semi-skilled, manual",
"junior non-manual","skilled manual","int. Non-man, foreman",
"foreman manual","managers, small","int. Non-manual",
"professional empolyees","managers, large")
emp.wv17 <- c(357,234,
              133,66,
              460,140,
              1130,421,
              624,39,
              157,65,
              204,36,
              319,68,
              548,865,
              143,143,
              389,118)
emp.wv17 <- matrix(emp.wv17,ncol=2,byrow=TRUE)
emp.wv1 <- c(168,122,
             138,91,
             386,111,
             863,269,
             564,57,
             195,74,
             211,56,
             200,47,
             206,398,
             144,83,
             277,146)
emp.wv1 <- matrix(emp.wv1,ncol=2,byrow=TRUE)
pr.emp.wv1 <- prop.table(emp.wv1,2)
pr.emp.wv17 <- prop.table(emp.wv17,2)
apply(pr.emp.wv17,2,sum)


bar.dta2 <- data.frame(Employees=c(as.vector(pr.emp.wv1),as.vector(pr.emp.wv17)),
                       Sector=gl(2,11,44,labels=c("Private","Public")),
      SEG=gl(11,k=1,length=44,labels=pl.segs),
      Year=gl(2,22,labels=c("1991","2008")))

bb2 <- ggplot(data=bar.dta2,aes(x=SEG,y=Employees)) + opts(axis.text.x=theme_text(size=15),axis.text.y=theme_text(size=15),axis.title.x=theme_text(size=15,vjust=0)) + theme_bw()
bb2 + geom_bar(aes(fill=Sector),position="dodge") + coord_flip() + facet_grid(.~Year) + scale_y_continuous(name="Employees",formatter="percent") + xlab("")

const <- c(.955,1.14,1.07,1.18,.843,1.25,1.17,1.20,1.46,1.38)
pub <- c(.118,.116,.180,.024,.097,.162,.061,.040,-.100,.058)
sgs <- c("Personal service","Unskilled manual","Junior non-manual","Semi-skilled manual","Skilled manual","Foreman, manual","Manager, small","Intermediate non-manual","Manager, large", "Professional")
pdta <- data.frame(LHRWAGE=c(const,const+pub),Sector=gl(2,10,labels=c("Private","Public")),SEG=c(sgs,sgs))
gg <- ggplot(data=pdta,aes(x=SEG))
gg + geom_point(aes(y=LHRWAGE,colour=Sector)) + coord_flip()