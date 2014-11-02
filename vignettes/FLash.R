
## ------------------------------------------------------------------------
library(FLCore)
library(FLash)
library(FLBRP)
library(ggplotFL)
library(plyr)


## ------------------------------------------------------------------------
data(ple4)


## ------------------------------------------------------------------------
#### SRR
sr   =as.FLSR(ple4,model="bevholt")
sr   =fmle(sr,control=list(silent=TRUE))

plot(sr)


## ------------------------------------------------------------------------
#### BRPs
eql=FLBRP(ple4,sr=sr)
computeRefpts(eql)
eql=brp(eql)

plot(eql)

## ------------------------------------------------------------------------
ggplot(FLQuants(eql,"m","mat","stock.wt","catch.sel"))+
  geom_line(aes(age,data))+
  facet_wrap(~qname,scale="free_y")


## ----,echo=FALSE---------------------------------------------------------
stk=ple4
save(stk,eql,sr,file="/tmp/flash.RData")


## ------------------------------------------------------------------------
stk=fwdWindow(ple4,end=2020,eql)


## ------------------------------------------------------------------------
refpts(eql)["msy",c("harvest","yield","rec","ssb")]

benchMarks=refpts(eql)["msy",c("harvest","yield","rec","ssb")]
f0.1      =refpts(eql)["f0.1","harvest",drop=T]

## ------------------------------------------------------------------------
hvt=FLQuant(f0.1,dimnames=list(year=2009:2020))
stk=fwd(stk,f=hvt,sr=sr)

plot(stk)+
  geom_vline(aes(xintercept=2009),col="green")+
  geom_hline(aes(yintercept=data),col="red",
             data=transform(as.data.frame(benchMarks),
                            qname=c("Harvest","Catch","Rec","SSB")[quantity]))


## ------------------------------------------------------------------------
refs=refpts(eql)[c("msy","f0.1","fmax"),"harvest",drop=T]

targetF=FLQuants(mlply(data.frame(refs),
                       function(refs) 
                           FLQuant(refs,dimnames=list(year=2009:2020))))

names(targetF)=names(refs)

names(targetF)[]="f"
stks=fwd(stk,f=targetF,sr=sr)

plot(FLStocks(llply(stks, function(x) window(x, start=2000))))

## ------------------------------------------------------------------------
TACs=FLQuants(mlply(seq(0,1,.1),
                          function(x) 
                            FLQuant(x*c(refpts(eql)["msy","yield"]),dimnames=list(year=2009:2020))))

names(TACs)[]="catch"

stks=fwd(stk,TACs,sr=sr)
names(stks)=seq(0,1,.1)

plot(stks)



## ------------------------------------------------------------------------
msys=FLQuants("f"    =targetF[[1]],
              "catch"=TACs[[   6]])
stks=fwd(stk,msys,sr=sr)
names(stks)=names(msys)
plot(stks)


## ------------------------------------------------------------------------
stks=fwd(stk,f=targetF,sr=sr)
names(stks)=1:2
plot(stks)


## ------------------------------------------------------------------------
catch=FLQuant(c(refpts(eql)["msy","yield"])*2,
                          dimnames=list(year=2009:2020))

stk=fwd(stk,catch=catch,sr=sr)

plot(stk)



## ------------------------------------------------------------------------
capacity=FLQuant(1,dimnames=list(year=2009:2020))
q       =rlnorm(1,FLQuant(0,dimnames=list(year=2009:2020)),.2)
maxF    =q*capacity

stk=fwd(stk,catch=catch,sr=sr,maxF=maxF)

plot(stk)


## ----1-------------------------------------------------------------------
f.landings=function(x) apply((harvest(x)*
                   landings.n(x)/
                   catch.n(x))[ac( range(stk)["minfbar"]:range(stk)["maxfbar"])],2,mean)
f.discards=function(x) apply((harvest(x)*
                   landings.n(x)/
                   catch.n(x))[ac( range(stk)["minfbar"]:range(stk)["maxfbar"])],2,mean)
mnsz     =function(x) apply(stock.n(x)*stock.wt(x),2,sum)/
                       apply(stock.n(x),2,sum)
                  
flqs=FLQuants(stk,"ssb", "biomass"=stock,  
                  "landings", "discards", "fbar",
                  "f.landings", "f.discards","mnsz")
         
#             effort, costs, revenue, profit, .

ggplot(flqs)+
  geom_line(aes(year,data))+
  facet_grid(qname~.,scale="free_y")


## ------------------------------------------------------------------------
ggplot(FLQuants(eql,"catch.sel","discards.sel","landings.sel"))+
  geom_line(aes(age,data,col=qname))


## ------------------------------------------------------------------------
catch(stk)<-computeCatch(stk)

## ------------------------------------------------------------------------
noDiscards=stk
discards.n(noDiscards)[,ac(2009:2020)]=0
catch.n(   noDiscards)[,ac(2009:2020)]<-landings.n(noDiscards)[,ac(2009:2020)]
catch(noDiscards)<-computeCatch(noDiscards)

## Note adjustment of harvest
harvest(noDiscards)[,ac(2009:2020)]=harvest(stk)[,ac(2009:2020)]*
  landings.n(stk)[,ac(2009:2020)]/catch.n(stk)[,ac(2009:2020)]
harvest(noDiscards)<-computeHarvest(noDiscards)

F0.1=FLQuant(f0.1,dimnames=list(year=2009:2020))
noDiscards=fwd(noDiscards,f=F0.1,sr=sr)
stk       =fwd(stk,       f=F0.1,sr=sr)

plot(FLStocks("No Discards"=noDiscards,"F0.1"=stk))


## ------------------------------------------------------------------------
poorFec=stk
mat(poorFec)[1:5,ac(2009:2020)]=c(0,0,0,0,.5)

poorFec=fwd(poorFec,f=F0.1,sr=sr)

plot(FLStocks("Reduced \nFecundity"=poorFec,"F0.1"=stk))


## ------------------------------------------------------------------------
srDev=rlnorm(100,FLQuant(0,dimnames=list(year=2009:2020)),0.3)
plot(srDev)


## ------------------------------------------------------------------------
stk  =fwd(stk,f=F0.1,sr=sr,sr.residuals=srDev)
plot(stk)

## ------------------------------------------------------------------------
ctrl=fwdControl(data.frame(year    =2009:2018,
                           val     =c(refpts(eql)["f0.1","harvest"]),
                           quantity="f"))


## ------------------------------------------------------------------------
slotNames(ctrl)


## ------------------------------------------------------------------------
slotNames(ctrl)
ctrl


## ------------------------------------------------------------------------
target=fwdControl(data.frame(year=2009,val=0.8,quantity="f")) 
stk   =fwdWindow(ple4,end=2010,eql)

stk=fwd(stk,ctrl=target,sr=eql)

fbar(stk)[,"2009"]
ssb( stk)[,"2010"]


## ------------------------------------------------------------------------
target<-fwdControl(data.frame(year=c(2009,2009),
                              val =c( 0.8,  NA),
                              min =c(NA,230000),
                              quantity=c("f","ssb")))

stk=fwd(stk,ctrl=target,sr=sr)

fbar(stk)[,"2009"]
ssb( stk)[,"2010"]


## ------------------------------------------------------------------------
harvest.spwn(stk)[]=0.5

stk=fwd(stk,ctrl=target,sr=sr)

fbar(stk)[,  "2009"]
ssb( stk)[,c("2009","2010")]


## ----hcr-fc--------------------------------------------------------------
msy  =refpts(eql)["msy","yield"]
stk  =fwdWindow(ple4,end=2020,eql)

#### constant catch with an upper F bound
ctrl=fwdControl(data.frame(year    =rep(2009:2020,each=2),
                           val     =rep(c(msy*0.7,NA),12),
  	                 	     max     =rep(c(NA,f0.1),12),
                           quantity=rep(c("catch","f"),12)))
stk=fwd(stk,ctrl=ctrl,sr=sr)

plot(stk[,ac(2005:2020)])


## ----hcr-F-TACconstraint-------------------------------------------------
ctrl=fwdControl(data.frame(year    =rep(2009:2020,each=2),
                           rel.year=c(t(array(c(rep(NA,12),2008:2019),c(12,2)))),
                           val     =rep(c(f0.1,NA),12),
                           min     =rep(c(NA,0.85),12),
                           quantity=rep(c("f","catch"),12)))
stk=fwd(stk,ctrl=ctrl,sr=sr)

plot(stk[,ac(2005:2020)])


## ----hcr-10ssb-----------------------------------------------------------
ctrl=fwdControl(data.frame(year    =rep(2009:2020,each=2),
                           rel.year=c(t(array(c(rep(NA,12),2008:2019),c(12,2)))),
                           max     =rep(c(f0.1,NA),12),
                           val     =rep(c(NA,1.1),12),
                           quantity=rep(c("f","ssb"),12)))
stk=fwd(stk,ctrl=ctrl,sr=sr)

plot(stk[,ac(2005:2019)])


## ----hcr-func------------------------------------------------------------
hcrF=function(iYr,SSB,Bpa,Blim,Fmin,Fmax){
    val =pmin(Fmax,Fmax-(Fmax-Fmin)*(Bpa-SSB)/(Bpa-Blim))
    trgt=fwdTarget(year=iYr+1,quantity="f",valueval)

    return(trgt)}

# for (iYr in 2009:2020){
#   
#   ctrl@trgtArray[2,"val", ]<-c(hcr(apply(ssb(MP[,ac(iYr)]),6,mean),MPbrp@refpts["f0.1",,],Ftar,Btrig,Fmin,Blim))
#   
#   OM <-fwd(OM,ctrl=ctrl,sr=list(model="bevholt",params=srPar),sr.residuals=srRsdl)
#   
#   }


## ----6-------------------------------------------------------------------
ssbTarget = mean(ssb(stk)[,ac(1970:1989)])

## function to minimise
f<-function(x,stk,ssbTarget,ctrl,sr){
  
       # set target F for all years
       ctrl@target[,   "val"] =x
       ctrl@trgtArray[,"val",]=x

       # project
       stk=fwd(stk,ctrl=ctrl,sr=sr)
       
       # Squared Difference
       return((ssb(stk)[,ac(range(stk)["maxyear"])]-ssbTarget)^2)}

## control object
ctrl=fwdControl(data.frame(year=2009:2020,val=.5,rel=2008,quantity="f"))

xmin=optimize(f, c(0.1, 1.0), tol = 0.0000001, stk=stk, ssbTarget=ssbTarget, 
              ctrl=ctrl, sr=eql)
ctrl=fwdControl(data.frame(year=2009:2020,val=xmin$minimum,rel=2008,quantity="f"))

stk     =fwd(stk,ctrl=ctrl,sr=eql)

# update catch slot
catch(stk) = computeCatch(stk)

# Have we reached the target?
ssbTarget
ssb(stk)
# At what level of constant F
fbar(stk)

plot(stk)+
  geom_hline(aes(yintercept=data),data=data.frame(data=ssbTarget,qname="SSB"),col="red")


## ----7-------------------------------------------------------------------
ctrl=fwdControl(data.frame(year=2009:2020,val=c(catch(stk)[,"2001"]),quantity="catch"))

xmin=optimize(f, c(100, 100000), tol = 0.0000001, stk=stk, 
              ssbTarget=ssbTarget, ctrl=ctrl, sr=sr)
ctrl=fwdControl(data.frame(year=2009:2020,val=xmin$minimum,quantity="catch"))
stkC      =fwd(stk,ctrl=ctrl,sr=sr)

# Have we reached the target?
ssbTarget
ssb(stkC)[,ac(2002:2020)]
# At what level of constant catch
computeCatch(stkC)[,ac(2002:2020)]
# And at what level of F
fbar(stkC)[,ac(2002:2006)]
# Update the catch slot
catch(stkC) = computeCatch(stkC)
# 'ave a butchers
plot(stkC[,ac(1957:2006)])


## ----8,eval=FALSE--------------------------------------------------------
# Assessment up to and including 2001

# set courtship and egg laying in Autumn
stk@m.spwn[]      =0.66
stk@harvest.spwn[]=0.66

# assessment is in year 2002, set catch constraint in 2002 and a first guess for F in 2003
ctrl          =fwdControl(data.frame(year=2002:2003,val=c(85000,.5),quantity=c("catch","f")))
stk    =fwd(stk, ctrl=ctrl, sr=list(model="mean", params=FLPar(25000)))

# HCR specifies F=0.1 if ssb<100000, F=0.5 if ssb>300000
# otherwise linear increase as SSB increases
min.ssb=100000
max.ssb=300000
min.f  =0.1
max.f  =0.5

# slope of HCR
a.    =(max.f-min.f)/(max.ssb-min.ssb)
b.    =min.f-a.*min.ssb

# plot of HCR
plot(c(0.0,min.ssb,max.ssb,max.ssb*2),c(min.f,min.f,max.f,max.f),type="l",ylim=c(0,max.f*1.25),xlim=c(0,max.ssb*2))

## find F through iteration
t.    =999
i     =0
while (abs(ctrl@target[2,"val"]-t.)>10e-6 & i<50)
   {
   t.=ctrl@target[2,"val"]  ## save last val of F

   # calculate new F based on SSB last iter
   ctrl@target[2,"val"]    =a.*c(ssb(stk)[,"2003"])+b.
   ctrl@trgtArray[2,"val",]=a.*c(ssb(stk)[,"2003"])+b.
   stk=fwd(stk, ctrl=ctrl, sr=list(model="mean", params=FLPar(25000)))

   # 'av a gander
   points(c(ssb(stk)[,"2003"]),c(ctrl@target[2,"val"]),cex=1.25,pch=19,col=i)
   print(c(ssb(stk)[,"2003"]))
   print(c(ctrl@target[2,"val"]))
   i=i+1
   }

# F bounds
stk      =fwd(stk, ctrl=ctrl, sr=list(model="mean",params=FLPar(25000)))
plot(FLStocks(stk))


## ----9,eval=FALSE--------------------------------------------------------
#### Create a random variable for M
stkM   =ple4
ctch=c(catch(stkM)[,ac(2000:2008)])
m(stkM)=propagate(m(stkM),100)

mDev=rlnorm(prod(dim(m(stkM))),0,0.3)
mean(mDev)
var(mDev)^.5

m(stkM)=m(stkM)*FLQuant(mDev,dimnames=dimnames(m(stkM)))
plot(m(stkM))

harvest(stkM)=computeHarvest(stkM)
catch(  stkM)=computeCatch(  stkM,"all")
ctch=c(catch(stkM)[,ac(2000:2008)])

ctrl=fwdControl(data.frame(year=2000:2008,val=ctch,quantity="catch"))
stkM=fwd(stkM,ctrl=ctrl,sr=sr)

plot(stkM)


## ----10,eval=FALSE-------------------------------------------------------
#### Create a random variable for M
stkM1   =stkM
m(stkM1)[1:3,]          =m(stkM)[1:3,]*2

harvest(stkM1)=computeHarvest(stkM1)
catch(  stkM1)=computeCatch(  stkM1,"all")
stkM1         =fwd(stkM1,ctrl=ctrl,sr=sr)

stkM2   =stkM
m(stkM2)[,ac(2000:2008)]=m(stkM)[,ac(2000:2008)]*2

harvest(stkM2)=computeHarvest(stkM2)
catch(  stkM2)=computeCatch(  stkM2,"all")
stkM2         =fwd(stkM2,ctrl=ctrl,sr=sr)

#bug
plot(FLStocks("1"=stkM,"2"=stkM1,"3"=stkM2))


## ----11,eval=FALSE-------------------------------------------------------
#### process error in recruitment
srDev=FLQuant(rlnorm(20*100,0.0,0.3),dimnames=list(year=2000:2008,iter=1:100))
stkRec=fwd(stkM,ctrl=ctrl,sr=sr,sr.residuals=srDev)
plot(stkRec)


## ----12,eval=FALSE-------------------------------------------------------
#### SRR regime shifts
sr       =as.FLSR(stk)
model(sr)=bevholtSV()

sr       =fmle(sr,fixed=list(spr0=c(spr0(eql))))
sr1=ab(fmle(sr,fixed=list(spr0=c(spr0(stk)),s=0.75)))
sr2=ab(fmle(sr,fixed=list(spr0=c(spr0(stk)),v=0.75*params(sr)["v"])))
 
srDev=rlnorm(100,FLQuant(0,dimnames=list(year=2009:2020)),0.3)
plot(FLStocks("1"=fwd(stk,f=F0.1,sr=sr1,sr.residuals=srDev),
              "2"=fwd(stk,f=F0.1,sr=sr2,sr.residuals=srDev)))





## ----13,eval=FALSE-------------------------------------------------------
#### HCR

stk=window(stk,end=2009)
hvt=hcr(stk,refpts(eql)["msy"])
tac(stk,eql,hvt[[1]])
