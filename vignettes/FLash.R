
## ------------------------------------------------------------------------
library(FLCore)
library(FLash)
library(FLBRP)
library(ggplotFL)


## ------------------------------------------------------------------------
data(ple4)


## ------------------------------------------------------------------------
#### SRR
sr   =as.FLSR(ple4,model="bevholt")
sr   =fmle(sr,control=list(silent=TRUE))


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


## ------------------------------------------------------------------------
stk=fwdWindow(ple4,end=2020,eql)
unlist(dims(stk))


## ------------------------------------------------------------------------
F0.1=FLQuant(refpts(eql)["f0.1","harvest",drop=T],dimnames=list(year=2009:2020))


## ------------------------------------------------------------------------
stk=fwd(stk,f=F0.1,sr=sr)

plot(FLStocks("Historic"=ple4,"F0.1"=stk))


## ------------------------------------------------------------------------
library(plyr)

dimnames(refpts(eql))$refpt

refs=refpts(eql)[c("msy","f0.1","fmax","spr.30"),"harvest",drop=T]

targetF=FLQuants(mlply(data.frame(refs),
                       function(refs) 
                           FLQuant(refs,dimnames=list(year=2009:2020))))

names(targetF)=names(refs)

names(targetF)[]="f"
stks=fwd(stk,targetF,sr=sr)

plot(stks)


## ------------------------------------------------------------------------
msyTargets=FLQuants(mlply(seq(0,2,.25),
                       function(x) 
                           FLQuant(x*refs["msy"],dimnames=list(year=2009:2020))))

names(msyTargets)[]="f"

stks=fwd(stk,msyTargets,sr=sr)

plot(stks)


## ------------------------------------------------------------------------
refpts(eql)["msy"]

refpts(eql)["msy",c("harvest","yield")]

msy=FLQuant(c(refpts(eql)["msy","yield"]),dimnames=list(year=2009:2020))

stks=fwd(stk,catch=msy,sr=sr)

plot(stks)


## ------------------------------------------------------------------------
msys=FLQuants("f"    =targetF[[1]],
              "catch"=msy)
stks=fwd(stk,msys,sr=sr)

plot(stks)


## ------------------------------------------------------------------------
catch=FLQuant(c(refpts(eql)["msy","yield"])*2,
                          dimnames=list(year=2009:2020))

stk=fwd(stk,catch=catch,sr=sr)

plot(stk)


## ------------------------------------------------------------------------
stk=fwd(stk,catch=catch,sr=sr,maxF=1)

plot(stk)


## ------------------------------------------------------------------------
capacity=FLQuant(1,dimnames=list(year=2009:2020))
q       =rlnorm(1,FLQuant(0,dimnames=list(year=2009:2020)),.2)
maxF    =q*capacity

stk=fwd(stk,catch=catch,sr=sr,maxF=maxF)

plot(stk)


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
target  <-fwdControl(data.frame(year=c(2009,2009),
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


## ------------------------------------------------------------------------
albF1     =fwd(stk,ctrl=target,sr=sr)
plot(albF1)


## ------------------------------------------------------------------------
ctrl      =fwdControl(data.frame(year    =2009:2020,
                                 val     =c(refpts(eql)["f0.1","harvest"])*0.5,
                                 quantity="f"))
stk=fwdWindow(ple4,end=2020,eql)
albF2     =fwd(stk, ctrl=ctrl, sr=sr)


## ------------------------------------------------------------------------
ctrl     =fwdControl(data.frame(year    =2009:2020,
                                val     =c(refpts(eql)["f0.1","harvest"])*2.0,
                                quantity="f"))
albF3     =fwd(stk, ctrl=ctrl, sr=sr)


## ----,eval=FALSE---------------------------------------------------------
## ## Create an FlStock object
## albF0.1=FLStocks("F0.1"=albF1,"half"=albF2,"double"=albF3)
## plot(albF0.1)
## 
## ## Cut the plots
## plot(lapply(albF0.1,window,start=1990))
## 
## ## Compare alternatives
## lapply(lapply(albF0.1,window,start=2009),computeCatch)
## 
## #### Total catch
## lapply(lapply(lapply(albF0.1,window,start=2009),computeCatch),sum)
## 
## #### Short-term
## unlist(lapply(lapply(lapply(albF0.1,window,start=2009,end=2014),computeCatch),sum))
## 
## #### Medium-term
## unlist(lapply(lapply(lapply(albF0.1,window,start=2017,end=2021),computeCatch),sum))
## 
## #### Long-term
## unlist(lapply(lapply(lapply(albF0.1,window,start=2024,end=2028),computeCatch),sum))


## ----1,eval=FALSE--------------------------------------------------------
## #### constant catch startegies
## ctch=mean(computeCatch(albNEA)[,ac(2004:2008)])
## 
## albC=FLStocks()
## ctrl=fwdControl(data.frame(year=2009:2028,val=ctch,quantity="catch"))
## albC[["1.0"]]     =fwd(stk,ctrl=ctrl,sr=sr)
## 
## ctrl     =fwdControl(data.frame(year=2009:2028,val=0.5*ctch,quantity="catch"))
## albC[["0.5"]]     =fwd(stk,ctrl=ctrl,sr=sr)
## 
## ctrl     =fwdControl(data.frame(year=2009:2028,val=1.5*ctch,quantity="catch"))
## albC[["1.5"]]     =fwd(stk,ctrl=ctrl,sr=sr)
## plot(albC)
## 
## #### compare strategies
## plot(FLStocks(albC[[1]],albF0.1[[1]]))


## ----2,eval=FALSE--------------------------------------------------------
## #### constant catch with an upper F bound
## ctrl=fwdControl(data.frame(year    =rep(2009:2028,each=20),
##                             val     =rep(c(ctch*1.5,NA),20),
##   		      max     =rep(c(NA,F0.1),20),
##                             quantity=rep(c("catch","f"),20)))
## albFC=fwd(stk,ctrl=ctrl,sr=sr)


## ----3,eval=FALSE--------------------------------------------------------
## #### 5% F reduction
## ctrl=fwdControl(data.frame(year    =rep(2009:2028,each=2),
##                             rel.year=c(t(array(c(2008:2027,rep(NA,20)),c(20,2)))),
##                             val     =rep(c(0.95,NA),20),
##                             min     =rep(c(NA,F0.1*.5),20),
##                             quantity=rep(c("catch","f"),20)))
## albFC=fwd(stk,ctrl=ctrl,sr=sr)
## plot(albFC)


## ----4,eval=FALSE--------------------------------------------------------
## #### 10% SSB increase
## ctrl=fwdControl(data.frame(year    =2009:2028,
##                             rel.year=2008:2027,
##                             min     =1.10,
##                             quantity="ssb"))
## albSSB=fwd(stk,ctrl=ctrl,sr=sr)
## plot(albSSB)


## ----5,eval=FALSE--------------------------------------------------------
## hcrF=function(iYr,SSB,Bpa,Blim,Fmin,Fmax){
##     val =pmin(Fmax,Fmax-(Fmax-Fmin)*(Bpa-SSB)/(Bpa-Blim))
##     trgt=fwdTarget(year=iYr+1,quantity="f",valueval)
## 
##     return(trgt)}


## ----6,eval=FALSE--------------------------------------------------------
## data(ple4)
## 
## # Set up the stock for the next 6 years
## pleProj =stf(ple4,6)
## 
## # Set a constant recruitment based on the geometric mean of last 10 years
## mnRec = FLPar(exp(mean(log(rec(ple4)[,ac(1992:2001)]))))
## # Set ssb target to level 19 years ago
## ssbTarget = ssb(ple4)[,"1992"]
## 
## ## function to minimise
## f = function(x,stk,ssbTarget,ctrl,sr)
##        {
##        ctrl@target[,"val"]    =x
##        ctrl@trgtArray[,"val",]=x
## 
##        ssb.=c(ssb(fwd(stk,ctrl=ctrl,sr=sr))[,"2006"])
## 
##        return((ssb.-ssbTarget)^2)
##        }
## 
## ## Recover stock to BMY in 2006 with a constant F strategy
## ctrl=fwdControl(data.frame(year=2002:2006,val=.5,rel=2001,quantity="f"))
## 
## xmin=optimize(f, c(0.1, 1.0), tol = 0.0000001, stk=pleProj, ssbTarget=ssbTarget, ctrl=ctrl, sr=list(model="mean",params=mnRec))
## ctrl=fwdControl(data.frame(year=2002:2006,val=xmin$minimum,rel=2001,quantity="f"))
## pleProjF     =fwd(pleProj,ctrl=ctrl,sr=list(model="mean", params=mnRec))
## 
## # update catch slot
## catch(pleProjF) = computeCatch(pleProjF)
## 
## # Have we reached the target?
## ssbTarget
## ssb(pleProjF)[,ac(2002:2006)]
## # At what level of constant F
## fbar(pleProjF)[,ac(2002:2006)]
## # 'ave a butchers
## plot(pleProjF[,ac(1957:2006)])
## 
## plot(albSSB)


## ----7,eval=FALSE--------------------------------------------------------
## 
## data(ple4)
## pleProj=stf(ple4,6)
## 
## ## Recover stock to the desired SSB in 2006 with a constant Catch strategy
## # Here val can be anything in the ctrl because it is overwritten in the optimisation loop
## ctrl=fwdControl(data.frame(year=2002:2006,val=c(catch(pleProj)[,"2001"]),quantity="catch"))
## 
## xmin=optimize(f, c(100, 100000), tol = 0.0000001, stk=pleProj, ssbTarget=ssbTarget, ctrl=ctrl, sr=list(model="mean",params=mnRec))
## ctrl=fwdControl(data.frame(year=2002:2006,val=xmin$minimum,quantity="catch"))
## pleProjC      =fwd(pleProj,ctrl=ctrl,sr=list(model="mean", params=mnRec))
## 
## # Have we reached the target?
## ssbTarget
## ssb(pleProjC)[,ac(2002:2006)]
## # At what level of constant catch
## computeCatch(pleProjC)[,ac(2002:2006)]
## # And at what level of F
## fbar(pleProjC)[,ac(2002:2006)]
## # Update the catch slot
## catch(pleProjC) = computeCatch(pleProjC)
## # 'ave a butchers
## plot(pleProjC[,ac(1957:2006)])


## ----8,eval=FALSE--------------------------------------------------------
## # Assessment up to and including 2001
## data(ple4)
## black.bird               =stf(pleProj,nyrs=2)
## 
## # set courtship and egg laying in Autumn
## black.bird@m.spwn[]      =0.66
## black.bird@harvest.spwn[]=0.66
## 
## # assessment is in year 2002, set catch constraint in 2002 and a first guess for F in 2003
## ctrl          =fwdControl(data.frame(year=2002:2003,val=c(85000,.5),quantity=c("catch","f")))
## black.bird    =fwd(black.bird, ctrl=ctrl, sr=list(model="mean", params=FLPar(25000)))
## 
## # HCR specifies F=0.1 if ssb<100000, F=0.5 if ssb>300000
## # otherwise linear increase as SSB increases
## min.ssb=100000
## max.ssb=300000
## min.f  =0.1
## max.f  =0.5
## 
## # slope of HCR
## a.    =(max.f-min.f)/(max.ssb-min.ssb)
## b.    =min.f-a.*min.ssb
## 
## # plot of HCR
## plot(c(0.0,min.ssb,max.ssb,max.ssb*2),c(min.f,min.f,max.f,max.f),type="l",ylim=c(0,max.f*1.25),xlim=c(0,max.ssb*2))
## 
## ## find F through iteration
## t.    =999
## i     =0
## while (abs(ctrl@target[2,"val"]-t.)>10e-6 & i<50)
##    {
##    t.=ctrl@target[2,"val"]  ## save last val of F
## 
##    # calculate new F based on SSB last iter
##    ctrl@target[2,"val"]    =a.*c(ssb(black.bird)[,"2003"])+b.
##    ctrl@trgtArray[2,"val",]=a.*c(ssb(black.bird)[,"2003"])+b.
##    black.bird=fwd(black.bird, ctrl=ctrl, sr=list(model="mean", params=FLPar(25000)))
## 
##    # 'av a gander
##    points(c(ssb(black.bird)[,"2003"]),c(ctrl@target[2,"val"]),cex=1.25,pch=19,col=i)
##    print(c(ssb(black.bird)[,"2003"]))
##    print(c(ctrl@target[2,"val"]))
##    i=i+1
##    }
## 
## # F bounds
## black.bird      =fwd(black.bird, ctrl=ctrl, sr=list(model="mean",params=FLPar(25000)))
## plot(FLStocks(black.bird))


## ----9,eval=FALSE--------------------------------------------------------
## #### Create a random variable for M
## albM   =stk
## m(albM)=propagate(m(albM),100)
## 
## mDev=rlnorm(prod(dim(m(albM))),0,0.3)
## mean(mDev)
## var(mDev)^.5
## 
## m(albM)=m(albM)*FLQuant(mDev,dimnames=dimnames(m(albM)))
## plot(m(albM))
## 
## harvest(albM)=computeHarvest(albM)
## catch(  albM)=computeCatch(  albM,"all")
## 
## plot(FLStocks(albM,stk28))
## 
## ctrl=fwdControl(data.frame(year=2009:2028,val=ctch,quantity="catch"))
## albM     =fwd(albM,ctrl=ctrl,sr=sr)
## 
## plot(albM)


## ----10,eval=FALSE-------------------------------------------------------
## #### Create a random variable for M
## albM1   =albM
## m(albM1)[1:3,]          =m(albM)[1:3,]*2
## 
## harvest(albM1)=computeHarvest(albM1)
## catch(  albM1)=computeCatch(  albM1,"all")
## albM1         =fwd(albM1,ctrl=ctrl,sr=sr)
## 
## albM2   =albM
## m(albM2)[,ac(2000:2028)]=m(albM)[,ac(2000:2028)]*2
## 
## harvest(albM2)=computeHarvest(albM2)
## catch(  albM2)=computeCatch(  albM2,"all")
## albM2         =fwd(albM2,ctrl=ctrl,sr=sr)
## 
## plot(FLStocks(albM,albM1,albM2))


## ----11,eval=FALSE-------------------------------------------------------
## #### process error in recruitment
## srDev=FLQuant(rlnorm(20*100,0.0,0.3),dimnames=list(year=2008:2028,iter=1:100))
## sr=fwd(albM,ctrl=ctrl,sr=sr,sr.residuals=srDev)
## plot(sr)


## ----12,eval=FALSE-------------------------------------------------------
## #### SRR regime shifts
## albSV       =as.FLSR(albNEA)
## model(sr)=bevholtSV()
## albSV =fmle(albSV,fixed=list(spr0(albNEA)))
## albSV1=fmle(albSV,fixed=list(spr0=spr0(albNEA),s=0.75))
## albSV2=fmle(albSV,fixed=list(spr0=spr0(albNEA),v=0.75*params(albSV)["v"]))
## 
## #### Prior for steepness
## albSV3=fmle(albSV,fixed=list(s=qnorm(seq(0.01,0.99,length.out=101),.75,.1)))
## albSV3=fwd(albSV3,ctrl=ctrl,sr=sr,sr.residuals=srDev)
## 
## plot(albSV1,albSV2,albSV3)


## ----13,eval=FALSE-------------------------------------------------------
## #### SRR regime shifts
## albBRP=brp(FLBRP(albM))
## refpts(albBRP)
## 
## albSV3=fmle(albSV,fixed=list(s=qnorm(seq(0.01,0.99,length.out=101),.75,.1)))
## albSV3=fwd(albSV3,ctrl=ctrl,sr=sr,sr.residuals=srDev)
## 
## plot(albSV1,albSV2,albSV3)


## ----14,eval=FALSE-------------------------------------------------------
## # F bounds
## black.bird      =fwd(black.bird, ctrl=ctrl, sr=list(model="mean",params=FLPar(25000)))
## plot(FLStocks(black.bird))


## ----15,eval=FALSE-------------------------------------------------------
## library(FLash)
## library(FLAssess)
## 
## #### Set up a short term forecast for an FLStock object by adding extra years
## ## The default forecast is 3 years,
## alb3=stf(alb)
## 
## ## Check what?s happened
## summary(alb)
## summary(alb3)
## 
## ## by default future F is the mean of last 3 years
## mean(fbar(alb)[,ac(2007-(0:2))])
## fbar(alb3)[,ac(2007+(1:3))]
## 
## ## by default future F is the mean of last 3 years
## mean(fbar(alb)[,ac(2007-(0:2))])
## fbar(alb3)[,ac(2007+(1:3))]


## ----16,eval=FALSE-------------------------------------------------------
## ## Constant F Projection for a 20 year projection
## stk=stf(alb,nyear=20)
## 
## #### SRR
## sr       =as.FLSR(alb)
## model(sr)=bevholt()
## sr       =fmle(sr)
## 
## #### BRPs
## albBRP=FLBRP(alb,sr=sr)
## computeRefpts(albBRP)
## 
## 
## albBRP=brp(albBRP)
## 
## 
## # Use F0.1 as fishing mortality target
## F0.1=refpts(albBRP)["f0.1","harvest",drop=T]
## #### bug
## ctrl     =fwdControl(data.frame(
##                year    =2008:2027,
##                val     =F0.1,
##                quantity="f"))
## 
## albF1     =fwd(stk,ctrl=ctrl,sr=sr)
## 
## plot(albF1)
## ctrl     =fwdControl(data.frame(
##                year    =2008:2027,
##                val     =F0.1*0.5,
##                quantity="f"))
## albF2     =fwd(stk, ctrl=ctrl, sr=sr)
## 
## ctrl     =fwdControl(data.frame(
##                year    =2008:2027,
##                val     =F0.1*2.0,
##                quantity="f"))
## albF3     =fwd(stk, ctrl=ctrl, sr=sr)
## 
## 
## ## Create an FlStock object
## albF0.1=FLStocks("F0.1"=albF1,"half"=albF2,"double"=albF3)
## plot(albF0.1)
## 
## ## Cut the plots
## plot(lapply(albF0.1,window,start=1990))
## 
## ## Compare alternatives
## lapply(lapply(albF0.1,window,start=2008),computeCatch)
## 
## #### Total catch
## lapply(lapply(lapply(albF0.1,window,start=2008),computeCatch),sum)
## 
## #### Short-term
## unlist(lapply(lapply(lapply(albF0.1,window,start=2008,end=2013),computeCatch),sum))
## #### Medium-term
## unlist(lapply(lapply(lapply(albF0.1,window,start=2016,end=2020),computeCatch),sum))
## #### Long-term
## unlist(lapply(lapply(lapply(albF0.1,window,start=2023,end=2027),computeCatch),sum))
## 


## ----17,eval=FALSE-------------------------------------------------------
## #### constant catch startegies
## ctch=mean(computeCatch(alb)[,ac(2003:2007)])
## 
## albC=FLStocks()
## ctrl=fwdControl(data.frame(year=2008:2027,val=ctch,quantity="catch"))
## albC[["1.0"]]     =fwd(stk,ctrl=ctrl,sr=sr)
## 
## ctrl     =fwdControl(data.frame(year=2008:2027,val=0.5*ctch,quantity="catch"))
## albC[["0.5"]]     =fwd(stk,ctrl=ctrl,sr=sr)
## 
## ctrl     =fwdControl(data.frame(year=2008:2027,val=1.5*ctch,quantity="catch"))
## albC[["1.5"]]     =fwd(stk,ctrl=ctrl,sr=sr)
## plot(albC)
## 
## #### compare startegies
## plot(FLStocks(albC[[1]],albF0.1[[1]]))
## 


## ----18,eval=FALSE-------------------------------------------------------
## #### constant catch with an upper F bound
## ctrl=fwdControl(data.frame(year    =rep(2008:2027,each=20),
##                             val     =rep(c(ctch*1.5,NA),20),
##   		      max     =rep(c(NA,F0.1),20),
##                             quantity=rep(c("catch","f"),20)))
## albFC=fwd(stk,ctrl=ctrl,sr=sr)
## plot(albFC)


## ----19,eval=FALSE-------------------------------------------------------
## #### 5% F reduction
## ctrl=fwdControl(data.frame(year    =rep(2008:2027,each=2),
##                             rel.year=c(t(array(c(2007:2026,rep(NA,20)),c(20,2)))),
##                             val     =rep(c(0.95,NA),20),
##                             min     =rep(c(NA,F0.1*.5),20),
##                             quantity=rep(c("catch","f"),20)))
## albFC=fwd(stk,ctrl=ctrl,sr=sr)
## plot(albFC)


## ----20,eval=FALSE-------------------------------------------------------
## #### 10% SSB increase
## ctrl=fwdControl(data.frame(year    =2008:2027,
##                             rel.year=2007:2026,
##                             min     =1.10,
##                             quantity="ssb"))
## albSSB=fwd(stk,ctrl=ctrl,sr=sr)
## plot(albSSB)


## ----21,eval=FALSE-------------------------------------------------------
## hcrF=function(iYr,SSB,Bpa,Blim,Fmin,Fmax){
##     val =pmin(Fmax,Fmax-(Fmax-Fmin)*(Bpa-SSB)/(Bpa-Blim))
##     trgt=fwdTarget(year=iYr+1,quantity="f",valueval)
## 
##     return(trgt)}


## ----22,eval=FALSE-------------------------------------------------------
## ## Ogives
## dnormal=function(x,a,sL,sR){
##   pow=function(a,b) a^b
## 
##   func=function(x,a,sL,sR){
##     if (x < a) return(pow(2.0,-((x-a)/sL*(x-a)/sL)))
##     else       return(pow(2.0,-((x-a)/sR*(x-a)/sR)))}
## 
##   sapply(x,func,a,sL,sR)}
## 
## logistic=function(x,a50,ato95){
##   pow=function(a,b) a^b
## 
##   func=function(x,a50,ato95){
##      if ((a50-x)/ato95 > 5)   return(0)
##      if ((a50-x)/ato95 < -5)  return(1)
## 
##      return(1.0/(1.0+pow(19.0,(a50-x)/ato95)))}
## 
##   sapply(x,func,a50,ato95)}
## 
## prices    =data.frame(rbind(cbind(Age=1:10,Price=dnormal( 1:10,3,10,20),Type="Peaking"),
##                              cbind(age=1:10,Price=logistic(1:10,2,3),    Type="Increasing")))
## prices$Age=as.numeric(ac(prices$Age))
## 
## p    = ggplot(prices,aes(x=Age, y=Price, group=Type))
## p    = p + geom_line(aes(colour=Type))
## p
## 
## refIPrice=brp(FLBRP(alb,fbar=seq(0,1,length.out=101)))
## refPPrice=refIPrice
## 
## price(refIPrice)=logistic(1:15,4,3)
## price(refPPrice)=dnormal( 1:15,5,1,5)
## 
## refIPrice=brp(refIPrice)
## refPPrice=brp(refPPrice)
## 
## breakEven=refIPrice
## #### bug why not no recycling
## refpts(breakEven)=refpts(as.numeric(c(refpts(refIPrice)["fmax","revenue"]*2,rep(NA,7))),refpt=c("breakEven"))
## computeRefpts(breakEven)[,"revenue"]
## 
## vcost(refIPrice)=c(computeRefpts(breakEven)[,"revenue"]*0.20)
## fcost(refIPrice)=vcost(refIPrice)*4.0
## 
## vcost(refPPrice)=vcost(refIPrice)
## fcost(refPPrice)=fcost(refIPrice)
## 
## refIPrice=brp(refIPrice)
## refPPrice=brp(refPPrice)
## 
## price(refIPrice)=price(refIPrice)/c(refpts(refIPrice)["mey","profit"])
## price(refPPrice)=price(refPPrice)/c(refpts(refPPrice)["mey","profit"])
## 
## refIPrice=brp(refIPrice)
## refPPrice=brp(refPPrice)
## 
## plot(refPPrice)
## plot(refIPrice)


## ----23,eval=FALSE-------------------------------------------------------
## data(ple4)
## 
## # Set up the stock for the next 6 years
## pleProj =stf(ple4,6)
## 
## # Set a constant recruitment based on the geometric mean of last 10 years
## mnRec = FLPar(exp(mean(log(rec(ple4)[,ac(1992:2001)]))))
## # Set ssb target to level 19 years ago
## ssbTarget = ssb(ple4)[,"1992"]
## 
## ## function to minimise
## f = function(x,stk,ssbTarget,ctrl,sr)
##        {
##        ctrl@target[,"val"]    =x
##        ctrl@trgtArray[,"val",]=x
## 
##        ssb.=c(ssb(fwd(stk,ctrl=ctrl,sr=sr))[,"2006"])
## 
##        return((ssb.-ssbTarget)^2)
##        }
## 
## ## Recover stock to BMY in 2006 with a constant F strategy
## ctrl=fwdControl(data.frame(year=2002:2006,val=.5,rel=2001,quantity="f"))
## 
## xmin=optimize(f, c(0.1, 1.0), tol = 0.0000001, stk=pleProj, ssbTarget=ssbTarget, ctrl=ctrl, sr=list(model="mean",params=mnRec))
## ctrl=fwdControl(data.frame(year=2002:2006,val=xmin$minimum,rel=2001,quantity="f"))
## pleProjF     =fwd(pleProj,ctrl=ctrl,sr=list(model="mean", params=mnRec))
## 
## # update catch slot
## catch(pleProjF) = computeCatch(pleProjF)
## 
## # Have we reached the target?
## ssbTarget
## ssb(pleProjF)[,ac(2002:2006)]
## # At what level of constant F
## fbar(pleProjF)[,ac(2002:2006)]
## # 'ave a butchers
## plot(pleProjF[,ac(1957:2006)])
## 
## plot(albSSB)


## ----24,eval=FALSE-------------------------------------------------------
## data(ple4)
## pleProj=stf(ple4,6)
## 
## ## Recover stock to the desired SSB in 2006 with a constant Catch strategy
## # Here val can be anything in the ctrl because it is overwritten in the optimisation loop
## ctrl=fwdControl(data.frame(year=2002:2006,val=c(catch(pleProj)[,"2001"]),quantity="catch"))
## 
## xmin=optimize(f, c(100, 100000), tol = 0.0000001, stk=pleProj, ssbTarget=ssbTarget, ctrl=ctrl, sr=list(model="mean",params=mnRec))
## ctrl=fwdControl(data.frame(year=2002:2006,val=xmin$minimum,quantity="catch"))
## pleProjC      =fwd(pleProj,ctrl=ctrl,sr=list(model="mean", params=mnRec))
## 
## # Have we reached the target?
## ssbTarget
## ssb(pleProjC)[,ac(2002:2006)]
## # At what level of constant catch
## computeCatch(pleProjC)[,ac(2002:2006)]
## # And at what level of F
## fbar(pleProjC)[,ac(2002:2006)]
## # Update the catch slot
## catch(pleProjC) = computeCatch(pleProjC)
## 
## plot(pleProjC[,ac(1957:2006)])


## ----25,eval=FALSE-------------------------------------------------------
## # Assessment upto and including 2001
## data(ple4)
## black.bird               =stf(pleProj,nyear=2)
## 
## # set courtship and egg laying in Autumn
## black.bird@m.spwn[]      =0.66
## black.bird@harvest.spwn[]=0.66
## 
## # assessment is in year 2002, set catch constraint in 2002 and a first guess for F in 2003
## ctrl          =fwdControl(data.frame(year=2002:2003,val=c(85000,.5),quantity=c("catch","f")))
## black.bird    =fwd(black.bird, ctrl=ctrl, sr=list(model="mean", params=FLPar(25000)))
## 
## # HCR specifies F=0.1 if ssb<100000, F=0.5 if ssb>300000
## # otherwise linear increase as SSB increases
## min.ssb=100000
## max.ssb=300000
## min.f  =0.1
## max.f  =0.5
## 
## # slope of HCR
## a.    =(max.f-min.f)/(max.ssb-min.ssb)
## b.    =min.f-a.*min.ssb
## 
## # plot of HCR
## plot(c(0.0,min.ssb,max.ssb,max.ssb*2),c(min.f,min.f,max.f,max.f),type="l",ylim=c(0,max.f*1.25),xlim=c(0,max.ssb*2))
## 
## ## find F through iteration
## t.    =999
## i     =0
## while (abs(ctrl@target[2,"val"]-t.)>10e-6 & i<50)
##    {
##    t.=ctrl@target[2,"val"]  ## save last val of F
## 
##    # calculate new F based on SSB last iter
##    ctrl@target[2,"val"]    =a.*c(ssb(black.bird)[,"2003"])+b.
##    ctrl@trgtArray[2,"val",]=a.*c(ssb(black.bird)[,"2003"])+b.
##    black.bird=fwd(black.bird, ctrl=ctrl, sr=list(model="mean", params=FLPar(25000)))
## 
##    # 'av a gander
##    points(c(ssb(black.bird)[,"2003"]),c(ctrl@target[2,"val"]),cex=1.25,pch=19,col=i)
##    print(c(ssb(black.bird)[,"2003"]))
##    print(c(ctrl@target[2,"val"]))
##    i=i+1
##    }
## 
## # F bounds
## black.bird      =fwd(black.bird, ctrl=ctrl, sr=list(model="mean",params=FLPar(25000)))
## plot(FLStocks(black.bird))


## ----26,eval=FALSE-------------------------------------------------------
## #### Create a random variable for M
## albM   =albF1
## m(albM)=propagate(m(albM),100)
## 
## mDev=rlnorm(prod(dim(m(albM))),0,0.3)
## mean(mDev)
## var(mDev)^.5
## 
## m(albM)=m(albM)*FLQuant(mDev,dimnames=dimnames(m(albM)))
## plot(m(albM))
## 
## harvest(albM)=computeHarvest(albM)
## catch(  albM)=computeCatch(  albM,"all")
## 
## ctrl=fwdControl(data.frame(year=2008:2027, val=c(fbar(albF1)[,ac(2008:2027)]),quantity="f"))
## albM=fwd(albM,ctrl=ctrl,sr=sr)
## 
## plot(FLStocks(albM,albF1))


## ----27,eval=FALSE-------------------------------------------------------
## #### Create a random variable for M
## albM1	   =albM
## m(albM1)[1:3,]          =m(albM)[1:3,]*2
## 
## harvest(albM1)=computeHarvest(albM1)
## catch(  albM1)=computeCatch(  albM1,"all")
## albM1         =fwd(albM1,ctrl=ctrl,sr=sr)
## 
## albM2   =albM
## m(albM2)[,ac(2000:2027)]=m(albM)[,ac(2000:2027)]*2
## 
## harvest(albM2)=computeHarvest(albM2)
## catch(  albM2)=computeCatch(  albM2,"all")
## albM2         =fwd(albM2,ctrl=ctrl,sr=sr)
## 
## plot(FLStocks(albM,albM1,albM2))


## ----28,eval=FALSE-------------------------------------------------------
## #### process error in recruitment
## srDev=FLQuant(rlnorm(20*100,0.0,0.3),dimnames=list(year=2008:2027,iter=1:100))
## sr=fwd(albM,ctrl=ctrl,sr=sr,sr.residuals=srDev)
## plot(sr)


## ----29,eval=FALSE-------------------------------------------------------
## sr =as.FLSR(alb,model="bevholtSV")
## sr1=fmle(sr,fixed=list(spr0=spr0(alb)))
## 
## #### SRR regime shifts
## sr2=fmle(sr,fixed=list(spr0=spr0(alb),v=0.75*params(sr)["v"]))
## 
## alb2=fwd( sr3,ctrl=ctrl,sr=sr2,sr.residuals=srDev)
## 
## plot(FLStocks(sr,sr2))


