# code for analyses in Zwart et al. In Review 
thermocline<-read.table('thermocline.txt',header=T,stringsAsFactor=F,row.names=NULL)
thermocline$datetime<-as.POSIXct(paste(thermocline$row.names,thermocline$datetime))
thermocline<-thermocline[,c(2:6)]
tp<-read.table('tp.txt',header=T,stringsAsFactor=F)
tn<-read.table('tn.txt',header=T,stringsAsFactor=F)
chl_a<-read.table('chl_a.txt',header=T,stringsAsFactor=F)
color<-read.table('color.txt',header=T,stringsAsFactor=F)
doc<-read.table('doc.txt',header=T,stringsAsFactor=F)
kd<-read.table('kd.txt',header=T,stringsAsFactor=F)
srp<-read.table('srp.txt',header=T,stringsAsFactor=F)
lightClimate<-read.table('lightClimate.txt',header=T,stringsAsFactor=F)
pH<-read.table('pH.txt',header=T,stringsAsFactor=F)
poc<-read.table('poc.txt',header=T,stringsAsFactor=F)
wholeLakeTemp<-read.table('wholeLakeTemp.txt',header=T,stringsAsFactor=F)
EL_metab<-read.table('EL_metab.txt',header=T,stringsAsFactor=F)
WL_metab<-read.table('WL_metab.txt',header=T,stringsAsFactor=F)

# add EL / WL distinction 
tThermo<-t.test((thermocline$thermo.depth[thermocline$lakeID=='EL'&strftime(thermocline$datetime,'%Y')%in%c(2011,2012)]-
                   thermocline$thermo.depth[thermocline$lakeID=='WL'&strftime(thermocline$datetime,'%Y')%in%c(2011,2012)]),
                (thermocline$thermo.depth[thermocline$lakeID=='EL'&strftime(thermocline$datetime,'%Y')%in%c(2013,2014)]-
                   thermocline$thermo.depth[thermocline$lakeID=='WL'&strftime(thermocline$datetime,'%Y')%in%c(2013,2014)]))

tzMix<-t.test((thermocline$meta.top[thermocline$lakeID=='EL'&strftime(thermocline$datetime,'%Y')%in%c(2011,2012)]-
                 thermocline$meta.top[thermocline$lakeID=='WL'&strftime(thermocline$datetime,'%Y')%in%c(2011,2012)]),
              (thermocline$meta.top[thermocline$lakeID=='EL'&strftime(thermocline$datetime,'%Y')%in%c(2013,2014)]-
                 thermocline$meta.top[thermocline$lakeID=='WL'&strftime(thermocline$datetime,'%Y')%in%c(2013,2014)]))

tDOC<-t.test((doc$DOC[doc$lakeID=='EL'&strftime(doc$datetime,'%Y')%in%c(2011,2012)]-
                 doc$DOC[doc$lakeID=='WL'&strftime(doc$datetime,'%Y')%in%c(2011,2012)]),
              (doc$DOC[doc$lakeID=='EL'&strftime(doc$datetime,'%Y')%in%c(2013,2014)]-
                 doc$DOC[doc$lakeID=='WL'&strftime(doc$datetime,'%Y')%in%c(2013,2014)]))

tTP<-t.test((tp$TP[tp$lakeID=='EL'&strftime(tp$datetime,'%Y')%in%c(2011,2012)]-
                tp$TP[tp$lakeID=='WL'&strftime(tp$datetime,'%Y')%in%c(2011,2012)]),
             (tp$TP[tp$lakeID=='EL'&strftime(tp$datetime,'%Y')%in%c(2013,2014)]-
                tp$TP[tp$lakeID=='WL'&strftime(tp$datetime,'%Y')%in%c(2013,2014)]))

tTN<-t.test((tn$TN[tn$lakeID=='EL'&strftime(tn$datetime,'%Y')%in%c(2011,2012)]-
                tn$TN[tn$lakeID=='WL'&strftime(tn$datetime,'%Y')%in%c(2011,2012)]),
             (tn$TN[tn$lakeID=='EL'&strftime(tn$datetime,'%Y')%in%c(2013,2014)]-
                tn$TN[tn$lakeID=='WL'&strftime(tn$datetime,'%Y')%in%c(2013,2014)]))

tkD<-t.test((kd$kd[kd$lakeID=='EL'&strftime(kd$datetime,'%Y')%in%c(2011,2012)]-
               kd$kd[kd$lakeID=='WL'&strftime(kd$datetime,'%Y')%in%c(2011,2012)]),
            (kd$kd[kd$lakeID=='EL'&strftime(kd$datetime,'%Y')%in%c(2013,2014)]-
               kd$kd[kd$lakeID=='WL'&strftime(kd$datetime,'%Y')%in%c(2013,2014)]))

tChla<-t.test((chl_a$chla[chl_a$lakeID=='EL'&strftime(chl_a$datetime,'%Y')%in%c(2011,2012)]-
               chl_a$chla[chl_a$lakeID=='WL'&strftime(chl_a$datetime,'%Y')%in%c(2011,2012)]),
            (chl_a$chla[chl_a$lakeID=='EL'&strftime(chl_a$datetime,'%Y')%in%c(2013,2014)]-
               chl_a$chla[chl_a$lakeID=='WL'&strftime(chl_a$datetime,'%Y')%in%c(2013,2014)]))

tColor<-t.test((color$color[color$lakeID=='EL'&strftime(color$datetime,'%Y')%in%c(2011,2012)]-
               color$color[color$lakeID=='WL'&strftime(color$datetime,'%Y')%in%c(2011,2012)]),
            (color$color[color$lakeID=='EL'&strftime(color$datetime,'%Y')%in%c(2013,2014)]-
               color$color[color$lakeID=='WL'&strftime(color$datetime,'%Y')%in%c(2013,2014)]))

tpH<-t.test((pH$pH[pH$lakeID=='EL'&strftime(pH$datetime,'%Y')%in%c(2011,2012)]-
               pH$pH[pH$lakeID=='WL'&strftime(pH$datetime,'%Y')%in%c(2011,2012)]),
            (pH$pH[pH$lakeID=='EL'&strftime(pH$datetime,'%Y')%in%c(2013,2014)]-
               pH$pH[pH$lakeID=='WL'&strftime(pH$datetime,'%Y')%in%c(2013,2014)]))

tPOC<-t.test((poc$POC[poc$lakeID=='EL'&strftime(poc$datetime,'%Y')%in%c(2011,2012)]-
               poc$POC[poc$lakeID=='WL'&strftime(poc$datetime,'%Y')%in%c(2011,2012)]),
            (poc$POC[poc$lakeID=='EL'&strftime(poc$datetime,'%Y')%in%c(2013,2014)]-
               poc$POC[poc$lakeID=='WL'&strftime(poc$datetime,'%Y')%in%c(2013,2014)]))

tSRP<-t.test((srp$SRP[srp$lakeID=='EL'&strftime(srp$datetime,'%Y')%in%c(2011,2012)]-
               srp$SRP[srp$lakeID=='WL'&strftime(srp$datetime,'%Y')%in%c(2011,2012)]),
            (srp$SRP[srp$lakeID=='EL'&strftime(srp$datetime,'%Y')%in%c(2013,2014)]-
               srp$SRP[srp$lakeID=='WL'&strftime(srp$datetime,'%Y')%in%c(2013,2014)]))

tWholeLakeTemp<-t.test((wholeLakeTemp$wholeLakeTemp[wholeLakeTemp$lakeID=='EL'&strftime(wholeLakeTemp$datetime,'%Y')%in%c(2011,2012)]-
               wholeLakeTemp$wholeLakeTemp[wholeLakeTemp$lakeID=='WL'&strftime(wholeLakeTemp$datetime,'%Y')%in%c(2011,2012)]),
            (wholeLakeTemp$wholeLakeTemp[wholeLakeTemp$lakeID=='EL'&strftime(wholeLakeTemp$datetime,'%Y')%in%c(2013,2014)]-
               wholeLakeTemp$wholeLakeTemp[wholeLakeTemp$lakeID=='WL'&strftime(wholeLakeTemp$datetime,'%Y')%in%c(2013,2014)]))

tkD<-t.test((kd$kd[kd$lakeID=='EL'&strftime(kd$datetime,'%Y')%in%c(2011,2012)]-
               kd$kd[kd$lakeID=='WL'&strftime(kd$datetime,'%Y')%in%c(2011,2012)]),
            (kd$kd[kd$lakeID=='EL'&strftime(kd$datetime,'%Y')%in%c(2013,2014)]-
               kd$kd[kd$lakeID=='WL'&strftime(kd$datetime,'%Y')%in%c(2013,2014)]))

## Metabolism analyses 
# run 1000 welch's t-tests for metabolism data incorporating uncertainty 
welchR<-data.frame(p=rep(NA,1000),diffPre=rep(NA,1000),diffPost=rep(NA,1000),
                   meanTrtPre=rep(NA,1000),meanTrtPost=rep(NA,1000),
                   sdTrtPre=rep(NA,1000),sdTrtPost=rep(NA,1000),meanRefPre=rep(NA,1000),meanRefPost=rep(NA,1000),
                   sdRefPre=rep(NA,1000),sdRefPost=rep(NA,1000)) 
welchGPP<-data.frame(p=rep(NA,1000),diffPre=rep(NA,1000),diffPost=rep(NA,1000),
                     meanTrtPre=rep(NA,1000),meanTrtPost=rep(NA,1000),
                     sdTrtPre=rep(NA,1000),sdTrtPost=rep(NA,1000),meanRefPre=rep(NA,1000),meanRefPost=rep(NA,1000),
                     sdRefPre=rep(NA,1000),sdRefPost=rep(NA,1000))
welchNEP<-data.frame(p=rep(NA,1000),diffPre=rep(NA,1000),diffPost=rep(NA,1000),
                     meanTrtPre=rep(NA,1000),meanTrtPost=rep(NA,1000),
                     sdTrtPre=rep(NA,1000),sdTrtPost=rep(NA,1000),meanRefPre=rep(NA,1000),meanRefPost=rep(NA,1000),
                     sdRefPre=rep(NA,1000),sdRefPost=rep(NA,1000))
welchRareal<-data.frame(p=rep(NA,1000),diffPre=rep(NA,1000),diffPost=rep(NA,1000),
                        meanTrtPre=rep(NA,1000),meanTrtPost=rep(NA,1000),
                        sdTrtPre=rep(NA,1000),sdTrtPost=rep(NA,1000),meanRefPre=rep(NA,1000),meanRefPost=rep(NA,1000),
                        sdRefPre=rep(NA,1000),sdRefPost=rep(NA,1000)) 
welchGPPareal<-data.frame(p=rep(NA,1000),diffPre=rep(NA,1000),diffPost=rep(NA,1000),
                          meanTrtPre=rep(NA,1000),meanTrtPost=rep(NA,1000),
                          sdTrtPre=rep(NA,1000),sdTrtPost=rep(NA,1000),meanRefPre=rep(NA,1000),meanRefPost=rep(NA,1000),
                          sdRefPre=rep(NA,1000),sdRefPost=rep(NA,1000))
welchNEPareal<-data.frame(p=rep(NA,1000),diffPre=rep(NA,1000),diffPost=rep(NA,1000),
                          meanTrtPre=rep(NA,1000),meanTrtPost=rep(NA,1000),
                          sdTrtPre=rep(NA,1000),sdTrtPost=rep(NA,1000),meanRefPre=rep(NA,1000),meanRefPost=rep(NA,1000),
                          sdRefPre=rep(NA,1000),sdRefPost=rep(NA,1000))

#WARNING: this for loop takes several minutes to run 
for(i in 1:1000){
  # pull out ith boot for both EL and WL and run t.test 
  if(i %in% c(100,200,300,400,500,600,700,800,900)){print(i)}
  curEL<-EL_metab[EL_metab$boot.iter==i,]
  curWL<-WL_metab[WL_metab$boot.iter==i,]
  curEL<-curEL[curEL$datetime%in%curWL$datetime,]
  curWL<-curWL[curWL$datetime%in%curEL$datetime,]  
  preR<-curEL$rho[strftime(curEL$datetime,'%Y')==2011|strftime(curEL$datetime,'%Y')==2012]-
    curWL$rho[strftime(curWL$datetime,'%Y')==2011|strftime(curWL$datetime,'%Y')==2012]
  postR<-curEL$rho[strftime(curEL$datetime,'%Y')==2013|strftime(curEL$datetime,'%Y')==2014]-
    curWL$rho[strftime(curWL$datetime,'%Y')==2013|strftime(curWL$datetime,'%Y')==2014]
  preGPP<-curEL$GPP[strftime(curEL$datetime,'%Y')==2011|strftime(curEL$datetime,'%Y')==2012]-
    curWL$GPP[strftime(curWL$datetime,'%Y')==2011|strftime(curWL$datetime,'%Y')==2012]
  postGPP<-curEL$GPP[strftime(curEL$datetime,'%Y')==2013|strftime(curEL$datetime,'%Y')==2014]-
    curWL$GPP[strftime(curWL$datetime,'%Y')==2013|strftime(curWL$datetime,'%Y')==2014]
  preNEP<-(curEL$GPP[strftime(curEL$datetime,'%Y')==2011|strftime(curEL$datetime,'%Y')==2012]-
             curEL$rho[strftime(curEL$datetime,'%Y')==2011|strftime(curEL$datetime,'%Y')==2012])-
    (curWL$GPP[strftime(curWL$datetime,'%Y')==2011|strftime(curWL$datetime,'%Y')==2012]-
       curWL$rho[strftime(curWL$datetime,'%Y')==2011|strftime(curWL$datetime,'%Y')==2012])
  postNEP<-(curEL$GPP[strftime(curEL$datetime,'%Y')==2013|strftime(curEL$datetime,'%Y')==2014]-
              curEL$rho[strftime(curEL$datetime,'%Y')==2013|strftime(curEL$datetime,'%Y')==2014])-
    (curWL$GPP[strftime(curWL$datetime,'%Y')==2013|strftime(curWL$datetime,'%Y')==2014]-
       curWL$rho[strftime(curWL$datetime,'%Y')==2013|strftime(curWL$datetime,'%Y')==2014])
  
  preRareal<-curEL$rhoAreal[strftime(curEL$datetime,'%Y')==2011|strftime(curEL$datetime,'%Y')==2012]-
    curWL$rhoAreal[strftime(curWL$datetime,'%Y')==2011|strftime(curWL$datetime,'%Y')==2012]
  postRareal<-curEL$rhoAreal[strftime(curEL$datetime,'%Y')==2013|strftime(curEL$datetime,'%Y')==2014]-
    curWL$rhoAreal[strftime(curWL$datetime,'%Y')==2013|strftime(curWL$datetime,'%Y')==2014]
  preGPPareal<-curEL$GPPAreal[strftime(curEL$datetime,'%Y')==2011|strftime(curEL$datetime,'%Y')==2012]-
    curWL$GPPAreal[strftime(curWL$datetime,'%Y')==2011|strftime(curWL$datetime,'%Y')==2012]
  postGPPareal<-curEL$GPPAreal[strftime(curEL$datetime,'%Y')==2013|strftime(curEL$datetime,'%Y')==2014]-
    curWL$GPPAreal[strftime(curWL$datetime,'%Y')==2013|strftime(curWL$datetime,'%Y')==2014]
  preNEPareal<-(curEL$GPPAreal[strftime(curEL$datetime,'%Y')==2011|strftime(curEL$datetime,'%Y')==2012]-
                  curEL$rhoAreal[strftime(curEL$datetime,'%Y')==2011|strftime(curEL$datetime,'%Y')==2012])-
    (curWL$GPPAreal[strftime(curWL$datetime,'%Y')==2011|strftime(curWL$datetime,'%Y')==2012]-
       curWL$rhoAreal[strftime(curWL$datetime,'%Y')==2011|strftime(curWL$datetime,'%Y')==2012])
  postNEPareal<-(curEL$GPPAreal[strftime(curEL$datetime,'%Y')==2013|strftime(curEL$datetime,'%Y')==2014]-
                   curEL$rhoAreal[strftime(curEL$datetime,'%Y')==2013|strftime(curEL$datetime,'%Y')==2014])-
    (curWL$GPPAreal[strftime(curWL$datetime,'%Y')==2013|strftime(curWL$datetime,'%Y')==2014]-
       curWL$rhoAreal[strftime(curWL$datetime,'%Y')==2013|strftime(curWL$datetime,'%Y')==2014])
  
  tGPP<-t.test(preGPP,postGPP) # t.test comparing differences between basins pre and post manipulation 
  tR<-t.test(preR,postR)
  tNEP<-t.test(preNEP,postNEP)
  tGPPareal<-t.test(preGPPareal,postGPPareal) # t.test comparing differences between basins pre and post manipulation 
  tRareal<-t.test(preRareal,postRareal)
  tNEPareal<-t.test(preNEPareal,postNEPareal)
  
  
  welchR$p[i]<-tR$p.value # p-value for welch's t-test 
  welchR$diffPre[i]<-tR$estimate[1] #mean differences between basins pre manipulation 
  welchR$diffPost[i]<-tR$estimate[2] #mean differences between basins post manipulation 
  welchR$meanTrtPre[i]<-mean(curEL$rho[strftime(curEL$datetime,'%Y')==2011|strftime(curEL$datetime,'%Y')==2012])
  welchR$meanTrtPost[i]<-mean(curEL$rho[strftime(curEL$datetime,'%Y')==2013|strftime(curEL$datetime,'%Y')==2014])
  welchR$sdTrtPre[i]<-sd(curEL$rho[strftime(curEL$datetime,'%Y')==2011|strftime(curEL$datetime,'%Y')==2012])
  welchR$sdTrtPost[i]<-sd(curEL$rho[strftime(curEL$datetime,'%Y')==2013|strftime(curEL$datetime,'%Y')==2014])
  welchR$meanRefPre[i]<-mean(curWL$rho[strftime(curWL$datetime,'%Y')==2011|strftime(curWL$datetime,'%Y')==2012])
  welchR$meanRefPost[i]<-mean(curWL$rho[strftime(curWL$datetime,'%Y')==2013|strftime(curWL$datetime,'%Y')==2014])
  welchR$sdRefPre[i]<-sd(curWL$rho[strftime(curWL$datetime,'%Y')==2011|strftime(curWL$datetime,'%Y')==2012])
  welchR$sdRefPost[i]<-sd(curWL$rho[strftime(curWL$datetime,'%Y')==2013|strftime(curWL$datetime,'%Y')==2014])
  welchGPP$p[i]<-tGPP$p.value # p-value for welch's t-test 
  welchGPP$diffPre[i]<-tGPP$estimate[1]
  welchGPP$diffPost[i]<-tGPP$estimate[2]
  welchGPP$meanTrtPre[i]<-mean(curEL$GPP[strftime(curEL$datetime,'%Y')==2011|strftime(curEL$datetime,'%Y')==2012])
  welchGPP$meanTrtPost[i]<-mean(curEL$GPP[strftime(curEL$datetime,'%Y')==2013|strftime(curEL$datetime,'%Y')==2014])
  welchGPP$sdTrtPre[i]<-sd(curEL$GPP[strftime(curEL$datetime,'%Y')==2011|strftime(curEL$datetime,'%Y')==2012])
  welchGPP$sdTrtPost[i]<-sd(curEL$GPP[strftime(curEL$datetime,'%Y')==2013|strftime(curEL$datetime,'%Y')==2014])
  welchGPP$meanRefPre[i]<-mean(curWL$GPP[strftime(curWL$datetime,'%Y')==2011|strftime(curWL$datetime,'%Y')==2012])
  welchGPP$meanRefPost[i]<-mean(curWL$GPP[strftime(curWL$datetime,'%Y')==2013|strftime(curWL$datetime,'%Y')==2014])
  welchGPP$sdRefPre[i]<-sd(curWL$GPP[strftime(curWL$datetime,'%Y')==2011|strftime(curWL$datetime,'%Y')==2012])
  welchGPP$sdRefPost[i]<-sd(curWL$GPP[strftime(curWL$datetime,'%Y')==2013|strftime(curWL$datetime,'%Y')==2014])
  welchNEP$p[i]<-tNEP$p.value # p-value for welch's t-test 
  welchNEP$diffPre[i]<-tNEP$estimate[1]
  welchNEP$diffPost[i]<-tNEP$estimate[2]
  welchNEP$meanTrtPre[i]<-mean(curEL$GPP[strftime(curEL$datetime,'%Y')==2011|strftime(curEL$datetime,'%Y')==2012]-
                                 curEL$rho[strftime(curEL$datetime,'%Y')==2011|strftime(curEL$datetime,'%Y')==2012])
  welchNEP$meanTrtPost[i]<-mean(curEL$GPP[strftime(curEL$datetime,'%Y')==2013|strftime(curEL$datetime,'%Y')==2014]-
                                  curEL$rho[strftime(curEL$datetime,'%Y')==2013|strftime(curEL$datetime,'%Y')==2014])
  welchNEP$sdTrtPre[i]<-sd(curEL$GPP[strftime(curEL$datetime,'%Y')==2011|strftime(curEL$datetime,'%Y')==2012]-
                             curEL$rho[strftime(curEL$datetime,'%Y')==2011|strftime(curEL$datetime,'%Y')==2012])
  welchNEP$sdTrtPost[i]<-sd(curEL$GPP[strftime(curEL$datetime,'%Y')==2013|strftime(curEL$datetime,'%Y')==2014]-
                              curEL$rho[strftime(curEL$datetime,'%Y')==2013|strftime(curEL$datetime,'%Y')==2014])
  welchNEP$meanRefPre[i]<-mean(curWL$GPP[strftime(curWL$datetime,'%Y')==2011|strftime(curWL$datetime,'%Y')==2012]-
                                 curWL$rho[strftime(curWL$datetime,'%Y')==2011|strftime(curWL$datetime,'%Y')==2012])
  welchNEP$meanRefPost[i]<-mean(curWL$GPP[strftime(curWL$datetime,'%Y')==2013|strftime(curWL$datetime,'%Y')==2014]-
                                  curWL$rho[strftime(curWL$datetime,'%Y')==2013|strftime(curWL$datetime,'%Y')==2014])
  welchNEP$sdRefPre[i]<-sd(curWL$GPP[strftime(curWL$datetime,'%Y')==2011|strftime(curWL$datetime,'%Y')==2012]-
                             curWL$rho[strftime(curWL$datetime,'%Y')==2011|strftime(curWL$datetime,'%Y')==2012])
  welchNEP$sdRefPost[i]<-sd(curWL$GPP[strftime(curWL$datetime,'%Y')==2013|strftime(curWL$datetime,'%Y')==2014]-
                              curWL$rho[strftime(curWL$datetime,'%Y')==2013|strftime(curWL$datetime,'%Y')==2014])
  
  welchRareal$p[i]<-tRareal$p.value # p-value for welch's t-test 
  welchRareal$diffPre[i]<-tRareal$estimate[1] #mean differences between basins pre manipulation 
  welchRareal$diffPost[i]<-tRareal$estimate[2] #mean differences between basins post manipulation 
  welchRareal$meanTrtPre[i]<-mean(curEL$rhoAreal[strftime(curEL$datetime,'%Y')==2011|strftime(curEL$datetime,'%Y')==2012])
  welchRareal$meanTrtPost[i]<-mean(curEL$rhoAreal[strftime(curEL$datetime,'%Y')==2013|strftime(curEL$datetime,'%Y')==2014])
  welchRareal$sdTrtPre[i]<-sd(curEL$rhoAreal[strftime(curEL$datetime,'%Y')==2011|strftime(curEL$datetime,'%Y')==2012])
  welchRareal$sdTrtPost[i]<-sd(curEL$rhoAreal[strftime(curEL$datetime,'%Y')==2013|strftime(curEL$datetime,'%Y')==2014])
  welchRareal$meanRefPre[i]<-mean(curWL$rhoAreal[strftime(curWL$datetime,'%Y')==2011|strftime(curWL$datetime,'%Y')==2012])
  welchRareal$meanRefPost[i]<-mean(curWL$rhoAreal[strftime(curWL$datetime,'%Y')==2013|strftime(curWL$datetime,'%Y')==2014])
  welchRareal$sdRefPre[i]<-sd(curWL$rhoAreal[strftime(curWL$datetime,'%Y')==2011|strftime(curWL$datetime,'%Y')==2012])
  welchRareal$sdRefPost[i]<-sd(curWL$rhoAreal[strftime(curWL$datetime,'%Y')==2013|strftime(curWL$datetime,'%Y')==2014])
  welchGPPareal$p[i]<-tGPPareal$p.value # p-value for welch's t-test 
  welchGPPareal$diffPre[i]<-tGPPareal$estimate[1]
  welchGPPareal$diffPost[i]<-tGPPareal$estimate[2]
  welchGPPareal$meanTrtPre[i]<-mean(curEL$GPPAreal[strftime(curEL$datetime,'%Y')==2011|strftime(curEL$datetime,'%Y')==2012])
  welchGPPareal$meanTrtPost[i]<-mean(curEL$GPPAreal[strftime(curEL$datetime,'%Y')==2013|strftime(curEL$datetime,'%Y')==2014])
  welchGPPareal$sdTrtPre[i]<-sd(curEL$GPPAreal[strftime(curEL$datetime,'%Y')==2011|strftime(curEL$datetime,'%Y')==2012])
  welchGPPareal$sdTrtPost[i]<-sd(curEL$GPPAreal[strftime(curEL$datetime,'%Y')==2013|strftime(curEL$datetime,'%Y')==2014])
  welchGPPareal$meanRefPre[i]<-mean(curWL$GPPAreal[strftime(curWL$datetime,'%Y')==2011|strftime(curWL$datetime,'%Y')==2012])
  welchGPPareal$meanRefPost[i]<-mean(curWL$GPPAreal[strftime(curWL$datetime,'%Y')==2013|strftime(curWL$datetime,'%Y')==2014])
  welchGPPareal$sdRefPre[i]<-sd(curWL$GPPAreal[strftime(curWL$datetime,'%Y')==2011|strftime(curWL$datetime,'%Y')==2012])
  welchGPPareal$sdRefPost[i]<-sd(curWL$GPPAreal[strftime(curWL$datetime,'%Y')==2013|strftime(curWL$datetime,'%Y')==2014])
  welchNEPareal$p[i]<-tNEPareal$p.value # p-value for welch's t-test 
  welchNEPareal$diffPre[i]<-tNEPareal$estimate[1]
  welchNEPareal$diffPost[i]<-tNEPareal$estimate[2]
  welchNEPareal$meanTrtPre[i]<-mean(curEL$GPPAreal[strftime(curEL$datetime,'%Y')==2011|strftime(curEL$datetime,'%Y')==2012]-
                                      curEL$rhoAreal[strftime(curEL$datetime,'%Y')==2011|strftime(curEL$datetime,'%Y')==2012])
  welchNEPareal$meanTrtPost[i]<-mean(curEL$GPPAreal[strftime(curEL$datetime,'%Y')==2013|strftime(curEL$datetime,'%Y')==2014]-
                                       curEL$rhoAreal[strftime(curEL$datetime,'%Y')==2013|strftime(curEL$datetime,'%Y')==2014])
  welchNEPareal$sdTrtPre[i]<-sd(curEL$GPPAreal[strftime(curEL$datetime,'%Y')==2011|strftime(curEL$datetime,'%Y')==2012]-
                                  curEL$rhoAreal[strftime(curEL$datetime,'%Y')==2011|strftime(curEL$datetime,'%Y')==2012])
  welchNEPareal$sdTrtPost[i]<-sd(curEL$GPPAreal[strftime(curEL$datetime,'%Y')==2013|strftime(curEL$datetime,'%Y')==2014]-
                                   curEL$rhoAreal[strftime(curEL$datetime,'%Y')==2013|strftime(curEL$datetime,'%Y')==2014])
  welchNEPareal$meanRefPre[i]<-mean(curWL$GPPAreal[strftime(curWL$datetime,'%Y')==2011|strftime(curWL$datetime,'%Y')==2012]-
                                      curWL$rhoAreal[strftime(curWL$datetime,'%Y')==2011|strftime(curWL$datetime,'%Y')==2012])
  welchNEPareal$meanRefPost[i]<-mean(curWL$GPPAreal[strftime(curWL$datetime,'%Y')==2013|strftime(curWL$datetime,'%Y')==2014]-
                                       curWL$rhoAreal[strftime(curWL$datetime,'%Y')==2013|strftime(curWL$datetime,'%Y')==2014])
  welchNEPareal$sdRefPre[i]<-sd(curWL$GPPAreal[strftime(curWL$datetime,'%Y')==2011|strftime(curWL$datetime,'%Y')==2012]-
                                  curWL$rhoAreal[strftime(curWL$datetime,'%Y')==2011|strftime(curWL$datetime,'%Y')==2012])
  welchNEPareal$sdRefPost[i]<-sd(curWL$GPPAreal[strftime(curWL$datetime,'%Y')==2013|strftime(curWL$datetime,'%Y')==2014]-
                                   curWL$rhoAreal[strftime(curWL$datetime,'%Y')==2013|strftime(curWL$datetime,'%Y')==2014])  
}

mean(welchNEP$p)
median(welchNEP$p)
mean(welchGPP$p)
median(welchGPP$p)
mean(welchR$p)
median(welchR$p)

mean(welchNEPareal$p)
mean(welchRareal$p)
mean(welchGPPareal$p)
mean(welchNEPareal$diffPost-welchNEPareal$diffPre)
mean(welchRareal$diffPost-welchRareal$diffPre)
mean(welchGPPareal$diffPost-welchGPPareal$diffPre)

mean(welchR$meanTrtPre)
mean(welchR$meanRefPre)
mean(welchR$meanTrtPost)
mean(welchR$meanRefPost)
mean(welchR$sdTrtPre)
mean(welchR$sdRefPre)
mean(welchR$sdTrtPost)
mean(welchR$sdRefPost)

mean(welchGPP$meanTrtPre)
mean(welchGPP$meanRefPre)
mean(welchGPP$meanTrtPost)
mean(welchGPP$meanRefPost)
mean(welchGPP$sdTrtPre)
mean(welchGPP$sdRefPre)
mean(welchGPP$sdTrtPost)
mean(welchGPP$sdRefPost)

mean(welchNEP$meanTrtPre)
mean(welchNEP$meanRefPre)
mean(welchNEP$meanTrtPost)
mean(welchNEP$meanRefPost)
mean(welchNEP$sdTrtPre)
mean(welchNEP$sdRefPre)
mean(welchNEP$sdTrtPost)
mean(welchNEP$sdRefPost)

mean(welchRareal$meanTrtPre)
mean(welchRareal$meanRefPre)
mean(welchRareal$meanTrtPost)
mean(welchRareal$meanRefPost)
mean(welchRareal$sdTrtPre)
mean(welchRareal$sdRefPre)
mean(welchRareal$sdTrtPost)
mean(welchRareal$sdRefPost)

mean(welchGPPareal$meanTrtPre)
mean(welchGPPareal$meanRefPre)
mean(welchGPPareal$meanTrtPost)
mean(welchGPPareal$meanRefPost)
mean(welchGPPareal$sdTrtPre)
mean(welchGPPareal$sdRefPre)
mean(welchGPPareal$sdTrtPost)
mean(welchGPPareal$sdRefPost)

mean(welchNEPareal$meanTrtPre)
mean(welchNEPareal$meanRefPre)
mean(welchNEPareal$meanTrtPost)
mean(welchNEPareal$meanRefPost)
mean(welchNEPareal$sdTrtPre)
mean(welchNEPareal$sdRefPre)
mean(welchNEPareal$sdTrtPost)
mean(welchNEPareal$sdRefPost)

#background respiration 
years=c(2011,2012,2013,2014)
lakes=c('EL','WL')
regrCoefs <- data.frame(b0=rep(NA,1000),alpha=rep(NA,1000),b1=rep(NA,1000))
for(j in 1:length(lakes)){
  if(lakes[j]=='EL'){
    curLake<-EL_metab
  }else{
    curLake<-WL_metab
  }
  for(i in 1:length(years)){
    curYr<-curLake[strftime(curLake$datetime,'%Y')==years[i],]
    for (b in 1:1000) {
      if (b %in% c(100,200,300,400,500,600,700,800,900)) (print(b))
      #Pull out rhoEst and iotaEst for each day from the bth bootstrapped dataset
      dataB <- curYr[curYr$boot.iter==b,c("datetime","rho","iota",'GPP','temp')]
      start<-dataB$datetime[1]
      end<-dataB$datetime[length(dataB$datetime)]
      dataBall<-data.frame(datetime=as.character(seq(as.Date(start),as.Date(end),'day')))
      dataBall<-merge(dataBall,dataB,by='datetime',all.x=T)
      #Temperature-correct R and GPP to 20C - Holtgrieve et al. 2011, Venkiteswaran et al. 2007, etc
      R20 <- dataBall$rho*1.047^(20-dataBall$temp)
      P20 <- dataBall$GPP*1.047^(20-dataBall$temp)
      #Fit R~P regression to the bth boostrapped data set      
      lm1 <- lm(R20[2:length(R20)]~R20[1:(length(R20)-1)]+P20[2:length(R20)])
      #Save results from the bth regression
      regrCoefs[b,] <- coef(lm1)
    } #end loop over bootstrap samples
    assign(paste(lakes[j],years[i],'backR',sep='_'),regrCoefs)
  }
}

allBackR<-rbind(EL_2011_backR,EL_2012_backR,EL_2013_backR,EL_2014_backR,WL_2011_backR,
                WL_2012_backR,WL_2013_backR,WL_2014_backR)
allBackR$year<-c(rep(2011,1000),rep(2012,1000),rep(2013,1000),rep(2014,1000),rep(2011,1000),
                 rep(2012,1000),rep(2013,1000),rep(2014,1000))
allBackR$lakeID<-c(rep('EL',4000),rep('WL',4000))
allBackR$trt<-c(rep('Pre',2000),rep('Post',2000),rep('Pre',2000),rep('Post',2000))

elBackPre<-mean(allBackR$b0[allBackR$lakeID=='EL'&allBackR$year%in%c(2011,2012)])
elBackPost<-mean(allBackR$b0[allBackR$lakeID=='EL'&allBackR$year%in%c(2013,2014)])
wlBackPre<-mean(allBackR$b0[allBackR$lakeID=='WL'&allBackR$year%in%c(2011,2012)])
wlBackPost<-mean(allBackR$b0[allBackR$lakeID=='WL'&allBackR$year%in%c(2013,2014)])

increaseBackR<-(elBackPost-wlBackPost)-(elBackPre-wlBackPre)

lakes=c('EL','WL')
years=c(2011,2012,2013,2014)
trt<-c('Pre','Post')
backRsummary<-data.frame()
for(j in 1:length(trt)){
  for(i in 1:length(lakes)){
    curMean<-mean(allBackR$b0[allBackR$lakeID==lakes[i]&allBackR$trt==trt[j]])
    curSd<-sd(allBackR$b0[allBackR$lakeID==lakes[i]&allBackR$trt==trt[j]])*1.96
    cur<-data.frame(lakeID=lakes[i],trt=trt[j],mean=curMean,sd=curSd,CI2.5=curMean-curSd,
                    CI97.5=curMean+curSd)
    backRsummary<-rbind(backRsummary,cur)
  }
}


