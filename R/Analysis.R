# code for analyses in Zwart et al. In Review 
<<<<<<< HEAD
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
=======


# add EL / WL distinction 
tThermo<-t.test((thermocline$thermo.depth[thermocline$lakeID=='EL'&strftime(thermocline$datetime,'%Y')%in%c(2011,2012)]-
thermocline$thermo.depth[thermocline$lakeID=='WL'&strftime(thermocline$datetime,'%Y')%in%c(2011,2012)]),
(thermocline$thermo.depth[thermocline$lakeID=='EL'&strftime(thermocline$datetime,'%Y')%in%c(2013,2014)]-
thermocline$thermo.depth[thermocline$lakeID=='WL'&strftime(thermocline$datetime,'%Y')%in%c(2013,2014)]))

tzMix<-t.test((thermocline$meta.top-thermocline$meta.top),(post1$top-post2$top))


>>>>>>> bbf4bea17593fbce5a2f9cd0f40607dd7a73449d

