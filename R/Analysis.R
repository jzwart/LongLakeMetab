# code for analyses in Zwart et al. In Review 


# add EL / WL distinction 
tThermo<-t.test((thermocline$thermo.depth[thermocline$lakeID=='EL'&strftime(thermocline$datetime,'%Y')%in%c(2011,2012)]-
thermocline$thermo.depth[thermocline$lakeID=='WL'&strftime(thermocline$datetime,'%Y')%in%c(2011,2012)]),
(thermocline$thermo.depth[thermocline$lakeID=='EL'&strftime(thermocline$datetime,'%Y')%in%c(2013,2014)]-
thermocline$thermo.depth[thermocline$lakeID=='WL'&strftime(thermocline$datetime,'%Y')%in%c(2013,2014)]))

tzMix<-t.test((thermocline$meta.top-thermocline$meta.top),(post1$top-post2$top))



