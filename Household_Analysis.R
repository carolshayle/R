library(readxl)
newdata<-read_excel("~/Dropbox/timorleste/TL HBDA Merged Data 20210707.xlsx")
library(data.table)
library(openxlsx)
as.data.table(newdata)->q
names(q)[33]<-"municipality"
levels(factor(q$municipality))
names(q)[487]<-"code"
q[!is.na(code)]->q


newtables <- loadWorkbook("~/Dropbox/timorleste/newtables.xlsx")


#Sample size by municipality

q[,.N,municipality]->a
addWorksheet(newtables, "samplesize")
writeData(newtables, sheet = "samplesize", a, colNames = T)
saveWorkbook(newtables,"~/Dropbox/timorleste/newtables.xlsx",overwrite = T)




names(q)[537:542]<-c("walls","roof","ceiling","floor","flood","foundation")
levels(factor(q$walls))->a
factor(q$walls,levels=c(a[4],a[2],a[3],a[6],a[1],a[5]))->q$walls
levels(q$walls)<-c("None","Minor","Moderate","Severe","Complete","Nodata")
levels(factor(q$roof))->a
factor(q$roof,levels=c(a[4],a[2],a[3],a[6],a[1],a[5]))->q$roof
levels(q$roof)<-c("None","Minor","Moderate","Severe","Complete","Nodata")

levels(factor(q$foundation))->a
factor(q$foundation,levels=c(a[4],a[2],a[3],a[6],a[1],a[5]))->q$foundation
levels(q$foundation)<-c("None","Minor","Moderate","Severe","Complete","Nodata")

q[,.N,.(walls,roof,foundation)]
ifelse(q$foundation=="Complete","Complete",
       ifelse(q$wall=="Complete"&q$roof=="Complete","Complete",
              ifelse(q$wall=="Complete"&q$roof=="Severe","Complete",
                     ifelse(q$wall=="Complete"&q$roof=="Moderate","Complete",
                            ifelse(q$wall=="Severe"&q$roof=="Complete","Complete",
                                   ifelse(q$wall=="Severe","Severe",
                                          ifelse(q$wall=="Moderate"&q$roof=="Severe","Severe",
                                                 ifelse(q$wall=="Severe"&q$roof=="None","Severe",
                                                        ifelse(q$wall=="Complete"&q$roof=="None","Severe",
                                                               ifelse(q$wall=="Severe"&q$roof=="Moderate","Severe",
                                                                      ifelse(q$wall=="Severe"&q$roof=="Minor","Severe",
                                                                             ifelse(q$wall=="Complete"&q$roof=="Minor","Severe",
                                                                                    ifelse(q$wall=="None"&q$roof=="Severe","Moderate",
                                                                                           ifelse(q$wall=="Moderate"&q$roof=="None","Moderate",
                                                                                                  ifelse(q$wall=="Moderate"&q$roof=="Moderate","Moderate",
                                                                                                         ifelse(q$wall=="Moderate"&q$roof=="Minor","Moderate",
                                                                                                                ifelse(q$wall=="None"&q$roof=="Complete","Moderate",
                                                                                                                       ifelse(q$wall=="Minor"&q$roof=="Complete","Moderate",
                                                                                                                              ifelse(q$roof=="Minor","Minor",
                                                                                                                                     ifelse(q$roof=="Moderate","Minor",
                                                                                                                                            ifelse(q$wall=="Minor"&q$roof=="None","Minor",
                                                                                                                                                   ifelse(q$wall=="None"&q$roof=="None"&q$foundation=="Minor","Minor",
                                                                                                                                                          ifelse(q$wall=="None"&q$roof=="None"&q$foundation=="None","None","Nodata")))))))))))))))))))))))->q$housedamage



q[housedamage=="Nodata",housedamage:="Complete"]
q[,.N,.(housedamage,municipality)]->a
q[,.N,.(municipality)]->b
merge(a,b,by="municipality")->c
round(c$N.x*100/c$N.y,1)->c$prop

dcast(a,municipality~housedamage,sum)->a
a$var<-"Absolute"
dcast(c,municipality~housedamage,sum,value.var="prop")->c
c$var<-"Proportion"

rbind(a,c)->a

addWorksheet(newtables, "municipality-housedamage")
writeData(newtables, sheet = "municipality-housedamage", a, colNames = T)
saveWorkbook(newtables,"~/Dropbox/timorleste/newtables.xlsx",overwrite = T)

names(q)[40]<-"completionstatus"
q[,.N,completionstatus]->a
addWorksheet(newtables, "completionstatus")
writeData(newtables, sheet = "completionstatus", a, colNames = T)
saveWorkbook(newtables,"~/Dropbox/timorleste/newtables.xlsx",overwrite = T)

q[,.N,.(housedamage,completionstatus)]->a
q[,.N,.(completionstatus)]->b
merge(a,b,by="completionstatus")->c
round(c$N.x*100/c$N.y,1)->c$prop

dcast(a,completionstatus~housedamage,sum)->a
a$var<-"Absolute"
dcast(c,completionstatus~housedamage,sum,value.var="prop")->c
c$var<-"Proportion"
rbind(a,c)->a

addWorksheet(newtables, "completionstatus-damage")
writeData(newtables, sheet = "completionstatus-damage", a, colNames = T)
saveWorkbook(newtables,"~/Dropbox/timorleste/newtables.xlsx",overwrite = T)

names(q)[42]<-"year"
names(q)[43]<-"approxage"

ifelse(q$year>=2010,"After 2010",
   ifelse(q$year>=2000,"After 2000",
      ifelse(q$year>1990,"After 1990","Before 1990")))->q$age 
q[is.na(age)&approxage=="0-5 years",age:="After 2010"]
q[is.na(age)&approxage=="6-10 years",age:="After 2010"]
q[is.na(age)&approxage=="11-20 years",age:="After 2000"]
q[is.na(age)&approxage=="21-50 years",age:="Before 1990"]
q[is.na(age)&approxage=="51-100 years",age:="Before 1990"]


q[,.N,age]->a
addWorksheet(newtables, "houseage")
writeData(newtables, sheet = "houseage", a, colNames = T)
saveWorkbook(newtables,"~/Dropbox/timorleste/newtables.xlsx",overwrite = T)

q[,.N,.(housedamage,age)]->a
q[,.N,.(age)]->b
merge(a,b,by="age")->c
round(c$N.x*100/c$N.y,1)->c$prop

dcast(a,age~housedamage,sum)->a
a$var<-"Absolute"
dcast(c,age~housedamage,sum,value.var="prop")->c
c$var<-"Proportion"
rbind(a,c)->a

addWorksheet(newtables, "age-damage")
writeData(newtables, sheet = "age-damage", a, colNames = T)
saveWorkbook(newtables,"~/Dropbox/timorleste/newtables.xlsx",overwrite = T)


names(q)[22]<-"buildingtype"
q[,.N,.(housedamage,buildingtype)]->a
q[,.N,.(buildingtype)]->b
merge(a,b,by="buildingtype")->c
round(c$N.x*100/c$N.y,1)->c$prop

dcast(a,buildingtype~housedamage,sum)->a
a$var<-"Absolute"
dcast(c,buildingtype~housedamage,sum,value.var="prop")->c
c$var<-"Proportion"
rbind(a,c)->a

addWorksheet(newtables, "buildingtype-damage")
writeData(newtables, sheet = "buildingtype-damage", a, colNames = T)
saveWorkbook(newtables,"~/Dropbox/timorleste/newtables.xlsx",overwrite = T)

grep("toilet",names(q),ignore.case = TRUE,value=TRUE)
names(q)[95]<-"toiletnumber"
names(q)[96]<-"toiletsafterflood"
q[toiletnumber==888,toiletnumber:=0]
q[toiletsafterflood==888,toiletsafterflood:=0]

q[,.N,.(toilets=toiletnumber)]->a
q[,.N,.(toilets=toiletsafterflood)]->b
merge(a,b,by="toilets")->c
names(c)<-c("toilets","numberbeforefloods","numberafterfloods")

addWorksheet(newtables, "toilets")
writeData(newtables, sheet = "toilets", c, colNames = T)
saveWorkbook(newtables,"~/Dropbox/timorleste/newtables.xlsx",overwrite = T)

grep("water",names(q),ignore.case = TRUE)


names(q)[c(89,91)]<-c("water","waterafterflood")
#q[toiletnumber==888,toiletnumber:=0]
#q[toiletsafterflood==888,toiletsafterflood:=0]

q[,.N,.(water=water)]->a
q[,.N,.(waterafterflood,water)]->b
dcast(b,water~waterafterflood,sum,value.var="N")->b

addWorksheet(newtables, "waterafterfloods")
writeData(newtables, sheet = "waterafterfloods", b, colNames = T)
saveWorkbook(newtables,"~/Dropbox/timorleste/newtables.xlsx",overwrite = T)


grep("electricity",names(q),ignore.case = TRUE)
names(q)[85]<-"electricity"
names(q)[87]<-"electricitydamage"

q[,.N,.(electricity,electricitydamage)]->a
dcast(a,electricity~electricitydamage,sum,value.var="N")->a
addWorksheet(newtables, "electricityafterfloods")
writeData(newtables, sheet = "electricityafterfloods", a, colNames = T)
saveWorkbook(newtables,"~/Dropbox/timorleste/newtables.xlsx",overwrite = T)


#grep("hazard",names(q),ignore.case = TRUE,value=TRUE)
