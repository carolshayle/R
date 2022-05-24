library(readxl)
BDA_23rd_June_filtered <- read_excel("Dropbox/timorleste/BDA_23rd_June_filtered.xlsx")
newdata<-read_excel("~/Dropbox/timorleste/TL HBDA Merged Data 20210707.xlsx")
library(data.table)
library(openxlsx)
as.data.table(BDA_23rd_June_filtered)->p
names(p)[56:61]<-c("walls","roof","ceiling","floor","flood","foundation")
levels(factor(p$walls))->a
factor(p$walls,levels=c(a[4],a[2],a[3],a[6],a[1],a[5]))->p$walls
levels(p$walls)<-c("None","Minor","Moderate","Severe","Complete","Nodata")
levels(factor(p$roof))->a
factor(p$roof,levels=c(a[4],a[2],a[3],a[6],a[1],a[5]))->p$roof
levels(p$roof)<-c("None","Minor","Moderate","Severe","Complete","Nodata")

levels(factor(p$foundation))->a
factor(p$foundation,levels=c(a[4],a[2],a[3],a[6],a[1],a[5]))->p$foundation
levels(p$foundation)<-c("None","Minor","Moderate","Severe","Complete","Nodata")

p[,.N,.(walls,roof,foundation)]
ifelse(p$foundation=="Complete","Complete",
  ifelse(p$wall=="Complete"&p$roof=="Complete","Complete",
    ifelse(p$wall=="Complete"&p$roof=="Severe","Complete",
      ifelse(p$wall=="Complete"&p$roof=="Moderate","Complete",
        ifelse(p$wall=="Severe"&p$roof=="Complete","Complete",
          ifelse(p$wall=="Severe","Severe",
            ifelse(p$wall=="Moderate"&p$roof=="Severe","Severe",
              ifelse(p$wall=="Severe"&p$roof=="None","Severe",
                ifelse(p$wall=="Complete"&p$roof=="None","Severe",
                  ifelse(p$wall=="Severe"&p$roof=="Moderate","Severe",
                    ifelse(p$wall=="Severe"&p$roof=="Minor","Severe",
                      ifelse(p$wall=="Complete"&p$roof=="Minor","Severe",
                        ifelse(p$wall=="None"&p$roof=="Severe","Moderate",
                          ifelse(p$wall=="Moderate"&p$roof=="None","Moderate",
                            ifelse(p$wall=="Moderate"&p$roof=="Moderate","Moderate",
                              ifelse(p$wall=="Moderate"&p$roof=="Minor","Moderate",
                                 ifelse(p$wall=="None"&p$roof=="Complete","Moderate",
                                   ifelse(p$wall=="Minor"&p$roof=="Complete","Moderate",
                                      ifelse(p$roof=="Minor","Minor",
                                        ifelse(p$roof=="Moderate","Minor",
                                           ifelse(p$wall=="Minor"&p$roof=="None","Minor",
                                              ifelse(p$wall=="None"&p$roof=="None"&p$foundation=="Minor","Minor",
                                                ifelse(p$wall=="None"&p$roof=="None"&p$foundation=="None","None","Nodata")))))))))))))))))))))))->p$housedamage

p[,.N,housedamage]->a
wb <- loadWorkbook("~/Dropbox/timorleste/surveytables.xlsx")
#addWorksheet(wb, "housedamage-categories")
#writeData(wb, sheet = "housedamage-categories", a, colNames = T)
#saveWorkbook(wb,"~/Dropbox/timorleste/surveytables.xlsx",overwrite = T)



## Wall and Roof Material

names(p)[c(36,38)]<-c("wallmaterial","roofmaterial")
factor(p$wallmaterial)->p$wallmaterial2
levels(p$wallmaterial2)[c(2,11,15,1,5,6,7,12)]<-"Semipucca"
levels(p$wallmaterial2)[c(2,3,7,8)]<-"Pucca"
levels(p$wallmaterial2)[3]<-"Kutcha"
levels(p$wallmaterial2)[5]<-"Semipucca"

factor(p$roofmaterial)->p$roofmaterial2
levels(p$roofmaterial2)[c(2,3,5,6,7,11)]<-"Lightweight/Long-lasting" #Sheets Shingles
levels(p$roofmaterial2)[c(4,5,6)]<-"Temporary" #Sod, Thatch, #Tarp
p[,.N,.(wallmaterial2,roofmaterial2)]

p[,.N,.(wallmaterial2)]->a
p[housedamage=="Complete"|housedamage=="Severe",.N,.(wallmaterial2)]->b
merge(a,b,by="wallmaterial2")->a
round(a[,3]*100/a[,2])->a$percent
names(a)[c(2,3)]<-c("All_houses","Damaged_houses")
#addWorksheet(wb, "damage-wallmaterial-categories")
#writeData(wb, sheet = "damage-wallmaterial-categories", a, colNames = T)
#saveWorkbook(wb,"~/Dropbox/timorleste/surveytables.xlsx",overwrite = T)


p[,.N,.(roofmaterial2)]->a
p[housedamage=="Complete"|housedamage=="Severe",.N,.(roofmaterial2)]->b
merge(a,b,by="roofmaterial2")->a
round(a[,3]*100/a[,2])->a$percent
names(a)[c(2,3)]<-c("All_houses","Damaged_houses")
#addWorksheet(wb, "damage-roofmaterial-categories")
#writeData(wb, sheet = "damage-roofmaterial-categories", a, colNames = T)
#saveWorkbook(wb,"~/Dropbox/timorleste/surveytables.xlsx",overwrite = T)




##Settlements

names(p)[89]<-"settlement"
names(p)[90]<-"settlement_description"
factor(p$settlement)->p$settlement
levels(p$settlement)[c(1,2)]<-"Coastal/Fishing"
levels(p$settlement)[c(2,4,5)]<-"Rural"
levels(p$settlement)[3]<-"Suburban"
factor(p$settlement,levels=c("Coastal/Fishing","Rural","Suburban","Urban"))->p$settlement
##names(p)[c(12,13)]<-c("Latitude","Longitude")

## Number of houses in different types of settlement

p[,.N,settlement]->a
removeWorksheet(wb, "number-by-settlement")
addWorksheet(wb, "number-by-settlement")
writeData(wb, sheet = "number-by-settlement", a, colNames = T)
saveWorkbook(wb,"~/Dropbox/timorleste/surveytables.xlsx",overwrite = T)


##p[settlement=="Other (specify)",.(Latitude,Longitude)]
p[,.N,settlement]->a
p[,.N,.(housedamage,settlement)]->b
merge(b,a,by="settlement")->a
a[,percent:=round(N.x*100/N.y,1)]
dcast(a,settlement~housedamage,sum,value.var = "percent")->a
a[,c(1,2,7,4,3,6,5)]->a
removeWorksheet(wb, "percent-damage-settlement")
addWorksheet(wb, "percent-damage-settlement")
writeData(wb, sheet = "percent-damage-settlement", a, colNames = T)
saveWorkbook(wb,"~/Dropbox/timorleste/surveytables.xlsx",overwrite = T)

##Type of housing in settlements
p[,.N,settlement]->a
p[,.N,.(roofmaterial2,settlement)]->b
merge(b,a,by="settlement")->a
a[,percent:=round(N.x*100/N.y,1)]
dcast(a,settlement~roofmaterial2,sum,value.var = "percent")->a
a[,c(1,2,3,5,4)]->a
removeWorksheet(wb, "percent-roofmaterial-settlement")
addWorksheet(wb, "percent-roofmaterial-settlement")
writeData(wb, sheet = "percent-roofmaterial-settlement", a, colNames = T)
saveWorkbook(wb,"~/Dropbox/timorleste/surveytables.xlsx",overwrite = T)


p[,.N,settlement]->a
p[,.N,.(wallmaterial2,settlement)]->b
merge(b,a,by="settlement")->a
a[,percent:=round(N.x*100/N.y,1)]
dcast(a,settlement~wallmaterial2,sum,value.var = "percent")->a
a[,c(1,3,2,4,5)]->a
removeWorksheet(wb, "percent-wallmaterial-settlement")
addWorksheet(wb, "percent-wallmaterial-settlement")
writeData(wb, sheet = "percent-wallmaterial-settlement", a, colNames = T)
saveWorkbook(wb,"~/Dropbox/timorleste/surveytables.xlsx",overwrite = T)

## Grade/Slope
names(p)[18]<-"gradeslope"
factor(p$gradeslope,levels=c("Flat","Leveled","Low slope","Steep slope","Not sure"))->p$gradeslope
levels(p$gradeslope)[2]<-"Flat"
p[,.N,gradeslope]->a
#addWorksheet(wb, "slopetype")
writeData(wb, sheet = "slopetype", a, colNames = T)
saveWorkbook(wb,"~/Dropbox/timorleste/surveytables.xlsx",overwrite = T)

names(p)[21]<-"height"
names(p)[22]<-"stories"

as.numeric(p$height)->p$heightn
p[heightn==888,heightn:=NA]
p[heightn<5,stories:=0]
p[,.N,stories]->a
#addWorksheet(wb, "stories")
writeData(wb, sheet = "stories", a, colNames = T)
saveWorkbook(wb,"~/Dropbox/timorleste/surveytables.xlsx",overwrite = T)

## Proximity

names(p)[24]<-"proximity"
p[,.N,proximity]->a
#addWorksheet(wb, "proximity")
writeData(wb, sheet = "proximity", a, colNames = T)
saveWorkbook(wb,"~/Dropbox/timorleste/surveytables.xlsx",overwrite = T)

## building material
names(p)[51]<-"foundationmaterial"
factor(p$foundationmaterial)->p$foundationmaterial2
levels(p$foundationmaterial2)[c(2,3,4,7)]<-"Stone/concrete foundation"
p[,.N,wallmaterial2]->a
p[,.N,roofmaterial2]->b
p[,.N,foundationmaterial2]->c

p[,.N,.(wallmaterial2,roofmaterial2,foundationmaterial2)]->d

#addWorksheet(wb, "wallmaterial2")
writeData(wb, sheet = "wallmaterial2", a, colNames = T)
#addWorksheet(wb, "roofmaterial2")
writeData(wb, sheet = "roofmaterial2", b, colNames = T)
#addWorksheet(wb, "foundationmaterial2")
writeData(wb, sheet = "foundationmaterial2", c, colNames = T)
#addWorksheet(wb, "roofwallfoundation")
writeData(wb, sheet = "roofwallfoundation", d, colNames = T)
saveWorkbook(wb,"~/Dropbox/timorleste/surveytables.xlsx",overwrite = T)

## Additional damage/debris

names(p)[62]<-"urgentrepairs"
p[,.N,.(housedamage,urgentrepairs)]->a
dcast(a,housedamage~urgentrepairs,sum,value.var = "N")->a
addWorksheet(wb, "urgentrepairs")
writeData(wb, sheet = "urgentrepairs", a, colNames = T)
saveWorkbook(wb,"~/Dropbox/timorleste/surveytables.xlsx",overwrite = T)
names(p)[63]<-"urgentdescription"
names(p)[c(64,65,66,67,68)]<-c("structuralelements","utilityhazards","roofing","restoreaccess","other")
p[urgentrepairs=="Yes"]->a
as.numeric(a$structuralelements)->a$structuralelements
as.numeric(a$utilityhazards)->a$utilityhazards
as.numeric(a$roofing)->a$roofing
as.numeric(a$restoreaccess)->a$restoreaccess
as.numeric(a$other)->a$other
a[,.(structuralelements=sum(structuralelements),
     utilityhazards=sum(utilityhazards),
     roofing=sum(roofing),
     restoreaccess=sum(restoreaccess),
     other=sum(other),
     all=length(other))]->a
addWorksheet(wb, "urgentrepairdetail")
writeData(wb, sheet = "urgentrepairdetail", a, colNames = T)
saveWorkbook(wb,"~/Dropbox/timorleste/surveytables.xlsx",overwrite = T)

##p[urgentrepairs=="Yes",.N,urgentdescription]

## Debris

names(p)[70]<-"debris"
names(p)[71]<-"debrisvolume"

p[,.N,debris]->a
p[debris=="Yes",.N,.(housedamage)]->b
p[,.(debrisvolume=sum(as.numeric(debrisvolume),na.rm=TRUE))]->c
addWorksheet(wb, "debriscount")
writeData(wb, sheet = "debriscount", a, colNames = T)
addWorksheet(wb, "debrishousedamage")
writeData(wb, sheet = "debrishousedamage", b, colNames = T)
removeWorksheet(wb, "debrisvolume")
addWorksheet(wb, "debrisvolume")
writeData(wb, sheet = "debrisvolume", c, colNames = T)

saveWorkbook(wb,"~/Dropbox/timorleste/surveytables.xlsx",overwrite = T)

##names(p)[72]<-"debristype"
##p[debris=="Yes",.N,debristype]

# Road access
names(p)[91]<-"roadaccess"
factor(p$roadaccess)->p$roadaccess1
levels(p$roadaccess1)[c(1,2)]<-"Paved"
p[,.N,roadaccess1]->a
addWorksheet(wb, "roadaccess")
writeData(wb, sheet = "roadaccess", a, colNames = T)

# Ground level
names(p)[95]<-"groundlevel"
p[,.N,groundlevel]->a
addWorksheet(wb, "plithlevel")
writeData(wb, sheet = "plithlevel", a, colNames = T)
saveWorkbook(wb,"~/Dropbox/timorleste/surveytables.xlsx",overwrite = T)



p[,.N,.(groundlevel,flood)]->a
addWorksheet(wb, "plinthandflooding")
writeData(wb, sheet = "plinthandflooding", a, colNames = T)
saveWorkbook(wb,"~/Dropbox/timorleste/surveytables.xlsx",overwrite = T)

## protected space
names(p)[96]<-"protectedspace"
p[,.N,protectedspace]->a
#addWorksheet(wb, "protectedspace")
writeData(wb, sheet = "protectedspace", a, colNames = T)
saveWorkbook(wb,"~/Dropbox/timorleste/surveytables.xlsx",overwrite = T)

## New Data

