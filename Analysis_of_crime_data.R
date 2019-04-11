library(ggplot2)
library(chron)
library(Rcpp)
crime<-read.csv("C:/Users/Lenovo/Downloads/Kiru/my projects/Modelling+multivariate/Crime.csv")

#month week time
time.tag<-chron(times=c("00:00:00","06:00:00","12:00:00","18:00:00","23:59:00"))
crime$Time.tag<-cut(times(crime$Time),breaks=time.tag,labels=c("00-06","06--12","12-18","18-00"),include.lowest=TRUE)  
qplot(crime$Time.tag,xlab="Time of day",ylab="Number of crimes",main="Crime by time of day")
crime$Day<-weekdays(dates(as.character(crime$Date)))
qplot(crime$Day,xlab="Day of week",main="Crimes by day of week")
crime$month<-months(dates(as.character(crime$Date)))
qplot(crime$month,xlab="Months",main="Crimes by Month")

#Finding unique crime types
table(crime$Description)
length(unique(crime$Description))
crime$crime<-as.character(crime$Primary.Type)
crime$crime<-ifelse(crime$crime%in%c("CRIME SEXUAL ASSAULT","PROSTITUTION","SEX OFFENSE"),"SEX",crime$crime)
crime$crime<-ifelse(crime$crime%in%c("MOTOR VEHICLE THEFT"),"MVT",crime$crime)
crime$crime<-ifelse(crime$crime%in%c("GAMBLING",INTERFERE WITH PUBLIC OFFICER","INTERFERENCE WITH PUBLIC OFFICER","INTIMIDATION","LIQUOR LAW VIOLATION","OBSCENITY","NONCRIMINAL","PUBLIC PEACE VIOLATION","PUBLIC INDECENCY","STALKING","NON-CRIMINAL(SUBJECT SPECIFIED)","NONVIO",crime$crime)
crime$crime<-ifelse(crime$crime=="CRIMINAL DAMAGE","DAMAGE",crime$crime)
crime$crime<-ifelse(crime$crime=="CRIMINAL TRESPASS","TRESPASS",crime.data$crime)
crime$crime<-ifelse(crime$crime%in%c("NARCOTICS","OTHERNARCOTIC VIOLATION","OTHER NARCOTIC VIOLATION"),"DRUG",crime$crime)
crime$crime<-ifelse(crime$crime=="DECEPTIVE PRACTICE","FRAUD",crime$crime)
crime$crime<-ifelse(crime$crime %in%c(OTHER OFFENSE","OTHEROFFENSE"),"OTHER",crime$crime)
crime$crime<-ifelse(crime$crime %in%c("KIDNAPPING","WEAPONSVIOLATION","OFFENSE INVOLVING CHILDREN"),"VIO",crime$crime)
table(crime$crime)
length(unique(crime$crime))
table(crime$crime)
qplot(crime$crime,xlab="Months",main="Crimes by Month")

#FINDINGS THAT LEADS TO ARREST
table(crime$ARREST)
qplot(crime$crime,xlab="Months",main="Crimes by Month")

#VISUALIZING CRIMES BY TIME OF DAY USING HEATMAP
temp<-aggregate(crime$crime,by=list(crime$crime,crime$Time.tag),FUN=length)
names(temp)<-c("crime","time.tag","count")
ggplot(temp,aes(x=crime,y=factor(time.tag)))+geom_tile(aes(fill=count))+scale_x_discrete("Crime",expand=c(0,0))+scale_y_discrete("Time of day",expand=c(0,-2))+
scale_fill_gradient("Number of crimes",low="white",high="steelblue")

#VISUALIZING CRIMES BY TIME OF DAY USING HEATMAP
temp1<-aggregate(ARREST~crime+Day,data=crime,FUN=length)
names(temp1)[3]<-"count"
 ggplot(temp1,aes(x=crime,y=Day,fill=count))+geom_tile(aes(fill=count))+scale_x_discrete("Crime",expand=c(0,0))+scale_y_discrete("Time of day",expand=c(0,-2))+
scale_fill_gradient("Number of crimes",low="white",high="steelblue")

#VISUALIZING CRIMES BY TIME OF MONTH OF THE YEAR USING HEATMAP
temp2<-aggregate(ARREST~crime+month,data=crime,FUN=length)
names(temp2)[3]<-"count"
 ggplot(temp2,aes(x=crime,y=month,fill=count))+geom_tile(aes(fill=count))+scale_x_discrete("Crime",expand=c(0,0))+scale_y_discrete("Time of day",expand=c(0,-2))+
scale_fill_gradient("Number of crimes",low="white",high="steelblue")



