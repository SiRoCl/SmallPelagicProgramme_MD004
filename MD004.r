#Small Pelagic Sampling Programme (MD004)
#@silvia Rodriguez Climent
# 15-05-2022; last modified : 05-10-2022
#---------------------------------------------------------------------##

# ===================================================--
# 0. Set directories----
# ===================================================--
rm(list=ls())
# set input, output directories
path.data="Y:/MD004G_Small pelagic sampling programme/Working_Area/2022-23/Database"

list.files(path.data,recursive=TRUE) # recursive TRUE to see inside the different folders

#creates output folder where the database is
dir.create(file.path(path.data,'results'),showWarnings=TRUE)
path.results <- "Y:/MD004G_Small pelagic sampling programme/Working_Area/2022-23/Database/results"

# load libraries
required.packages <- c( "ggplot2","mapdata","lubridate","plotrix","geosphere","RColorBrewer",
                        "rgdal","maptools","plyr","hexbin","raster","rgeos","xlsx","shapefiles",
                        "dplyr","tidyr","rgdal","mapdata", "RColorBrewer","data.table",
                        "devtools","maps","googleway","ggrepel","ggspatial",
                        "rnaturalearth","rnaturalearthdata","gtsummary","cowplot",
                        "sf","devtools","maps","ggspatial","tibble","janitor")

new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>=1){lapply(new.packages, install.packages)}
lapply(required.packages, require, character.only=T)


#0. Read data----
db0 <- read.xlsx(paste(path.data,"Small Pelagic Sampling Programme_Database.xlsx",sep="/"),1,stringsAsFactors = F)
summary(db0);dim(db0);names(db0);str(db0)
head(db0)

#explore the data
table(db0$Species)
table(db0$year,db0$Sample)
table(db0$month,db0$year)
summary(db0)

#see the NA
db0[is.na(db0$Weight..g.),]
db0[is.na(db0$Length),]

#add fishing season
for (i in 1:nrow(db0)){
  {if (db0$month[i]%in%c("9")&db0$year[i]=="2018")
    db0$fishingseason[i] <-"2018-2019"}
  {if (db0$month[i]%in%c("1","2")&db0$year[i]=="2019")
    db0$fishingseason[i] <-"2018-2019"}
  {if (db0$month[i]%in%c("8","9","10","11","12")&db0$year[i]=="2019")
    db0$fishingseason[i] <-"2019-2020"}
  {if (db0$month[i]%in%c("1","2")&db0$year[i]=="2020")
    db0$fishingseason[i] <-"2019-2020"}
  {if (db0$month[i]%in%c("8","9","10","11","12")&db0$year[i]=="2020")
    db0$fishingseason[i] <-"2020-2021"}
  {if (db0$month[i]%in%c("1","2")&db0$year[i]=="2021")
    db0$fishingseason[i] <-"2020-2021"}
  {if (db0$month[i]%in%c("6","7","8","9","10","11","12")&db0$year[i]=="2021")
    db0$fishingseason[i] <-"2021-2022"}
  {if (db0$month[i]%in%c("1","2")&db0$year[i]=="2022")
    db0$fishingseason[i] <-"2021-2022"}
  {if (db0$month[i]%in%c("6","7","8","9","10","11","12")&db0$year[i]=="2022")
    db0$fishingseason[i] <-"2022-2023"}
  {if (db0$month[i]%in%c("1","2")&db0$year[i]=="2023")
    db0$fishingseason[i] <-"2022-2023"}
}


table(db0$month,db0$year,db0$fishingseason)

head(db0);str(db0)

#split database into the different fishing seasons
db1 <- subset(db0,fishingseason=="2018-2019")
db2 <- subset(db0,fishingseason=="2019-2020")
db3 <- subset(db0,fishingseason=="2020-2021")
db4 <- subset(db0,fishingseason=="2021-2022")
db5 <- subset(db0,fishingseason=="2022-2023")

head(db1)
table(db1$fishingseason)
head(db2)
table(db2$fishingseason)
head(db3)
table(db3$fishingseason)
head(db4)
table(db4$fishingseason)
head(db5)
table(db5$fishingseason)


###
####1. Evolution of the programme through the years ----
###
## 1.1 Number of samples----

#Need to aggregate number of samples per year
head(db0)
str(db0)
count <- with(db0,aggregate(Sample,list(Species=Species,fishingseason=fishingseason),length))
count2 <- with(db0,aggregate(Sample,list(Species=Species,month=month,fishingseason=fishingseason),length))
count3 <- with(db0,aggregate(Sample,list(Species=Species,month=month,Lab=Lab,fishingseason=fishingseason),length))


#total number of individuals sampled per season
boxplot(count$x~count$fishingseason,xlab="Fishing season",ylab="Number of individuals sampled")
#Save plot
dev.print(device=png,filename=paste(path.results,"boxplot_samples_years.png",sep="/"),width=1000,height=800)


#total number of individuals sampled per season and species
ggplot(count, aes(x = fishingseason, y = x, fill = Species)) + theme_bw()+
  geom_col(position = position_dodge2(width=0.9,preserve="single"))+ xlab("fishing season")+ylab("Number of individuals sampled")

#Save plot
dev.print(device=png,filename=paste(path.results,"N_samples_years.png",sep="/"),width=1000,height=800)


#total number of individuals sampled per season,species and month
str(count2)
table(count2$month)
count2$month[count2$month=="6"] <- "Jun"
count2$month[count2$month=="7"] <- "Jul"
count2$month[count2$month=="8"] <- "Aug"
count2$month[count2$month=="9"] <- "Sep"
count2$month[count2$month=="10"] <- "Oct"
count2$month[count2$month=="11"] <- "Nov"
count2$month[count2$month=="12"] <- "Dec"
count2$month[count2$month=="1"] <- "Jan"
count2$month[count2$month=="2"] <- "Feb"

count2$month<- factor(count2$month,levels = c("Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb"),ordered=TRUE)  

ggplot(count2, aes(x = month, y = x, fill = Species)) + theme_bw()+
  geom_col(position=position_dodge2(width=0.9,preserve="single")) +
  facet_wrap(~fishingseason,ncol=2)+
  xlab("fishing season")+ylab("Number of individuals sampled")

#Save plot
dev.print(device=png,filename=paste(path.results,"N_samples_monthyears.png",sep="/"),width=1000,height=800)


#total number of individuals sampled per season,species and month and location
str(count3)
table(count3$month)
count3$month[count3$month=="6"] <- "Jun"
count3$month[count3$month=="7"] <- "Jul"
count3$month[count3$month=="8"] <- "Aug"
count3$month[count3$month=="9"] <- "Sep"
count3$month[count3$month=="10"] <- "Oct"
count3$month[count3$month=="11"] <- "Nov"
count3$month[count3$month=="12"] <- "Dec"
count3$month[count3$month=="1"] <- "Jan"
count3$month[count3$month=="2"] <- "Feb"

count3$month<- factor(count3$month,levels = c("Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb"),ordered=TRUE)  

table(count3$Lab)
count3$Lab <- tolower(count3$Lab)

ggplot(count3, aes(x = month, y = x, fill = Species)) + theme_bw()+
  geom_col(position=position_dodge2(width=0.9,preserve="single")) +
  facet_wrap(~fishingseason+Lab,ncol=2)+
  xlab("fishing season")+ylab("Number of individuals sampled")

#Save plot
dev.print(device=png,filename=paste(path.results,"N_samples_monthLabyears.png",sep="/"),width=1000,height=800)


## 1.2 Species-----
head(db0)
#db0_agg <- with(db0,aggregate(Length,list(Lab=Lab,Date.sampled=Date.sampled, Species=Species, Length=Length,fishingseason=fishingseason,month=month,year=year),length))

# ggplot(db0, aes(x=Length,color=Species)) + 
#   geom_histogram(fill="white",alpha=0.5,position="identity")+facet_grid(fishingseason~.)+theme_bw()

ggplot(db0,aes(x=Length, fill=Species)) + 
  geom_histogram(color="black",alpha=0.5, position="identity")+
  facet_grid(fishingseason~Species,scales="free")+theme_bw()+theme(legend.position = "none")

#Save plot
dev.print(device=png,filename=paste(path.results,"summary_Lengths_years.png",sep="/"),width=1000,height=800)



###
#### 2. Map of the different sampling locations ----
###

#bathy data
setwd("C:/Users/SRC01/OneDrive - CEFAS/SC/Rscripts/PELTIC_BubbleMaps/Geodata/")

bathy <- readOGR('.','Bathy_contours')
bathy <- bathy[bathy$Contour %in% c(-20,-50,-100,-200),]
# transform to lat/long
bathy <- spTransform(bathy, CRS("+proj=longlat +datum=WGS84 +no_defs"))
# convert to data frame so can be used by ggplot
bathyF <- fortify(bathy)
# attach contour values to fortified data frame
bathyF$contour <- bathy@data[bathyF$id,'Contour']


xlim <- c(-8.0,-2.0)
ylim <- c(49.5,51.5)

coast <- map_data("worldHires", xlim = xlim,ylim=ylim)
coast.poly <- geom_polygon(data=coast, aes(x=long, y=lat, group=group), colour= "#999999", fill="#999999", lwd=0.2)
# base map
base <- ggplot()+
  geom_path(data=bathyF,aes(x=long,y=lat,group=group,color=contour))+
  coast.poly+
  coord_quickmap(xlim,ylim)+
  xlab("Longitude")+ylab("Latitude")+
  theme_bw()+annotate(geom="text",x=-4.3, y=50.3, label="Plymouth",color="black",size=4,fontface="bold")+
  annotate(geom="text",x=-5.5, y=50.11, label="Newlyn",color="black",size=4,fontface="bold")+
  annotate(geom="text",x=-3.0,y=49.8,label="English Channel",color="blue",size=5,fontface="italic")+
  annotate(geom="text",x=-7.0,y=50.8,label="Celtic Sea",color="blue",size=5,fontface="italic")+
  annotate(geom="text",x=-4.5,y=50.15,label="Plymouth Sound",color="blue",size=3,fontface="italic")+
  annotate(geom="text",x=-5.6,y=49.95,label="Mounts Bay",color="blue",size=3,fontface="italic")+
  
  ggtitle("")+
  xlab("Longitude")+ylab("Latitude")+
  theme(panel.grid.major = element_line(color=gray(.5),linetype="dashed",size=0.5),panel.background = element_rect(fill="aliceblue"))+
  
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

base

ggsave(device=png,filename=paste(path.results,"map.png",sep="/"),limitsize = FALSE)
#dev.print(device=png,filename=paste(path.results,"map2.png",sep="/"),width=800,height=800)


###
#### 3. Current Year ----
###
head(db0)#global database
table(db5$fishingseason) #current year
table(db5$Lab)
db5$Lab <- tolower(db5$Lab)

db <- db5

##3.1 Total species collected----
distr <- table(db$Sample,db$year,db$month)

## Total species collected 
#counts <- table(db$Species)

#if ANE,HOM,MAC present run this
# barplot(counts, main="Pelagic Species sampled under the programme",
#         xlab="Pelagic Species", ylab="Number of individuals", col=c("green","yellow","red"),
#         ylim = c(0,1600),
#         space=c(0.1,0.1,0.1),cex.names = 2.5,cex.axis=2.5,cex.main=2.0) #ICES colours

# #if HOM,MAC present run this
# barplot(counts, main="Pelagic Species sampled under the programme",
#         xlab="Pelagic Species", ylab="Number of individuals", col=c("yellow","red"),
#         ylim = c(0,800),
#         space=c(0.1,0.1),cex.names = 2.0,cex.axis=2.0,cex.main=2.0) #ICES colours
# #Save plot
# #dev.print(device=png,filename=paste(path.results,"SPSP_species.png",sep="/"),width=1000,height=800)


#better plot
summary(db)
db.1 <- as.data.table(db)
ans <-db.1[, .(.N), by = .(Species)] # number of observations by vessel
a <- ggplot(ans,aes(Species,N,fill=Species))+geom_bar(stat="identity")+theme_bw()  #fill=as.factor(month)
a

#Save plot
dev.print(device=png,filename=paste(path.results,"SPSP_species.png",sep=""),width=1000,height=800)



## Total Species per month and year
# summary(db)
# db.1 <- as.data.table(db)

ans <-db.1[, .(.N), by = .(Vessel)] # number of observations by vessel
a <- ggplot(ans,aes(Vessel,N))+geom_bar(stat="identity") #fill=as.factor(month)
a

ans <- db.1[, .(.N), by = .(Species,month,year)] # number of observations by mont/year and Species
#put name into months and order them
ans$month[ans$month=="6"] <- "Jun"
ans$month[ans$month=="7"] <- "Jul"
ans$month[ans$month=="8"] <- "Aug"
ans$month[ans$month=="9"] <- "Sep"
ans$month[ans$month=="10"] <- "Oct"
ans$month[ans$month=="11"] <- "Nov"
ans$month[ans$month=="12"] <- "Dec"
ans$month[ans$month=="1"] <- "Jan"
ans$month[ans$month=="2"] <- "Feb"

ans$month<- factor(ans$month,levels = c("Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb"),ordered=TRUE)

a <- ggplot(ans,aes(Species,N,fill=as.factor(month)))+geom_bar(stat="identity")+theme_bw() #fill=as.factor(month)
a+facet_grid(rows=vars(month),vars(year),labeller = label_both)+theme(legend.position = "none")
#Save plot
dev.print(device=png,filename=paste(path.results,"SPSP_speciesbyMonthYear.png",sep="/"),width = 25, height = 20, units = "cm",res=300)


#Between labs----
ans <- db.1[, .(.N), by = .(Species,month,year,Lab)] # number of observations by Lab and Species

#put name into months and order them
ans$month[ans$month=="6"] <- "Jun"
ans$month[ans$month=="7"] <- "Jul"
ans$month[ans$month=="8"] <- "Aug"
ans$month[ans$month=="9"] <- "Sep"
ans$month[ans$month=="10"] <- "Oct"
ans$month[ans$month=="11"] <- "Nov"
ans$month[ans$month=="12"] <- "Dec"
ans$month[ans$month=="1"] <- "Jan"
ans$month[ans$month=="2"] <- "Feb"

ans$month<- factor(ans$month,levels = c("Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb"),ordered=TRUE)

a <- ggplot(ans,aes(Species,N,fill=as.factor(month)))+geom_bar(stat="identity")+theme_bw() #fill=as.factor(month)
a+facet_grid(rows=vars(month,year),vars(Lab),labeller = label_both)+theme(legend.position = "none")

#Save plot
dev.print(device=png,filename=paste(path.results,"SPSP_spmonthLAB.png",sep="/"),width=1000,height=800)


# 3.2 Length distributions----
#with the count of the observations
ggplot(db, aes(x=Length)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  facet_grid(rows=vars(Species))+theme_bw()

#separate plots with common axis
ggplot(db, aes(x=Length)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(Length, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)+
  #facet_grid(cols=vars(Species))
  facet_wrap(~Species)+theme_bw()


#by Lab (only MAC)
dbmac <- subset(db,Species=="MAC")

ggplot(dbmac, aes(x=Length)) +
  geom_histogram(binwidth=.5, colour="black", fill="grey") +
  geom_vline(aes(xintercept=mean(Length, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)+
  facet_grid(rows=vars(Lab))+ theme_bw()+
  theme (strip.text= element_text(size=15,color="black"))

#Save plot
dev.print(device=png,filename=paste(path.results,"SPSP_MAClabs.png",sep="/"),width=1000,height=800)



#3.3 by species in a loop----
var <- unique(db$Species)

for(i in 1:length(var)){
  aux=db[db$Species==var[i],] #selecciona species
  tit=paste(var[i])
  #aux2<- subset(aux,!value==0)
  png(paste(path.results,"/Length_",tit,".png",sep=""),width=800,height=800,res=100)
  print(a <- ggplot(aux, aes(x=Length)) +
          geom_histogram(binwidth=.5, colour="black", fill="white") +
          geom_vline(aes(xintercept=mean(Length, na.rm=T)),color="red", linetype="dashed", size=1)+
          theme_bw())
  a+scale_y_continuous(expand = c(0, 0))
  dev.off()
}



#3.3 by species in a loop----
var <- unique(db$Species)

for(i in 1:length(var)){
  aux=db[db$Species==var[i],] #selecciona species
  tit=paste(var[i])
  #aux2<- subset(aux,!value==0)
  png(paste(path.results,"Length_",tit,".png",sep=""),width=800,height=800,res=100)
  print(a <- ggplot(aux, aes(x=Length)) +
          geom_histogram(binwidth=.5, colour="black", fill="white") +
          geom_vline(aes(xintercept=mean(Length, na.rm=T)),color="red", linetype="dashed", size=1)+
          theme_bw())
  a+scale_y_continuous(expand = c(0, 0))
  dev.off()
}


#your data
db

#your species
species <- c("MAC","HOM","ANE")
species[1]

#loop (check names are correct in line 27 or change them)
plot1.TL1 <- list()
plot1.TL2 <- list()
plot1.TL3 <- list()

for(i in 1:length(species)){
  
  plot1=db[db$Species==species[i],]
  
  names(plot1) <- c("lab","sample", "date","lat","lon","species","length_cm","weight_g","sex","maturity","tray","cell","sampler","vessel","ICESrect","notes","otholitread","picture","month","year","fishingseason")
  
  # Length distribution per month
  plot1_cumMonth <- data.table(table(plot1$length_cm,plot1$month))
  plot1_cum <- data.table(table(plot1$length_cm))
  names(plot1_cumMonth) <- c("TL", "Month", "Freq")
  names(plot1_cum) <- c("TL", "Freq")
  
  # weighted average overall and weighted average by month
  plot1_cumMonth[,TL:=as.numeric(TL)]
  plot1_cumMonth[,Freq:=as.numeric(Freq)]
  plot1_cumMonth[,wtMean:=weighted.mean(TL,Freq),by="Month"]
  
  plot1_cum[,TL:=as.numeric(TL)]
  plot1_cum[,wtMean:=weighted.mean(TL,Freq)]
  
  #put name into months and order them
  plot1_cumMonth$Month[plot1_cumMonth$Month=="6"] <- "Jun"
  plot1_cumMonth$Month[plot1_cumMonth$Month=="7"] <- "Jul"
  plot1_cumMonth$Month[plot1_cumMonth$Month=="8"] <- "Aug"
  plot1_cumMonth$Month[plot1_cumMonth$Month=="9"] <- "Sep"
  plot1_cumMonth$Month[plot1_cumMonth$Month=="10"] <- "Oct"
  plot1_cumMonth$Month[plot1_cumMonth$Month=="11"] <- "Nov"
  plot1_cumMonth$Month[plot1_cumMonth$Month=="12"] <- "Dec"
  plot1_cumMonth$Month[plot1_cumMonth$Month=="1"] <- "Jan"
  plot1_cumMonth$Month[plot1_cumMonth$Month=="2"] <- "Feb"
  
  plot1_cumMonth$Month<- factor(plot1_cumMonth$Month,levels = c("Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb"),ordered=TRUE)  
  
  #save the file
  write.csv(plot1_cumMonth,paste(path.results,"/",species[i],"_cumMonth.csv",sep=""),row.names = F)
  write.csv(plot1_cum,paste(path.results,"/",species[i],"_cum.csv",sep=""),row.names = F)
  
  ##Overall LFD--
  plot1.TL1[[i]] <- ggplot(plot1_cum, aes(TL, Freq)) + geom_bar(stat="identity", position="dodge",fill="grey") +
    theme_bw(25) +ylab("N")+xlab("Total length (cm)")+ 
    geom_vline(aes(xintercept=wtMean), col="red", size=1.2,lty=2)+
    theme(legend.position = "none")+ggtitle(paste(species[i]))+
    scale_y_continuous(limits=c(0,max(plot1_cum$Freq)+10),expand=c(0,0))
  
  ggsave(filename = paste(path.results,paste(species[i],"aTL.png",sep="_"),sep="/"), 
         plot = plot1.TL1[[i]], width = 25, 
         height = 20, units = "cm", dpi = 300, type = "cairo-png") 
  
  
  ##LFD by month--
  plot1.TL2[[i]] <- ggplot(plot1_cumMonth, aes(TL,Freq, group=factor(Month), fill=factor(Month))) + geom_bar(stat="identity") + 
    theme_bw(25) + scale_y_continuous(limits=c(0,max(plot1_cumMonth$Freq)+10), expand=c(0,0)) + 
    ylab("N") + xlab("Total length (cm)")+ facet_grid(rows = vars(Month)) + #facet_grid(.~Month) by column
    geom_vline(aes(xintercept=wtMean), col="red", size=1.2,lty=2) +
    theme(legend.position="none")+
    ggtitle(paste(species[i],unique(plot1$processor))) 
  
  #make text smaller
  plot1.TL3[[i]] <- plot1.TL2[[i]]+theme(text = element_text(size=rel(5.0)))+ scale_fill_brewer(palette="Set1")
  
  ggsave(filename = paste(path.results,paste(species[i],unique(plot1$processor),"bTLbyMonth.png",sep=""),sep="/"), 
         plot = plot1.TL3[[i]], width = 25, 
         height = 20, units = "cm", dpi = 300, type = "cairo-png") 
  
}


#3.4 Linear regression by species in a loop-----

#your data
db

#your species
species <- c("MAC","HOM","ANE")
species[1]

#loop (check names are correct in line 27 or change them)
SP_LWrel <- list()

for(i in 1:length(species)){
  
  plot1=db[db$Species==species[i],]
  
  names(plot1) <- c("lab","sample", "date","lat","lon","species","length_cm","weight_g","sex","maturity","tray","cell","sampler","vessel","ICESrect","notes","otholitread","picture","month","year","fishingseason")
  
  #Fit a Linear regression (cm/g) and log transform--
  plot1<- subset(plot1,length_cm>0) #delete the zeros
  
  plot1$logL <- log(plot1$length_cm)
  plot1$logW <- log(plot1$weight_g)

  fit <- lm(logW~logL,data=plot1)
  #fitPlot(fit,xlab="log Total Length (cm)",ylab="log Weight (g)",main="plot1") # not working with new FSAmisc
  summary(fit)
  par(mfrow=c(2,2));plot(fit)# LWR normally do not follow a regression pattern
  
  a= exp(coefficients(fit)[1])
  b= coefficients(fit)[2] #(intercept)
  
  
  #fit the linear regression
  ggplotRegression <- function (fit) {  
    require(ggplot2)  
    ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
      geom_point() +
      stat_smooth(method = "lm", col = "red")+
      ggtitle(paste(species[i],"\n","Adj R2 = ",signif(summary(fit)$adj.r.squared,2),
                    "; a =",round(exp(signif(fit$coef[[1]])),5),
                    "; b =",signif(fit$coef[[2]],3),
                    "; p-value =",signif(summary(fit)$coef[2,4],5)))+
      theme_bw()
  }
  
  SP_LWrel[[i]] <- ggplotRegression(lm(logW~logL,data=plot1))
  
  #print(SP_LWrel[[i]])
  
  ggsave(filename = paste(path.results,paste(species[i],unique(plot1$Species),"LWrel.png",sep="_"),sep="/"), 
         plot = SP_LWrel[[i]], width = 25, 
         height = 20, units = "cm", dpi = 300, type = "cairo-png") 

}

print(SP_LWrel[[1]])
print(SP_LWrel[[2]])
print(SP_LWrel[[3]])


#Mac species between labs----
i=1
plot1=db[db$Species==species[i],]
table(plot1$Species)
par(mfrow=c(1,1))

head(plot1)

ggplot(plot1, aes(log(Length), log(Weight..g.), col=Lab)) + geom_point() +
  geom_smooth(method="lm", se=TRUE)+theme_bw()+xlab("LogL")+ylab("logW")+
  scale_color_brewer(palette = "Dark2")

#Save plot
dev.print(device=png,filename=paste(path.results,"SPSP_MACLWRlabs.png",sep="/"),width=1000,height=800)


#3.5 Otholits and sex sampled----
summary(db)
db <- data.frame(db)
head(db)
table(db$Cell!=0)# 1200 otholits collected this season (1065 last year)

otolits <- subset(db,Cell>0)
dim(otolits) #otholits collected this season

otolits%>%group_by(fishingseason,Species)%>%
  summarize(sum=length(Cell))

otolits%>%group_by(fishingseason,month)%>%
  summarize(sum=length(Cell))

#some summaries
oto <- otolits%>%group_by(fishingseason,month,Species,Lab)%>%
  summarize(sum=length(Cell))

otosum <- otolits%>%group_by(month)%>%
  summarize(sum=length(Cell))


oto <- as.data.frame(oto)

oto$month[oto$month=="6"] <- "Jun"
oto$month[oto$month=="7"] <- "Jul"
oto$month[oto$month=="8"] <- "Aug"
oto$month[oto$month=="9"] <- "Sep"
oto$month[oto$month=="10"] <- "Oct"
oto$month[oto$month=="11"] <- "Nov"
oto$month[oto$month=="12"] <- "Dec"
oto$month[oto$month=="1"] <- "Jan"
oto$month[oto$month=="2"] <- "Feb"

oto$month <- factor(oto$month,levels = c("Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb"),ordered=TRUE)  

a <- ggplot(oto,aes(Species,sum,fill=Species))+geom_bar(stat = "identity")+theme_bw()
a+facet_grid(rows=vars(month))+scale_x_discrete(name= "Species")+scale_y_continuous(name= "Number of otholits")+theme(legend.position = "none")
#Save plot
dev.print(device=png,filename=paste(path.results,"SPSP_Numotholits.png",sep="/"),width=1000,height=800)


#per lab
a <- ggplot(oto,aes(Species,sum,fill=Species))+geom_bar(stat = "identity")+theme_bw()
a+facet_grid(rows=vars(month),cols=vars(Lab))+scale_x_discrete(name= "Species")+scale_y_continuous(name= "Number of otholits")+theme(legend.position = "none")
#Save plot
dev.print(device=png,filename=paste(path.results,"SPSP_NumotholitsLAB.png",sep="/"),width=1000,height=800)


#3.6 Some numbers for the report----
summary(db)
db.1 <- as.data.table(db)
db.1[, .(.N),] #Number of samples

table(db.1$Lab)
db.1$Lab <- tolower(db.1$Lab)
db.1[, .(.N), by = .(Lab)] # number of observations by Lab

table(db.1$Sampler)
db.1$Sampler <- tolower(db.1$Sampler)
db.1$Sampler[db.1$Sampler=="éilís "] <- "éilís"
db.1[, .(.N), by = .(Sampler)] # number of observations by Sampler


table(db.1$Species)#Species sampled

db.1$Notes <- tolower(db.1$Notes)
table(db.1$Notes)# fresh versus frozen samples


db%>%group_by(month,year,fishingseason,Species)%>%
  summarize(sum=length(Length))

db%>%group_by(fishingseason,Species)%>%
  summarize(sum=length(Length))

db%>%group_by(fishingseason)%>%
  summarize(sum=length(Length))

db%>%group_by(fishingseason,Species)%>%
  summarize(mean_L=mean(Length),
            min_L=min(Length),
            max_L=max(Length))

db%>%group_by(fishingseason,Species)%>%
  summarize(mean_L=mean(Length),
            min_L=min(Length),
            max_L=max(Length))



#compare with last year
lasty <-db4%>%group_by(fishingseason,Species)%>%
  summarize(mean_L=mean(Length),
            min_L=min(Length),
            max_L=max(Length),
            sum=length(Sample))


comp <- db%>%group_by(fishingseason,Species,Lab)%>%
  summarize(mean_L=mean(Length),
            min_L=min(Length),
            max_L=max(Length),
            sum=length(Sample))

#
write.csv(comp,paste(path.results,'compotheryears.csv',sep="/"),row.names=F)

#samples by Lab and month this year
resum <- table(db$Species,db$month,db$year)
resum

#number of samples per species and sampling day
with(db,aggregate(Length,list(Sample=Sample,Species=Species,month=month,year=year),length))


#3.7 Variation of weight by month----
db$month[db$month=="6"] <- "Jun"
db$month[db$month=="7"] <- "Jul"
db$month[db$month=="8"] <- "Aug"
db$month[db$month=="9"] <- "Sep"
db$month[db$month=="10"] <- "Oct"
db$month[db$month=="11"] <- "Nov"
db$month[db$month=="12"] <- "Dec"
db$month[db$month=="1"] <- "Jan"
db$month[db$month=="2"] <- "Feb"

db$month <- factor(db$month,levels = c("Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb"),ordered=TRUE)  


# w <- ggplot(db, aes(as.factor(month),Weight..g., color = Species)) +
#   geom_point()+theme_minimal_grid(12)+
#   labs(title = "",y="Individual weight (g)",x="Month")
# 
# w + facet_grid(rows=vars(Species))
# 
# #Save plot
# #dev.print(device=png,filename=paste(path.results,"Weight_variationsp.png",sep="/"),width=1000,height=800)
# 
# 
# 
# #same grapgh with geom_violin()
# w <- ggplot(db, aes(as.factor(month),Weight..g., color = Species)) +
#   geom_violin()+theme_minimal_grid(12)+geom_jitter()
# w+labs( title= "", y="Individual weight (g)", x = "Month")
# 
# #print(w+labs( title= "", y="Individual weight (g)", x = "Month"))
# 
# w1 <- w + geom_violin() + geom_jitter(height = 0, width = 0.1)+facet_grid(rows=vars(Species))
# 
# #Save plot
# #dev.print(device=png,filename=paste(path.results,"Weight_variationsp_2.png",sep="/"),width=1000,height=800)


#geom_violin (selected graph)
p <- ggplot(db, aes(month, Weight..g.,fill=Species))
p2 <- p + geom_violin()+ylab("Weight (g)")+xlab("Month")

p2 + facet_grid(rows=vars(Species))+theme_bw()+theme(legend.position = "none")

#Save plot
dev.print(device=png,filename=paste(path.results,"Weight_variationsp_3.png",sep="/"),width=1000,height=800)



#3.8 Age data-----
list.files(path.data)
#2 date formats so i am reading separately to make it easier
ages1 <- read.xlsx(paste(path.data,"2019-22 GARI bio data retrieval(1).xlsx",sep="/"),1,detectDates=TRUE)
ages2 <- read.xlsx(paste(path.data,"2019-22 GARI bio data retrieval(2).xlsx",sep="/"),1,detectDates=TRUE)

summary(ages1);dim(ages1);names(ages1);str(ages1)
summary(ages2);dim(ages2);names(ages2);str(ages2)


ages1 <- ages1[,c("fldSampleEventDate","fldProjectDescription","fldSpeciesID","PortOfSampling","fldBiologicalSampleTypeID",
                  "fldNumberInSample","fldIndividualIndex","fldSize","fldWeight","fldMaturityID","fldDoNotUseIndividual",
                  "X025.Bio.Param.Age.............................fldParameterFinalValue")]

names(ages1) <- c("date","description","species","samplingport","typeID","numberinsample","indindex","size","weight","maturity","donotuseind","age")

ages2 <- ages2[,c("fldSampleEventDate","fldProjectDescription","fldSpeciesID","PortOfSampling","fldBiologicalSampleTypeID",
                  "fldNumberInSample","fldIndividualIndex","fldSize","fldWeight","fldMaturityID","fldDoNotUseIndividual",
                  "X025.Bio.Param.Age.............................fldParameterFinalValue")]

names(ages2) <- c("date","description","species","samplingport","typeID","numberinsample","indindex","size","weight","maturity","donotuseind","age")

head(ages1)
head(ages2)

is.Date(ages1$date)
is.Date(ages2$date) #this one is in the correct format (yyyy-mm-dd)

table(ages1$date)
table(ages2$date)

ages1$date <- as.Date(ages1$date,format = "%m/%d/%y",origin="1970-01-01")


#merge them now
ages <- rbind(ages1,ages2)
dim(ages)
dim(ages1);dim(ages2)

head(ages)

table(ages$date)

ages$description[ages$description=="Cefas Port Sampling Programme                                   "] <- "Cefas Port Sampling Programme"
ages$samplingport[ages$samplingport=="LOWESTOFT                                                       "] <- "LOWESTOFT"
ages$samplingport[ages$samplingport=="NEWLYN                                                          "] <- "NEWLYN"
ages$samplingport[ages$samplingport=="PLYMOUTH                                                        "] <- "PLYMOUTH"

head(ages)
ages$year <- year(ages$date)
table(ages$year)

#remove lowestoft port
table(ages$samplingport)
ages <- subset(ages,!samplingport=="LOWESTOFT")

#remove NA for ages
ages3 <- ages[!is.na(ages$age),]
summary(ages3)
ages <- ages3

#plot(ages$age~ages$date)
#ggplot(ages, aes(date, age,colour=species,shape=samplingport))+geom_point(size=2)
ggplot(ages, aes(date, age,colour=species)) +geom_boxplot()
ageyear <- ggplot(ages, aes(species, age))+geom_boxplot(size=0.5)+facet_grid(rows=vars(year))
#ggplot(ages, aes(species, age))+geom_boxplot(size=0.5)+facet_grid(rows=vars(samplingport))
agesplab <- ggplot(ages, aes(species, age,colour=samplingport))+geom_boxplot(size=0.5)+theme_bw()

#Save plot
ggsave(filename = paste(path.results,paste("Ages_year.png",sep=""),sep="/"),plot = ageyear, width = 25,height = 20, units = "cm", dpi = 300, type = "cairo-png") 
ggsave(filename = paste(path.results,paste("Ages_sp_lab.png",sep=""),sep="/"),plot = agesplab, width = 25,height = 20, units = "cm", dpi = 300, type = "cairo-png") 


###############################################  END  ############################################################################----