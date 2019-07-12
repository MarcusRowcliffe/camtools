#Get camtools_1_0.R from github.com/MarcusRowcliffe/camtools
#Source code for manipulating and analysing trap rate data
source("C:/Users/rowcliffe.m/OneDrive - Zoological Society of London/GitHub/camtools/camtools_1_0.R")

path <- "D:/Survey_xxx"
exifdat <- read.csv(file.path(path, "exifdata.csv"), stringsAsFactors = FALSE)
posdat <- read.csv(file.path(path, "posdat.csv"), stringsAsFactors = FALSE)
seqdat <- read.csv(file.path(path, "seqdat.csv"), stringsAsFactors = FALSE)



tagdat <- extract.tags(exifdat)
tagdat <- plyr::rename(tagdat, c(CreateDate="date", placeID="station"))
View(tagdat)
str(tagdat)

#Some tidying up / temporary fixes
i <- is.na(tagdat$station)
tagdat[i,"station"] <- basename(dirname(tagdat$SourceFile))[i] #infer missing station IDs
#create deployment data
dt <- as.POSIXct(tagdat$date, format="%Y:%m:%d %H:%M:%S", tz="UTC")
trange <- with(tagdat, tapply(dt, station, range))
orgn <- as.POSIXct("1970:01:01 00:00:00", format="%Y:%m:%d %H:%M:%S", tz="UTC")
stt <- as.character(as.POSIXct(unlist(lapply(trange, function(x) x[1]))-36000, tz="UTC",origin=orgn), format="%Y:%m:%d %H:%M:%S", tz="UTC")
stp <- as.character(as.POSIXct(unlist(lapply(trange, function(x) x[2]))+36000, tz="UTC",origin=orgn), format="%Y:%m:%d %H:%M:%S", tz="UTC")
stn <- sort(unique(tagdat$station))
depdat <- data.frame(station=stn, start=stt, stop=stp, stringsAsFactors = FALSE)
depdat$station <- substr(depdat$station, 1, 2)
tagdat$station <- substr(tagdat$station, 1, 2)
rownames(depdat) <- NULL
View(depdat)
depdat[8:14,]
head(tagdat[,c(2,4,5,6,8,9)])
write.csv(depdat, file.path(path, "depdat.csv"), row.names = FALSE)
q <- read.csv(file.path(path, "depdat.csv"), stringsAsFactors = FALSE,
              colClasses = "character")
q$station


#Visual check and create trap rate data
par(mfrow=c(1,1))
contactdat <- subset(tagdat, contact==1)
plot.deployments(contactdat, depdat)
chk <- check.dates(contactdat, depdat)
chk$bad.data

#Create trap rate data
trdat <- event.count(contactdat, depdat)
head(trdat)

#Define species to analyse
sp <- "Fox"

#Activity analysis
library(activity)
tms <- subset(contactdat, species==sp)$time
plot(fitact(tms))
actmod <- fitact(tms, 
                 bounds=c(18,8)*pi/12,
                 sample="data",
                 reps=100)
plot(actmod)
actmod@act

#Detection zone analysis
source("C:/Users/rowcliffe.m/OneDrive - Zoological Society of London/GitHub/distanceDF/distancedf.r")
#Get ditancedf.r from github.com/MarcusRowcliffe/distanceDF

dzdat <- subset(posdat, frame_count==1 & species==sp)
dzdat$angle <- abs(dzdat$angle)
hist(dzdat$radius)
radmod <- fitdf(radius~1, dzdat, key="hr", transect="point", order=0)
plot(radmod$ddf, pdf=TRUE)
radmod$edd

angmod <- fitdf(angle~1, dzdat, order=0)
plot(angmod$ddf)
angmod$edd


#Speed analysis
source("C:/Users/rowcliffe.m/OneDrive - Zoological Society of London/GitHub/sbd/sbd_1_0.r")
#Get sbd_1_0.r from github.com/MarcusRowcliffe/sbd
spdat <- subset(seqdat, species==sp & speed>0.001 & speed<10)
(spdest <- hmean(spdat$speed))

#Density analysis
param <- list(v = spdest["mean"] * 14*60^2 / 1000,
              p = actmod@act["act"],
              r = radmod$edd$estimate / 1000,
              theta = angmod$edd$estimate * 2)
paramse <- list(v = spdest["se"] * 14*60^2 / 1000,
                p = actmod@act["se"],
                r = radmod$edd$se / 1000,
                theta = angmod$edd$se * 2)
bootTRD(trdat$Fox, trdat$effort.days, param, paramse)

<<<<<<< HEAD



#############################
#Christel19
edat <- read.csv("C:/Users/rowcliffe.m/OneDrive - Zoological Society of London/GitHub/camtools/Christel19/exifdata.csv",
                 stringsAsFactors = F)
edat <- edat[-c(7099,7101,91,4709,6227),]
adat <- extract.tags(edat)
View(adat)
=======
install.packages(c("rmarkdown", "tinytex"))
>>>>>>> 13a77a773ff0a35bc4537469ba95b66981433a17
