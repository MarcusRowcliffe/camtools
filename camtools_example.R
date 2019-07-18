#Get camtools_1_0.R from github.com/MarcusRowcliffe/camtools
#Source code for manipulating and analysing trap rate data
source("C:/Users/rowcliffe.m/OneDrive - Zoological Society of London/GitHub/camtools/camtools_1_1.R")

path <- "C:/Users/rowcliffe.m/OneDrive - Zoological Society of London/GitHub/camtools/Christel19"
exifdat <- read.csv(file.path(path, "exifdata.csv"), stringsAsFactors = FALSE)
posdat <- read.csv(file.path(path, "posdat.csv"), stringsAsFactors = FALSE)
seqdat <- read.csv(file.path(path, "seqdat.csv"), stringsAsFactors = FALSE)



tagdat <- extract.tags(exifdat)
tagdat <- plyr::rename(tagdat, c(CreateDate="date", placeID="deployment"))
View(tagdat)
str(tagdat)
unique(tagdat$deployment)

#Some tidying up / temporary fixes
i <- is.na(tagdat$deployment)
tagdat[i,"deployment"] <- basename(dirname(tagdat$SourceFile))[i] #infer missing station IDs

tagdat$station <- substr(tagdat$deployment, 1, 2)
depdat <- read.csv(file.path(path, "depdat.csv"), stringsAsFactors = FALSE,
              colClasses = "character")

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


