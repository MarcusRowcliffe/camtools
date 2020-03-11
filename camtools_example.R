#Code as implemented in camtools.pdf vignette

###DATA PREPARATION
source("camtools.R")

path <- "./Data"

exifdat <- read.csv(file.path(path, "exifdata.csv"), stringsAsFactors = FALSE)
posdat <- read.csv(file.path(path, "posdat.csv"), stringsAsFactors = FALSE)
seqdat <- read.csv(file.path(path, "seqdat.csv"), stringsAsFactors = FALSE)
depdat <- read.csv(file.path(path, "depdat.csv"), stringsAsFactors = FALSE)
depdat[8:14,]

tagdat <- extract.tags(exifdat)
tagdat <- plyr::rename(tagdat, c(CreateDate="date", placeID="station"))
head(tagdat)

contactdat <- subset(tagdat, contact==1)
plot.deployments(contactdat, depdat)
chk <- check.dates(contactdat, depdat)
chk$bad.data
trdat <- event.count(contactdat, depdat)
head(trdat)

###ANALYSIS
sp <- "Fox"

#Activity analysis
library(activity)
actmod <- fitact(subset(contactdat, species==sp)$time, 
bounds=c(18,8)*pi/12,
sample="data",
reps=100)
actmod@act
plot(actmod, centre="night", dline=list(col="grey"))

#Speed estimation
source("C:/Users/rowcliffe.m/OneDrive - Zoological Society of London/GitHub/sbd/sbd.r")
speeds <- subset(seqdat, species==sp )$speed
hist(log10(speeds), main="", xlab="Speed (log10[m/s])")
speeds <- subset(seqdat, species==sp & speed>0.001 & speed<10)$speed
(spdest <- hmean(speeds))

### Detection zone estimation
source("C:/Users/rowcliffe.m/OneDrive - Zoological Society of London/GitHub/distanceDF/distancedf.r")
dzdat <- subset(posdat, frame_count==1 & species==sp)
radmod <- fitdf(radius~1, dzdat, transect="point", key="hr", order=0, truncation=10)
radmod$edd
plot(radmod$ddf, pdf=TRUE)
dzdat$angle <- abs(dzdat$angle)
angmod <- fitdf(angle~1, dzdat, order=0)
angmod$edd
plot(angmod$ddf)

#Density estimation
param <- list(v = spdest["mean"] * 14*60^2 / 1000,
              p = actmod@act["act"],
              r = radmod$edd$estimate / 1000,
              theta = angmod$edd$estimate * 2)
paramse <- list(v = spdest["se"] * 14*60^2 / 1000,
                p = actmod@act["se"],
                r = radmod$edd$se / 1000,
                theta = angmod$edd$se * 2)
bootTRD(trdat[, sp], trdat$effort.days, param, paramse)

