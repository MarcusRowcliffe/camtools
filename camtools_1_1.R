library(data.table) #for rbindlist, used in read.multi
library(readxl) #for read_xls and read_xlsx, used in read.multi

#READING DATA#############################################

#defactor

#Converts factors to numeric if possible, otherwise character. Non-factor
#data are returned unchanged.

#INPUT
# dat: a factor or dataframe to convert

#OUTPUT
# If a single factor vector supplied, a character or numeric vector;
# if a dataframe supplied, a dataframe with any factor columns defactored.

defactor <- function(dat){
  f <- function(x){
    if(class(x)!="factor") res <- x else{
      cha <- as.character(x)
      num <- suppressWarnings(as.numeric(cha))
      if(all(is.na(cha)==is.na(num))) res<-num else res<-cha
    }
    res
  }
  if(is.null(ncol(dat))) f(dat) else
    data.frame(lapply(dat, f), stringsAsFactors=FALSE)
}

#decimal.time#

#Converts text time data to decimal time of day. Default format hh:mm:ss, but can handle 
#other separators and minutes and seconds can be missing.

#INPUT
# dat: an array of character times, with hours, minutes and seconds in that order separated by sep.
# sep: the character used to separate time components.

#OUTPUT
# An array of decimal times in hours.

decimal.time <- function(dat, sep=":"){
  dat <- as.character(dat)
  f1 <- function(x, i) x[i]
  f2 <- function(x){
    res <- as.numeric(x[1])
    if(length(x)>1) res <- res+as.numeric(x[2])/60
    if(length(x)>2) res <- res+as.numeric(x[3])/60^2
    res/24
  }
  spaces <- unique(grepl(" " , dat))
  if(length(spaces)!=1) stop("Time formats don't seem to be consistent")
  if(spaces) dat <- unlist(lapply(strsplit(dat, " "), f1, 2))
  tt <- strsplit(as.character(dat), sep)
  unlist(lapply(tt, f2))
}

#extract.tags#

#Extracts and separates tags from image metadata. When contacts are flagged and/or
#species labelled, expects the respective column headings "contact" and "species" 
#to be used. Where multiple species or individuals per image are tagged, these 
#headings should be suffixed with integers indicating the ith contact (eg contact2
#and species2 for the second species contact in an image).

#INPUT

#dat: dataframe of image metadata containing tags
#fieldsep: character(s) used to separate fields in the tags
#headsep: character(s) used to separate headings from values within fields
#tagcol: name of the column in dat containing the tags
#exifcols: names of other columns in dat to retain in the output

#OUTPUT

#A dataframe of tag data separated into columns, with any additional columns 
#specified by exifcols. When CreateDate is included in exifcols, an additional 
#radian time of day column is added. When more than one animal is recorded per 
#image, second and further records are appended as additional rows.

extract.tags <- function(dat, fieldsep=", ", headsep=": ", tagcol="Keywords",
                         exifcols=c("SourceFile", "CreateDate", tagcol)){
  
  f1 <- function(x,i) if(i==1) x[i] else
      if(length(x)==1) 1 else x[i]
  f2 <- function(x, i) lapply(x, f1, i=i)
  
  tag.error.check <- function(){
    combis <- table(dat2$row, dat2$head)
    duff <- as.vector(which(combis>1, TRUE)[,"row"])
    
    if(length(duff)>0)
      dat2[dat2$row %in% duff, "head"] <- 1:sum(dat2$row %in% duff)
    dat3 <- reshape::cast(dat2, row~head, value="val")
    dat3 <- defactor(dat3)
    nms <- names(dat3)
    cnms <- nms[grep("contact", nms)]
    n <- substr(cnms, nchar(cnms), nchar(cnms))
    for(i in n){
      ci <- paste0("contact", i)
      si <- paste0("species", i)
      if(si %in% names(dat3)){
        duff <- c(duff, which(!is.na(dat3[,ci]) & is.na(dat3[,si])))
      } else
        duff <- c(duff, which(!is.na(dat3[,ci])))
    }
    if(length(duff)>0)
      stop(paste("Tagging errors found in row(s):", 
               paste(sort(duff), collapse=" "))) else
                 dat3
  }

  dat1 <- as.character(dat[, tagcol])
  dat1[which(dat1=="")] <- "NoTags"
  datlist1 <- strsplit(dat1, fieldsep)
  datlist2 <- lapply(datlist1, strsplit, headsep)
  dat2 <- data.frame(row = rep(1:length(datlist2), lapply(datlist2, length)),
                     head = unlist(lapply(datlist2, f2, i=1)),
                     val = unlist(lapply(datlist2, f2, i=2)),
                     stringsAsFactors = FALSE)
  dat3 <- tag.error.check()
  edat <- dat[, exifcols]
  if("CreateDate" %in% exifcols){
    cd <- as.character(edat$CreateDate)
    times <- unlist(lapply(strsplit(cd, " "), function(x) x[2]))
    edat$time <- decimal.time(times)*2*pi
    nas <- which(is.na(edat$time))
    if(length(nas)>0) 
      warning(paste("CreateDate format was not recognisable in row(s):",
                    paste(nas, collapse="")))
  }
  dat4 <- cbind(edat, dat3[, -1])
  dat5 <- dat4
  spcols <- names(dat4)[grepl("species", names(dat4))]
  if(length(spcols)>1){
    for(sp in spcols[-1])
      dat5 <- rbind(dat5, subset(dat4, !is.na(dat4[,sp])))
  }
  dat5 <- dat5[, !names(dat5) %in% c(spcols[-1], sub("species", "contact", spcols[-1]))]
  names(dat5)[pmatch(c("contact","species"), names(dat5))] <- c("contact","species")
  dat5
}

#read.multi
#Read and stack multiple data files into a single dataframe.
#Reads the following file types using the relevant read function:
#  TYPE  FUNCTION
#  csv   read.csv
#  txt   read.table
#  xls   read_xls (from readxl package)
#  xlsx  read_xlsx (from readxl package)
#Types other than that specified are ignored. Data files must have congruent columns if
#fill=FALSE, otherwise matches columns where possible and back-fills with missing
#values when not.

#INPUT
# dir: directory path name in which to serch for files
# type: extension of files to read
# recursive: whether to search in sub-directories
# match.names: if true stacks data with matching column names;
#              if false simply stacks columns in order
# idcol: whether to add an ID column; taken from file names if TRUE
# ...: other arguments passed to the relevant read function - see those functions for options

#OUTPUT
# A dataframe stacking all file contents together.
read.multi <- function(dir, type=c(".csv", ".txt", ".xls", ".xlsx"), 
                       recursive=FALSE, match.cols=FALSE, idcol=FALSE, trimto=NULL, ...){
  type <- match.arg(type)
  files <- list.files(dir, full.names=TRUE, recursive=recursive)
  files <- files[grep(type, files)]
  dat <- switch(type,
                .csv=lapply(files, read.csv, ...),
                .txt=lapply(files, read.table, ...),
                .xls=lapply(files, function(x) as.data.frame(read_xls(x, ...))),
                .xlsx=lapply(files, function(x) as.data.frame(read_xlsx(x, ...)))
  )
  if(!match.cols)
    for(i in 1:length(dat)) names(dat[[i]]) <- paste0("X", 1:ncol(dat[[i]]))
  if(!is.null(trimto))
    for(i in 1:length(dat)) dat[[i]] <- dat[[i]][, 1:trimto]
  if(idcol)
    names(dat) <- sub("\\..*$", "", basename(files))
  rbindlist(dat, fill=TRUE, idcol=idcol)
}

error.check <- function(obsdat, tz, format, depdat=NULL){
  msgs1 <- c("obsdat must contain columns station, species and date",
             "depdat must contain columns station, start and stop")
  msgs2 <- c("Some dates in obsdat are missing or not convertible to POSIXct",
            "Some dates in depdat are missing or not convertible to POSIXct",
            "Not all stations in obsdat have data in depdat")
  
  if(is.null(depdat)){
    if(!all(c("species","station","date") %in% names(obsdat))) 
      stop(msgs1[1])
  } else{
    ok <- c(all(c("species","station","date") %in% names(obsdat)),
            all(c("station","start","stop") %in% names(depdat)))
    if(any(!ok)) stop(paste(msgs1[!ok], collapse="\n  "))
  }

  obsstn <- as.character(obsdat$station)
  obs <- as.numeric(as.POSIXct(obsdat$date, format=format, tz=tz))
  if(!is.null(depdat)){
    depstn <- as.character(depdat$station)
    start <- as.numeric(as.POSIXct(depdat$start, format=format, tz=tz))
    stop <- as.numeric(as.POSIXct(depdat$stop, format=format, tz=tz))
  }

  if(is.null(depdat)){
    if(any(is.na(obs))) stop(msgs2[1])
  } else{
    ok <- c(!any(is.na(obs)), 
            !any( is.na(start) | is.na(stop) ),
            all(unique(obsstn) %in% depstn))
    if(any(!ok)) stop(paste(msgs2[!ok], collapse="\n  "))
  }
}

check.dates <- function(obsdat, depdat, format="%Y:%m:%d %H:%M:%S", tz="UTC"){
  error.check(obsdat, tz, format, depdat)
  
  obsstn <- as.character(obsdat$station)
  depstn <- as.character(depdat$station)
  obs <- as.numeric(as.POSIXct(obsdat$date, format=format, tz=tz))
  start <- as.numeric(as.POSIXct(depdat$start, format=format, tz=tz))
  stop <- as.numeric(as.POSIXct(depdat$stop, format=format, tz=tz))
  
  good <- sapply(1:length(obs), function(i){
    dif1 <- start[obsstn[i]==depstn] - obs[i]
    dif2 <- obs[i] - stop[obsstn[i]==depstn]
    duff <- sum(c(dif1, dif2)<0) %% 2 != length(dif1) %% 2
  })
  
  list(good.data=obsdat[good, ], bad.data=obsdat[!good, ])
}

#plot.dates
#Plots a Gantt chart of times of deployment operation (in black) and observations (in red)

#INPUT
# obsdat: dataframe of camera observations, one row per image data; must include columns:
#    station: character camera station identifier
#    date: character or POSIX date-times of observations; converted using as.POSIXct
# depdat: dataframe of deployment start and stop times; must include columns:
#    station: character camera station identifier
#    start, stop: character start and stop date-times in the format indicated by the format argument
#                 *TIP*: to avoid date-time formats being fouled up by Excel:
#                        1. avoid opening existing files in Excel;
#                        2. if creating a new file in Excel, prefix with dates with ' to format as text.
# format: format for date conversion passed to as.POSIXct
# tz: time zone for date conversion, passed to as.POSIXct
plot.deployments <- function(obsdat, depdat, format="%Y:%m:%d %H:%M:%S", tz="UTC"){
  error.check(obsdat, tz, format, depdat)
  
  obsdat$station <- as.character(obsdat$station)
  depdat$station <- as.character(depdat$station)
  obsdat$date <- as.POSIXct(obsdat$date, format, tz=tz)
  depdat$start <- as.POSIXct(depdat$start, format, tz=tz)
  depdat$stop <- as.POSIXct(depdat$stop, format, tz=tz)
  
  rng <- range(c(depdat$start, depdat$stop, obsdat$date))
  attr(rng, "tzone") <- "UTC"
  stn <- sort(unique(depdat$station))
  n <- length(stn)
  plot(rng, c(1,n+0.5), type="n", xlab="", ylab="", yaxt="n")
  axis(2, (1:n)+0.05, stn, las=1, cex.axis=0.7)

  for(i in 1:n){
    obs <- subset(obsdat, station==stn[i])$date
    points(obs, rep(i+0.1, length(obs)), cex=0.2, pch=16, col=2)
    
    dep <- depdat[depdat$station==stn[i], c("start","stop")]
    attr(dep, "tzone") <- "UTC"
    for(d in 1:nrow(dep)){
      lines(dep[d, ], rep(i,2))
    }
  }
}

#thin.events
#Create data frame with a row per independent event, with non-independence defined as
#within a given time of a prior record

#INPUT
# obsdat: dataframe of camera observations, one row per image data; must include columns:
#    species: character species identifier
#    station: character camera station identifier
#    date: character or POSIX date-times of observations; converted using as.POSIXct
# interval: independence interval in hours
# format: format for date conversion passed to as.POSIXct
# tz: time zone for date conversion, passed to as.POSIXct

#OUTPUT
# A dataframe with the same columns as the input obsdat but (potentially) fewer rows
thin.events <- function(obsdat, interval, format="%Y:%m:%d %H:%M:%S", tz="UTC"){
  error.check(obsdat, tz, format)
  
  sp.stn <- paste(obsdat$species, obsdat$station, sep=".")
  date <- as.POSIXct(obsdat$date, format=format, tz=tz)
  i <- order(sp.stn, date)
  obsdat <- obsdat[i, ]
  sp.stn <- sp.stn[i]
  date <- date[i]

  ii <- i <- 1
  while(i<length(date)){
    base <- tail(ii,1)
    i <- base+1
    while(difftime(date[i], date[base], units="hours")<interval & 
          sp.stn[i]==sp.stn[base] & 
          i<length(date)) i <- i+1
    if(difftime(date[i], date[base], units="hours")>=interval | 
       sp.stn[i]!=sp.stn[base]) ii <- c(ii,i)
  }
  res <- obsdat[ii, ]
  res$time <- as.numeric(format(date[ii], "%H")) + 
    as.numeric(format(date[ii], "%M"))/60 + 
    as.numeric(format(date[ii], "%H"))/60^2
  res
}

#calc.traprate
#Create a stations by species matrix of event counts

#INPUT
# obsdat: a row per event dataframe of the kind created using get.eventdat; must have (at least) columns:
#    station: station identifiers
#    species: species identifiers
# depdat: dataframe of deployment start and stop times; must include columns:
#    station: character camera station identifier
#    start, stop: character start and stop date-times in the format indicated by the format argument

#OUTPUT
# A stations by species matrix of observation counts
event.count <- function(obsdat, depdat, format="%Y:%m:%d %H:%M:%S", tz="UTC"){
  error.check(obsdat, tz, format, depdat)

  checked.obs <- check.dates(obsdat, depdat)
  obsdat <- checked.obs$good.data
  if(nrow(checked.obs$bad.data)>0)
    warning("Some observations fall outide given deployment times and were discarded\n  Use check.dates() to check which")
  
  depdat$start <- as.POSIXct(depdat$start, format=format, tz=tz)
  depdat$stop <- as.POSIXct(depdat$stop, format=format, tz=tz)
  obsdat$station <- as.character(obsdat$station)
  depdat$station <- as.character(depdat$station)

  effort.days <- as.numeric(with(depdat, difftime(stop, start, units="days")))
  effort.days <- tapply(effort.days, depdat$station, sum)
  events <- table(obsdat$station, obsdat$species)
  station <- rownames(effort.days)
  events <- events[match(station, rownames(events)), ]
  events[is.na(events)] <- 0
  rownames(events) <- NULL
  effort.days <- as.vector(effort.days)
  data.frame(station, effort.days, as.data.frame.matrix(events))
}


#get.dmatrix
#Create a detection matrix for occupancy analysis

#INPUT
# obsdat: dataframe of observations with (at least) columns:
#    station: character station identifier
#    date: character or POSIX date-times of observations; converted using as.POSIXct
# depdat: dataframe of of deployment data with (at least) columns:
#    station: character station identifier
#    start, stop: character or POSIX date-times of deployment starts and stops; converted using as.POSIXct
# interval: occasion length in days
# offset: first occasion starts at min(start)+offset in days
# order: vector of station names against which to order the matrix rows
# format:
# tz:

#OUTPUT
#A list with items:
# detection: detection matrix
# effort: effort matrix (in days)
# cuts: cut times of occasions
get.dmatrix <- function(obsdat, depdat, interval, offset=0, order=NULL, format="%Y:%m:%d %H:%M:%S", tz="UTC"){

  get.effort <- function(stn){
    s <- depdat$station==stn
    stt <- start[s]
    stp <- stop[s]
    grd1 <- expand.grid(tail(steps, -1), stt)
    grd2 <- expand.grid(head(steps, -1), stp)
    dif1 <- matrix(apply(grd1, 1, function(x) x[2]-x[1]), ncol=length(stt))
    dif2 <- matrix(apply(grd2, 1, function(x) x[1]-x[2]), ncol=length(stp))
    i <- apply(cbind(dif1, dif2), 1, function(x) sum(x<0) %% 2 == sum(s) %% 2)
    i1 <- apply(rbind(1, dif1), 2, function(x) which(abs(diff(x<0))==1))
    i2 <- apply(rbind(dif2, 1), 2, function(x) which(abs(diff(x<0))==1)) + 1
    res <- steps
    res[c(i1, i2)] <- c(stt,stp)
    res <- diff(res)
    res[i] <- 0
    res
  }
  
  error.check(obsdat, tz, format, depdat)
  checked.obs <- check.dates(obsdat, depdat)
  obsdat <- checked.obs$good.data
  duffdf <- checked.obs$bad.data
  if(nrow(checked.obs$bad.data)>0)
    warning("Some observations fall outide given deployment times and were discarded\n  See outofbounds output component to check which")

  secperday <- 24*60^2
  obsdat$station <- as.character(obsdat$station)
  depdat$station <- as.character(depdat$station)
  obsdat$date <- as.POSIXct(obsdat$date, format=format, tz=tz)
  depdat$start <- as.POSIXct(depdat$start, format=format, tz=tz)
  depdat$stop <- as.POSIXct(depdat$stop, format=format, tz=tz)
  mindate <- as.numeric(min(depdat$start))/secperday+offset
  maxdate <- as.numeric(max(depdat$stop))/secperday
  steps <- seq(mindate, maxdate, interval)
  if(tail(steps,1) < maxdate) steps <- c(steps, maxdate)
  display.steps <- as.POSIXct(steps*secperday, tz="UTC", origin="1970-01-01 00:00:00")
  obs <- as.numeric(obsdat$date)/secperday
  start <- as.numeric(depdat$start)/secperday
  stop <- as.numeric(depdat$stop)/secperday
  
  station <- c(as.character(obsdat$station), as.character(depdat$station), 
               rep("", length(steps)-1))
  occasion <- c(sapply(obs, function(x) sum(x>steps[-length(steps)])),
              rep("", nrow(depdat)), 1:(length(steps)-1))
  dmat <- table(station, occasion)
  attributes(dmat)$class <- "matrix"
  dmat <- dmat[-1,-1]
  if(!is.null(order)) dmat <- dmat[match(order, rownames(dmat)),]
  dmat <- dmat[, order(as.numeric(colnames(dmat)))]
  dmat[dmat>0] <- 1
  emat <- t(sapply(rownames(dmat), get.effort))
  dmat[emat==0] <- NA
  dimnames(emat) <- dimnames(dmat)
  
  list(detection=dmat, effort=emat, cuts=display.steps, outofbounds=duffdf)
}


#Function parameter definitions:
#P: vector of photo number per location
#T: vector of time per location (same length as P)
#param: a named list of model parameters containing list(v, p, g, a, theta):
#paramSE: a named list of model parameters SEs containing list(v, p, g, a, theta):
#	v: speed of movement (either day range or speed while active)
#	p: proportion of time active (set to 1 if v is day range)
#	g: group size
#	a: camera detection radius
#	theta: camera detection arc (in radians)
#	NB - ensure that units are consistent (i.e. distance in v and a, and time in v and T)
#strata: a factor the same length as P defining which stratum each location is in
#areas: a named list giving the area of each stratum (names must correspond to the levels in strata)
#its: the number of resamples for bootstrapping


#Single stratum density estimation
#For use in TRD

STRD <- function(i, P, T, param, strata){
  x <- strata==levels(strata)[i]
  res <- pi * param$g * sum(P[x]) / (sum(T[x]) * param$v * param$p * param$r * (2+param$theta))
  names(res) <- NULL
  res
}


#Trap rate density estimator, with or without stratification
TRD <- function(P, T, param, strata=NULL, areas=NULL){
  if(length(P)!=length(T)) stop("P and T have unequal lengths")
  if(!("g" %in% names(param))) param <- c(param,g=1)
  if(!("p" %in% names(param))) param <- c(param,p=1)
  if(is.null(strata))
  {	res <- pi * param$g * sum(P) / (sum(T) * param$v * param$p * param$r * (2+param$theta))
  names(res) <- NULL
  } else
  {	if(is.null(areas)) stop("areas are missing")
    if(length(strata)!=length(P)) stop("strata vector is a different length to P/T")
    if(sum(names(areas) %in% levels(strata)) != length(names(areas)) |
       sum(levels(strata) %in% names(areas)) != length(levels(strata)))
      stop("strata levels do not match areas names")
    nstrata <- length(areas)
    areas <- unlist(areas[order(names(areas))])
    locdens <- sapply(1:nstrata, STRD, P, T, param, strata)
    res <- sum(locdens * areas) / sum(areas)
  }
  res
}

#Single resampled traprate density estimate
#For use in bootTRD
TRDsample <- function(i, P, T, param, strata=NULL, areas=NULL){
  if(is.null(strata)){
    x <- sample(1:length(T), replace=TRUE)
  } else{
    nstrata <- length(areas)
    x <- NULL
    for (i in 1:nstrata) x <- c(x, sample(which(strata==levels(strata)[i]), replace=TRUE))
    strata <- sort(strata)
  }
  TRD(P[x], T[x], param, strata, areas)
}

#Dixon test for extremity of highest outlier
dixon <- function(x){
  x <- sort(x, TRUE)
  (x[1]-x[2]) / (x[1]-x[length(x)])
}
#Simulate Dixon outlier test value from neg binomial distribution with offset T
DvalSim <- function (i, T, size, mu) 
  dixon(rnbinom(length(T),size=size,mu=mu*T))

#Produce Dixon test p value(s) for one or more alpha values 
pdixon <- function(Dval, T, size, mu, its=1000){
  res <- sort(sapply(1:its, DvalSim, T, size, mu))
  i <- findInterval(Dval,res)
  (its-i)/its
}
nbNLL <- function(param, P, T){
  size <- exp(param[1])
  mu <- exp(param[2])
  -sum(dnbinom(P, size=size, mu=mu*T, log=TRUE))
}

#Trap rate density estimate with bootstrapped confidence intervals and variance
bootTRD <- function(P, T, param, paramSE, strata=NULL, areas=NULL, its=1000){
  BSdens <- sapply(1:its, TRDsample, P, T, param, strata, areas)
  BSse <- sd(BSdens)
  Dens <- TRD(P,T,param,strata,areas)
  prms <- length(param)
  addn <- rep(0,prms)
  addn[which(names(param)=="theta")] <- 2
  Es <- c(Dens,addn+unlist(param))
  SEs <- c(BSse,unlist(paramSE))
  SE <- Dens * sqrt(sum((SEs/Es)^2))
  cbind(Density=Dens, SE=SE)
}

#Trap rate density estimate with bootstrapped confidence intervals and variance,
#both with and without the most extreme trap rate observation removed.
#Returns the probability of the observation being a true outlier based on 
#bootstrapped probability given observed distribution

robustTRD <- function(P, T, param, paramSE, strata=NULL, areas=NULL, boots=1000, outits=1000){
  rate <- P/T
  OI1 <- which(rate==max(rate))
  OI2 <- which(rate==sort(rate,TRUE)[2])
  mn <- mean(rate)
  vr <- var(rate)
  initsize <- log(ifelse(vr>mn, mn^2/(vr-mn), 100))
  dispar <- exp(optim(c(initsize,log(mn)), nbNLL, P=P, T=T)$par)
  
  Dval1 <- dixon(rate)
  Dval2 <- dixon(rate[-OI1])
  p <- pdixon(c(Dval1,Dval2), T, dispar[1], dispar[2], outits)
  
  res <- cbind(bootTRD(P, T, param, paramSE, strata, areas, boots), NA, NA)
  res1 <- cbind(bootTRD(P[-OI1], T[-OI1], param, paramSE, strata, areas, boots), OI1, p[1])
  res2 <- cbind(bootTRD(P[-c(OI1,OI2)], T[-c(OI1,OI2)], param, paramSE, strata, areas, boots), OI2, p[2])
  
  res <- rbind(res, res1, res2)
  dimnames(res)[[2]][3:4] <- c("Obs#Cut", "OutlierP")
  dimnames(res)[[1]] <- c("All data", "Cut 1", "Cut 2")
  res
}



multiTRD <- function(plt, sp,P,T,param){
  bootTRD(nPhoto[species==sp | plotID==plt], tDeploy[species==sp | plotID==plt], param)
}

deltaCV <- function(x, se) sqrt(sum( (se/x)^2 ))