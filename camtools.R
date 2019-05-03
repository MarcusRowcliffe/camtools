library(data.table) #for rbindlist, used in read.multi
library(readxl) #for read_xls and read_xlsx, used in read.multi

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

check.dates <- function(obsdat, depdat){
  secperday <- 24*60^2
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
# format: format for date conversion passed to as.POSIXct
# tz: time zone for date conversion, passed to as.POSIXct
plot.deployments <- function(obsdat, depdat, format="%Y-%m-%d %H:%M:%S", tz="UTC"){
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
  sp.stn <- paste(obsdat$species, obsdat$station, sep=".")
  date <- as.POSIXct(obsdat$date, format=format, tz=tz)
  i <- order(sp.stn, date)
  obsdat <- obsdat[i, ]
  sp.stn <- sp.stn[i]
  date <- date[i]

  if(!all(c("species","station","date") %in% names(obsdat))) 
    stop("obsdat must contain columns species, station and date")
  if(any(is.na(date))) 
    stop("At least some dates in obsdat are missing or not convertible to POSIXct")
  
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
#     station: station identifiers
#     species: species identifiers
# stations: a vector of identifiers for all the stations in the survey

#OUTPUT
# A stations by species matrix of observation counts
event.count <- function(obsdat, depdat, format="%Y-%m-%d %H:%M:%S", tz="UTC"){
  if(!all(c("species","station") %in% names(obsdat))) 
    stop("obsdat must contain columns species and station")
  if(!all(c("station","start","stop") %in% names(depdat))) 
    stop("depdat must contain columns station, start and stop")
  
  checked.obs <- check.dates(obsdat, depdat)
  obsdat <- checked.obs$good.data
  if(nrow(checked.obs$bad.data)>0)
    warning("Some observations fall outide given deployment times and were discarded\n  Use check.dates() to check which")
  
  depdat$start <- as.POSIXct(depdat$start, format=format, tz=tz)
  depdat$stop <- as.POSIXct(depdat$stop, format=format, tz=tz)
  if(any( is.na(depdat$start) | is.na(depdat$stop) ))
    stop("At least some dates in depdat are missing or not convertible to POSIXct")

  obsdat$station <- as.character(obsdat$station)
  depdat$station <- as.character(depdat$station)
  if(!all(unique(obsdat$station) %in% depdat$station)) 
    stop("Not all stations in obsdat have data in depdat")
  
  effort.days <- as.numeric(with(depdat, difftime(stop, start, units="days")))
  effort.days <- tapply(effort.days, depdat$station, sum)
  events <- table(obsdat$station, obsdat$species)
  station <- rownames(effort.days)

  if(any(!rownames(events) %in% station))
    stop("Not all stations in obsdat are present in depdat")
  
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
get.dmatrix <- function(obsdat, depdat, interval, offset=0, order=NULL, format="%Y-%m-%d %H:%M:%S", tz="UTC"){

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
  
  if(!all(c("station","date") %in% names(obsdat))) 
    stop("obsdat must contain columns station and date")
  if(!all(c("station","start","stop") %in% names(depdat))) 
    stop("depdat must contain columns station, start and stop")
  checked.obs <- check.dates(obsdat, depdat)
  obsdat <- checked.obs$good.data
  duffdf <- checked.obs$bad.data
  if(nrow(checked.obs$bad.data)>0)
    warning("Some observations fall outide given deployment times and were discarded\n  See outofbounds output component to check which")

  obsdat$station <- as.character(obsdat$station)
  depdat$station <- as.character(depdat$station)
  obsdat$date <- as.POSIXct(obsdat$date, format=format, tz=tz)
  depdat$start <- as.POSIXct(depdat$start, format=format, tz=tz)
  depdat$stop <- as.POSIXct(depdat$stop, format=format, tz=tz)
  
  if(any(is.na(obsdat$date))) 
    stop("At least some dates in obsdat are missing or not convertible to POSIXct")
  if(any( is.na(depdat$start) | is.na(depdat$stop) ))
    stop("At least some dates in depdat are missing or not convertible to POSIXct")
  if(!all(unique(obsdat$station) %in% depdat$station)) 
    stop("Not all stations in obsdat have data in depdat")
  
  secperday <- 24*60^2
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
