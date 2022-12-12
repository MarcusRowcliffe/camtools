library(tidyr)

# Check observation and deployment data for consistency
#
# INPUT
# See get_detection_matrix
#
# OUTPUT
# If some observations lie outside given deployment times, returns a dataframe
# containing the problematic records from obsdat with start and end timestamps
# from depdat for comparison. Otherwise, returns obsdat. An attribute "error"
# is added, set to TRUE in the former case or FALSE in the latter.
check_detection_data <- function(obsdat, depdat){
  # Necessary fields present?
  fieldsOK <- all(c("timestamp", "locationID") %in% names(obsdat),
                  c("start", "end", "locationID") %in% names(depdat))
  if(!fieldsOK) 
    stop("Can't find the necessary data: obsdat must contain columns named 
         timestamp and locationID; depdat must contain columns named start, 
         end and locationID")
  
  # Found all locationIDs from obsdat in depdat?
  obsdat$locationID <- as.character(obsdat$locationID)
  depdat$locationID <- as.character(depdat$locationID)
  missingLocs <- unique(obsdat$locationID[!obsdat$locationID %in% depdat$locationID])
  if(length(missingLocs)>0)
    stop(paste("These locationID values in obsdat are missing from depdat:", 
               paste(missingLocs, collapse = " ")))
  
  # All observation timestamps are within their location deployment period?
  checkdat <- dplyr::left_join(obsdat, 
                               dplyr::select(depdat, deploymentID, start, end),
                               by="deploymentID")
  bad <- with(checkdat, timestamp<start | timestamp>end)
  if(sum(bad) > 0){
    message("Error: some observations occur outside their deployment time, 
            returning problematic observations")
    res <- checkdat[bad, ]
    attr(res, "error") <- TRUE
  } else{
    res <- obsdat
    attr(res, "error") <- FALSE
  }
  return(res)
}

# Generate detection occasion cutpoints
#
# INPUT
# See get_detection_matrix
#
# OUTPUT
# A vector of POSIX occasion cutpoints
get_occasion_cuts <- function(depdat, interval=1, start_hour=0){
    mn <- min(depdat$start)
    mx <- max(depdat$end)
    mnlt <- as.POSIXlt(mn)
    mntime <- mnlt$hour + mnlt$min/60 + mnlt$sec/3600
    mn <- if(mntime > start_hour) 
      mn - 3600 * (mntime + start_hour) else
        mn - 3600 * (mntime + 24 - start_hour)
    return(seq(mn, mx+interval*86400, interval*86400))
}

# Get a detection matrix for occupancy analysis
#
# INPUT
# obsdat: dataframe of observation data with (at least) columns:
#   timestamp: POSIX date-times when observations occurred
#   locationID: unique camera trap location identifier matchable with
#       the same key in depdat
#   species: species identifiers
# depdat: dataframe of deployment data with one row per deployment and
#   (at least) columns:
#     start, end: POSIX data-times when deployments started 
#                 and ended
#     locationID: unique camera trap location identifier matchable with
#                 locationID in obsdat
# interval: length of occasion interval in days.
# start_hour: a number from 0 to 24 giving the time of day at which to start
#             occasions.
# trim: if TRUE, detection records for all deployment occasions with less 
#       than full interval effort set missing, otherwise only those with zero 
#       effort.
#
# OUTPUT
# A list with elements:
#  matrix: the detection matrix
#  effort: a matrix of effort (days) for each deployment occasion
#  cuts: a vector of the time cuts defining occasions
#  interval: the occasion interval length in days
#
# EXAMPLE
# obsdat <- read.csv("observations.csv")
# depdat <- read.csv("deployments.csv")
# mat <- get_detection_matrix(subset(obsdat, species=="fox"), depdat,
#                             interval = 3,
#                             trim = TRUE)
get_detection_matrix <- function(obsdat, depdat, 
                                 interval=1, 
                                 start_hour=0,
                                 trim=FALSE,
                                 species="all"){
  
  make_dmat <- function(sp){
    dat <- subset(obsdat, species==sp)
    ijk <- expand.grid(dep=1:ndep, occ=1:nocc, obs=1:nrow(dat))
    loc <- depdat$locationID[ijk$dep]
    isin <- dat$timestamp[ijk$obs] >= cuts[ijk$occ] & 
      dat$timestamp[ijk$obs] <= cuts[ijk$occ+1] & 
      dat$locationID[ijk$obs] == loc
    mat <- isin %>%
      tapply(list(loc, ijk$occ), any) %>%
      as.numeric() %>%
      matrix(ncol=nocc)
    if(trim) mat[effort<interval] <- NA else
      mat[effort==0] <- NA
    mat
  }
  
  obsdat <- check_detection_data(obsdat, depdat)
  if(attributes(obsdat)$error) return(obsdat) else{
    cuts <- get_occasion_cuts(depdat, interval, start_hour)
    ndep <- nrow(depdat)
    nocc <- length(cuts) - 1
    
    # CREATE EFFORT MATRIX
    ij <- expand.grid(dep=1:ndep, occ=1:nocc)
    effort <- with(depdat, 
                   cbind(difftime(cuts[ij$occ], start[ij$dep], units="day"),
                         difftime(cuts[ij$occ+1], start[ij$dep], units="day"),
                         difftime(end[ij$dep], cuts[ij$occ], units="day"),
                         difftime(end[ij$dep], cuts[ij$occ+1], units="day"))
    ) %>%
      apply(1, function(x){
        signsum <- sum(sign(x))
        ifelse(signsum==0, 0, 
               ifelse(signsum==4, interval, 
                      ifelse(x[1]<=0, x[2], x[3])))
      }) %>%
      matrix(ncol=nocc) %>%
      apply(2, tapply, depdat$locationID, sum)
    
    # CREATE DETECTION MATRIX
    allspp <- unique(obsdat$species)
    if(species == "all") species <- allspp
    if(!all(species %in% allspp))
      stop("Not all species given are present in obsdat")
    mat <- sapply(species, make_dmat, simplify=FALSE)
    if(length(species) > 1){
      mat <- array(unlist(mat), dim=c(dim(mat[[1]]), length(mat)))
      dimnames(mat) <- list(NULL, NULL, species)
    }

    return(list(matrix=mat, effort=effort, cuts=cuts, interval=interval))
  }
}

# Get a time since event matrix

# INPUT
# eventdat: a dataframe giving times and locations of events; must contain columns:
#   locationID: location identifier
#   timestamp: POSIX date/times of each event (e.g. lure application)
# matrix: a detection matrix object returned by get_detection_matrix

# OUTPUT
# A locations by occasions matrix matching that in the matrix input,
# giving time since the last event at each location. Values are:
#  When last event occurs before beginning of occasion: time from event to occasion midpoint
#  When event occurs within occasion: 0
#  When no events occur within or before occasion: NA
get_tse_matrix <- function(eventdat, matrix){
  stt <- head(matrix$cuts, -1)
  stp <- tail(matrix$cuts, -1)
  midpoints <- stt + difftime(stp, stt) / 2
  locs <- rownames(matrix$effort)
  int <- matrix$interval
  eg <- expand.grid(midpoints, eventdat$timestamp)
  dt <- difftime(eg$Var1, eg$Var2, units = "day")
  dt[dt < -int/2] <- NA
  loc <- rep(eventdat$locationID, each=length(midpoints))
  res <- sapply(locs, function(l){
    m <- matrix(dt[loc==l], nrow=length(midpoints))
    apply(m, 1, function(x)
      if(all(is.na(x))) NA else min(x, na.rm=TRUE))
  })
  res <- t(res)
  res[abs(res) < int/2] <- 0
  res
}