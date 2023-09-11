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
  fieldsOK <- all(c("locationID", "deploymentID", "species", "timestamp") %in% names(obsdat),
                  c("locationID", "deploymentID", "start", "end") %in% names(depdat))
  if(!fieldsOK) 
    stop("Can't find the necessary data: obsdat must contain columns named 
         locationID, deploymentID, species and timestamp; 
         depdat must contain columns named locationID, deploymentID, start and end")
  obsmode <- apply(dplyr::select(obsdat, locationID, deploymentID), 2, mode)
  depmode <- apply(dplyr::select(depdat, locationID, deploymentID), 2, mode)
  if(any(c(obsmode, depmode) != "character"))
    stop("locationID and deploymentID must hold character data in both obsdat and depdat")
  
  # Found all locationIDs from obsdat in depdat?
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
    diff <- start_hour - mntime
    if(diff > 0) diff <- diff - 24
    mn <- mn + 3600 * diff
    return(seq(mn, mx+interval*86400, interval*86400))
}

# Get a detection matrix for occupancy analysis
#
# INPUT
# obsdat: dataframe of observation data with (at least) columns:
#   locationID: character camera trap location identifiers matchable with 
#               locationID in depdat
#   deploymentID: character camera trap deployment identifiers matchable with 
#                 deploymentID in depdat
#   species: species identifiers
#   timestamp: POSIX date-times when observations occurred
# depdat: dataframe of deployment data with one row per deployment and
#   (at least) columns:
#     start, end: POSIX data-times when deployments started 
#                 and ended
#     locationID: character camera trap location identifiers matchable with
#                 locationID in obsdat
#     deploymentID: character camera trap deployment identifier matchable with 
#                   deploymentID in obsdat
# interval: length of occasion interval in days.
# start_hour: a number from 0 to 24 giving the time of day at which to start
#             occasions.
# trim: if TRUE, detection records for all deployment occasions with less 
#       than full interval effort set missing, otherwise (default) only those 
#       with zero effort.
# species: which species to create the detection matrix for; default "all"
#          returns for all species in obsdat$species
# output: whether to return detection matrices as array or list
#
# OUTPUT
# A list with elements:
#  matrix: an array or list of detection matrices; 
#   when output=list, a named list of species-specific locations x occasions matrices; 
#   when output=array:
#     if a single species is selected, a locations x occasions matrix; 
#     if multiple species are selected, a species x locations x occasions matrix. 
#  effort: a matrix of effort (days) for each deployment occasion
#  cuts: a vector of the time cuts defining occasions
#  interval: the occasion interval length in days
get_detection_matrix <- function(obsdat, depdat, 
                                 interval=1, 
                                 start_hour=0,
                                 trim=FALSE,
                                 species="all",
                                 output=c("array", "list")){
  
  make_dmat <- function(sp){
    dat <- subset(obsdat, species==sp)
    dat$occasion <- findInterval(dat$timestamp, cuts)
    empty_occs <- which(!1:nocc %in% unique(dat$occasion))
    empty_locs <- setdiff(depdat$locationID, dat$locationID)
    if(length(empty_locs)>0)
      dat <- dplyr::bind_rows(dat, data.frame(locationID = empty_locs))
    if(length(empty_occs)>0)
      dat <- dplyr::bind_rows(dat, data.frame(occasion = empty_occs))
    mat <- dat %>%
      tidyr::pivot_wider(id_cols=locationID, 
                         names_from=occasion, 
                         values_from=timestamp,
                         names_sort = TRUE,
                         values_fill = 0,
                         values_fn = function(x) return(1)) %>%
      dplyr::select(-1) %>%
      as.matrix()
    if(length(empty_locs)>0) mat <- mat[, -ncol(mat)]
    if(length(empty_occs)>0) mat <- head(mat, -1)
    if(trim) mat[effort<interval] <- NA else
      mat[effort==0] <- NA
    mat
  }
  
  output <- match.arg(output)
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
        signsum_tot <- sum(sign(x))
        signsum_mid <- sum(sign(x[2:3]))
        ifelse(signsum_tot == 4, interval, 
               ifelse(signsum_tot == 2, ifelse(x[1]<=0, x[2], x[3]),
                      ifelse(signsum_mid == 0, 0, interval + sum(x[c(1,4)]))))
      }) %>%
      matrix(ncol=nocc) %>%
      apply(2, tapply, depdat$locationID, sum)
    
    # CREATE DETECTION MATRIX
    allspp <- unique(obsdat$species)
    if(length(species)==1) if(species=="all") species <- allspp
    if(!all(species %in% allspp))
      stop("Not all the species given are present in obsdat")
    mat <- sapply(species, make_dmat, simplify=FALSE)
    if(output=="array"){
      if(length(mat) == 1)
        mat <- mat[[1]] else{
          mat <- mat %>%
            unlist() %>%
            array(dim = c(dim(mat[[1]]), length(mat)),
                  dimnames = list(rownames(effort), NULL, species)) %>%
            aperm(c(3,1,2))
        }
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
#  when last event occurs before beginning of occasion: time from event to occasion midpoint
#  when event occurs within occasion: 0
#  when no events occur within or before occasion: NA
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

