# Load functions and data
devtools::source_url("https://raw.githubusercontent.com/MarcusRowcliffe/camtools/master/get_detection_matrix.r")
obdat <- read.csv("observations.csv")
dpdat <- read.csv("deployments.csv")
lrdat <- read.csv("lure_events.csv")

obdat <- dplyr::mutate(obdat, 
                       timestamp = as.POSIXct(timestamp, tz="UTC"))
dpdat <- dplyr::mutate(dpdat, 
                       start = as.POSIXct(start, tz="UTC"),
                       end = as.POSIXct(end, tz="UTC"))
lrdat <- dplyr::mutate(lrdat, 
                       timestamp = as.POSIXct(timestamp, tz="UTC"))

# All species detection matrix as species x sites x occasions array
mat <- get_detection_matrix(obdat, dpdat, interval=3)
mat$cuts
mat$matrix
mat$matrix["lynx",,]
mat$effort
mat$interval
# Single species detection matrix as sites x occasions array
mat <- get_detection_matrix(obdat, dpdat, interval=3, 
                            species="fox")
mat$matrix
# Selected species detection matrices as list
mat <- get_detection_matrix(obdat, dpdat, interval=3, 
                            species=c("fox", "lynx"), 
                            output="list")
mat$matrix

# Create a matrix of time since lure - values in decimal days
# (zero if lure application event lies within an occasion;
# NA if no events within or before the occasion)
tsemat <- get_tse_matrix(lrdat, mat)
# Convert the above matrix to occasions since event
tsemat <- ceiling(tsemat/mat$interval)
tsemat


### More tests - Pablo data Sept23

load("C:/Users/rowcliffe.m/OneDrive - Zoological Society of London/GitHub/camtools/CarnivData06_09_23.RData")
lrdat <- dplyr::rename(lrdat, timestamp=urine_rep)
mat <- get_detection_matrix(obsdat, depdat, interval=3)
tse <- get_tse_matrix(as_tibble(lrdat), mat)


### More tests - large dataset test

nobs <- 1e6
nloc <- 1e4
nocc <- 500
nspp <- 50
t0 <- as.POSIXct("2000/01/01 12:00:00", tz="UTC")
depdat <- data.frame(locationID = as.character(1:nloc),
                     start = t0,
                     end = t0 + nocc*24*60^2)
obsdat <- data.frame(locationID = sample(as.character(1:nloc), nobs, replace=TRUE),
                     species = sample(paste0("s", 1:nspp), nobs, replace=TRUE),
                     timestamp = t0 + runif(nobs, 0, nocc*24*60^2))
depdat <- mutate(depdat, deploymentID = locationID)
obsdat <- mutate(obsdat, deploymentID = locationID)

obsdat <- obdat
depdat <- dpdat
obsdat$timestamp[1] <- obsdat$timestamp[10] - 100*86400
obsdat$timestamp[10] <- obsdat$timestamp[10] + 10*86400
obsdat$timestamp[15] <- obsdat$timestamp[10] + 100*86400

obsdat <- check_detection_data(obsdat, depdat)
attr(obsdat, "error")

t1 <- Sys.time()
mat <- get_detection_matrix(obsdat, depdat, interval=1, output="l")
t2 <- Sys.time()
difftime(t2,t1)
