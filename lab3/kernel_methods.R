  set.seed(1234567890)
  library(geosphere)
  
  stations <- read.csv("TDDE01/lab3/stations.csv")
  temps <- read.csv("TDDE01/lab3/temps50k.csv")
  st <- merge(stations,temps,by="station_number")
  
  #sum(kernel*temp)/sum(kernel)
  
  h_distance <- 100000 # These three values are up to the students
  h_date <- 100
  h_time <- 1
  
  p.lat <- 58.4274 # vadstena n??gonstans
  p.long <- 14.826
  position <- c(p.long, p.lat)
  
  date <- "2013-11-04" # The date to predict (up to the students)
  times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00", "24:00:00")
  temp <- vector(length=length(times))
  
  dates <- temps["date"]
  station.positions <- cbind(stations[,5], stations[,4])
  
  #diff.date = apply(dates, 1, function(x) abs(difftime(x, date)))
  diff.dist = apply(station.positions, 1, function(x) distHaversine(position, x))
  
  gaussKernel.dist = function(x) {
    return(exp(-x/h_distance))
  }
  #View(diff.date)
  
  smoothed.date = sapply(diff.date, function(x) exp(-x/h_date))
  smoothed.dist = sapply(diff.distances, gaussKernel.dist)
  
  #View(cbind(diff.date, smoothed.date))
  #View(cbind(diff.dist, smoothed.dist))
  
  plot(temp, type="o")