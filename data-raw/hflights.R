hflights <- tibble::as_tibble(hflights::hflights)
hflights$Date <- as.Date(paste0(hflights$Year, '-', hflights$Month, '-', hflights$DayofMonth))
hflights <- dplyr::select(hflights, Date, DepTime, ArrTime, UniqueCarrier, AirTime, ArrDelay, DepDelay, Distance, Origin, Dest)
