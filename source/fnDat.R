sppAbbrv<-function(spp) {
  # Split the genus and species names
  names=strsplit(spp, " ")
  
  abbrv=sapply(names, function(name) {
    paste0(substr(name[1], 1, 1), ". ", name[2])})}

# Adjusting time format
adjust_time_format<-function(time)
  format(as.POSIXct(time*86400, origin="1970-01-01", tz="UTC"), "%H:%M:%S")

catchSmry<-function(data) {
  # Filter out NAs
  filtered=filter(data, !is.na(lat), !is.na(lon), !is.na(year))
  # Group data
  rtn=group_by(filtered, speciesScientificName,gear,year,lat,lon)
  rtn$Species=gsub(" ","\n",rtn$speciesScientificName)
  
  rtn}
