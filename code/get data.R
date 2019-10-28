libs <- c("httr", "maps", "maptools", "ggplot2")
invisible(lapply(libs, library, character.only = TRUE))

#California map information

Newyork<- map('state', region='new york', fill = TRUE, plot = FALSE)
boundary <- maptools::map2SpatialPolygons(Newyork, IDs=c( "new york:manhattan","new york:main","new york:staten island","new york:long island"), proj4string=CRS("+proj=longlat +datum=WGS84"))
bndary <- boundary@polygons[[2]]@Polygons[[1]]@coords #main
bndary_plot <- data.frame(bndary); names(bndary_plot) <- c("lon", "lat");
#generate all points following original, i.e., most granular, grid
#x2 <- seq(-79.8, -73.2, by = 0.04)
#y2 <- seq(45.0, 40.8, by = -0.04)
x2 <- seq(-79.8, -73.2, by = 0.2)
y2 <- seq(45.0, 40.8, by = -0.2)
loc2 <- expand.grid(x2,y2) #PSM3 grid
loc_irreg <- loc2[which(point.in.polygon(point.x = loc2[,1], point.y = loc2[,2], pol.x = bndary[,1], pol.y = bndary[,2])==1),]
#loc_irreg <- loc_irreg[sample(x = 1:nrow(loc_irreg), size = nrow(loc_reg), replace = FALSE),]

setwd('D:/columbia/research/nydata2/')
#ggsave(filename = "lattice.pdf", plot = p, path = getwd(), scale = 1, width = 80, height = 45, unit = "mm", dpi = 300)

#API 8PUa9gAOpOV9pJ6XTt7VlRg1BsvCt48Mf40hrFN5

################################################################################
# API request parameters, except for longitude and latitude
# Declare all variables as strings. Spaces must be replaced with '+', i.e., change 'John Smith' to 'John+Smith'.
################################################################################
# You must request an NSRDB api key from the link above
api_key <- '8PUa9gAOpOV9pJ6XTt7VlRg1BsvCt48Mf40hrFN5'
# Set the attributes to extract (e.g., dhi, ghi, etc.), separated by commas.
attributes <- 'dhi,dni'
# Choose year of data
################################################################################
###change the year here
################################################################################
year = '2013'
# Set leap year to true or false. True will return leap day data if present, false will not.
leap_year = 'true'
# Set time interval in minutes, i.e., '30' is half hour intervals. Valid intervals are 30 & 60.
interval = '60'
# Specify Coordinated Universal Time (UTC), 'true' will use UTC, 'false' will use the local time zone of the data.
# NOTE: In order to use the NSRDB data in SAM, you must specify UTC as 'false'. SAM requires the data to be in the
# local time zone.
utc = 'false'
# Your full name, use '+' instead of spaces.
your_name = 'Bingquan+Wu'
# Your reason for using the NSRDB.
reason_for_use = 'research'
# Your affiliation
your_affiliation = 'Columbia+University'
# Your email address
your_email = 'bw2585@columbia.edu'
# Please join our mailing list so we can keep you up-to-date on new developments.
mailing_list = 'false'
################################################################################

################################################################################
###delete this line, to get all the data. This line is to get 3 of them, because of sampling, or it will be to long
################################################################################
#loc_irreg <- loc_irreg[sample(1:nrow(loc_irreg), 3, replace = FALSE),]



names(loc_irreg) <- c("lon", "lat")

for(i in 1:nrow(loc_irreg))
{
  lat <- loc_irreg$lat[i]
  lon <- loc_irreg$lon[i]
  
  # Declare url string
  URL <- paste0('http://developer.nrel.gov/api/solar/nsrdb_psm3_download.csv?wkt=POINT(', lon, '+', lat, ')&names=', year, '&leap_day=', leap_year, '&interval=', interval, '&utc=', utc, '&full_name=', your_name, '&email=', your_email, '&affiliation=', your_affiliation, '&mailing_list=', mailing_list, '&reason=', reason_for_use, '&api_key=', api_key, '&attributes=', attributes)
  # name the output file
  output_file <- paste0(lat, "_", lon, "_", year, ".csv")
  # API request and saving
  GET(url = URL, write_disk(output_file))
  print(i)
}
