#--------------------------------------------------------------------------------
#  This script will download the latest OISST time series data for the spatial bounds
#  Latitude 52:62
#  Longitude 200:215 (aka -160:-145)
#  from 9/1/1981 to present
#  It will average all spatial temperature values within each day to provide a single SST datum per day

#  This pulls data from here: https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg.html
#  This dataset is "research quality" and has a two week lag.

#  The near real-time version of the data can instead be access by using this url instead:
#  https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21NrtAgg.graph

#  Created: 06/02/2020
#  Author: Jordan.Watson@noaa.gov
#--------------------------------------------------------------------------------


#  Load R libraries
library(tidyverse)
library(ncdf4)
library(RCurl)
library(tidync)
library(lubridate)

#----------------------------------------------------------------------------------------------------------
#  This chunk should not need to be run again. It downloads data from 9/1/1981 to 5/16/2020
#----------------------------------------------------------------------------------------------------------

#  Download the data for a fixed spatial and temporal period.
#  (note this is a lot of data and will take a few minutes to download if you do the whole thing)

#  The commented out line is the OISST dataset. Note that longitudes are 0-360.
#x <- getBinaryURL("https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg.nc?sst[(1981-09-01T12:00:00Z):1:(2020-05-16T12:00:00Z)][(0.0):1:(0.0)][(52):1:(62)][(200):1:(215)]")

#  Curry - This line of code is setup to pull the MUR SST data for  6/10/2020. You can see the dates in the line of code. 
#  Also you can see the lat lon coordinates in this code: 55.5:59 lat, -162.5:-157 longitude. 
x <- getBinaryURL("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(2020-06-10T09:00:00Z):1:(2020-06-10T09:00:00Z)][(55.5):1:(59)][(-162.5):1:(-157)]")

#  If you want to pull from say, June 1 to the most recent date you could change the second date to be "last"
#x <- getBinaryURL("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(2020-06-10T09:00:00Z):1:(last)][(55.5):1:(59)][(-162.5):1:(-157)]")

#  If you want to pull multiple dates, no problem but 1 day with this lat-lon range is 141,033 data points so you can see how it will quickly add up. If you are going to pull lots of 
#  dates, it can be faster to set a loop and paste your date into the url to loop through. This is especially useful in a case like this where you are pulling in a bunch of data from
#  the GOA that you don't want. Alternatively (and I do this often) is to break your pull into multiple smaller rectangles that allow you to avoid pulling data you don't want. 
#  Then just bind them afterwwards. 


#  Convert and open the netcdf file
tmpSST <- tempfile(pattern="xwB", fileext=".nc")
writeBin(object=x, con=tmpSST)

#----------------------------------------------------------------
#  Extract netCDF data, convert date (seconds since 1970) to date time, and spatially average daily data to a single daily point.
#  This code below was to extract many dates from one netcdf but because MUR has much more data you may want to loop through days and thus your code may look a little different.
tidync(tmpSST) %>% 
  hyper_tibble() %>% 
  mutate(date=as_datetime(time)) %>% 
  group_by(date) %>% 
  summarise(msst=mean(sst)) %>% 
  saveRDS("OISST_1981_to_051620.RDS")

#  The way that I usually do it is to have my loop through dates or whatever run once (the archived dataset bascially) Save it as an RDS. Then you can load that file, ask R what the most recent date in that file is, 
#  then paste that date + 1 into the url and have that date range go through "last" (see note above about "last" to pull the most recent date). 

data <- readRDS("myarchiveddata.RDS")
mydate <- max(data$date) + 1 # may have to futz with date format to add one to a date. I can't recall offhand

x <- getBinaryURL(paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(",mydate,"T09:00:00Z):1:(last)][(55.5):1:(59)][(-162.5):1:(-157)]")
