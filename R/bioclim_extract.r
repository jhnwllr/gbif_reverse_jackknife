
# if(FALSE) { # extract data from bioclim variables 

library(sp)
library(raster)
library(dplyr)

path = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/data/"
# getData('worldclim', var='bio', res=10,path=path) %>% 
# saveRDS(paste0(path,"worldclim.rda"))

r = readRDS(paste0(path,"worldclim.rda"))

res = 0.1
lat = seq(-90,90,res)
lon = rep(seq(-180,180,res),each=length(lat))

specify_decimal = function(x, k) trimws(format(round(x, k), nsmall=k))

# avoid rounding issues because we will merge by decimal later
lat_text = specify_decimal(lat,1)
lon_text = specify_decimal(lon,1)

coords = data.frame(lon,lat,lat_text,lon_text,stringsAsFactors=FALSE) 

points = SpatialPoints(coords[c("lon","lat")],proj4string = r@crs)

values = extract(r,points) %>%
as.data.frame() %>%
`/`(10) %>%
mutate(decimallatitude = coords$lat_text) %>%
mutate(decimallongitude = coords$lon_text) %>%
na.omit() %>% 
glimpse()

d = values %>% 
select(decimallatitude,decimallongitude,everything()) %>%
glimpse() %>% 
readr::write_tsv(paste0(path,paste0("bioclim_",res,"_extract.tsv")))

if(FALSE) {
}
# scp -r /cygdrive/c/Users/ftw712/Desktop/gbif_reverse_jackknife/data/bioclim_0.1_extract.tsv jwaller@c4gateway-vh.gbif.org:/home/jwaller/
# scp -r /cygdrive/c/Users/ftw712/Desktop/gbif_reverse_jackknife/data/bioclim_0.1_extract.tsv jwaller@c5gateway-vh.gbif.org:/home/jwaller/
# hdfs dfs -put bioclim_0.1_extract.tsv bioclim_0.1_extract.tsv
# hdfs dfs -ls


# }


