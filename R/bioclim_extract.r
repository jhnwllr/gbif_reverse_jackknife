
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

coords = data.frame(lon,lat) 

points = SpatialPoints(coords, proj4string = r@crs)

values = extract(r,points) %>%
as.data.frame() %>%
`/`(10) %>%
mutate(decimallatitude = coords$lat) %>%
mutate(decimallongitude = coords$lon) %>%
na.omit() %>% 
glimpse()

# pca = princomp(values[paste0("bio",1:19)], cor = TRUE)$scores %>% 
# as.data.frame() %>%
# janitor::clean_names() %>%  
# select(comp_1,comp_2,comp_3,comp_4) %>% 
# glimpse()
# cbind(pca) %>%

d = values %>% 
select(decimallatitude,decimallongitude,everything()) %>%
glimpse() %>% 
readr::write_tsv(paste0(path,paste0("bioclim_",res,"_extract.tsv")))


# scp -r /cygdrive/c/Users/ftw712/Desktop/gbif_reverse_jackknife/data/bioclim_0.1_extract.tsv jwaller@c4gateway-vh.gbif.org:/home/jwaller/
# scp -r /cygdrive/c/Users/ftw712/Desktop/gbif_reverse_jackknife/data/bioclim_0.1_extract.tsv jwaller@c5gateway-vh.gbif.org:/home/jwaller/
# hdfs dfs -put bioclim_0.1_extract.tsv bioclim_0.1_extract.tsv
# hdfs dfs -ls


# }


