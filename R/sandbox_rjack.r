
if(FALSE) { # check why examples drop points from map 

library(raster)
library(dplyr) # always dplyr after raster
library(sp)
library(purrr)

if(TRUE) { # hide bio data tribble
bio_data = tibble::tribble(~bio_var,~long_name,
"bio1","Annual Mean Temperature",
"bio2","Mean Diurnal Range",
"bio3","Isothermality (bio2/bio7) (×100)",
"bio4","Temperature Seasonality",
"bio5","Max Temperature of Warmest Month",
"bio6","Min Temperature of Coldest Month",
"bio7","Temperature Annual Range (bio5-bio6)",
"bio8","Mean Temperature of Wettest Quarter",
"bio9","Mean Temperature of Driest Quarter",
"bio10","Mean Temperature of Warmest Quarter",
"bio11","Mean Temperature of Coldest Quarter",
"bio12","Annual Precipitation",
"bio13","Precipitation of Wettest Month",
"bio14","Precipitation of Driest Month",
"bio15","Precipitation Seasonality",
"bio16","Precipitation of Wettest Quarter",
"bio17","Precipitation of Driest Quarter",
"bio18","Precipitation of Warmest Quarter",
"bio19","Precipitation of Coldest Quarter")
}

path = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/data/"

dbscan_outliers = data.table::fread("C:/Users/ftw712/Desktop/gbif_geographic_outliers/data/dbscan_outliers/dbscan_outliers_export.tsv") 

# filter(familykey == 3925) %>%
rjack_outliers = data.table::fread(paste0(path,"rjack_outliers_export.tsv")) %>%
filter(orderkey == 640) %>%
rename_at(vars(paste0("bio",1:19)), ~ paste0("outlier",1:19)) %>%
select("gbifid","specieskey",contains("bio"),contains("outlier")) %>% 
filter(n_bioclim_outliers >= 5) %>% 
glimpse()

specieskey_with_outlier = rjack_outliers %>% 
pull(specieskey) %>% 
unique() %>% 
nth(11)

specieskey_with_outlier = 5284989

# get outliers ids 
dbscan_outlier_gbifid = dbscan_outliers %>%
filter(specieskey %in% !!specieskey_with_outlier) %>%
pull(gbifid) 

rjack_outlier_gbifid = rjack_outliers %>%
filter(specieskey %in% !!specieskey_with_outlier) %>%
pull(gbifid)

# extracted_table = "bioclim_lagomorpha_export.tsv"
extracted_table = "bioclim_pinaceae_export.tsv"
# extracted_table = "bioclim_primates_export.tsv"
# "bioclim_primates_export.tsv"

d = data.table::fread(paste0(path,extracted_table)) %>%
filter(specieskey %in% specieskey_with_outlier) %>%
mutate(rjack_outlier = gbifid %in% !!rjack_outlier_gbifid) %>% 
mutate(dbscan_outlier = gbifid %in% !!dbscan_outlier_gbifid) %>% 
merge(rjack_outliers,id="gbifid",all.x=TRUE) %>%
arrange(-n_bioclim_outliers) %>% 
glimpse()

print(" ---- focal taxa ---- ")
focal_class = d %>% pull(class) %>% unique()
focal_order = d %>% pull(order_) %>% unique()
focal_species = d %>% pull(species) %>% unique()
focal_species_key = d %>% pull(specieskey) %>% unique()
outliers = rjack_outlier_gbifid

bio_d = d %>% 
select(contains("bio"),
gbifid,
rounded_decimallatitude,
rounded_decimallongitude,
-n_bioclim_outliers,
-decimallatitude_bioclim,
-decimallongitude_bioclim,
-dbscan_outlier,
-rjack_outlier) %>%
tidyr::pivot_longer(cols=contains("bio"),names_to="bio_var",values_to="bio_value") %>%
mutate(id=paste0(bio_var,"_",bio_value)) %>%
glimpse()
# na.omit() %>% 

bio_d = bio_d %>% filter(gbifid == 1699352106) %>% 
select(rounded_decimallongitude,rounded_decimallatitude,bio_value) 

bio_d

d %>% filter(gbifid == 1699352106) %>% 
select(rounded_decimallongitude,rounded_decimallatitude) 


# coords = bio_d %>%
# select(rounded_decimallongitude,rounded_decimallatitude) 

# r = readRDS("C:/Users/ftw712/Desktop/worldclim.rda")
# points = SpatialPoints(coords,proj4string=r@crs)

# extract(r,points)


# pdf("C:/Users/ftw712/Desktop/plot.pdf")
# plot(r[[1]])
# plot(points,add=TRUE)
# dev.off()

 
outlier_d = d %>% 
select(contains("outlier"),
-n_bioclim_outliers,
-decimallatitude_bioclim,
-decimallongitude_bioclim,
-dbscan_outlier,
-rjack_outlier) %>% 
tidyr::pivot_longer(cols=contains("outlier"),names_to="outlier_var",values_to="outlier_value") %>%
mutate(outlier_var = stringr::str_replace_all(outlier_var,"outlier","bio")) %>%
na.omit() %>% 
mutate(id=paste0(outlier_var,"_",outlier_value)) %>%
glimpse()

print("here")

d = merge(bio_d,outlier_d,id="id",all.x=TRUE) %>%
mutate(is_outlier = !is.na(outlier_value)) %>%
select(
gbifid,
lat = rounded_decimallatitude,
lon = rounded_decimallongitude,
bio_var,
bio_value,
outlier_var,
outlier_value,
is_outlier
) %>%
group_by(bio_var) %>% 
mutate(any_outlier = any(is_outlier)) %>%
ungroup() %>% 
filter(any_outlier) %>% 
glimpse()


d_cum_freq = d %>% 
group_by(lat,lon,bio_var,bio_value,is_outlier) %>%
summarise(freq = n()) %>%
arrange(bio_var,bio_value) %>%
ungroup() %>%
group_by(bio_var) %>% 
mutate(cum_freq = cumsum(freq)) %>% 
group_by(bio_var) %>% 
mutate(any_outlier = any(is_outlier)) %>%
filter(any_outlier) %>% 
merge(bio_data,id=bio_var,all.x=TRUE) %>%
mutate(bio_num = as.numeric(stringr::str_replace_all(bio_var,"bio",""))) %>% 
mutate(long_name=paste0(bio_var,"-",long_name)) %>% 
mutate(long_name=forcats::fct_reorder(long_name,bio_num)) %>%
mutate(id=paste0(lat,"_",lon)) 
 
n_outliers = d %>%  
select(bio_var,lat,lon,is_outlier) %>%
unique() %>%
group_by(lat,lon) %>% 
summarise(n_outliers=sum(is_outlier)) %>%
ungroup() %>% 
mutate(id=paste0(lat,"_",lon)) %>%
select(id,n_outliers) 

d_cum_freq = merge(d_cum_freq,n_outliers,id="id",all.x=TRUE) %>% 
mutate(n_outliers = as.character(n_outliers)) %>%
mutate(n_outliers = stringr::str_replace_all(n_outliers,"0","")) 

print(" ---- starting plots ---- ")

path = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/data/"
# getData('worldclim', var='bio', res=10,path=path) %>% 
# saveRDS(paste0(path,"worldclim.rda"))

r = readRDS(paste0(path,"worldclim.rda"))

library(ggplot2)
library(patchwork)

bio_vars_with_outliers = d %>% 
pull(bio_var) %>% 
unique() %>%
stringr::str_replace_all("bio","") %>%
as.numeric() %>%
sort()

plot_list = bio_vars_with_outliers %>%
map(~ {
r_df = as(r[[.x]], "SpatialPixelsDataFrame") %>% 
as.data.frame() %>%
tidyr::pivot_longer(cols=contains("bio"),names_to="bio_var") %>% 
merge(bio_data,id="bio_var") 

print(.x)

df_points = d %>% 
filter(bio_var == paste0("bio",.x))

max_x = max(df_points$lon) + 10
min_x = min(df_points$lon) - 10 
max_y = max(df_points$lat) + 10
min_y = min(df_points$lat) - 10

p_raster = ggplot(r_df,aes(x,y)) +
geom_raster(aes(fill=value))+
scale_fill_gradientn(colours=c("brown","red","yellow","darkgreen","green"))+
coord_equal() + 
geom_point(data=df_points,aes(lon,lat),color="white",size=2.5) + 
geom_point(data=df_points,aes(lon,lat,color=is_outlier),size=1) + 
scale_colour_manual(values = c("gray","black")) +
theme_bw() + 
facet_wrap(~bio_var,ncol=1) + 
theme(legend.position="none") +
theme(plot.margin = margin(0, 0, 0, 0)) +
scale_y_continuous(limits = c(min_y, max_y)) +
scale_x_continuous(limits = c(min_x, max_x)) + 
theme(strip.background = element_rect(fill="#f9f9f9")) +
xlab("") +
ylab("") 

df_cum_freq = d_cum_freq %>% 
filter(bio_var == paste0("bio",.x))

# geom_text() +
p_cum_freq = ggplot(df_cum_freq,aes(bio_value,cum_freq,label=n_outliers)) + 
geom_point(aes(color=is_outlier),size=3) + 
geom_text(hjust = -1) +
facet_wrap(~long_name,scales="free",ncol=2) + 
theme_bw() + 
theme(legend.position="top") + 
theme(strip.background = element_rect(fill="#f9f9f9")) +
theme(strip.text=element_text(face="bold")) + 
scale_color_manual(values=c("gray","black")) + 
theme(plot.margin = margin(0, 0, 0, 0)) +
ylab("") + 
xlab("") + 
labs(
caption = paste0(focal_class," - ",focal_order," - ",focal_species,"\n"," specieskey=", focal_species_key,"\n","outlier gbifids: ",paste(outliers,collapse=" "))
) +
guides(color=guide_legend(title="Outlier")) 

out = list(p_raster,p_cum_freq)

}) %>%
flatten()


p = wrap_plots(plot_list,ncol=2)

save_dir = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/plots/raster_plots/"
gbifapi::save_ggplot_formats(p,save_dir,specieskey_with_outlier,height=4*length(plot_list)/2,width=7.5,formats=c("pdf","jpg"))

}

if(FALSE) { # make biogeo example for blog post

library(tibble)
library(raster)
library(dplyr) # load dplyr after raster to avoid select conflict
library(sp)
library(rgbif)
library(biogeo)

# get data from worldclim
path = "C:/Users/ftw712/Desktop/" # set a path to somewhere to save the data
r = getData('worldclim', var='bio', res=10,path=path) 

# get data from GBIF
occ_data = occ_search(
taxonKey = 5284989,
limit = 2000,
hasCoordinate = TRUE,
hasGeospatialIssue = FALSE,
country = "US",
)$data %>%
filter(
coordinateUncertaintyInMeters < 1000 | # filter with high uncertainty
is.na(coordinateUncertaintyInMeters)) %>%  
filter(basisOfRecord == "PRESERVED_SPECIMEN") %>% # Let's only consider other specimens
mutate(row_number = row_number()) %>%
select(row_number,key,scientificName,decimalLatitude,decimalLongitude) %>%
glimpse() 

coords = occ_data %>%
select(decimalLongitude,decimalLatitude) 

points = SpatialPointsDataFrame(coords,data=occ_data,proj4string=r@crs)

values = extract(r,points) %>%
as.data.frame() %>%
`/`(10) %>% # divide by 10 to get temp in C
mutate(row_number = occ_data$row_number) %>%
na.omit() 

# reverse jackknife from biogeo 
# this function returns the row_number of the outlier 
outlier_index = rjack(values$bio4 ) 

# merge back with original occ_data
outliers = values %>%
select(bio4,row_number) %>%
mutate(rjack_outlier = row_number %in% outlier_index) %>%
filter(rjack_outlier) %>%
merge(occ_data,id="row_number") 

outliers # which points where flagged as outliers 

# this outlier is centroid of the USA
# https://www.gbif.org/occurrence/1701728186

}


if(FALSE) { # make machine tag for rjack 

library(dplyr)
library(purrr)

load("C:/Users/ftw712/Desktop/griddedDatasets/authentication.rda")
ls()

path = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/data/"

L = data.table::fread(paste0(path,"rjack_outliers_export.tsv")) %>%
filter(n_bioclim_outliers >= 5) %>%
select(datasetkey,gbifid) %>% 
mutate(gbifid=as.character(gbifid)) %>% 
group_by(datasetkey) %>%
summarise(gbifid = jsonlite::toJSON(gbifid,auto_unbox=TRUE)) %>% 
slice(1) %>%
glimpse()

# pull(gbifid) %>%
# jsonlite::fromJSON() %>%
# %>%
# purrr::transpose() %>% 
# map(~ .x[2:length(.)] %>% jsonlite::toJSON(auto_unbox=TRUE)) %>%
# glimpse()

# L %>% 
# map(~ length(.x$gbifid)) %>%
# jsonlite::toJSON(auto_unbox=TRUE) 

# library(gbifMachineTagger)

# L %>%  
# map(~  
# createMachineTag(
# datasetkey=.x$datasetkey,
# namespace="rjackOutliersInDataset.jwaller.gbif.org",
# name="rjackOutliers",
# value=.x$gbifid,
# embedValueList=FALSE,
# user = authentication$user,
# password = authentication$password,
# api="http://api.gbif-uat.org/v1/dataset/")
# )


# %>%
# jsonlite::fromJSON()


# getMachineTagData("griddedDataSet.jwaller.gbif.org") 

# getMachineTagData("rjackOutliersInDataset.jwaller.gbif.org",api="http://api.gbif-uat.org/v1/dataset?")

 # %>% 
# jsonValuesToColumns() # no nested json 


# select(specieskey,basisofrecord,rounded_decimallatitude,rounded_decimallongitude)
}

if(FALSE) { # nick porch example 

library(dplyr)

path = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/data/"

# filter(n_bioclim_outliers >= 5) %>%
# rjack_outliers = data.table::fread(paste0(path,"rjack_outliers_export.tsv")) %>%
# glimpse() %>% 
# pull(gbifid) %>%
# as.character()

# d = readr::read_tsv(paste0(path,"Onychophora.csv")) %>%
# glimpse() %>%
# mutate(gbifid = as.character(gbifID)) %>%
# mutate(rjack_outlier = gbifid %in% !!rjack_outliers) %>%
# select(rjack_outlier,decimalLatitude,decimalLongitude) %>%
# unique() %>% 
# saveRDS("C:/Users/ftw712/Desktop/d.rda")

df_points = readRDS("C:/Users/ftw712/Desktop/d.rda") %>%
mutate(lat = decimalLatitude) %>%
mutate(lon = decimalLongitude) %>%
glimpse()

library(dplyr)
library(raster)

path = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/data/"
# getData('worldclim', var='bio', res=10,path=path) %>% 
# saveRDS(paste0(path,"worldclim.rda"))

r = readRDS(paste0(path,"worldclim.rda"))

r_df = as(r[[12]], "SpatialPixelsDataFrame") %>% 
as.data.frame() %>%
tidyr::pivot_longer(cols=contains("bio"),names_to="bio_var") 

max_x = max(df_points$lon) + 2
min_x = min(df_points$lon) - 2 
max_y = max(df_points$lat) + 2
min_y = min(df_points$lat) - 2

library(ggplot2)
p = ggplot(r_df,aes(x,y)) +
geom_raster(aes(fill=value))+
scale_fill_gradientn(colours=c("brown","red","yellow","darkgreen","green"))+
geom_point(data=df_points,aes(lon,lat),color="#f5f2d0",size=2.5) + 
geom_point(data=df_points,aes(lon,lat,color=rjack_outlier),size=1) + 
scale_colour_manual(values = c("gray","black")) +
coord_equal() +
theme_void() +
theme(legend.position="none") +
scale_x_continuous(limits = c(min_x, max_x)) + 
scale_y_continuous(limits = c(min_y, max_y)) 

# + 

# + 
# facet_wrap(~bio_var,ncol=1) + 
# theme(plot.margin = margin(0, 0, 0, 0)) +
# theme(strip.background = element_rect(fill="#f9f9f9")) +
# xlab("") +
# ylab("") 

save_dir = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/plots/"
gbifapi::save_ggplot_formats(p,save_dir,"nick_porch_plot",height=4,width=7,formats=c("pdf","jpg"))
}

if(FALSE) { # make pure pretty raster plots fro blog post 

library(dplyr)
library(raster)

path = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/data/"
# getData('worldclim', var='bio', res=10,path=path) %>% 
# saveRDS(paste0(path,"worldclim.rda"))

r = readRDS(paste0(path,"worldclim.rda"))

r_df = as(r[[1]], "SpatialPixelsDataFrame") %>% 
as.data.frame() %>%
tidyr::pivot_longer(cols=contains("bio"),names_to="bio_var") 

library(ggplot2)
p = ggplot(r_df,aes(x,y)) +
geom_raster(aes(fill=value))+
scale_fill_gradientn(colours=c("brown","red","yellow","darkgreen","green"))+
coord_equal() +
theme_void() +
theme(legend.position="none") 

# + 
# geom_point(data=df_points,aes(lon,lat),color="white",size=2.5) + 
# geom_point(data=df_points,aes(lon,lat,color=is_outlier),size=1) + 
# scale_colour_manual(values = c("gray","black")) +

# + 
# facet_wrap(~bio_var,ncol=1) + 
# theme(plot.margin = margin(0, 0, 0, 0)) +
# scale_x_continuous(limits = c(min_x, max_x)) + 
# scale_y_continuous(limits = c(min_y, max_y)) +
# theme(strip.background = element_rect(fill="#f9f9f9")) +
# xlab("") +
# ylab("") 

save_dir = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/plots/"
gbifapi::save_ggplot_formats(p,save_dir,"surface_plot",height=4,width=7,formats=c("pdf","jpg"))

}

# if(FALSE) { # most common basis of record for rjack outliers 

library(dplyr)

path = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/data/"
d = parqr::parquet_readr(paste0(path,"rjack_outliers_export.parquet")) %>%
filter(n_bioclim_outliers >= 5) %>%
select(specieskey,basisofrecord,rounded_decimallatitude,rounded_decimallongitude) %>%
unique() %>%
group_by(basisofrecord) %>% 
count() %>%
ungroup() %>% 
arrange(-n) %>% 
mutate(basisofrecord = tolower(basisofrecord)) %>%
mutate(basisofrecord = stringr::str_replace_all(basisofrecord,"_"," ")) %>%
mutate(basisofrecord = forcats::fct_reorder(basisofrecord,n)) %>%
glimpse() 

library(ggplot2)

p = ggplot(d,aes(basisofrecord,n)) +
geom_col(stat="identity",fill="#4B9E46") + 
coord_flip() + 
xlab("") + 
ylab("number of outliers") +
theme_bw() +
theme(axis.text.y=element_text(face="plain",size=15,color="#535362")) +
theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0), size = 11, face="plain")) +
labs(caption=">5 climate surfaces flagged as outliers")

save_dir = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/plots/"
gbifapi::save_ggplot_formats(p,save_dir,"basisofrecord_barplot",height=4,width=6,formats=c("pdf","jpg","svg"))

# }

if(FALSE) { # make make of rjack outlier locations 

library(dplyr)

path = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/data/"
d = data.table::fread(paste0(path,"rjack_outliers_export.tsv")) %>%
filter(n_bioclim_outliers >= 5) %>%
select(specieskey,rounded_decimallatitude,rounded_decimallongitude) %>%
unique() %>%
group_by(rounded_decimallatitude,rounded_decimallongitude) %>% 
count() %>%
ungroup() %>% 
filter(n > 50) %>% 
glimpse() 

countries = gbifapi::ggplot2_small_map_data(dTolerance=0.8)

library(ggplot2)

d_sf = d %>%
sf::st_as_sf(coords = c("rounded_decimallongitude","rounded_decimallatitude"), crs = 4326)

p = ggplot() +
theme_bw() +
geom_polygon(data=countries,aes(x=long, y=lat, group=group), fill="#D8DACF", color="#D8DACF",alpha=0.8) +
geom_sf(data=d_sf,aes(size=n),alpha=0.1)

save_dir = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/plots/"
gbifapi::save_ggplot_formats(p,save_dir,"remoteness_map",height=4,format=c("jpg","pdf"))

}

if(FALSE) { # check datasets with most rjack outliers 

library(dplyr)

path = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/data/"

d = parqr::parquet_readr(paste0(path,"rjack_outliers_export.parquet")) %>%
filter(n_bioclim_outliers >= 5) %>%
group_by(datasetkey) %>% 
summarise(outlier_count=n()) %>%
filter(outlier_count > 100) %>% 
mutate(dataset_count = gbifapi::get_dataset_counts(datasetkey)) %>%
mutate(percent_outlier = outlier_count/dataset_count) %>%
arrange(-percent_outlier) %>%  
select(datasetkey,percent_outlier)  

d %>% print(n=100)

}

if(FALSE) { # check reverse jackknife against coordinate uncertainty meters 

library(dplyr)

path = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/data/"

d = data.table::fread(paste0(path,"rjack_outliers_export.tsv")) %>%
filter(basisofrecord == "PRESERVED_SPECIMEN") %>% 
select(specieskey,rounded_decimallatitude,rounded_decimallongitude,coordinateuncertaintyinmeters, n_bioclim_outliers) %>%
unique() %>%
na.omit() %>%
arrange(-coordinateuncertaintyinmeters) %>%
group_by(n_bioclim_outliers) %>% 
summarise(cum = mean(coordinateuncertaintyinmeters)) %>% 
arrange(-cum)

library(ggplot2)

p = ggplot(d,aes(n_bioclim_outliers,cum)) +
geom_point()

save_dir = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/plots/"
gbifapi::save_ggplot_formats(p,save_dir,"cum_outliers",height=4,width=4)


# filter(n_bioclim_outliers >= 6) %>% 
# group_by(familykey) %>% 
# count() %>% 
# arrange(-n)

}

if(FALSE) { # check groups with most outliers 
library(dplyr)

path = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/data/"

data.table::fread(paste0(path,"rjack_outliers_export.tsv")) %>%
filter(n_bioclim_outliers >= 6) %>% 
group_by(familykey) %>% 
count() %>% 
arrange(-n)
# glimpse()
}

if(FALSE) { # combine cumfreq + raster plot

library(raster)
library(dplyr) # always dplyr after raster
library(tibble)
library(sp)
library(purrr)

if(TRUE) { # hide bio data tribble
bio_data = tibble::tribble(~bio_var,~long_name,
"bio1","Annual Mean Temperature",
"bio2","Mean Diurnal Range",
"bio3","Isothermality (bio2/bio7) (×100)",
"bio4","Temperature Seasonality",
"bio5","Max Temperature of Warmest Month",
"bio6","Min Temperature of Coldest Month",
"bio7","Temperature Annual Range (bio5-bio6)",
"bio8","Mean Temperature of Wettest Quarter",
"bio9","Mean Temperature of Driest Quarter",
"bio10","Mean Temperature of Warmest Quarter",
"bio11","Mean Temperature of Coldest Quarter",
"bio12","Annual Precipitation",
"bio13","Precipitation of Wettest Month",
"bio14","Precipitation of Driest Month",
"bio15","Precipitation Seasonality",
"bio16","Precipitation of Wettest Quarter",
"bio17","Precipitation of Driest Quarter",
"bio18","Precipitation of Warmest Quarter",
"bio19","Precipitation of Coldest Quarter")
}

path = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/data/"

# dbscan_outliers = data.table::fread("C:/Users/ftw712/Desktop/gbif_geographic_outliers/data/dbscan_outliers/dbscan_outliers_export.tsv") 
# rjack_outliers = data.table::fread(paste0(path,"rjack_outliers_export.tsv")) %>%

rjack_outliers = parqr::parquet_readr(paste0(path,"rjack_outliers_export.parquet")) %>%
filter(familykey == 3925) %>%
rename_at(vars(paste0("bio",1:19)), ~ paste0("outlier",1:19)) %>%
select("gbifid","specieskey",contains("bio"),contains("outlier")) %>% 
mutate(gbifid = as.character(gbifid)) %>% 
filter(n_bioclim_outliers >= 5) %>% 
glimpse()

specieskey_with_outlier = rjack_outliers %>% 
pull(specieskey) %>% 
unique() %>% 
nth(15)

# specieskey_with_outlier = 5284989

# get outliers ids 
# dbscan_outlier_gbifid = dbscan_outliers %>%
# filter(specieskey %in% !!specieskey_with_outlier) %>%
# pull(gbifid) 

rjack_outlier_gbifid = rjack_outliers %>%
filter(specieskey %in% !!specieskey_with_outlier) %>%
pull(gbifid)

# extracted_table = "bioclim_lagomorpha_export.tsv"
extracted_table = "bioclim_pinaceae_export.tsv"
# extracted_table = "bioclim_primates_export.tsv"
# "bioclim_primates_export.tsv"
# mutate(dbscan_outlier = gbifid %in% !!dbscan_outlier_gbifid) %>% 

d = data.table::fread(paste0(path,extracted_table)) %>%
mutate(gbifid = as.character(gbifid)) %>%
filter(specieskey %in% specieskey_with_outlier) %>%
mutate(rjack_outlier = gbifid %in% !!rjack_outlier_gbifid) %>% 
merge(rjack_outliers,id="gbifid",all.x=TRUE) %>%
arrange(-n_bioclim_outliers) %>% 
glimpse()

print(" ---- focal taxa ---- ")
focal_class = d %>% pull(class) %>% unique()
focal_order = d %>% pull(order_) %>% unique()
focal_species = d %>% pull(species) %>% unique()
focal_species_key = d %>% pull(specieskey) %>% unique()
outliers = rjack_outlier_gbifid

bio_d = d %>% 
select(contains("bio"),
gbifid,
rounded_decimallatitude,
rounded_decimallongitude,
-n_bioclim_outliers,
-decimallatitude_bioclim,
-decimallongitude_bioclim,
-rjack_outlier) %>%
tidyr::pivot_longer(cols=contains("bio"),names_to="bio_var",values_to="bio_value") %>%
na.omit() %>% 
mutate(id=paste0(bio_var,"_",bio_value)) %>%
glimpse()
 
outlier_d = d %>% 
select(contains("outlier"),
-n_bioclim_outliers,
-decimallatitude_bioclim,
-decimallongitude_bioclim,
-rjack_outlier) %>% 
tidyr::pivot_longer(cols=contains("outlier"),names_to="outlier_var",values_to="outlier_value") %>%
mutate(outlier_var = stringr::str_replace_all(outlier_var,"outlier","bio")) %>%
na.omit() %>% 
mutate(id=paste0(outlier_var,"_",outlier_value)) %>%
glimpse()

print("here")

d = merge(bio_d,outlier_d,id="id",all.x=TRUE) %>%
mutate(is_outlier = !is.na(outlier_value)) %>%
select(
gbifid,
lat = rounded_decimallatitude,
lon = rounded_decimallongitude,
bio_var,
bio_value,
outlier_var,
outlier_value,
is_outlier
) %>%
group_by(bio_var) %>% 
mutate(any_outlier = any(is_outlier)) %>%
ungroup() %>% 
filter(any_outlier) %>% 
glimpse()


d_cum_freq = d %>% 
group_by(lat,lon,bio_var,bio_value,is_outlier) %>%
summarise(freq = n()) %>%
arrange(bio_var,bio_value) %>%
ungroup() %>%
group_by(bio_var) %>% 
mutate(cum_freq = cumsum(freq)) %>% 
group_by(bio_var) %>% 
mutate(any_outlier = any(is_outlier)) %>%
filter(any_outlier) %>% 
merge(bio_data,id=bio_var,all.x=TRUE) %>%
mutate(bio_num = as.numeric(stringr::str_replace_all(bio_var,"bio",""))) %>% 
mutate(long_name=paste0(bio_var,"-",long_name)) %>% 
mutate(long_name=forcats::fct_reorder(long_name,bio_num)) %>%
mutate(id=paste0(lat,"_",lon)) 
 
n_outliers = d %>%  
select(bio_var,lat,lon,is_outlier) %>%
unique() %>%
group_by(lat,lon) %>% 
summarise(n_outliers=sum(is_outlier)) %>%
ungroup() %>% 
mutate(id=paste0(lat,"_",lon)) %>%
select(id,n_outliers) 

d_cum_freq = merge(d_cum_freq,n_outliers,id="id",all.x=TRUE) %>% 
mutate(n_outliers = as.character(n_outliers)) %>%
mutate(n_outliers = stringr::str_replace_all(n_outliers,"0","")) 

print(" ---- starting plots ---- ")

path = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/data/"
# getData('worldclim', var='bio', res=10,path=path) %>% 
# saveRDS(paste0(path,"worldclim.rda"))

r = readRDS(paste0(path,"worldclim.rda"))

library(ggplot2)
library(patchwork)

bio_vars_with_outliers = d %>% 
pull(bio_var) %>% 
unique() %>%
stringr::str_replace_all("bio","") %>%
as.numeric() %>%
sort()

plot_list = bio_vars_with_outliers %>%
map(~ {
r_df = as(r[[.x]], "SpatialPixelsDataFrame") %>% 
as.data.frame() %>%
tidyr::pivot_longer(cols=contains("bio"),names_to="bio_var") %>% 
merge(bio_data,id="bio_var") 

print(.x)

df_points = d %>% 
filter(bio_var == paste0("bio",.x))

max_x = max(df_points$lon) + 10
min_x = min(df_points$lon) - 10 
max_y = max(df_points$lat) + 10
min_y = min(df_points$lat) - 10

p_raster = ggplot(r_df,aes(x,y)) +
geom_raster(aes(fill=value))+
scale_fill_gradientn(colours=c("brown","red","yellow","darkgreen","green"))+
coord_equal() + 
geom_point(data=df_points,aes(lon,lat),color="white",size=2.5) + 
geom_point(data=df_points,aes(lon,lat,color=is_outlier),size=1) + 
scale_colour_manual(values = c("gray","black")) +
theme_bw() + 
facet_wrap(~bio_var,ncol=1) + 
theme(legend.position="none") +
theme(plot.margin = margin(0, 0, 0, 0)) +
scale_y_continuous(limits = c(min_y, max_y)) +
scale_x_continuous(limits = c(min_x, max_x)) + 
theme(strip.background = element_rect(fill="#f9f9f9")) +
xlab("") +
ylab("") 

df_cum_freq = d_cum_freq %>% 
filter(bio_var == paste0("bio",.x))

# geom_text() +
p_cum_freq = ggplot(df_cum_freq,aes(bio_value,cum_freq,label=n_outliers)) + 
geom_point(aes(color=is_outlier),size=3) + 
geom_text(hjust = -1) +
facet_wrap(~long_name,scales="free",ncol=2) + 
theme_bw() + 
theme(legend.position="top") + 
theme(strip.background = element_rect(fill="#f9f9f9")) +
theme(strip.text=element_text(face="bold")) + 
scale_color_manual(values=c("gray","black")) + 
theme(plot.margin = margin(0, 0, 0, 0)) +
ylab("") + 
xlab("") + 
labs(
caption = paste0(focal_class," - ",focal_order," - ",focal_species,"\n"," specieskey=", focal_species_key,"\n","outlier gbifids: ",paste(outliers,collapse=" "))
) +
guides(color=guide_legend(title="Outlier")) 

out = list(p_raster,p_cum_freq)

}) %>%
flatten()


p = wrap_plots(plot_list,ncol=2)

save_dir = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/plots/raster_plots/"
gbifapi::save_ggplot_formats(p,save_dir,specieskey_with_outlier,height=4*length(plot_list)/2,width=7.5,formats=c("pdf","jpg"))

}

if(FALSE) { # facet wrapped rasters 

library(raster)
library(dplyr)
library(purrr)

path = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/data/"

# set up occurrence data points 
rjack_outliers = data.table::fread(paste0(path,"rjack_outliers_export.tsv")) %>%
filter(familykey == 3925) %>%
rename_at(vars(paste0("bio",1:19)), ~ paste0("outlier",1:19)) %>%
select("gbifid","specieskey",contains("bio"),contains("outlier"),-contains("comp")) 
or
specieskey_with_outlier = rjack_outliers %>% 
pull(specieskey) %>% 
unique() %>% 
head(1)

d = data.table::fread(paste0(path,"bioclim_primates_export.tsv")) %>%
filter(specieskey %in% specieskey_with_outlier) %>%
merge(rjack_outliers,id="gbifid",all.x=TRUE) %>% 
glimpse()

# re-define d
bio_d = d %>% 
select(contains("bio"),
gbifid,
rounded_decimallatitude,
rounded_decimallongitude,
n_bioclim_outliers) %>% 
select(gbifid,lat=rounded_decimallatitude,lon=rounded_decimallongitude,n_outliers = n_bioclim_outliers,starts_with("bio")) %>% 
tidyr::pivot_longer(cols=contains("bio"),names_to="bio_var",values_to="bio_value") %>%
glimpse() 

outlier_d = d %>%
select(gbifid,starts_with("outlier")) %>%
tidyr::pivot_longer(cols=starts_with("outlier"),names_to="outlier_var",values_to="outlier_value") %>%
na.omit()

d = merge(bio_d,outlier_d,id="gbifid",all.x=TRUE) %>%
mutate(is_outlier = !is.na(outlier_value)) %>%
select(gbifid,lat,lon,n_outliers,bio_var,bio_value,is_outlier) %>% 
glimpse()

d %>% 
filter(is_outlier) %>%
group_by(bio_var) %>%
count()

if(TRUE) { # hide bio data tribble
bio_data = tibble::tribble(~bio_var,~long_name,
"bio1","Annual Mean Temperature",
"bio2","Mean Diurnal Range",
"bio3","Isothermality (bio2/bio7) (×100)",
"bio4","Temperature Seasonality",
"bio5","Max Temperature of Warmest Month",
"bio6","Min Temperature of Coldest Month",
"bio7","Temperature Annual Range (bio5-bio6)",
"bio8","Mean Temperature of Wettest Quarter",
"bio9","Mean Temperature of Driest Quarter",
"bio10","Mean Temperature of Warmest Quarter",
"bio11","Mean Temperature of Coldest Quarter",
"bio12","Annual Precipitation",
"bio13","Precipitation of Wettest Month",
"bio14","Precipitation of Driest Month",
"bio15","Precipitation Seasonality",
"bio16","Precipitation of Wettest Quarter",
"bio17","Precipitation of Driest Quarter",
"bio18","Precipitation of Warmest Quarter",
"bio19","Precipitation of Coldest Quarter")
}


d_cum_freq = d %>% 
group_by(bio_var,bio_value,is_outlier) %>%
summarise(freq = n()) %>%
arrange(bio_var,bio_value) %>%
ungroup() %>%
group_by(bio_var) %>% 
mutate(cum_freq = cumsum(freq)) %>% 
group_by(bio_var) %>% 
mutate(any_outlier = any(is_outlier)) %>% 
filter(any_outlier) %>% 
merge(bio_data,id=bio_var,all.x=TRUE) %>%
mutate(bio_num = as.numeric(stringr::str_replace_all(bio_var,"bio",""))) %>% 
mutate(long_name=paste0(bio_var,"-",long_name)) %>% 
mutate(long_name=forcats::fct_reorder(long_name,bio_num)) %>% 
glimpse()

library(ggplot2)

p = ggplot(d_cum_freq,aes(bio_value,cum_freq)) + 
geom_point(aes(color=is_outlier),size=3) + 
facet_wrap(~long_name,scales="free",ncol=2) + 
theme_bw() + 
theme(legend.position="top") + 
theme(strip.background = element_rect(fill="#f9f9f9")) +
theme(strip.text=element_text(face="bold")) + 
scale_color_manual(values=c("#509E2F","#FDAF02")) + 
guides(color=guide_legend(title="Outlier")) + 
ylab("cumulative frequency") + 
xlab("bioclim variable")

save_dir = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/plots/"
gbifapi::save_ggplot_formats(p,save_dir,"raster",height=4*3,width=7.5)




path = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/data/"
# getData('worldclim', var='bio', res=10,path=path) %>% 
# saveRDS(paste0(path,"worldclim.rda"))

r = readRDS(paste0(path,"worldclim.rda"))

library(ggplot2)
library(patchwork)

plot_list = 1:3 %>% 
map(~ {
r_df = as(r[[.x]], "SpatialPixelsDataFrame") %>% 
as.data.frame() %>%
tidyr::pivot_longer(cols=contains("bio"),names_to="bio_var") %>% 
merge(bio_data,id="bio_var") %>% 
glimpse()

d %>% glimpse()

df_points = d %>% 
filter(bio_var == paste0("bio",.x))

max_x = max(df_points$lon) + 10
min_x = min(df_points$lon)
max_y = max(df_points$lat) + 10
min_y = min(df_points$lat)

p = ggplot(r_df,aes(x,y)) +
geom_raster(aes(fill=value))+
scale_fill_gradientn(colours=c("brown","red","yellow","darkgreen","green"))+
coord_equal() + 
geom_point(data=df_points,aes(lon,lat,color=is_outlier),size=0.5) + 
scale_colour_manual(values = c("gray","red")) +
theme_bw() + 
facet_wrap(~bio_var,ncol=1) + 
theme(legend.position="none") +
theme(plot.margin = margin(0, 0, 0, 0)) +
scale_x_continuous(limits = c(min_x, max_x)) + 
scale_y_continuous(limits = c(min_y, max_y)) 

})


p = wrap_plots(plot_list,ncol=1)

save_dir = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/plots/"
gbifapi::save_ggplot_formats(p,save_dir,"raster",height=4*3,width=7.5)

}

if(FALSE) { # facet wrapped bio cumfreq plots 
library(raster)
library(dplyr) # always dplyr after raster
library(sp)

if(TRUE) { # hide bio data tribble
bio_data = tibble::tribble(~bio_var,~long_name,
"bio1","Annual Mean Temperature",
"bio2","Mean Diurnal Range",
"bio3","Isothermality (bio2/bio7) (×100)",
"bio4","Temperature Seasonality",
"bio5","Max Temperature of Warmest Month",
"bio6","Min Temperature of Coldest Month",
"bio7","Temperature Annual Range (bio5-bio6)",
"bio8","Mean Temperature of Wettest Quarter",
"bio9","Mean Temperature of Driest Quarter",
"bio10","Mean Temperature of Warmest Quarter",
"bio11","Mean Temperature of Coldest Quarter",
"bio12","Annual Precipitation",
"bio13","Precipitation of Wettest Month",
"bio14","Precipitation of Driest Month",
"bio15","Precipitation Seasonality",
"bio16","Precipitation of Wettest Quarter",
"bio17","Precipitation of Driest Quarter",
"bio18","Precipitation of Warmest Quarter",
"bio19","Precipitation of Coldest Quarter")
}

path = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/data/"

dbscan_outliers = data.table::fread("C:/Users/ftw712/Desktop/gbif_geographic_outliers/data/dbscan_outliers/dbscan_outliers_export.tsv") 

rjack_outliers = data.table::fread(paste0(path,"rjack_outliers_export.tsv")) %>%
filter(orderkey == 798) %>%
rename_at(vars(paste0("bio",1:19)), ~ paste0("outlier",1:19)) %>%
select("gbifid","specieskey",contains("bio"),contains("outlier"),-contains("comp")) 

specieskey_with_outlier = rjack_outliers %>% 
pull(specieskey) %>% 
unique() %>% 
head(1)

# get outliers ids 
dbscan_outlier_gbifid = dbscan_outliers %>%
filter(specieskey %in% !!specieskey_with_outlier) %>%
pull(gbifid)

rjack_outlier_gbifid = rjack_outliers %>%
filter(specieskey %in% !!specieskey_with_outlier) %>%
pull(gbifid) 

d = data.table::fread(paste0(path,"bioclim_primates_export.tsv")) %>%
filter(specieskey %in% specieskey_with_outlier) %>%
glimpse() %>%
mutate(rjack_outlier = gbifid %in% !!rjack_outlier_gbifid) %>% 
mutate(dbscan_outlier = gbifid %in% !!dbscan_outlier_gbifid) %>% 
select(-contains("comp_")) %>%
merge(rjack_outliers,id="gbifid",all.x=TRUE) %>%
arrange(-n_bioclim_outliers)

bio_d = d %>% 
select(contains("bio"),
-n_bioclim_outliers,
-decimallatitude_bioclim,
-decimallongitude_bioclim,
-dbscan_outlier,
-rjack_outlier) %>% 
tidyr::pivot_longer(cols=contains("bio"),names_to="bio_var",values_to="bio_value") %>%
na.omit() %>% 
mutate(id=paste0(bio_var,"_",bio_value)) %>%
glimpse()

outlier_d = d %>% 
select(contains("outlier"),
-n_bioclim_outliers,
-decimallatitude_bioclim,
-decimallongitude_bioclim,
-dbscan_outlier,
-rjack_outlier) %>% 
tidyr::pivot_longer(cols=contains("outlier"),names_to="outlier_var",values_to="outlier_value") %>%
mutate(outlier_var = stringr::str_replace_all(outlier_var,"outlier","bio")) %>%
na.omit() %>% 
mutate(id=paste0(outlier_var,"_",outlier_value)) %>%
glimpse()

d = merge(bio_d,outlier_d,id="id",all.x=TRUE) %>%
mutate(is_outlier = !is.na(outlier_value)) %>%
select(bio_var,bio_value,is_outlier) %>% 
glimpse()

d_cum_freq = d %>% 
group_by(bio_var,bio_value,is_outlier) %>%
summarise(freq = n()) %>%
arrange(bio_var,bio_value) %>%
ungroup() %>%
group_by(bio_var) %>% 
mutate(cum_freq = cumsum(freq)) %>% 
group_by(bio_var) %>% 
mutate(any_outlier = any(is_outlier)) %>% 
filter(any_outlier) %>% 
merge(bio_data,id=bio_var,all.x=TRUE) %>%
mutate(bio_num = as.numeric(stringr::str_replace_all(bio_var,"bio",""))) %>% 
mutate(long_name=paste0(bio_var,"-",long_name)) %>% 
mutate(long_name=forcats::fct_reorder(long_name,bio_num)) %>% 
glimpse() 

library(ggplot2)

p = ggplot(d_cum_freq,aes(bio_value,cum_freq)) + 
geom_point(aes(color=is_outlier),size=3) + 
facet_wrap(~long_name,scales="free",ncol=2) + 
theme_bw() + 
theme(legend.position="top") + 
theme(strip.background = element_rect(fill="#f9f9f9")) +
theme(strip.text=element_text(face="bold")) + 
scale_color_manual(values=c("#509E2F","#FDAF02")) + 
guides(color=guide_legend(title="Outlier")) + 
ylab("cumulative frequency") + 
xlab("bioclim variable")


save_dir = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/plots/"
gbifapi::save_ggplot_formats(p,save_dir,"cum_freq",height=12,width=7.5)
}


if(FALSE) { # plot bioclim and distance outliers 

library(raster)
library(dplyr) # always dplyr after raster
library(sp)

path = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/data/"

dbscan_outliers = data.table::fread("C:/Users/ftw712/Desktop/gbif_geographic_outliers/data/dbscan_outliers/dbscan_outliers_export.tsv") 

rjack_outliers = data.table::fread(paste0(path,"rjack_outliers_export.tsv")) %>%
filter(orderkey == 798) %>%
rename_at(vars(paste0("bio",1:19)), ~ paste0("bio_outlier",1:19)) %>%
select("gbifid","specieskey",contains("bio"),-contains("comp")) 

specieskey_with_outlier = rjack_outliers %>% 
pull(specieskey) %>% 
unique() %>% 
head(1)

# get outliers ids 
dbscan_outlier_gbifid = dbscan_outliers %>%
filter(specieskey %in% !!specieskey_with_outlier) %>%
pull(gbifid)

rjack_outlier_gbifid = rjack_outliers %>%
filter(specieskey %in% !!specieskey_with_outlier) %>%
pull(gbifid)

d = data.table::fread(paste0(path,"bioclim_primates_export.tsv")) %>%
filter(specieskey %in% specieskey_with_outlier) %>%
mutate(rjack_outlier = gbifid %in% !!rjack_outlier_gbifid) %>% 
mutate(dbscan_outlier = gbifid %in% !!dbscan_outlier_gbifid) %>% 
select(-contains("comp_")) %>%
merge(rjack_outliers,id="gbifid",all.x=TRUE) %>%
arrange(-n_bioclim_outliers)

# rjack_outliers %>% glimpse()

focal_class = d %>% pull(class) %>% unique()
focal_order = d %>% pull(order_) %>% unique()
focal_species = d %>% pull(species) %>% unique()
focal_species_key = d %>% pull(specieskey) %>% unique()

countries = gbifapi::ggplot2_small_map_data(dTolerance=0.8)

library(ggplot2)
library(patchwork)

if(FALSE) { # old plots
d_sf = d %>%
sf::st_as_sf(coords = c("rounded_decimallongitude", "rounded_decimallatitude"), crs = 4326)

# theme(legend.position = c(0.05,0.15)) + 
p1 = ggplot() +
theme_bw() +
geom_polygon(data=countries,aes(x=long, y=lat, group=group), fill="#D8DACF", color="#D8DACF",alpha=0.8) +
geom_sf(data=d_sf,aes(group=rjack_outlier,color=rjack_outlier),size=0.5) +
scale_color_manual(values = c("#509E2F","#FDAF02")) + 
scale_fill_manual(values = c("gray")) +
xlab("") + 
ylab("") +
labs(title="Any bioclim variable is an outlier?")

# # caption = paste0(focal_class," - ",focal_order," - ",focal_species,"\n"," specieskey=", focal_species_key,"\n","outlier gbifids: ",paste("outliers",collapse=" "))
# # ) + 
# # + 
# # theme(plot.margin = margin(t=0, r=2, b=0, l=0, unit = "cm")) 

p2 = ggplot() +
theme_bw() +
geom_polygon(data=countries,aes(x=long, y=lat, group=group), fill="#D8DACF", color="#D8DACF",alpha=0.8) +
geom_sf(data=d_sf%>%filter(is.na(n_bioclim_outliers)),color="gray",size=0.5) + 
geom_sf(data=d_sf%>%filter(!is.na(n_bioclim_outliers)),aes(color=n_bioclim_outliers),size=0.5) + 
xlab("") + 
ylab("") 
}

# plot raster data 


path = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/data/"
# getData('worldclim', var='bio', res=10,path=path) %>% 
# saveRDS(paste0(path,"worldclim.rda"))

r = readRDS(paste0(path,"worldclim.rda"))


# bio_data %>% glimpse()

r_df = as(r[[4]], "SpatialPixelsDataFrame") %>% 
as.data.frame() %>%
glimpse()

p3 = ggplot(r_df,aes(x,y)) +
geom_raster(aes(fill=bio4))+
geom_point(data=d%>%filter(is.na(bio4)),aes(decimallongitude,decimallatitude,),color="gray",alpha=0.1,size=0.5) + 
geom_point(data=d%>%filter(!is.na(bio4)),aes(decimallongitude,decimallatitude),color="red",size=0.5) + 
scale_fill_gradientn(colours=c("brown","red","yellow","darkgreen","green"))+
coord_equal() + 
theme_bw()

# geom_point(data=data.frame(x=1,y=1),aes(x,y)) +
# p = (p1 + p2) / (p3 + p1)

# cumulative frequency plot

d %>%
glimpse()
# pivot_longer(!contains(""))

# pivot_longer(!religion, names_to = "income", values_to = "count")

# d_cum_freq = d %>% 
# mutate(is_outlier = !is.na(bio_outlier4)) %>%
# select(bio4,is_outlier) %>%
# group_by(bio4,is_outlier) %>%
# summarise(freq = n()) %>%
# arrange(bio4) %>% 
# ungroup() %>% 
# mutate(cum_freq = cumsum(freq)) %>% 
# na.omit() %>% 
# glimpse() 

# d_cum_freq$cum_freq

# p4 = ggplot(d_cum_freq) +
# geom_point(aes(bio4,cum_freq,color=is_outlier)) + 
# theme_bw()

# p = p3/p4

# save_dir = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/plots/"
# gbifapi::save_ggplot_formats(p,save_dir,focal_species,height=4,width=8)

# }
# # scale_color_manual(values = c("#509E2F","#FDAF02")) + 
# # scale_fill_manual(values = c("gray")) + 


# # + 
# # theme(plot.margin = margin(t=0, r=2, b=0, l=0, unit = "cm")) 

# p3 = ggplot() +
# theme_bw() +
# geom_polygon(data=countries,aes(x=long, y=lat, group=group), fill="#D8DACF", color="#D8DACF",alpha=0.8) +
# geom_sf(data=d_sf%>%filter(is.na(comp1)),color="gray",size=0.5) + 
# geom_sf(data=d_sf%>%filter(!is.na(comp1)),color="red",size=0.5) + 
# xlab("") + 
# ylab("") 

# p4 = ggplot() +
# theme_bw() +
# geom_polygon(data=countries,aes(x=long, y=lat, group=group), fill="#D8DACF", color="#D8DACF",alpha=0.8) +
# geom_sf(data=d_sf,aes(group=dbscan_outlier,color=dbscan_outlier),size=0.5) +
# scale_color_manual(values = c("#509E2F","#FDAF02")) + 
# scale_fill_manual(values = c("gray")) +
# xlab("") + 
# ylab("")

# # + 
# # theme(plot.margin = margin(t=0, r=2, b=0, l=0, unit = "cm")) 

# p = (p1 + p2)/ (p3 + p4)

# save_dir = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/plots/"
# gbifapi::save_ggplot_formats(p,save_dir,focal_species,height=8,width=8)

}


if(FALSE) { # extract bat outliers 

library(dplyr)

dbscan_outliers = data.table::fread("C:/Users/ftw712/Desktop/gbif_geographic_outliers/data/dbscan_outliers/dbscan_outliers_export.tsv") %>% 
pull(gbifid)  

path = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/data/"
rjack_table = data.table::fread(paste0(path,"rjack_outliers_export.tsv")) %>%
filter(orderkey == 734) %>%
select("gbifID"="gbifid",contains("bio")) %>%
glimpse()

rjack_outliers = rjack_table %>% pull(gbifID) 

d = data.table::fread("C:/Users/ftw712/Desktop/bat_records_for_outlier_det.csv") %>% 
merge(rjack_table,id="gbifid",all.x=TRUE) %>%
mutate(is_dbscan_outlier = gbifID %in% !!dbscan_outliers) %>% 
mutate(is_rjack_outlier = gbifID %in% !!rjack_outliers) %>% 
mutate(rjack_and_dbscan_outlier = is_dbscan_outlier & is_rjack_outlier) %>%
glimpse() 

d$is_dbscan_outlier %>% table()
d$is_rjack_outlier %>% table()
d$rjack_and_dbscan_outlier %>% table()

d %>% readr::write_tsv("C:/Users/ftw712/Desktop/bat_records.tsv",na="")

}



if(FALSE) { # test raster plotting 

library(sp)
library(raster)
library(dplyr)
# library(tidyverse)

path = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/data/"
# getData('worldclim', var='bio', res=10,path=path) %>% 
# saveRDS(paste0(path,"worldclim.rda"))

r = readRDS(paste0(path,"worldclim.rda"))

bio_data = tibble::tribble(~bio_var,~long_name,
"bio1","Annual Mean Temperature",
"bio2","Mean Diurnal Range (Mean of monthly (max temp - min temp))",
"bio3","Isothermality (bio2/bio7) (×100)",
"bio4","Temperature Seasonality (standard deviation × 100)",
"bio5","Max Temperature of Warmest Month",
"bio6","Min Temperature of Coldest Month",
"bio7","Temperature Annual Range (bio5-bio6)",
"bio8","Mean Temperature of Wettest Quarter",
"bio9","Mean Temperature of Driest Quarter",
"bio10","Mean Temperature of Warmest Quarter",
"bio11","Mean Temperature of Coldest Quarter",
"bio12","Annual Precipitation",
"bio13","Precipitation of Wettest Month",
"bio14","Precipitation of Driest Month",
"bio15","Precipitation Seasonality (Coefficient of Variation)",
"bio16","Precipitation of Wettest Quarter",
"bio17","Precipitation of Driest Quarter",
"bio18","Precipitation of Warmest Quarter",
"bio19","Precipitation of Coldest Quarter")

bio_data %>% glimpse()

r_df = as(r[[1]], "SpatialPixelsDataFrame") %>% 
as.data.frame() %>%
glimpse()

library(ggplot2)
# scale_fill_gradientn(colours=c("brown","red","yellow","darkgreen","green"),trans="log10")+

p = ggplot(r_df,aes(x,y)) +
geom_raster(aes(fill=bio1))+
scale_fill_gradientn(colours=c("brown","red","yellow","darkgreen","green"))+
geom_point(data=data.frame(x=1,y=1),aes(x,y)) +
coord_equal()

save_dir = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/plots/"
gbifapi::save_ggplot_formats(p,save_dir,"raster_plot",height=8,width=8)
}

if(FALSE) { # plot outliers 
library(dplyr)

get_d = function(focal_species_key,outliers) {

d = rgbif::occ_search(
focal_species_key,
hasGeospatialIssue=FALSE,
hasCoordinate = TRUE,
limit=50000) %>%
pluck("data") %>%
filter(basisOfRecord %in%
c(
"HUMAN_OBSERVATION",
"LITERATURE",
"MACHINE_OBSERVATION",
"OBSERVATION",
"PRESERVED_SPECIMEN",
"MATERIAL_SAMPLE")
) %>%
filter(year >= 1970) %>%
select(
gbifID,
species,
order,
class,
decimalLatitude,
decimalLongitude
) %>%
mutate(outlier = ifelse(gbifID %in% outliers,"yes","no")) %>%
glimpse() %>%
saveRDS("C:/Users/ftw712/Desktop/d.rda")
}

# outlier_data = data.table::fread("C:/Users/ftw712/Desktop/gbif_geographic_outliers/data/dbscan_outliers/dbscan_outliers_export.tsv") 

library(dplyr)
library(purrr)

outlier_rjack = data.table::fread("C:/Users/ftw712/Desktop/gbif_reverse_jackknife/data/rjack_outliers_export.tsv") 

outlier_rjack %>% glimpse()

focal_species_key = 
outlier_rjack %>% 
select(specieskey) %>%
unique() %>% 
sample_n(1) %>% 
pull(specieskey)

# focal_species_key = 5289845
focal_species_key

outliers = outlier_rjack %>%
filter(specieskey == !!focal_species_key) %>% 
pull(gbifid) %>%
as.character()

get_d(focal_species_key,outliers)

d = readRDS("C:/Users/ftw712/Desktop/d.rda") %>% 
glimpse()

focal_class = d %>% pull(class) %>% unique()
focal_order = d %>% pull(order) %>% unique()
focal_species = d %>% pull(species) %>% unique()
eps = "1500 km"
minPts = 3
countries = gbifapi::ggplot2_small_map_data(dTolerance=0.8)

library(ggplot2)
library(patchwork)

d_sf = d %>%
sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

buffer_color = "#cccccc"
buffer_color
buffer = 7
sf_buffer = d_sf %>% 
sf::st_buffer(buffer) %>%
sf::st_union()
# geom_sf(data = sf_buffer,aes(fill = buffer_color),colour=NA,alpha=0.5,show.legend=FALSE) +

outliers

p = ggplot() +
theme_bw() +
geom_polygon(data=countries,aes(x=long, y=lat, group=group), fill="#D8DACF", color="#D8DACF",alpha=0.8) +
geom_sf(data=d_sf,aes(group=outlier,color=outlier),size=0.5) +
scale_color_manual(values = c("#509E2F","#FDAF02")) + 
scale_fill_manual(values = c(buffer_color)) + 
labs(
caption = paste0(focal_class," - ",focal_order," - ",focal_species,"\n"," specieskey=", focal_species_key,"\n","outlier gbifids: ",paste(outliers,collapse=" "))
) + 
xlab("") + 
ylab("") + 
theme(legend.position = c(0.05,0.15)) + 
theme(plot.margin = margin(t=0, r=2, b=0, l=0, unit = "cm")) 

save_dir = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/plots/"

gbifapi::save_ggplot_formats(p,save_dir,focal_species,height=4)
  # width= 7,
  # height = 7,
  # formats = c("pdf","svg","jpg"),
  # dpi = 600,
  # subfolder = FALSE
# )


if(FALSE) {
ggsave(paste0(save_dir,"pdf/",focal_species,".pdf"),plot=p,width=8,height=4)
ggsave(paste0(save_dir,"svg/",focal_species,".svg"),plot=p,width=8,height=4)
ggsave(paste0(save_dir,"jpg/",focal_species,".jpg"),plot=p,width=8,height=4,dpi=600)

ggsave(paste0("C:/Users/ftw712/Desktop/data-blog/static/post/2020-07-06-outlier-detection-using-dbscan_files/",focal_species,".svg"),plot=p,width=8,height=4)

}


}



if(FALSE) { # test rjack.scala vs biogeo::rjack 

# list.files("C:/Users/ftw712/Desktop/gbif_reverse_jackknife/data/")

library(dplyr)
path = "C:/Users/ftw712/Desktop/gbif_reverse_jackknife/data/"

scala_outliers = data.table::fread(paste0(path,"rjack_outliers_export.tsv")) %>%
filter(orderkey == 798) %>%
glimpse()

specieskey_with_outlier = scala_outliers %>% 
pull(specieskey) %>% 
unique() %>% 
head(1)
specieskey_with_outlier

d = data.table::fread(paste0(path,"bioclim_primates_export.tsv")) %>%
filter(specieskey %in% specieskey_with_outlier) 

scala_values = scala_outliers %>% 
filter(specieskey == specieskey_with_outlier) %>% 
pull(bio1) %>% 
unique()

x = d %>% 
select(bio1) %>% 
na.omit() %>% 
pull(bio1) 
 
rjack = function (d) {

xx = d
d = unique(d)
rng = diff(range(d))
mx = mean(d)
n = length(d)
n1 = n - 1
t1 = (0.95 * sqrt(n)) + 0.2

x = sort(d)
y = rep(0, n1)

for (i in 1:n1) {
x1 = x[i + 1] # lagged 
if (x[i] < mx) { # if less than mean 
y[i] = (x1 - x[i]) * (mx - x[i])
}
else {
y[i] = (x1 - x[i]) * (x1 - mx)
}
}

my = mean(y)
z = y/(sqrt(sum((y - my)^2)/n1))
out = rep(0, length(xx))

if (any(z > t1)) {
f = which(z > t1)
v = x[f]
if (v < median(x)) {
xa = (xx <= v) * 1
out = out + xa
}
if (v > median(x)) {
xb = (xx >= v) * 1
out = out + xb
}

} else {
out = out
}

f = which(out == 1)
f
}
 
x[rjack(x)]
scala_values

}











