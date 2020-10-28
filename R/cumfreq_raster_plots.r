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
