rm (list = ls())

start_time <- Sys.time()

library(sf)
library(raster)
library(dplyr)
library(sp)
library(exactextractr)
library(lwgeom)
library(units)
# library(Lslide)
st_dissolve = function(x, by = NULL, ...) x %>% dplyr::group_by(.dots = by) %>% dplyr::summarise(...)

library(rgeos)
library(rmapshaper)
library(stringr)
library(geosphere)
library(Rfast)
options(scipen = 999)



# st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))
# read data ---------------------------------------------------------------

raster_wp<- raster("01_input/01_population_raster/01_world_pop/lbn_ppp_2020_UNadj_constrained.tif") 
raster_fb<- raster("01_input/01_population_raster/02_facebook/lbn_general_2020.tif")
raster_jrc<- raster("01_input/01_population_raster/03_jrc/JRC_population_mask.tif")# %>% projectRaster(crs = 3891)



water_body <- st_read("01_input/02_shapefile/hotosm_lbn_waterways_polygons.shp") |> st_transform(32637) |> as_Spatial() %>% 
  gBuffer(byid=TRUE, width=0) %>% 
  st_as_sf() 

non_liveable_area<-   st_read("01_input/02_shapefile/Non_viable.shp")  %>% st_transform(32637) %>% as_Spatial() %>% 
  gBuffer(byid=TRUE, width=0) %>% 
  st_as_sf()


lbn_boundary <- st_read("01_input/04_admin_boundary_without_pop/lbn_admbnda_adm0_cdr_20200810.shp")  %>% 
  st_transform(crs = 32637)

lbn_boundary <- lbn_boundary %>% as_Spatial()

admin_3 <- st_read("01_input/03_admin_boundary_with_pop/admin3.shp") #%>% st_transform(crs = 32637)

admin_3 <- admin_3 %>% dplyr::select(ends_with("NAME")) %>% st_transform(32637)

 # admin_2 <- admin_3 |> st_dissolve(by = c("admin0Name","admin1Name","admin2Name"))

admin_2 <- st_read("01_input/03_admin_boundary_with_pop/admin2.shp") %>% st_transform(crs = 32637)



# set_units(sum(st_area(admin_3)), "km2")


# create hexagon  ---------------------------------------------------------

HexPts <-spsample(lbn_boundary, "hexagonal",n=11851, offset=c(0,0))
grid <- HexPoints2SpatialPolygons(HexPts) 
grid <- grid %>% st_as_sf() %>% st_transform(crs= 32637) 

set_units(st_area(grid[1,]), "km2")

# st_write(grid,"grid.shp")
###################################



# Identify non_liveable_area ----------------------------------------------

# liveable <- c( "village","shelter",  "building",
#                "hamlet", "town","county","locality","city",               
#                "region","suburb")
# 
# 
# # for ted, delete villages
# 
# non_liveable_area <- non_liveable_area %>% filter(!fclass_133 %in% liveable)


non_liveable_area_dis <- non_liveable_area %>%  st_dissolve()
waterbody_dis <- water_body %>% st_as_sf() %>% st_dissolve()


# crs(non_liveable_area_dis)


grid_remove_non_liveable <- rmapshaper::ms_erase(grid,non_liveable_area_dis)
grid_final <- rmapshaper::ms_erase(grid_remove_non_liveable,waterbody_dis)

grid_final <- grid_final |> mutate(
  row_id_psu =  row_number(grid_final))



############### start::split hex by admin 3 ##########
grid_final <- st_intersection(grid_final,admin_3) #|> dplyr::select(rmapshaperid)

grid_final <- grid_final |> 
  group_by(admin0Name,admin1Name,admin2Name,row_id_psu) |> 
  summarise() |> ungroup() |> select(row_id_psu)

# grid_final_st_inter$area_km <- st_area(grid_final_st_inter) %>% set_units( km^2)
# st_write(grid_final_st_inter,"check.shp")
###############end::split hex by admin 3 ###########################


grid_final$area_km <- st_area(grid_final) %>% set_units( km^2)

grid_wgs <-  grid_final %>% st_transform(crs = 4326)

grid_wgs <- grid_wgs %>% mutate(
  row_id = row.names(grid_wgs)
)

grid_wgs$row_id
# extract population value -----------------------------------------------------------

# exact_extract() - Sum of defined raster values within the polygon, accounting for coverage fraction
# https://cran.r-project.org/web/packages/exactextractr/readme/README.html

########################### WorldPop ###################################################
pop_frm_zone_wp<- exact_extract(raster_wp, grid_wgs, function(values,coverage_fractions)
  sum(values*coverage_fractions,na.rm = T))


Zone_with_data_wp <- data.frame(row_id = row.names(grid_wgs),
                             pop_wp= pop_frm_zone_wp %>% as.integer() )

############################# Facebook #################################################
pop_frm_zone_fb<- exact_extract(raster_fb, grid_wgs, function(values,coverage_fractions)
  sum(values*coverage_fractions,na.rm = T))


Zone_with_data_fb <- data.frame(row_id = row.names(grid_wgs),
                                pop_fb= pop_frm_zone_fb %>% as.integer() )



############################# JRC #################################################
pop_frm_zone_jrc<- exact_extract(raster_jrc, grid_wgs, function(values,coverage_fractions)
  sum(values*coverage_fractions,na.rm = T))


Zone_with_data_jrc <- data.frame(row_id = row.names(grid_wgs),
                                pop_jrc= pop_frm_zone_jrc %>% as.integer() )



# Zone_with_data$row_id <- paste0("ID",Zone_with_data$row_id)

zone_with_population<-grid_wgs %>% left_join(Zone_with_data_wp)
zone_with_population<-zone_with_population %>% left_join(Zone_with_data_fb)
zone_with_population<-zone_with_population %>% left_join(Zone_with_data_jrc)

pop_cols<- c("pop_fb","pop_wp","pop_jrc")


zone_with_population <- zone_with_population  %>% mutate(pop_min = pmin(pop_fb,pop_wp,pop_jrc))


# rowSums(zone_with_population[pop_cols],na.rm = T)
 
zone_with_population_with_info <- zone_with_population %>% mutate(
  cluster_character = case_when(pop_min == 0 ~ "no_population_in_the_cluster",
                                is.na(pop_min) ~ "population_NA",
                                1/as.numeric(area_km )< .5 ~ "more_than_half_of_the_area_falls_under_unliveable_zone",
                                pop_min < 200 ~ "cluster_population_is_less_than_200",
                                pop_min %in% c(200:500) ~ "need_accessibility_check_manually",
                                T~ "OK_for_sampling"), 
  draft_sampling_sts =  case_when(pop_min == 0 ~ "NOT_OK",
                                  is.na(pop_min) ~ "NOT_OK",
                                  1/as.numeric(area_km )< .3  ~ "NOT_OK",
                                  pop_min < 200 ~ "need_Manuel_check",
                                  pop_min %in% c(200:500) ~ "need_Manuel_check",
                                  T~ "OK")
)

zone_with_population_with_info$cluster_character %>% table()

zone_with_population_with_info  <- zone_with_population_with_info %>% st_transform(32637)
# 
hex_centriod <- st_point_on_surface(zone_with_population_with_info)
# 
# 
# st_write(zone_with_population_with_info,"with_info.shp")
# add admin boubdary ------------------------------------------------------

hex_point_admin_info <- st_intersection(hex_centriod,admin_3)


hex_point_admin_need_info <- hex_point_admin_info %>% dplyr::select(row_id,ends_with("Name")) %>%
  as.data.frame() %>%
  dplyr::select(-geometry)

 zone_with_population_with_info <- zone_with_population_with_info %>% st_transform(crs = 4326)

final_zone <- zone_with_population_with_info %>% left_join(hex_point_admin_need_info)

final_zone <- final_zone %>% st_transform(crs=32637)



final_zone <- final_zone %>% st_transform(crs= 4326)
final_zone <- final_zone %>% filter(!is.na(admin1Name))
final_zone <- final_zone %>% dplyr::select(row_id,starts_with("admin"),
                                    pop_wp,pop_fb,pop_jrc,pop_min,area_km,cluster_character,draft_sampling_sts) 

# write shapefile ---------------------------------------------------------

st_write(final_zone,paste0("01_input/05_cluster/lbn_one_km_hex.shp"))

end_time <- Sys.time()
end_time - start_time

