rm(list = ls())

# library -----------------------------------------------------------------


library(sf)
library(dplyr)
library(rgdal)
library(raster)
library(exactextractr)




# read --------------------------------------------------------------------

admin_zero <-st_read("01_input/04_admin_boundary_without_pop/lbn_admbnda_adm0_cdr_20200810.shp")
admin1_boundary <- st_read("01_input/04_admin_boundary_without_pop/lbn_admbnda_adm1_cdr_20200810.shp")
admin2_boundary <- st_read("01_input/04_admin_boundary_without_pop/lbn_admbnda_adm2_cdr_20200810.shp")
admin3_boundary <- st_read("01_input/04_admin_boundary_without_pop/lbn_admbnda_adm3_cdr_20200810.shp")

raster_worldpop <- raster::raster("01_input/01_population_raster/01_world_pop/lbn_ppp_2020_UNadj_constrained.tif")
raster_facebook <- raster::raster("01_input/01_population_raster/02_facebook/lbn_general_2020.tif")
raster_jrc <- raster::raster("01_input/01_population_raster/03_jrc/JRC_population_mask.tif")




# admin 1 -----------------------------------------------------------------

admin1_boundary <- admin1_boundary %>% mutate(
  row_id = row.names(admin1_boundary)
)

################################### world pop ###############################
adm1_zone<- exact_extract(raster_worldpop, admin1_boundary, function(values,coverage_fractions)
  sum(values*coverage_fractions,na.rm = T))


adm1_data_wp <- data.frame(row_id = row.names(admin1_boundary),
                        pop_wp = adm1_zone %>% as.integer() )

##################################facebook #################################
adm1_zone<- exact_extract(raster_facebook, admin1_boundary, function(values,coverage_fractions)
  sum(values*coverage_fractions,na.rm = T))


adm1_data_fb <- data.frame(row_id = row.names(admin1_boundary),
                           pop_fb = adm1_zone %>% as.integer() )

##################################jrc #################################
adm1_zone<- exact_extract(raster_jrc, admin1_boundary, function(values,coverage_fractions)
  sum(values*coverage_fractions,na.rm = T))


adm1_data_jrc <- data.frame(row_id = row.names(admin1_boundary),
                           pop_jrc = adm1_zone %>% as.integer() )

##################################################################################




# Zone_with_data$row_id <- paste0("ID",Zone_with_data$row_id)

adm1_with_population<-admin1_boundary %>% left_join(adm1_data_wp)
adm1_with_population<-adm1_with_population %>% left_join(adm1_data_fb)
adm1_with_population<-adm1_with_population %>% left_join(adm1_data_jrc)


st_write(adm1_with_population, "01_input/03_admin_boundary_with_pop/admin1.shp")



# admin 2 -----------------------------------------------------------------

admin2_boundary <- admin2_boundary %>% mutate(
  row_id = row.names(admin2_boundary)
)


################################### world pop ###############################
adm2_zone<- exact_extract(raster_worldpop, admin2_boundary, function(values,coverage_fractions)
  sum(values*coverage_fractions,na.rm = T))


adm2_data_wp <- data.frame(row_id = row.names(admin2_boundary),
                           pop_wp = adm2_zone %>% as.integer() )

##################################facebook #################################
adm2_zone<- exact_extract(raster_facebook, admin2_boundary, function(values,coverage_fractions)
  sum(values*coverage_fractions,na.rm = T))


adm2_data_fb <- data.frame(row_id = row.names(admin2_boundary),
                           pop_fb = adm2_zone %>% as.integer() )


##################################JRC #################################
adm2_zone<- exact_extract(raster_jrc, admin2_boundary, function(values,coverage_fractions)
  sum(values*coverage_fractions,na.rm = T))


adm2_data_jrc <- data.frame(row_id = row.names(admin2_boundary),
                           pop_jrc = adm2_zone %>% as.integer() )

##################################################################################


# Zone_with_data$row_id <- paste0("ID",Zone_with_data$row_id)

adm2_with_population<-admin2_boundary %>% left_join(adm2_data_wp)
adm2_with_population<-adm2_with_population %>% left_join(adm2_data_fb)
adm2_with_population<-adm2_with_population %>% left_join(adm2_data_jrc)


st_write(adm2_with_population, "01_input/03_admin_boundary_with_pop/admin2.shp")



# admin 3 -----------------------------------------------------------------

admin3_boundary <- admin3_boundary %>% mutate(
  row_id = row.names(admin3_boundary)
)


################################### world pop ###############################
adm3_zone<- exact_extract(raster_worldpop, admin3_boundary, function(values,coverage_fractions)
  sum(values*coverage_fractions,na.rm = T))


adm3_data_wp <- data.frame(row_id = row.names(admin3_boundary),
                           pop_wp = adm3_zone %>% as.integer() )

##################################facebook #################################
adm3_zone<- exact_extract(raster_facebook, admin3_boundary, function(values,coverage_fractions)
  sum(values*coverage_fractions,na.rm = T))


adm3_data_fb <- data.frame(row_id = row.names(admin3_boundary),
                           pop_fb = adm3_zone %>% as.integer() )



##################################JRC #################################
adm3_zone<- exact_extract(raster_jrc, admin3_boundary, function(values,coverage_fractions)
  sum(values*coverage_fractions,na.rm = T))


adm3_data_jrc <- data.frame(row_id = row.names(admin3_boundary),
                           pop_jrc = adm3_zone %>% as.integer() )

##################################################################################


# Zone_with_data$row_id <- paste0("ID",Zone_with_data$row_id)

adm3_with_population<-admin3_boundary %>% left_join(adm3_data_wp)
adm3_with_population<-adm3_with_population %>% left_join(adm3_data_fb)
adm3_with_population<-adm3_with_population %>% left_join(adm3_data_jrc)


st_write(adm3_with_population, "01_input/03_admin_boundary_with_pop/admin3.shp")

