jrc <- raster("01_input/01_population_raster/03_jrc/whole_path/GHS_POP_E2020_GLOBE_R2022A_54009_100_V1_0_R5_C22.tif")
admin0 <-st_read("01_input/04_admin_boundary_without_pop/lbn_admbnda_adm0_cdr_20200810.shp")


jrc_reproject <- projectRaster(jrc,crs = crs(admin0))


crop_jrc <- crop(jrc_reproject, extent(admin0))
jrc_masked <- mask(crop_jrc, admin0)

plot(jrc_masked)
plot(admin0,add =T)

writeRaster(jrc_masked,"01_input/01_population_raster/03_jrc/jrc_population.tif")