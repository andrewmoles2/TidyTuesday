# ideas and code comes from - https://twitter.com/researchremora/status/1418392590244892673?s=21
# data from: https://gadm.org/download_country.html
# codes from: https://wits.worldbank.org/wits/wits/witshelp/content/codes/country_codes.htm

#remotes::install_github("tylermorganwall/rayrender")
#remotes::install_github("tylermorganwall/rayshader")

# load libraries ----
library(rayshader)
library(elevatr)
library(raster)

# read shape file for uk using ISO3C code GBR ----
readRDS(url("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_GBR_0_sf.rds")) -> uk

# get rather elevation data ----
elevatr::get_elev_raster(uk, z = 6) -> dem

# mask raster with shape file ----
raster::mask(dem, uk) -> uk_dem

# raster to mat for rayshader ----
rayshader::raster_to_matrix(uk_dem) -> uk_mat

# reduce mat size to speed up rendering time (70%) ----
rayshader::resize_matrix(uk_mat, 0.7) -> uk_mat_reduced

# rayshading ----
uk_mat_reduced %>%
  sphere_shade(texture = "imhof3") %>%
  plot_3d(uk_mat_reduced, windowsize = c(1200, 1200), zscale = 20,
          zoom = 0.75, phi = 89, theta = 0, fov = 0, background = "black")

# adjust options
render_camera(zoom = 0.45, phi = 60, theta = -0.5, fov = 10)

# render high quality ----
rayshader::render_highquality(filename = "elevation_maps/uk.png", samples = 100,
                   width = 6000, height = 6000)
