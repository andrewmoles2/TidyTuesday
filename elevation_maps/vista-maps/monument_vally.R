#devtools::install_github("h-a-graham/rayvista", dependencies=TRUE)
#remotes::install_github("tylermorganwall/rayrender", force = TRUE)
#remotes::install_github("tylermorganwall/rayshader", force = TRUE)

library(rayshader)
library(rayvista)

# monument valley ----
mv_lat <- 36.987531
mv_long <- -110.083573

monument_valley <- plot_3d_vista(lat = mv_lat, long = mv_long, phi = 30,
                                 zscale = 10, zoom = 0.65, radius = 3100)

render_camera(theta = -115, phi = 25) #90 to be from cabin location

#render_snapshot(clear=TRUE, title_text = "Monument valley",
#                title_offset = c(60, 50),
#                title_size = 40,
#                title_font = "Avenir",
#                vignette = 0.1,
#                filename = "elevation_maps/vista-maps/outputs/monument_valley.png")

render_highquality(clear=TRUE, lightdirection = 65, lightaltitude = 25,
                   lightintensity = 600,
                   title_text = "Monument valley",
                   title_offset = c(60, 50),
                   title_size = 40,
                   title_font = "Avenir",
                   filename = "elevation_maps/vista-maps/outputs/monument_valley.png")

