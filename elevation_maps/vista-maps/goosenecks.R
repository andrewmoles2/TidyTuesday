#devtools::install_github("h-a-graham/rayvista", dependencies=TRUE)
#remotes::install_github("tylermorganwall/rayrender", force = TRUE)
#remotes::install_github("tylermorganwall/rayshader", force = TRUE)

library(rayshader)
library(rayvista)

# goosenecks ----
g_lat <- 37.167311
g_long <- -109.932617

goosenecks <- plot_3d_vista(lat = g_lat, long = g_long, phi = 35,
                            zscale = 5, zoom = 0.8, radius = 2500)

render_camera(theta = -45)

#render_snapshot(clear=TRUE, title_text = "Goosenecks",
#                title_offset = c(60, 50),
#                title_size = 40,
#                title_font = "Avenir",
#                vignette = 0.1)

render_highquality(clear=TRUE, lightdirection = 65, lightaltitude = 50,
                   lightintensity = 400,
                   title_text = "Goosenecks",
                   title_offset = c(60, 50),
                   title_size = 40,
                   title_font = "Avenir",
                   filename = "elevation_maps/vista-maps/outputs/goosenecks.png")