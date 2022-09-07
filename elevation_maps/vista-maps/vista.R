#devtools::install_github("h-a-graham/rayvista", dependencies=TRUE)
#remotes::install_github("tylermorganwall/rayrender", force = TRUE)
#remotes::install_github("tylermorganwall/rayshader", force = TRUE)

library(rayshader)
library(rayvista)

# Maroon bells ----
mb_lat <- 39.072186624969
mb_long <- -106.99161708476214

maroon_bells <- plot_3d_vista(lat = mb_lat, long = mb_long, phi = 30,
                              zscale = 5, zoom = 0.8)

render_snapshot(clear=TRUE, title_text = "Maroon bells",
                title_offset = c(60, 50),
                title_size = 40,
                title_font = "Avenir",
                vignette = 0.1,
                filename = "elevation_maps/vista-maps/outputs/maroon_bells.png")


# delicate arch ----
# not that great!
da_lat <- 38.74381627007101
da_long <- -109.49835283190927

#delicate_arch <- plot_3d_vista(lat = 38.7436, long = 109.4993, phi = 30,
                               #zscale=5, zoom=0.5, overlay_detail=14, 
                               #theta=-65, windowsize =1200)

delicate_arch <- plot_3d_vista(lat = da_lat, long = da_long, phi = 25,
                               zscale = 3, zoom = 0.8, radius = 1000)

render_snapshot(clear=TRUE, title_text = "Delicate Arch",
                title_offset = c(60, 50),
                title_size = 40,
                title_font = "Avenir",
                vignette = 0.1)

#render_highquality(lightdirection = 220, clear=TRUE)


# emerald lake ----
emerald_lake <- plot_3d_vista(lat = 40.309362, long = -105.659001, phi = 30,
                              zscale = 2, zoom = 0.8)

render_snapshot(clear=TRUE, title_text = "Dream lake",
                title_offset = c(60, 50),
                title_size = 40,
                title_font = "Avenir")




# grand canyon ----
grand_canyon <- plot_3d_vista(lat = 36.2135, long = 112.0581, phi=30)

render_snapshot(clear=TRUE)


# old man of stoor ----
stoor_lat <- 57.507224
stoor_long <- -6.175005

old_man_stoor <- plot_3d_vista(lat = stoor_lat, long = stoor_long, phi = 35,
                               zscale = 6, zoom = 0.75, radius = 1250)

render_camera(theta = -225)

render_highquality(clear=TRUE, lightdirection = 65, lightaltitude = 50,
                   title_text = "Old Man of Stoor",
                   title_offset = c(60, 50),
                   title_size = 40,
                   title_font = "Avenir")

# Watzmannhaus, Germany ----
waz_lat <- 47.571835503803214
waz_long <- 12.935347290903296

watzmannhaus <- plot_3d_vista(lat = waz_lat, long = waz_long, phi = 35,
                              zscale = 4, zoom = 0.9, radius = 5000)

render_camera(theta = -225)

#render_snapshot(clear=TRUE, title_text = "Watzmannhaus",
#                title_offset = c(60, 50),
#                title_size = 40,
#                title_font = "Avenir",
#                vignette = 0.1)

render_highquality(clear=TRUE, lightdirection = 65, lightaltitude = 50,
                   title_text = "Watzmannhaus",
                   title_offset = c(60, 50),
                   title_size = 40,
                   title_font = "Avenir",
                   filename = "elevation_maps/vista-maps/outputs/Watzmannhaus.png")

