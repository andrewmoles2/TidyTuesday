#devtools::install_github("h-a-graham/rayvista", dependencies=TRUE)
#remotes::install_github("tylermorganwall/rayrender", force = TRUE)
#remotes::install_github("tylermorganwall/rayshader", force = TRUE)

library(rayshader)
library(rayvista)
library(rgdal)

# delicate arch ----
# not that great!
da_lat <- 38.74381627007101
da_long <- -109.49835283190927

#delicate_arch <- plot_3d_vista(lat = 38.7436, long = 109.4993, phi = 30,
                               #zscale=5, zoom=0.5, overlay_detail=14, 
                               #theta=-65, windowsize =1200)

delicate_arch <- plot_3d_vista(lat = da_lat, long = da_long, phi = 30,
                               zscale = 10, zoom = 0.8)

render_snapshot(clear=TRUE, title_text = "Delicate Arch",
                title_offset = c(60, 50),
                title_size = 40,
                title_font = "Avenir",
                vignette = 0.1)

#render_highquality(lightdirection = 220, clear=TRUE)


# emerald lake ----
emerald_lake <- plot_3d_vista(lat = 40.309362, long = -105.659001, phi = 30,
                              zscale = 4, zoom = 0.8)

render_snapshot(clear=TRUE, title_text = "Dream lake",
                title_offset = c(60, 50),
                title_size = 40,
                title_font = "Avenir")


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
                filename = "elevation_maps/maroon_bells.png")

# glacier ----
library(rayshader) 
library(rayvista)

glacier <- plot_3d_vista(lat=48.7446731, long=-113.8591203, zscale=5, zoom=0.5,
                         overlay_detail=14, theta=-65, windowsize =1200, 
                         phi=25)

render_snapshot(clear=TRUE)

render_highquality(lightdirection = 220, clear=TRUE)

# cluillins ---- 
.lat <- 57.219566
.long <- -6.092690

cuillins <- plot_3d_vista(lat = .lat, long = .long, phi=30)

render_label(heightmap= cuillins, text='Bla Bheinn: 928 m', lat = .lat,
             long=.long, extent = attr(cuillins, 'extent'),altitude=600,
             clear_previous = T, zscale = 2)

render_compass() 

render_scalebar(limits=c(
  round(dim(cuillins)[2]*attr(cuillins, 'resolution')/1000,1)),
  label_unit = 'km')

render_snapshot(clear=TRUE)

# grand canyon ----
grand_canyon <- plot_3d_vista(lat = 36.2135, long = 112.0581, phi=30)

render_snapshot(clear=TRUE)
