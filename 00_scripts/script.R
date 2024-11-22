#' ----
#' aim: graphs
#' author: mauricio vancine
#' date: 13/11/2024
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(terra)
library(landscapemetrics)

# import data -------------------------------------------------------------

## land use ----
area <- terra::vect("01_data/hids_shape.gpkg")
area

lulc <- terra::rast("01_data/Hids_landuse.tif") %>% 
  terra::project(crs(area), method = "near")
lulc[lulc == 4294967296] <- NA
lulc

plot(lulc)
freq(lulc)

# lpc
lcp <- terra::vect("01_data/hids_lcp.shp") %>% 
  terra::project(crs(area))
lcp

# centroids
cen <- read.table("01_data/hids_frag_cent.txt") %>% 
  tibble::as_tibble() %>% 
  terra::vect(geom = c("X", "Y"), crs = "EPSG:4326") %>% 
  terra::project(crs(area))
cen

plot(lulc)
plot(area, border = "yellow", lwd = 2, add = TRUE)
plot(lcp, col = "red", add = TRUE)
plot(cen, col = "blue", add = TRUE)

# habitat
habitat <- lulc %in% c(3, 9) %>% 
  terra::crop(lulc, mask = TRUE)
habitat_na <- terra::ifel(habitat == 0, NA, habitat)

plot(habitat)
plot(habitat_na)
plot(cen, col = "blue", add = TRUE)

# distance
habitat_dist <- terra::distance(habitat_na)
habitat_dist <- terra::crop(habitat_dist, lulc, mask = TRUE)

plot(habitat_dist)
plot(habitat_na, col = "forestgreen", legend = FALSE, add = TRUE)

# antropic
antropic <- lulc %in% c(15, 19, 21, 25) %>% 
  terra::crop(lulc, mask = TRUE)
antropic_na <- terra::ifel(antropic == 0, NA, antropic)
antropic_na
plot(antropic_na)

# delaunay
del <- terra::delaunay(cen)
del_dist <- terra::size(del)
del_dist

83

plot(antropic_na, col = "orange2", legend = FALSE)
plot(habitat_na, col = "forestgreen", add = TRUE, legend = FALSE)
plot(lcp, col = "red", add = TRUE)
plot(del, add = TRUE)
plot(cen, col = "blue", add = TRUE)
