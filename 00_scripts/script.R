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

plot(antropic_na, col = "orange2", legend = FALSE)
plot(habitat_na, col = "forestgreen", add = TRUE, legend = FALSE)
# plot(buffer(lcp, 10), border = "tomato", add = TRUE)
plot(lcp, col = "red", add = TRUE)
# plot(del, add = TRUE)
plot(cen, col = "blue", add = TRUE)
plot(buffer(cen, 300), col = adjustcolor("purple", .5), border = "purple", add = TRUE)
# plot(buffer(cen, 150), col = adjustcolor("pink", .5), border = "pink", add = TRUE)

# distance ----------------------------------------------------------------

# fragment vector
habitat_na_patch <- terra::patches(habitat_na, directions = 8)
habitat_na_patch

plot(habitat_na_patch, col = rainbow(10))

habitat_na_patch_v <- habitat_na_patch %>% 
  terra::as.polygons() %>% 
  sf::st_as_sf()
habitat_na_patch_v
plot(habitat_na_patch_v, col = rainbow(10))

habitat_na_patch_v_dist <- sf::st_distance(habitat_na_patch_v) 
habitat_na_patch_v_dist[1:6, 1:6]

# point from minimal distance from st_distance() 
st_nearest_points_same <- function(x = x){
  
  da <- NULL
  line <- NULL

  for(i in 1:nrow(x)){
    
    da_i <- NULL
    line_i <- NULL
    
    for(j in 1:nrow(x)){
      
      cat(i, "and", j, "\n")
      
      line_ij <- sf::st_nearest_points(habitat_na_patch_v[i, ], habitat_na_patch_v[j, ])
      coords_ij <- sf::st_coordinates(line_ij)
      length_ij <- sf::st_length(line_ij)
      
      line_ij <- line_ij %>% 
        sf::st_as_sf() %>% 
        dplyr::mutate(length = as.numeric(length_ij)) %>% 
        dplyr::mutate(i = i, j = j, .before = 1)
      
      da_ij <- coords_ij %>%
        tibble::as_tibble() %>% 
        dplyr::select(-L1) %>% 
        dplyr::mutate(length = length_ij) %>% 
        dplyr::mutate(i = i, j = j, .before = 1)
      
      da_i <- rbind(da_i, da_ij) 
      line_i <- rbind(line_i, line_ij) 
      
    }
    
    da <- rbind(da, da_i) 
    line <- rbind(line, line_i) 
    
  }
  
  coords <- sf::st_as_sf(da, coords = c("X", "Y"), crs = st_crs(x))
  
  return(list(data = da, coordinates = coords, lines = line))
  
}

habitat_na_patch_v_near <- st_nearest_points_same(x = habitat_na_patch_v)
habitat_na_patch_v_near

habitat_na_patch_v_near_min <- habitat_na_patch_v_near$lines %>% 
  dplyr::filter(length != 0) %>% 
  dplyr::group_by(i) %>% 
  dplyr::arrange(length) %>%
  dplyr::slice_head(n = 1)
habitat_na_patch_v_near_min

# buffer dos frags
habitat_na_patch_v_buffer <- habitat_na_patch_v %>% 
  terra::vect() %>% 
  terra::buffer(100)
habitat_na_patch_v_buffer

plot(antropic_na, col = "orange2", legend = FALSE)
plot(habitat_na_patch_v$geometry, col = "forestgreen", add = TRUE)
plot(habitat_na_patch_v_buffer, border = "gray30", add = TRUE)
plot(habitat_na_patch_v_near_min$x, col = "blue", add = TRUE)
plot(del, add = TRUE)
