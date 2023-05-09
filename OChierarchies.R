# OC data: creating spatial hierarchy for Eric Kansa April 2023

# This script explores
# 1. VOronyi approach
# 2. Buffers approach
# 3. Select by distance approach
# 4. might look at neighborhood approach?

# merge units with sites and generate a column that lists either the site TRAP id, or a nonsite_1, nonsite_2. You can use Voronyi polygons too. 

library(sf)
library(raster)
library(tidyverse)

# Load survey polygons
kaz <- read_sf("C:/Users/adela/Downloads/kaz-survey-units-reproj-w-uuids-fixed.geojson")
yam <- read_sf("C:/Users/adela/Downloads/yam-survey-units-reproj-w-uuids.geojson")

kaz_35 <- st_transform(kaz, 32635)

plot(yam$geometry)
plot(kaz$geometry)



names(kaz)
st_crs(kaz)

# Load site polygons
k_sites <- read_sf("C:/Users/adela/Desktop/TRAP_Oxbow/KAZ/KAZ_margins.shp")
names(k_sites)
plot(k_sites$geometry)
k_sites

#  Load site centroids
kaz_sites <- read_sf("C:/Users/adela/Desktop/TRAP_Oxbow/KAZ/KAZ_scatterpoints.shp")
yam_sites<- read_sf("C:/Users/adela/Desktop/TRAP_Oxbow/YAM/YAM_scatterpoints.shp")

plot(kaz$geometry);plot(st_transform(kaz_sites$geometry, 4326), cex = 2, col = "red", add = T)

# Load site attributes (chronology)


################################ Join sites and polygons 
# Check for spatial join 
# to join attributes 

kaz_join_site <- kaz %>% 
  st_transform(32635) %>% 
  st_join(k_sites[,4:8])

kaz_join_site
colnames(kaz_join_site)
tail(kaz_join_site[30:36])

# check what groupings you get now


###################### Voronyi approach
?st_voronoi()


# need bounding box for Kaz survey as a second argument

kaz_master <- kaz %>% 
  st_transform(32635) %>% 
  st_simplify(dTolerance = 50) %>% 
  st_union(by_feature = F) %>% 
  st_buffer(1000)

plot(kaz_master)
st_bbox(kaz_master)
st_bbox(kaz_sites)

border <- st_as_sfc(st_bbox(kaz_sites))
point <- st_sfc(kaz_sites$geometry)
st_crs(border)==st_crs(point)

voronyi <- st_intersection(st_collection_extract(st_voronoi(do.call(c,point))),border)

# create Voronyi polygons and extract the actual polys from the collection
k_voronyi <- st_voronoi(st_union(kaz_sites), envelope = st_as_sfc(st_bbox(kaz_sites)))
vor_polys <- st_collection_extract(k_voronyi)

# plot
plot(kaz_master, col = "pink")
plot(kaz_sites$geometry, add =T)
plot(st_as_sfc(st_bbox(kaz_sites$geometry)), add =T)
plot(vor_polys, add =T)



# Join Voronyi with site information
vor_poly <- st_join(st_sf(vor_polys), st_sf(kaz_sites))
unique(vor_poly$TRAP_Code)

# Visualise with survey polygons
plot(kaz_sites$geometry)
plot(vor_poly$geometry, col = vor_poly$TRAP_Code, alpha = 0.5, add =T)
plot(st_transform(kaz$geometry, 32635), add =T)

# Intersect Voronyi with survey polygons
vor_units <- st_intersects(st_sf(vor_poly), kaz_35) # list of intersecting units
# to handle lists, I need something alongside: # https://gis.stackexchange.com/questions/375177/in-r-how-do-i-generate-r-random-sample-of-contiguous-polygons


kaz_vor <- kaz_35 %>% 
  st_join(vor_poly, left= TRUE, st_intersects) #actual polygons

kaz_vor
names(kaz_vor)

# Resulting number of polygons inside site Voronyi polygons
list_of_units_in_Vorpolys <- kaz_vor %>% 
  st_drop_geometry() %>% 
  group_by(TRAP_Code) %>% 
  tally() %>% 
  arrange(desc(n))


print(list_of_units_in_Vorpolys, n = 60)
# 250-408 units in the 10 largest Vor polygons!
# over 100 units in the 38 largest Vor polygons


################### Inside-Buffer polygons

# kaz_35 %>%
#   filter(st_intersects(geometry, st_buffer(kaz_sites,500),
#                        sparse = FALSE))
  
buffer500 <- st_buffer(kaz_sites, 500) 

kaz_sitebuf <- kaz_35 %>% 
  st_join(buffer500, st_intersects)
colnames(kaz_sitebuf)

list_sitebuf <- kaz_sitebuf %>% 
  st_drop_geometry() %>% 
  group_by(TRAP_Code) %>% 
  tally() %>% 
  arrange(desc(n))

print(list_sitebuf, n=72)


################### Nearest polygons

nearest <- st_nearest_feature(kaz_sites,st_centroid(kaz_35))

# distance-based k neighbors (no need to touch)
library(spdep)
nearest50 <- knn2nb(knearneigh(st_centroid(kaz_35), k = 50))
nearest_to_site <- st_nearest_feature(kaz_sites, kaz_35)

# subsetting a list of indeces (not entirely successful)
nb <- nearest50[nearest_to_site] # clearly there's overlap

# lets see how many unique unit indeces are there
units <- unique(unlist(nb))

# select the unit polygons by index number
near50polygons <- kaz_35[units,]

# plot the 50 nearest polygons for all sites (minus duplicates)
plot(kaz_sites$geometry);plot(near50polygons$geometry, add=T)

mapview(kaz_sites)+mapview(near50polygons)

#### I need to select the 50 nearest polys within the Voronyi polygons?
# then there will be less overlap
# but it will be an artificial division