
library(lidR)
library(rgl)
library(raster)
library(sf)
library(RCSF)
library(progress) # For the progress bar

setwd("F:/ARS/Masterarbeit_Theisen/R")
############################


# Mit ForestSens klassifizierte Punktwolke Laden
las<-readLAS("dense_pointcloud/results/highres_XYZ_out.laz")
# Rohe Punktwolke Laden für Tin 
las1<-readLAS("F:/ARS/Masterarbeit_Theisen/R/dense_pointcloud/cloudc9f909ea9b8b927e.las")

# # apply offset for to avoid numerical problems
means <- apply(las@data,2,mean)


etrs_midpoints<-means[1:2]# Locate middele of site and convert etrs coordinates to wgs (just those 2)
middlepoints_utm <- st_sfc(st_point(etrs_midpoints), crs = 25832)# Erstellen eines sf-Objekts mit den Koordinaten und dem entsprechenden CRS
points_wgs84 <- st_transform(middlepoints_utm, crs = 4326)# Transformation zu WGS 84
transformed_coords <- st_coordinates(points_wgs84)# Extrahieren der transformierten Koordinaten
lon <- transformed_coords[1]
lat <- transformed_coords[2]
print(paste("Middle of the site: Longitude:", lon, "Latitude:", lat))

#offset für forestSense Wolke
las@data$X <- las@data$X - means[1]
las@data$Y <- las@data$Y - means[2]
las@data$Z <- las@data$Z - means[3]

# offset für RAW Pointcloud
las1@data$X <- las1@data$X - means[1]
las1@data$Y <- las1@data$Y - means[2]
las1@data$Z <- las1@data$Z - means[3]


#solving offset issus
las2 <- LAS(las@data)
las3 <- LAS(las1@data)
########## Ground Classification ##############################################

# ground classification with Cloth Simulation Function algorythm
las3 <- classify_ground(las3, algorithm = csf())
ground<- filter_poi(las3, las3@data$Classification == 2)

# generates tin resolution adjustable
dtm_tin <- rasterize_terrain(las3, res = 0.2, algorithm = tin())
plot_dtm3d(dtm_tin, bg = "black")
writeLAS(ground,"F:/ARS/Masterarbeit_Theisen/R/dense_pointcloud/lidr_ground.las")
# writing the obj might take somne Time (Tin Export)
writeOBJ("F:/ARS/Masterarbeit_Theisen/R/dense_pointcloud/lidr_ground.obj")

###############################################################################



# Extract the unique list of tree instances
tree_ids <- unique(las2@data$PredInstance)

# Ensure the output directory exists
dir.create("instance_segmented_Trees", showWarnings = FALSE)
dir.create("Wood", showWarnings = FALSE)
dir.create("Leaf", showWarnings = FALSE)

# Initialize progress bar
pb <- progress_bar$new(total = length(tree_ids), format = "[:bar] :percent in :elapsed")

# Loop through each unique tree instance and save as separate LAS files
for (i in tree_ids) {
  las_tree <- filter_poi(las2, PredInstance == i)
  
  # Kombiniere Stämme (2) und Äste (4) in einem Filter
  las_stam_und_branch <- filter_poi(las_tree, PredSemantic %in% c(2, 4))
  
  # Kombiniere niedrige Vegetation (0) und höhere Vegetation (3) in einem Filter
  las_veg_combined <- filter_poi(las_tree, PredSemantic %in% c(0, 3))
  
  # Nur die Datei speichern, wenn die las_tree Punkte enthält
  if (npoints(las_tree) > 0) {
    writeLAS(las_tree, paste0("instance_segmented_Trees/Tree_", i, ".las"))
  }
  
  # Falls gewünscht: Separates Schreiben der kombinierten Klassen
  if (npoints(las_stam_und_branch) > 0) {
    writeLAS(las_stam_und_branch, paste0("Wood/Tree_", i, "_stam_branch.las"))
  }
  
  if (npoints(las_veg_combined) > 0) {
    writeLAS(las_veg_combined, paste0("Leaf/Tree_", i, "_veg_combined.las"))
  }
  
  pb$tick() # Fortschrittsanzeige aktualisieren
}

