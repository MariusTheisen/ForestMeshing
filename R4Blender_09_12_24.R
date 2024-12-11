library(lidR)
library(rgl)
library(raster)
library(sf)
library(RCSF)
library(progress) # For the progress bar

setwd("D:/Marius/R")

############################


# Mit ForestSens klassifizierte Punktwolke Laden
las<-readLAS("D:/Marius/results/11_14_L2_963pts_clip_out.laz")
# Rohe Punktwolke Laden für Tin 
las1<-readLAS("D:/Marius/2024_11_14_L2_MID015002.las")

# # apply offset for to avoid numerical problems
means <- apply(las@data,2,mean)
#minimum <- apply(las1@data,2,min)


etrs_midpoints<-means[1:2]# Locate middele of site and convert etrs coordinates to wgs (just those 2)
middlepoints_utm <- st_sfc(st_point(etrs_midpoints), crs = 25832)# Erstellen eines sf-Objekts mit den Koordinaten und dem entsprechenden CRS
points_wgs84 <- st_transform(middlepoints_utm, crs = 4326)# Transformation zu WGS 84
transformed_coords <- st_coordinates(points_wgs84)# Extrahieren der transformierten Koordinaten
lon <- transformed_coords[1]
lat <- transformed_coords[2]
print(paste("Middle of the site: Longitude:", lon, "Latitude:", lat))

#offset für forestSense Wolke Mitte entspricht Nullpunkt
las@data$X <- las@data$X - means[1]
las@data$Y <- las@data$Y - means[2]
las@data$Z <- las@data$Z - means[3]

# offset für RAW Pointcloud
las1@data$X <- las1@data$X - means[1]
las1@data$Y <- las1@data$Y - means[2]
las1@data$Z <- las1@data$Z - means[3]

#offset für asschließlich positive Werte
# las@data$X <- las@data$X - minimum[1]
# las@data$Y <- las@data$Y - minimum[2]
# las@data$Z <- las@data$Z - minimum[3]
# 
# las1@data$X <- las1@data$X - minimum[1]
# las1@data$Y <- las1@data$Y - minimum[2]
# las1@data$Z <- las1@data$Z - minimum[3]

#solving offset issus
las <- LAS(las@data)
las1 <- LAS(las1@data)
########## Ground Classification ##############################################

# ground classification with Cloth Simulation Function algorythm
las1 <- classify_ground(las1, algorithm = csf())
ground<- filter_poi(las1, las1@data$Classification == 2)


# generates tin resolution adjustable
dtm_tin <- rasterize_terrain(ground, res = 0.3, algorithm = tin())


plot_dtm3d(dtm_tin, bg = "black")
#writeLAS(ground,"F:/ARS/Masterarbeit_Theisen/R/dense_pointcloud/lidr_ground_Positiv_Numbers.las")
# writing the obj might take somne Time (Tin Export)
#writeOBJ("F:/ARS/Masterarbeit_Theisen/R/dense_pointcloud/lidr_ground_PositiveNumbers.obj")
#raster schreiben da write obj funktion des rgl pakets nicht funktioniert. Tif wird später in python zu ply file convertiert für blender import.
terra::writeRaster(dtm_tin, "output/lidr_ground_963.tif", overwrite=TRUE)
rm(las1)
##############################################################################

# Extract the unique list of tree instances
tree_ids <- unique(las@data$PredInstance)

# Ensure the output directory exists
dir.create("Branch_middle", showWarnings = FALSE)
dir.create("Wood_middle", showWarnings = FALSE)
dir.create("Leaf_middle", showWarnings = FALSE)

# Initialize progress bar
pb <- progress_bar$new(total = length(tree_ids), format = "[:bar] :percent in :elapsed")

# Setzen ungültiger ReturnNumber- und NumberOfReturns-Werte der ForestSense Klassifizierung auf 1

las@data$NumberOfReturns[las@data$NumberOfReturns == 0] <- 1

# Korrigieren von abnormen Werten in ReturnNumber
las@data$ReturnNumber[las@data$ReturnNumber > 15 | las@data$ReturnNumber < 1] <- 1
las@data$ScanDirectionFlag <- as.integer(1)


# Falls ReturnNumber nicht als Ganzzahl vorliegt, explizit umwandeln
las@data$ReturnNumber <- as.integer(las@data$ReturnNumber)
las@data$NumberOfReturns <- as.integer(las@data$NumberOfReturns)



# Loop through each unique tree instance and save as separate LAS files
# Loop through each unique tree instance and save as separate LAS files
# Startzeit erfassen
start_time <- Sys.time()
for (i in tree_ids) {
  las_tree <- filter_poi(las, PredInstance == i)
  las_tree <- LAS(las_tree@data)
  # Kombiniere Stämme (2) und Äste (4) in einem Filter
  #las_stam_und_branch <- filter_poi(las_tree, PredSemantic %in% c(2, 4))
  las_stam <- filter_poi(las_tree, PredSemantic == 2)
  las_stam <- LAS(las_stam@data)
  
  las_branch<- filter_poi(las_tree, PredSemantic == 4)
  las_branch <- LAS(las_branch@data)
  # Kombiniere niedrige Vegetation (0) und höhere Vegetation (3) in einem Filter
  las_veg_combined <- filter_poi(las_tree, PredSemantic == 3)
  las_veg_combined <- LAS(las_veg_combined@data)
  # Nur die Datei speichern, wenn die las_tree Punkte enthält
  #if (npoints(las_tree) > 0) {
  #  writeLAS(las_tree, paste0("instance_segmented_Trees_middle_of_scene/Tree_", i, ".las"))
  #}
  # Separates Schreiben der  Klassen
  if (npoints(las_stam) > 0) {
    writeLAS(las_stam, paste0("Wood_middle//Tree_", i, "_stam.las"))
  }
  
  if (npoints(las_veg_combined) > 0) {
    writeLAS(las_veg_combined, paste0("Leaf_middle//Tree_", i, "_veg_combined.las"))
  }
  
  if (npoints(las_branch) > 0) {
      writeLAS(las_branch, paste0("Branch_middle//Tree_", i, "_branch.las"))
  }
  
  pb$tick() # Fortschrittsanzeige aktualisieren
}

end_time <- Sys.time()

# Dauer in Sekunden berechnen
duration_secs <- as.numeric(difftime(end_time, start_time, units = "secs"))

# Dynamische Ausgabe der Dauer
if (duration_secs < 60) {
  cat("Dauer:", round(duration_secs, 2), "Sekunden")
} else if (duration_secs < 3600) {
  duration_mins <- duration_secs / 60
  cat("Dauer:", round(duration_mins, 2), "Minuten")
} else {
  duration_hours <- duration_secs / 3600
  cat("Dauer:", round(duration_hours, 2), "Stunden")
}



# Überprüfen der Werte des ReturnNumber-Attributs
#if (!is.null(las@data$ReturnNumber)) {
#  print(table(las@data$ReturnNumber))  # Gibt die Verteilung der ReturnNumber-Werte aus
#} else {
#  print("ReturnNumber-Attribut fehlt in den Daten.")
#}
