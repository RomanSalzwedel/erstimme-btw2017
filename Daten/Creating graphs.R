### Creating graphs
### a.k.a. Visualisazion

library(R2HTML)
library(shapefiles)
library(rgdal)
library(spdep)
library(maptools)
library(rgeos)
library(ggplot2)
library(plyr)




# Reading in shapefile from Bundeswahlleiter
shape <- readOGR(dsn="btw17_geometrie_wahlkreise_shp", layer="Geometrie_Wahlkreise_19DBT", stringsAsFactors = FALSE)

plot(shape)

#######
# WKR_NR = 18 (Hamburg) contains islands distorting the visualization
# I create a new shape (old shape: shape_old) without the island polygons

shape_old <- shape
hhs18 <- slot(shape, "polygons")[[18]]
hh18 <- slot(hhs18, "Polygons")
length(hh18)
sapply(hh18, function(x) {slot(x, "area")[1]})

res <- hh18[sapply(hh18, function(x) {slot(x, "area")[1]}) > 2957780]

slot(hhs18, "Polygons") <- res
slot(shape, "polygons")[[18]] <- hhs18
#######


# Assigning random colors to Wahlkreise
colors <- c("red", "black", "green", "purple", "yellow")

png(filename = "map_germany.png", width = 800, height = 1000, bg = "#18bc9c")
plot(shape, col = "lightgrey", lwd = 0.5)
for (i in 1:299){
  plot(shape[shape$WKR_NR == i, ], col = sample(colors,1), add = TRUE, lwd = .5)
}
dev.off()


# Exporting one file each for each Bundesland (small and large)
if(!dir.exists("Bundeslaender_small")) dir.create("Bundeslaender_small")
for (k in unique(shape$LAND_NAME)){
  if(k!="Thüringen" & k!="Baden-Württemberg") png(filename = paste0("Bundeslaender_small/", k, ".png"), width = 900, height = 650)
  if(k=="Thüringen") png(filename = paste0("Bundeslaender_small/", "Thueringen", ".png"), width = 900, height = 650)
  if(k=="Baden-Württemberg") png(filename = paste0("Bundeslaender_small/", "Baden-Wuerttemberg", ".png"), width = 900, height = 650) 
  sel <- shape$LAND_NAME == k
  plot(shape[sel, ], col = "lightgrey", lwd = 0.5)
  for (i in shape$WKR_NR[sel]){
    plot(shape[shape$WKR_NR == i, ], col = sample(colors,1), add = TRUE, lwd = .5)
  }
  dev.off()
}


if(!dir.exists("Bundeslaender_large")) dir.create("Bundeslaender_large")
for (k in unique(shape$LAND_NAME)){
  if(k!="Thüringen" & k!="Baden-Württemberg") png(filename = paste0("Bundeslaender_large/", k, ".png"), width = 1800, height = 1300)
  if(k=="Thüringen") png(filename = paste0("Bundeslaender_large/", "Thueringen", ".png"), width = 1800, height = 1300)
  if(k=="Baden-Württemberg") png(filename = paste0("Bundeslaender_large/", "Baden-Wuerttemberg", ".png"), width = 1800, height = 1300)   
  sel <- shape$LAND_NAME == k
  plot(shape[sel, ], col = "lightgrey", lwd = 0.5)
  for (i in shape$WKR_NR[sel]){
    plot(shape[shape$WKR_NR == i, ], col = sample(colors,1), add = TRUE, lwd = .5)
  }
  dev.off()
}