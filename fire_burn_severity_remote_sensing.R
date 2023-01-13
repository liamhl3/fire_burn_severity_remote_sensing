library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(plotly)
library(corrgram)
library(corrplot)
library(sp)
library(sf)
library(leaflet)
library(tidyverse)
library(stringr)
library(here)
library(widgetframe)
library(htmltools)
library(htmlwidgets)
library(tmap)
library(caret)
library(ggpubr)
library(car)
library(PerformanceAnalytics)
library(ggcorrplot)
library(naniar)
library(grid)
library(mice)
library(egg)
library(shiny)
library(lubridate)
library(devtools)
library(caTools)
library(rgdal)
library(raster)
#install.packages("RColorBrewer")
#install.packages("rgeos")
#install.packages("RCurl")
library(RColorBrewer)
library(rgeos)
library(RCurl)


##############################################
#functions
ndvi <- function(img, k, i) {
  bk <- img[[k]]
  bi <- img[[i]]
  vi <- (bk - bi) / (bk + bi)
  return(vi)
}

nbr <- function(img, k, i) {
  bk <- img[[k]]
  bi <- img[[i]]
  vi <- (bk - bi) / (bk + bi)
  return(vi)
}

##############################################
# ndvi analysis
ndvi <- vi(landsat, 5, 4)




##############################################
# Example Data Post

all_landsat_bands_post <- list.files("landsat_ex/LC80340322016205-SC20170127160728/crop",
                                     pattern = glob2rx("*band*.tif$"),
                                full.names = TRUE)
landsat_stack_csf_post <- stack(all_landsat_bands_post)
landsat_csf_br_post <- brick(landsat_stack_csf_post)
landsat_csf_br_post

plot(landsat_csf_br_post,
     col = gray(20:100 / 100))

par(col.axis = "white", col.lab = "white", tck = 0)
plotRGB(landsat_csf_br_post,
        r = 5, g = 4, b = 3,
        stretch = "lin",
        axes = TRUE,
        main = "Color infrared composite image\n Landsat Bands 5, 4, 3\n Post Fire Data")
box(col = "white")

##############################################
# Example Data Pre

all_landsat_bands_pre <- list.files("landsat_ex/LC80340322016189-SC20170128091153/crop",
                                     pattern = glob2rx("*band*.tif$"),
                                     full.names = TRUE)
landsat_stack_csf_pre <- stack(all_landsat_bands_pre)
landsat_csf_br_pre <- brick(landsat_stack_csf_pre)
landsat_csf_br_pre

plot(landsat_csf_br_pre,
     col = gray(20:100 / 100))

par(col.axis = "white", col.lab = "white", tck = 0)
plotRGB(landsat_csf_br_pre,
        r = 5, g = 4, b = 3,
        stretch = "lin",
        axes = TRUE,
        main = "Color infrared composite image\n Landsat Bands 5, 4, 3\n Pre Fire Data")
box(col = "white")

##############################################
# Example dnbr Analysis

nbr.post <- nbr(landsat_csf_br_post, 5, 7)
plot(nbr.post,
     main = "Landsat-derived NBR\n Pre-Fire \n with fire boundary overlay",
     axes = FALSE,
     box = FALSE)
summary(nbr.post)

nbr.pre <- nbr(landsat_csf_br_pre, 5, 7)
plot(nbr.pre,
     main = "Landsat-derived NBR\n Pre-Fire \n with fire boundary overlay",
     axes = FALSE,
     box = FALSE)

diff.nbr <- nbr.pre - nbr.post
plot(diff.nbr)

##############################################
# Example nbr classfication

hist(nbr.post,
     main = "Distribution of raster cell values in the DTM difference data",
     xlab = "Burn Severity", ylab = "Number of Pixels",
     col = "springgreen")

reclass.df <- c(-1, -0.1, 1,
                -0.1, 0.1, 2,
                0.1, 0.27, 3,
                0.27, 0.66, 4,
                0.66, 1, 5)
reclass.df

reclass.m <- matrix(reclass.df,
                    ncol = 3,
                    byrow = TRUE)
reclass.m

chm.classified <- reclassify(nbr.post,
                             reclass.m)

barplot(chm.classified,
        main = "Number of pixels in each class")

plot(chm.classified,
     col = c("red", "yellow", "green", "blue", "purple"))



##############################################
#Example dnbr classification

dnbr.classified <- reclassify(diff.nbr,
                             reclass.m)

barplot(dnbr.classified,
        main = "Number of pixels in each class")

plot(dnbr.classified,
     col = c("red", "yellow", "green", "blue", "purple"))


##############################################
#Example RdNBR Calculation

nbr.pre1 <- nbr.pre*1000
nbr.post1 <- nbr.post*1000


dnbr <- (nbr.pre - nbr.post)

rdnbr <- dnbr / (sqrt(abs(nbr.pre1/1000)))

rdnbr1 <- dnbr / abs(nbr.pre)^0.5

hist(rdnbr1)
plot(rdnbr1)

##############################################
#Example RBR Calculation

rbr <- diff.nbr / (nbr.pre + 1.001)

hist(rbr)
plot(rbr)


##############################################
# Example ndvi difference Analysis

ndvi.post <- ndvi(landsat_csf_br_post, 5, 4)
plot(ndvi.post,
     main = "Landsat-derived NDVI\n Pre-Fire \n with fire boundary overlay",
     axes = FALSE,
     box = FALSE)

ndvi.pre <- ndvi(landsat_csf_br_pre, 5, 4)
plot(ndvi.pre,
     main = "Landsat-derived NBR\n Pre-Fire \n with fire boundary overlay",
     axes = FALSE,
     box = FALSE)

diff_ndvi <- ndvi.pre - ndvi.post
plot(diff_ndvi)



##############################################
# Example Map


map <- leaflet() %>%
  addTiles()%>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
  addProviderTiles(providers$Esri.DeLorme, group = "Street Map") %>%
  addRasterImage(nbr.pre, group = "NBR Pre")%>%
  addRasterImage(nbr.post, group = "NBR Post")%>%
  addRasterImage(diff.nbr, group = "dNBR")%>%
  addRasterImage(chm.classified, group = "class")%>%
  addLayersControl(
    position = "topright",
    baseGroups = c("Imagery", "Steet Map"),
    overlayGroups = c("NBR Pre", "NBR Post", "dNBR", "class"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("NBR Post", "dNBR", "class"))
map
































############################################################################################
######### Paradise Fire File Upload

list.files("Paradise_Pre")
all_paradise_bands_pre <- list.files("Paradise_Pre",
                                     pattern = glob2rx("*BAND*.TIF$"),
                                     full.names = TRUE)

paradise_stack_pre <- stack(all_paradise_bands_pre)


list.files("Paradise_Post")
all_paradise_bands_post <- list.files("Paradise_Post",
                                      pattern = glob2rx("*SR_B*.TIF$"),
                                      full.names = TRUE)

paradise_stack_post <- stack(all_paradise_bands_post)

############################################################################################
######### Insert Paradise Fire Crop

paradise.boundary <- st_read("GIS_Data/Paradise_3.shp")
paradise.boundary.project <- st_transform(paradise.boundary, "+proj=utm +zone=10 +datum=WGS84 +units=m +no-defs")
crs(paradise.boundary.project)

############################################################################################
######### Paradise Fire Crop

paradise.pre.crop <- crop(paradise_stack_pre, paradise.boundary.project)
plot(paradise.pre.crop, main = "Cropped Pre-Paradise")

paradise.post.crop <- crop(paradise_stack_post, paradise.boundary.project)
plot(paradise.post.crop, main = "Cropped Post-Paradise")


############################################################################################
######### Paradise Fire Data Examine

plot(paradise.pre.crop,
     col = gray(20:100 / 100))

par(col.axis = "white", col.lab = "white", tck = 0)
plotRGB(paradise.pre.crop,
        r = 5, g = 4, b = 3,
        stretch = "lin",
        axes = TRUE,
        main = "Color infrared composite image\n Landsat Bands 5, 4, 3\n Pre Fire Data")
box(col = "white")



plot(paradise.post.crop,
     col = gray(20:100 / 100))

par(col.axis = "white", col.lab = "white", tck = 0)
plotRGB(paradise.post.crop,
        r = 5, g = 4, b = 3,
        stretch = "lin",
        axes = TRUE,
        main = "Color infrared composite image\n Landsat Bands 5, 4, 3\n Post Fire Data")
box(col = "white")


############################################################################################
######### Paradise Fire NBR Analysis


nbr.paradise.pre <- nbr(paradise.pre.crop, 5, 7)
plot(nbr.paradise.pre,
     main = "Landsat-derived NBR\n Pre-Fire \n with fire boundary overlay",
     axes = FALSE,
     box = FALSE)
summary(nbr.paradise.pre)


nbr.paradise.post <- nbr(paradise.post.crop, 5, 7)
plot(nbr.paradise.post,
     main = "Landsat-derived NBR\n Post-Fire \n with fire boundary overlay",
     axes = FALSE,
     box = FALSE)


############################################################################################
######### Paradise Fire dNBR Analysis

diff.nbr.paradise <- nbr.paradise.pre - nbr.paradise.post

plot(diff.nbr.paradise)
summary(diff.nbr.paradise)


############################################################################################
######### Paradise USGS dNBR Classification

#scale dNBR
dNBR_scaled_paradise <- 1000*diff.nbr.paradise
summary(dNBR_scaled_paradise)


dNBR_ranges <- c(-251, -101, 1, -101, 99, 2, 99, 269, 3, 
                 269, 439, 4, 439, 659, 5) 

class.matrix <- matrix(dNBR_ranges,
                       ncol = 3,
                       byrow = TRUE)
class.matrix

dnbr_reclass_paradise <- reclassify(dNBR_scaled_paradise,
                                  class.matrix,
                                  right=NA)

#turn raster into categorical
paradise_reclass <- ratify(dnbr_reclass_paradise)
rat <- levels(paradise_reclass)[[1]] 


rat$legend  <- c("Enhanced Regrowth", "Unburned", "Low Severity", 
                 "Moderate-low Severity", "Moderate-high Severity") 
levels(paradise_reclass) <- rat 


my_col <- c("darkolivegreen","limegreen", "yellow2", "orange2", "red")


#plot
plot(paradise_reclass,col=my_col,legend=F,box=F,axes=F, main="Burn Severity \n Paradise, California")
legend(x='right', legend =rat$legend, fill = my_col, y='right') 

############################################################################################
######### Paradise USGS dNBR Graphing




############################################################################################
######### Viedma et al Caldor dNBR Classification



############################################################################################
######### Viedma et al Caldor dNBR Graphing




############################################################################################
######### Paradise RBR Calculation

rbr.denominator.paradise <- nbr.paradise.pre +1.001
rbr.paradise <- diff.nbr.paradise/rbr.denominator.paradise


hist(rbr.paradise)
plot(rbr.paradise)
summary(rbr.paradise)

############################################################################################
######### Viedma et al Paradise RBR Classification




############################################################################################
######### Viedma et al Paradise RBR Graphing




############################################################################################
######### Paradise Leaflet

pal.nbr <- colorRampPalette(brewer.pal(11, "RdYlGn"))(100)
pal7    <- colorNumeric(c("#8ac926", "#ef233c", "#FFFFCC"), values(diff.nbr.paradise),
                     na.color = "transparent")
pal8    <- colorNumeric(c("#8ac926", "#ef233c", "#FFFFCC"), values(rbr.paradise),
                     na.color = "transparent")
pal.dNBR.USGS.paradise <- colorFactor(c("#386641", "#aacc00", "#FEE440", "#FF6D00", "#FF206E"),
                              values(paradise_reclass),
                              na.color = "transparent")



paradise.map <- leaflet() %>%
  addTiles()%>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
  addProviderTiles(providers$Esri.DeLorme, group = "Street Map") %>%
  addRasterImage(nbr.paradise.pre, group = "Pre-Fire NBR", 
                 colors = pal.nbr)%>%
  addRasterImage(nbr.paradise.post, group = "Post-Fire NBR", 
                 colors = pal.nbr)%>%
  addRasterImage(diff.nbr.paradise, group = "dNBR", 
                 colors = pal7)%>%
  addRasterImage(rbr.paradise, group = "RBR", 
                 colors = pal8)%>%
  addRasterImage(paradise_reclass, group = "USGS dNBR Classification", 
                 colors = pal.dNBR.USGS.paradise)%>%
  addLayersControl(
    position = "topright",
    baseGroups = c("Imagery", "Steet Map"),
    overlayGroups = c("Pre-Fire NBR","Post-Fire NBR", "dNBR", "RBR", "USGS dNBR Classification"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("Post-Fire NBR", "dNBR", "RBR", "USGS dNBR Classification"))%>%
addMeasure()
paradise.map



























############################################################################################
######### Caldor Fire File Upload

list.files("Caldor_Pre")
all_caldor_bands_pre <- list.files("Caldor_Pre",
                                     pattern = glob2rx("*SR_B*.TIF$"),
                                     full.names = TRUE)

caldor_stack_pre <- stack(all_caldor_bands_pre)


list.files("Caldor_Post")
all_caldor_bands_post <- list.files("Caldor_Post",
                                      pattern = glob2rx("*SR_B*.TIF$"),
                                      full.names = TRUE)

caldor_stack_post <- stack(all_caldor_bands_post)

crs(caldor_stack_post)
############################################################################################
######### Insert Caldor Fire Crop

caldor.boundary <- st_read("GIS_Data/Caldor.shp")
caldor.boundary.project <- st_transform(caldor.boundary, "+proj=utm +zone=10 +datum=WGS84 +units=m +no-defs")
crs(caldor.boundary.project)

############################################################################################
######### Paradise Fire Crop

caldor.pre.crop <- crop(caldor_stack_pre, caldor.boundary.project)
plot(caldor.pre.crop, main = "Cropped Pre-Caldor")

caldor.post.crop <- crop(caldor_stack_post, caldor.boundary.project)
plot(caldor.post.crop, main = "Cropped Post-Caldor")


############################################################################################
######### Caldor Fire Data Examine

plot(caldor.pre.crop,
     col = gray(20:100 / 100))

par(col.axis = "white", col.lab = "white", tck = 0)
plotRGB(caldor.pre.crop,
        r = 5, g = 4, b = 3,
        stretch = "lin",
        axes = TRUE,
        main = "Color infrared composite image\n Landsat Bands 5, 4, 3\n Pre Fire Data")
box(col = "white")



plot(caldor.post.crop,
     col = gray(20:100 / 100))

par(col.axis = "white", col.lab = "white", tck = 0)
plotRGB(caldor.post.crop,
        r = 5, g = 4, b = 3,
        stretch = "lin",
        axes = TRUE,
        main = "Color infrared composite image\n Landsat Bands 5, 4, 3\n Post Fire Data")
box(col = "white")


############################################################################################
######### Caldor Fire NBR Analysis


nbr.caldor.pre <- nbr(caldor.pre.crop, 5, 7)
plot(nbr.caldor.pre,
     main = "Landsat-derived NBR\n Pre-Fire \n with fire boundary overlay",
     axes = FALSE,
     box = FALSE)
summary(nbr.caldor.pre)


nbr.caldor.post <- nbr(caldor.post.crop, 5, 7)
plot(nbr.caldor.post,
     main = "Landsat-derived NBR\n Post-Fire \n with fire boundary overlay",
     axes = FALSE,
     box = FALSE)
summary(nbr.caldor.post)

############################################################################################
######### Caldor Fire dNBR Analysis

diff.nbr.caldor <- nbr.caldor.pre - nbr.caldor.post

plot(diff.nbr.caldor)

summary(diff.nbr.caldor)


############################################################################################
######### Caldor USGS dNBR Classification

#scale dNBR
dNBR_scaled_caldor <- 1000*diff.nbr.caldor
summary(dNBR_scaled_caldor)


dNBR_ranges <- c(-251, -101, 1, -101, 99, 2, 99, 269, 3, 
                269, 439, 4, 439, 659, 5) 

class.matrix <- matrix(dNBR_ranges,
                       ncol = 3,
                      byrow = TRUE)
class.matrix

dnbr_reclass_caldor <- reclassify(dNBR_scaled_caldor,
                              class.matrix,
                                right=NA)

#turn raster into categorical
caldor_reclass <- ratify(dnbr_reclass_caldor)
rat <- levels(caldor_reclass)[[1]] 


rat$legend  <- c("Enhanced Regrowth", "Unburned", "Low Severity", 
                 "Moderate-low Severity", "Moderate-high Severity") 
levels(caldor_reclass) <- rat 


my_col <- c("#386641","#aacc00", "#FEE440", "#FF6D00", "#FF206E")


#plot
plot(caldor_reclass,col=my_col,legend=F,box=F,axes=F, main="Burn Severity \n South Lake Tahoe, California")
legend(x='right', legend =rat$legend, fill = my_col, y='right') 


############################################################################################
######### Caldor USGS dNBR Graphing

#barplot comp
barplot(caldor_reclass,
        main = "Distribution of Classified dNBR Values",
        col = my_col,
        names.arg = c("A", "B", "C", "D", "E"), 
        las=0)

legend(x='right', y='top', legend =rat$legend, fill = my_col)





#barplot pixels
vals.caldor <- getValues(caldor_reclass)

pix_1.caldor <- length(subset(vals.caldor, vals.caldor == 1))
pix_2.caldor <- length(subset(vals.caldor, vals.caldor == 2))
pix_3.caldor <- length(subset(vals.caldor, vals.caldor == 3))
pix_4.caldor <- length(subset(vals.caldor, vals.caldor == 4))
pix_5.caldor <- length(subset(vals.caldor, vals.caldor == 5))


pix.values.caldor <- c(pix_1.caldor,pix_2.caldor,pix_3.caldor,pix_4.caldor,pix_5.caldor)

bp.caldor <- barplot(caldor_reclass,
              main = "Number of pixels by dNBR category",
              col = my_col,
              names.arg = c("A", "B", "C", "D", "E"), 
              las=0)

legend(x='right', y='top', legend =rat$legend, fill = my_col)

text(bp.caldor, y=2500, labels=pix.values.caldor) #y chosen manually




#pixel percentage

pix_total.caldor <- length(vals.caldor)


area_1_perc.caldor <- round(pix_1.caldor/pix_total.caldor*100, digits = 5)
area_2_perc.caldor <- round(pix_2.caldor/pix_total.caldor*100, digits = 5)
area_3_perc.caldor <- round(pix_3.caldor/pix_total.caldor*100, digits = 5)
area_4_perc.caldor <- round(pix_4.caldor/pix_total.caldor*100, digits = 5)
area_5_perc.caldor <- round(pix_5.caldor/pix_total.caldor*100, digits = 5)


area_perc_covered.caldor <- c(area_1_perc.caldor,area_2_perc.caldor,area_3_perc.caldor,area_4_perc.caldor,area_5_perc.caldor)

bp2 <- barplot(caldor_reclass,
              main = "Percentage of every dNBR category",
              col = my_col,
              names.arg = c("A", "B", "C", "D", "E"), 
              las=0)

legend(x='right', y='top', legend =rat$legend, fill = my_col)

text(bp2, y=2500, labels=area_perc_covered.caldor) #y chosen manually




#pixel percentage

ex.caldor <- extent(caldor_reclass)
# calculate extent of the raster in square kilometers
x.caldor <- (ex.caldor[2]-ex.caldor[1])/1000
y.caldor <- (ex.caldor[4]-ex.caldor[3])/1000
area_total.caldor <- x.caldor*y.caldor


area_1.caldor <- round((area_1_perc.caldor/100)*area_total.caldor, digits = 5)
area_2.caldor <- round((area_2_perc.caldor/100)*area_total.caldor, digits = 5)
area_3.caldor <- round((area_3_perc.caldor/100)*area_total.caldor, digits = 5)
area_4.caldor <- round((area_4_perc.caldor/100)*area_total.caldor, digits = 5)
area_5.caldor <- round((area_5_perc.caldor/100)*area_total.caldor, digits = 5)


area_covered.caldor <- c(area_1.caldor,area_2.caldor,area_3.caldor,area_4.caldor,area_5.caldor)

bp3 <- barplot(caldor_reclass,
              main = "Area by dNBR category",
              col = my_col,
              names.arg = c("A", "B", "C", "D", "E"), 
              las=0)

legend(x='right', y='top', legend =rat$legend, fill = my_col)

text(bp3, y=2500, labels=area_covered.caldor) #y chosen manually

area_covered.caldor

#need to change for new calculations
my_nested_list <- list(area_covered=list(  0.0153, 15.0831, 25.4448, 30.6585,  0.9369),
                       burn_severity=list('Enhanced Regrowth','Unburned','Low Severity','Moderate-low Severity','Moderate-high Severity'),
                       type=list('a','b','c','d','e'))
df <- as.data.frame(do.call(cbind, my_nested_list))

df$area_covered <- as.numeric(df$area_covered)
df$burn_severity <- as.character(df$burn_severity)
df$type <- as.character(df$type)

level_order <- c('Enhanced Regrowth','Unburned','Low Severity','Moderate-low Severity','Moderate-high Severity')

gg.test <- ggplot(df, aes(x=burn_severity, y=area_covered, fill = my_col))+
  geom_bar(stat = "identity", color="black", position = position_dodge())+
  geom_text(aes(label=area_covered), position=position_dodge(width=0.9), vjust=-0.25)+
  scale_x_discrete(limits = level_order)+
  scale_fill_manual(values = c('#386641','#aacc00', '#FEE440', '#FF206E', '#FF6D00'))+
  ggtitle("Caldor Fire Area by USGS dNBR Classfication System")+
  xlab("Burn Severity Category")+
  ylab("Area (km^2)")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = 'none')
gg.test

############################################################################################
######### Viedma et al Caldor dNBR Classification




############################################################################################
######### Viedma et al Caldor dNBR Graphing



############################################################################################
######### Caldor RBR Calculation

rbr.denominator.caldor <- nbr.caldor.pre +1.001
rbr.caldor <- diff.nbr.caldor/rbr.denominator.caldor


hist(rbr.caldor)
plot(rbr.caldor)
summary(rbr.caldor)

############################################################################################
######### Viedma et al Caldor RBR Classification




############################################################################################
######### Viedma et al Caldor RBR Graphing


############################################################################################
######### Caldor Leaflet

pal.nbr <- colorRampPalette(brewer.pal(11, "RdYlGn"))(100)
pal3    <- colorNumeric(c("#8ac926", "#ef233c", "#FFFFCC"), values(diff.nbr.caldor),
                     na.color = "transparent")
pal4    <- colorNumeric(c("#8ac926", "#ef233c", "#FFFFCC"), values(rbr.caldor),
                     na.color = "transparent")
pal.dNBR.USGS.caldor <- colorFactor(c("#386641", "#aacc00", "#FEE440", "#FF6D00", "#FF206E"),
                                      values(caldor_reclass),
                                      na.color = "transparent")


caldor.map <- leaflet() %>%
  addTiles()%>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
  addProviderTiles(providers$Esri.DeLorme, group = "Street Map") %>%
  addRasterImage(nbr.caldor.pre, group = "Pre-Fire NBR", 
                 colors = pal.nbr)%>%
  addRasterImage(nbr.caldor.post, group = "Post-Fire NBR", 
                 colors = pal.nbr)%>%
  addRasterImage(diff.nbr.caldor, group = "dNBR", 
                 colors = pal3)%>%
  addRasterImage(rbr.caldor, group = "RBR", 
                 colors = pal4)%>%
  addRasterImage(caldor_reclass, group = "USGS dNBR Classification",
                 colors = pal.dNBR.USGS.caldor)%>%
  addLayersControl(
    position = "topright",
    baseGroups = c("Imagery", "Steet Map"),
    overlayGroups = c("Pre-Fire NBR","Post-Fire NBR", "dNBR", "RBR", "USGS dNBR Classification"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("Post-Fire NBR", "dNBR", "RBR", "USGS dNBR Classification"))%>%
  addMeasure()
caldor.map
