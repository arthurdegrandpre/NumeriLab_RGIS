# setup

install.packages("sp")
install.packages("sf")
install.packages("tidyverse")
install.packages("ggmap")
install.packages("leaflet")
install.packages("mapview")
install.packages("raster")
install.packages("leafsync")
install.packages("htmlwidgets")
install.packages("ggsn")
webshot::install_phantomjs()


library("sp")
library("sf")
library("tidyverse")
library("ggmap")
library("leaflet")
library("mapview")
library("raster")
library("leafsync")
library("htmlwidgets")
library("ggsn")


# load data
df = read.csv("./Data/csv/data_picom_HT.csv",sep=";")
head(df)


# make it spatial with sp, or make it spatial with sf
# sp
dfsp = df
coordinates(dfsp) = c("lon","lat")
proj4string(dfsp) = CRS("+init=epsg:4269")
dfsp <- spTransform(dfsp, CRS("+init=epsg:4326"))

dfsp@data

# sf
dfs = df %>% 
  st_as_sf(coords = c("lon","lat")) %>% 
  st_set_crs("epsg:4269") %>% 
  st_transform("epsg:4326")

dfs




# let's plot the data using base R (can be used really fast to see if everything is ok)

plot(dfs)
plot(dfs["tot_drymass"])
plot(dfs["tot_drymass"],
     axes = T,
     key.pos = 1, #(1=below, 2=left, 3=above and 4=right)
     key.width = lcm(1.3),
     key.length = 1.0,
     breaks = "jenks",
     pch = 0
) 





# or use ggplot2

## dfs[,c(4:14,16)] = lapply(df[,c(6:16,18)], as.numeric)

ggplot(dfs)+
  geom_sf(aes(fill=tot_drymass, size=n_trees), shape=21)+
  scale_fill_viridis_c()





# And for actual maps? static maps can be created in ggplot2 with ggmap

# 1. basemap
map = get_stamenmap(bbox=bbox(as_Spatial(dfs))+c(-0.001,-0.001,0.001,0.001),
                    maptype = "terrain",
                    zoom = 15)

plot(map)

# other basemap types from stamen:
#“terrain”, “terrain-background”, “terrain-labels”, “terrain-lines”, “toner”, “toner-2010”, “toner-2011”, “toner-background”, “toner-hybrid”, “toner-labels”, “toner-lines”, “toner-lite”, “watercolor”

map1 = ggmap(map) +
  geom_sf(data = dfs,
          aes(fill = tot_drymass,
              size = n_trees),
          shape = 21,
          inherit.aes=F
  ) +
  labs(
    title = "Estimated forest drymass on UQTR campus, 2016",
    # subtitle = "Subtitle", 
    caption = "Basemap = Stamen Terrain, CRS = EPSG:4326 WGS84, produced 2021-02-02",
    tag = "a)",
    fill = "Tot. Drymass (kg)",
    size = "N. trees",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust=1),
    axis.text.y = element_text(angle = 45, hjust=1),
    plot.caption = element_text(hjust = 0.5),
    plot.title.position = "plot"
  )+
  north(dfs,scale=0.2)+
  scalebar(dfs, dist = 250, dist_unit = "m",
           transform = T, model = "WGS84", st.bottom = TRUE, st.dist = 0.05)

map1

ggsave("./Figures/test01.jpg", plot=map1)




# and now, leaflet for interactive maps

# using the default values, leaflet uses OpenStreetMap
leaflet(dfs) %>%
  addTiles()

leaflet(dfs) %>%
  addProviderTiles("Stamen.Toner")

# adding markers
leaflet(dfs) %>%
  addTiles() %>%
  addMarkers()

leaflet(dfs) %>%
  addTiles() %>%
  addCircleMarkers()



# adding colored markers

pal = colorNumeric(
  palette = "RdYlGn",
  domain = dfs$tot_drymass
)

leaflet(dfs) %>%
  addTiles() %>%
  addCircleMarkers(stroke = T, color = "black", opacity=0.5, fillColor=~pal(tot_drymass), fillOpacity = 0.8, radius=8) %>%
  addLegend("bottomright",
            pal=pal,
            values=~tot_drymass,
            title= "drymass (kg / 0.04 ha)")


qpal = colorQuantile("RdYlGn", dfs$tot_drymass, n = 4)

leaflet(dfs) %>%
  addTiles() %>%
  addCircleMarkers(color = "black", opacity=0.5, fillColor=~qpal(tot_drymass), fillOpacity = 0.5, radius=8) %>%
  addLegend("bottomright",
            pal=qpal,
            values=~tot_drymass,
            title= "Drymass Quantile") %>%
  addScaleBar(position="bottomleft")


# adding variable marker size (and a scalebar!)

leaflet(dfs) %>%
  addTiles() %>%
  addCircles(color = "black", opacity=0.4, fillColor=~pal(tot_drymass), fillOpacity = 0.8, radius=dfs$n_trees) %>%
  addLegend("bottomright",
            pal=pal,
            values=~tot_drymass,
            title= "drymass (kg / 0.04 ha)") %>%
  addScaleBar(position="bottomleft")


# saving your map 

## as static png or html

map2 = leaflet(dfs) %>%
  addTiles() %>%
  addCircles(color = "black", opacity=0.4, fillColor=~pal(tot_drymass), fillOpacity = 0.8, radius=dfs$n_trees) %>%
  addLegend("bottomright",
            pal=pal,
            values=~tot_drymass,
            title= "drymass (kg / 0.04 ha)") %>%
  addScaleBar(position="bottomleft")

mapshot(map2, file= "./Figures/test02.png")
saveWidget(map2, file="./Figures/test02.html", selfcontained = T)


# now that we are good with a single layer of points, let's look into reading other types of vectors and rasters

# polygon 

##adding polygon
campus = st_read("./Data/shapefiles/campus polygon.shp") %>%
  st_transform("epsg:4326")

leaflet(dfs) %>%
  addTiles() %>%
  addCircles(stroke=T, color="black", opacity=0.4, fillColor=~pal(tot_drymass), fillOpacity = 0.8, radius=dfs$n_trees) %>%
  addLegend("bottomright",
            pal=pal,
            values=~tot_drymass,
            title= "drymass (kg / 0.04 ha)") %>%
  addScaleBar(position="bottomleft") %>%
  addPolygons(data=campus, stroke=T, fillOpacity = 0)

# rasters

r = raster("./Data/S2/True color.tiff")
plot(r, main="UQTR on 2018-09-13, Sentinel-2")

# something seems quite wrong with the color, so let's inspect our raster
r

# let's try with grayscale
grayscale_colors = gray.colors(100,
                               start= 0.0,
                               end=1,
                               gamma=2.2,
                               alpha=NULL)
plot(r, main="UQTR on 2018-09-13, red band grayscale, Sentinel-2", col=grayscale_colors)

# let's read other bands
r2=raster("./Data/s2/True color.tiff", band=2)
r2 # this shows we are now using band 2 of 3 instead of 1

# and now all of them
r = brick("./Data/s2/True color.tiff")
r # we do have our 3 layers

# and add a fourth layer with NIR
r=addLayer(r, "./Data/s2/B08.tiff")





# and then visualise them
r=projectRaster(r, crs=("+init=epsg:4326"))
plot(r, col=grayscale_colors, main="UQTR on 2018-09-13, grayscale, Sentinel=2") # but the plot function does not plot them together. To do so, we need to specify each color bands, or use the function plotRGB
hist(r)
dev.off()
plotRGB(r, r=1,g=2,b=3,stretch="hist")

# can also be done in an interactive map, but not in leaflet. leaflet can only display single bands

viewRGB(r, 1,2,3, map=map2, quantiles = c(0.05, 0.95))

# in leaflet, you could include layer control, or use the sync function to look at multiple bands at once
leaflet(dfs) %>% 
  addTiles() %>% 
  addMarkers(group = "markers") %>% 
  addRasterImage(r[[1]], group = "red") %>% 
  addRasterImage(r[[4]], group = "nir") %>% 
addLayersControl(position = "bottomleft",
                 overlayGroups = c("markers","red","nir"))


m1 = leaflet(dfs) %>% 
  addTiles() %>% 
  addRasterImage(r[[1]])
m2 = leaflet(dfs) %>%
  addTiles() %>% 
  addRasterImage(r[[4]])

sync(m1,m2)


# with multiple raster bands, it's easy to make raster maths and obtain spectral indexes, such as ndvi
names(r) = c("r","g","b","n")

r$NDVI=(r$n-r$r)/(r$n+r$r)
plot(r$NDVI)


# and possible to reduce the often heavy raster data to only our area of interest, using our polygon
crop_tr=mask(r,campus)
plot(crop_tr$NDVI)


# and then add it back to leaflet as a meaningfull layer
val = as.numeric(c(0:1))
rpal = colorNumeric(c("red","yellow","green"), val, na.color="transparent")


leaflet(dfs) %>%
  addTiles() %>%
  addCircles(stroke=T, color="black", opacity=0.4, fillColor=~pal(tot_drymass), fillOpacity = 0.8, radius=dfs$n_trees) %>%
  addLegend("bottomright",
            pal=pal,
            values=~tot_drymass,
            title= "drymass (kg / 0.04 ha)") %>%
  addScaleBar(position="bottomleft") %>%
  addPolygons(data=campus, stroke=T, fillOpacity = 0) %>% 
  addRasterImage(crop_tr$NDVI, colors = rpal , opacity = 0.5) %>% 
  addLegend(pal = rpal, values = val, title = "NDVI")


# it is also possible to use our points data to extract informations from the raster, for statistical purposes

mean_ndvi = extract(crop_tr$NDVI, dfs, buffer=10, small=T, fun=mean)

dfs$ndvi=mean_ndvi

plot(dfs$ndvi~dfs$aerial_drymass)
plot(dfs$ndvi~dfs$est_age)
boxplot(dfs$ndvi~dfs$zone_eco)



# and finally, let's put some efforts into a nice map and see what comes out, with added popups on mouseover.

ndvi_val = as.numeric(c(-1:1))
ndvi_pal = colorNumeric("RdYlGn",
                        ndvi_val,
                        na.color = "transparent")

circ_pal = colorNumeric("viridis",
                        dfs$total_drymass)

map = leaflet(dfs) %>% 
  addScaleBar("bottomleft") %>% 
  addTiles(group = "BaseOSM") %>%
  addProviderTiles("Stamen.Toner",
                   group = "BaseStamen") %>% 
  addRasterImage(crop_tr$NDVI,
                 colors = ndvi_pal,
                 opacity = 0.6,
                 group = "NDVI") %>%
  addLegend("bottomright",
            pal = ndvi_pal,
            values = ndvi_val,
            title = "NDVI",
            group = "NDVI") %>% 
  addCircles(color = "black",
             opacity = 0.5,
             fillColor = ~circ_pal(tot_drymass),
             fillOpacity = 0.8,
             radius = dfs$n_trees,
             group = "drymass",
             stroke = F,
             popup = paste("n Trees:", dfs$n_trees, "<br>",
                           "mean DHP:", dfs$mean_dhp)) %>% 
  addLegend("bottomright",
            pal = circ_pal,
            values = ~tot_drymass,
            title = "drymass (kg / 0.04 ha)",
            group ="drymass") %>% 
  addLayersControl("bottomleft",
                   baseGroups = c("BaseOSM","BaseStamen"),
                   overlayGroups = c("NDVI","drymass")
  )
map

saveWidget(map, file="../Figures/map.html", selfcontained = T)