library(readr)
library(ggplot2)
library(dplyr)
library(rgeos)
library(maptools)
library(ggmap)
library(broom)
library(mapproj)
library(leaflet)
library(plotly)
library(geojsonR)
library(geojsonio)
library(sp)
#Reading the data
epi2020 <- read_csv("~/Documents/Data Vis/A3/epi2020.csv")
epi<- select(epi2020, country, EPI.new, EPI.rnk.new)
names(epi)[names(epi) == 'country'] <- 'name'

#Reading JSON file
world <- rgdal::readOGR("~/Documents/Data Vis/A3/countries.geo.json")

#Merginf .csv and .json file
epi_world <- sp::merge(world, epi, by = "name")

#Creating bins for EPI
bins <- quantile(epi$EPI.new, probs = seq(0,1,.2), names = FALSE, na.rm = TRUE)

#Defining color palette
pal <- colorBin(
  "Greens",
  domain = epi$EPI.new, 
  bins = bins
)

#Defining labels
labels <- sprintf(
  "<strong>%s</strong><br/> EPI(2020): %g<br/> Rank: %g",
  epi_world$name,
  epi_world$EPI.new,
  epi_world$EPI.rnk.new
) %>% lapply(htmltools::HTML)

#Plotting spatial data using leaflet
p1 <- leaflet(epi_world) %>% addPolygons(
  fillColor = ~pal(epi_world$EPI.new),
  weight = 0.7,
  opacity = 1,
  color = 'white',
  dashArray = '3',
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 3,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "12px",
    direction = "auto"))%>%
  addLegend(pal = pal, 
            values = ~EPI.new, 
            opacity = 0.7, title = "EPI Score",
            position = "bottomright")

p1

epi_change <- select(epi2020, country, EPI.new, EPI.rnk.new, EPI.change)
epi_change <- mutate(epi_change, EPI.old = EPI.new - EPI.change)
epi_change <- mutate(epi_change, change = EPI.change/EPI.old *100)
epi_change$change %>% round()

names(epi_change)[names(epi_change) == 'country'] <- 'name'
epi_world2 <- sp::merge(world, epi_change, by = "name")

#Creating bins for EPI change
bins2 <- quantile(epi_change$change, probs = seq(0,1,.2), names = FALSE, na.rm = TRUE) %>% round()

#Defining color palette
pal2 <- colorBin(
  "RdBu",
  domain = epi_change$change, 
  bins = bins2
)

labels2 <- sprintf(
  "<strong>%s</strong><br/> EPI(2020): %g<br/> Rank: %g<br/> 10-yr change: %g percent",
  epi_world2$name,
  epi_world2$EPI.new,
  epi_world2$EPI.rnk.new,
  epi_change$change
) %>% lapply(htmltools::HTML)

p2 <- leaflet(epi_world2) %>% addPolygons(
  fillColor = ~pal2(epi_world2$change),
  weight = 0.7,
  opacity = 1,
  color = 'white',
  dashArray = '3',
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 3,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels2,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "12px",
    direction = "auto"))%>%
  addLegend(pal = pal2, 
          values = ~change, 
          opacity = 0.7, title = "10-year % change",
          position = "bottomright")

p2


gdp <- read_csv("~/Documents/Data Vis/A3/GDP per capita.csv")
gdp <- select(gdp, `Country Name`, `2020 [YR2020]`)
names(gdp)[names(gdp) == 'Country Name'] <- 'name'
names(gdp)[names(gdp) == '2020 [YR2020]'] <- 'gdp_per_capita'
gdp$gdp_per_capita %>% as.numeric() %>% round()

continent <- read_csv("~/Documents/Data Vis/A3/Countries-Continents.csv")
names(continent)[names(continent) == 'Country'] <- 'name'

epi2 <- inner_join(epi, gdp, by = 'name') %>% inner_join(continent, by = 'name')

epi2$gdp_per_capita <- epi2$gdp_per_capita %>% as.numeric() %>% log()

p3 <- plot_ly(epi2, x = ~gdp_per_capita, y = epi2$EPI.new, type="scatter", 
              size = 0.5, color= ~Continent, mode="markers",
              hoverinfo = "text",
              text = paste(epi2$name, 
                           "<br><b>EPI</b> = ", epi2$EPI.new,
                           "<br><b>Rank</b> = ", epi2$EPI.rnk.new))%>%
  layout(yaxis = list(zeroline = FALSE, title = "EPI Score (2020)"),
         xaxis = list(zeroline = FALSE, title = "GDP Per Capita, 2020(current US$)(logged)"))

p3
