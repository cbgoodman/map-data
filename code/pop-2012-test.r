## Test Plot.ly county-level chloropleth map

library(plotly)

l <- list(color = toRGB("white"), width = 2)
  
g <- list(
  scope = 'usa',
  projection = list(type = 'Mercator'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
   
library(dplyr)
library(maps)
library(mapproj)

cnty <- map_data("county")
state <- map_data("state")
data(county.fips)

ggplot(cnty, aes(long,lat, group = group)) + 
    geom_polygon(aes(fill = region), show.legend=F, colour = rgb(1,1,1,0.2)) + 
    theme(legend.position="none") +
    coord_map("albers", lat0=30, lat1=40) +
    theme_void()

df <- read.csv("https://raw.githubusercontent.com/cbgoodman/map-data/master/data/2012_county_population.csv")

cnty2 <- cnty  %>%
	mutate(polyname = paste(region,subregion,sep=",")) %>%
    left_join(county.fips, by="polyname") 
    
cnty2.df <- inner_join(cnty2, df, by=c("fips" = "fips") )

ggplot(cnty2.df, aes(long, lat,group = group)) + 
  geom_polygon(aes(fill = pop2012), colour = rgb(1,1,1,0.2))  +
  theme(legend.position="none") +
  coord_map("albers", lat0=30, lat1=40) +
  theme_void()
  
pop.qt <- quantile(cnty2.df$pop2012, probs = c(0, 0.125, .25, 0.5, .75, .875 , 1 ))
cnty2.df$pop <- cut(cnty2.df$pop2012, breaks = pop.qt , labels = paste(pop.qt[-1]))

p <- ggplot(cnty2.df, aes(long, lat,group = group)) + 
  geom_polygon(aes(fill = pop), colour = NA)  +
  geom_polygon(data = state, colour = "white", size=0.2, fill = NA) +
  scale_fill_brewer(palette = "YlGn") +
  theme(legend.position="none") +
  coord_map("albers", lat0=30, lat1=40) +
  theme_void()
  
p <- ggplotly(p, tooltip = 'text') %>% 
  layout(
    hovermode = 'x',
    geo=g,
    legend = list(
      orientation = 'h',
      x = 0.5,
      y = 1.01,
      xanchor = 'center'))