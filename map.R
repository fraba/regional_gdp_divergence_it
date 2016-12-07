setwd('~/public_git/regional_gdp_divergence_it')

library(readr)
regional_data <- read_csv("regional_data.csv")
# View(regional_data)
names(regional_data)[3:9] <- paste0("y", names(regional_data)[3:9])

min(as.matrix(regional_data[,3:9]), na.rm=T)
max(as.matrix(regional_data[,3:9]), na.rm=T)

# Molise with Abruzzo

require("rgdal")
require("maptools")
require("ggplot2")
require("plyr")

it_map <- readOGR(dsn=".", layer="Reg2011_WGS84_simp", verbose = FALSE)

it_map <- merge(it_map, regional_data, by.x = 'COD_REG', by.y = 'code', all.x = TRUE)

it_map@data$id = rownames(it_map@data)
it_map.points = fortify(it_map, region="id")
it_map.df = join(it_map.points, it_map@data, by="id")

library("RColorBrewer")
require(ggplot2)
require(scales)
pal_gdpdiff <- colorRampPalette(brewer.pal(7,'Spectral'))
sc_gdpdiff <- scale_fill_gradientn(colours = pal_gdpdiff(100), limits=c(.3, 1.7), labels=percent)

require(viridis)
pal_gdpdiff <- colorRampPalette(viridis(7))
sc_gdpdiff <- scale_fill_gradientn(colours = pal_gdpdiff(100), limits=c(.3, 1.7), labels=percent)

extractLegend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
  

myPlot <- function(year, guide_pos = "none"){
  return(
  ggplot(it_map.df, aes_string(x = 'long', 
                               y = 'lat', 
                               group = 'group', 
                               fill = as.character(year))) +
    labs(caption = gsub("y","",year), fill = 'Percapita value added\n(Italy = 100%)') +
    geom_polygon() +
    theme_bw() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          legend.title=element_text(size=9),
          legend.text=element_text(size=9)) +
    sc_gdpdiff +
    theme(legend.position=guide_pos)
  )
}

my_legend <- extractLegend(myPlot("y1891",guide_pos='left'))

require(gridExtra)
p <- lapply(names(regional_data)[3:9], myPlot)
p[[8]] <- my_legend

require(reshape2)
regional_data_melt <- melt(regional_data, id.vars = c('regione','code'))
regional_data_melt$variable <- as.numeric(gsub("y","",regional_data_melt$variable))
regional_data_melt$area <- NA
regional_data_melt$area[regional_data_melt$code %in% 1:8] <- 'North'
regional_data_melt$area[regional_data_melt$code %in% 9:12] <- 'Centre'
regional_data_melt$area[regional_data_melt$code %in% 13:20] <- 'South'
regional_data_melt$area <- factor(regional_data_melt$area, levels = c('North','Centre','South'))
p_timeline <- 
  ggplot(regional_data_melt, aes(x=variable, y=value, group=regione, colour=area)) +
  geom_point() + 
  geom_smooth(data=regional_data_melt, aes(x=variable, y=value, group=area, colour=area), se = FALSE) + 
  theme_bw() +
  scale_y_continuous(limits = c(.3, 1.7), labels = percent) +
  scale_x_continuous(breaks = unique(regional_data_melt$variable)) +
  scale_colour_brewer(palette = 'Set2') +
  labs(x = NULL, y = NULL, colour = "", 
       caption = 'Data: Felice, E. (2011). Regional value added in Italy. The Economic History Review, 64(3), 929–950\nDesign: @FrBailo\nGit: https://github.com/fraba/regional_gdp_divergence_it') +
  theme(legend.position = 'bottom')

p[[9]] <- p_timeline
