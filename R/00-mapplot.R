# Make map plot
library(ggplot2)
library(sf); sf::sf_use_s2(FALSE) # throws error otherwise
library(ggrepel)
library(rnaturalearth)
library(png)
library(cowplot)
library(patchwork)

# Not on CRAN !
# devtools::install_github("seananderson/ggsidekick") 
library(ggsidekick)
theme_set(theme_sleek())

# Set path
home <- here::here()

# Lake druks
druks_y <- 55.6288
druks_x <- 26.5981

# Specify ranges for big map
ymin = druks_y - 3; ymax = druks_y + 5; xmin = druks_x - 7.5; xmax = druks_x + 7.5

map_data <- rnaturalearth::ne_countries(
    scale = "medium",
    returnclass = "sf", continent = "europe")

# Crop the polygon for plotting and efficiency:
st_bbox(map_data) # find the rough coordinates
map <- suppressWarnings(suppressMessages(
    st_crop(map_data,
            c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax))))

ggplot(map) +
    geom_sf()

# Add point to Druks coordinates
df <- data.frame(lon =  druks_x, lat = druks_y)

dflab <- data.frame(
    label = "Lake Drūkšiai",
    x = druks_x,
    y = druks_y,
    hjust = 0,
    vjust = 1,
    orientation = "upright",
    color = "black",
    fill = "white"
)

p1 <- map_plot <- ggplot(map) +
    #geom_sf(fill = "gray60", color = "white", linewidth = 0.2) +
    geom_sf(fill = "#6D7F86", color = "white", linewidth = 0.2) +
    geom_point(data = df, aes(lon, lat), color = "tomato3", size = 1) +
    geom_label_repel(data = dflab, aes(x, y, label = label), fill = "white", color = "tomato3", size = 3, 
                     nudge_y = 5, nudge_x = 5) +
    theme_sleek(base_size = 9) + 
    theme(axis.text.x = element_text(angle = 90)) +
    coord_sf(expand = 0) + 
    labs(y = "Latitude", x = "Longitude") +
    annotate("text", label = "LITHUANIA", y = 55.3, x = 23.88, color = "white", size = 2.3) +
    annotate("text", label = "POLAND", y = 53.50, x = 21, color = "white", size = 2.3) +
    annotate("text", label = "BELARUS", y = 53.66, x = 27.95, color = "white", size = 2.3) +
    annotate("text", label = "LATVIA", y = 56.8, x = 26.1, color = "white", size = 2.3) +
    annotate("text", label = "ESTONIA", y = 58.7, x = 24.9, color = "white", size = 2.3) +
    annotate("text", label = "FINLAND", y = 60.3, x = 24, color = "white", size = 2.3) +
    annotate("text", label = "RUSSIA", y =  56.5, x = 32, color = "white", size = 2.3) +
    annotate("text", label = "Baltic Sea", y =  59.5, x = 21.5, color = "#6D7F86", size = 3, fontface = "italic") +
    NULL

p1

## SMALL map (inset)
druk <- readPNG(paste0(home, "/figures/map.png"))

p2 <- ggdraw() +
    draw_image(druk)

p2

p1 + inset_element(p2, left = 0.45, bottom = 0.565, right = 1, top = 1)

ggsave(paste0(home, "/figures/map_plot.png"), width = 10, height = 10, units = "cm")
