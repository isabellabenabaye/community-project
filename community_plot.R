library(tidyverse)
library(ggbeeswarm)
library(paletteer)
library(ggimage)
library(png)
library(grid)
library(extrafont)
fonttable <- fonttable()
loadfonts(device = "win", quiet = TRUE) ## to load

# Import the dataset
community <- read_csv('Community_Episodes_IMDb_Ratings.csv')

# Add a column that has the seasons as strings to be used as the axis ticks
community <- community %>% 
  mutate(season_name = paste0('Season ',season))

summary(community)

# Theme
theme_set(theme_minimal())
theme <- theme_update(text = element_text(family = "Rubik", size = 13),
                      #title = element_text("Franklin Gothic", size = 20, color = "gray20"),
                      plot.title = element_text("College", size = 40, color = "gray20"),
                      plot.title.position = "plot",
                      #plot.subtitle = element_text("Source Sans Pro Light", size = 20, color = "gray20"),
                      axis.text = element_text(size = 16),
                      axis.title.x = element_text(size = 20),
                      axis.line.x = element_line(color = "gray80"),
                      axis.line.y = element_line(color = "gray80"),
                      panel.grid.minor = element_blank(),
                      plot.margin = margin(15, 30, 15, 15))


# Base plot
base <- ggplot(data = community,aes(fct_rev(season_name),rating))+
  geom_blank() +
  labs(title = "Community Episodes", x = "", y = "IMDb Rating") + 
  #geom_vline(xintercept = 3.5) +
  geom_rect(data = community, xmin = 2.5,
            xmax = 3.5, ymin = 6, ymax = 10,fill = "#EBEBEB",alpha = 0.1) +
  geom_quasirandom(aes(fct_rev(season_name),rating,color = season_name),size=4,alpha=0.7,show.legend = FALSE) +
  #geom_image(data = label_eps,aes(image = image),, asp = 1.5) +
  scale_y_continuous(limits = c(6,10),expand = c(0,0)) +
  scale_color_paletteer_d("ggsci::default_jco") +
  coord_flip()

# Prepare the annotations
t <- readPNG('8bit-troy.png')
troy =  rasterGrob(t, interpolate=TRUE)
p <- readPNG('8bit-pierce.png')
pierce = rasterGrob(p, interpolate=TRUE)

arrows1 <- tribble(
  ~label,       ~y1,    ~y2,   ~x1,   ~x2, 
  "MW",         9.5,    9.72,  6.25,   6,
)
arrows2 <- tribble(
  ~label,       ~y1,    ~y2,   ~x1,   ~x2,
  "Troy",       9.8,   9.47,    2.1,      2,
  "Pierce",       8.23,  8,     1.3, 1.57,  
  "RCT",        9.83,    9.86,   4.4,   4.05,
  "Finale",     9.2,  9.53,      0.85,   1.05
)

# Add the annotations & save the plot
base + annotation_custom(grob=troy, xmin=2.15, xmax=2.45, ymin=9, ymax=9.8) +
  annotation_custom(grob=pierce, xmin=1.3, xmax=1.6, ymin=8.2, ymax=8.45) +
  annotate("text", x = 3.1, y = 6.1, label = "Only season without\nshowrunner\nDan Harmon", hjust = 0, family = "Rubik") +
  
  annotate("text", x = 2.6, y = 9.15, label = "Geothermal Escapism,", hjust = 0, family = "Rubik") +
  annotate("text", x = 2.3, y = 9.55, label = "Troy's last\nappearance", hjust = 0, family = "Rubik") +
  annotate("text", x = 1.45, y = 8.475, label = "Repilot,\nPierce's last appearance", hjust = 0, family = "Rubik") +
  annotate("text", x = 6.35, y = 9.3, label = "Modern Warfare", hjust = 0, family = "Rubik") +
  annotate("text", x = 4.4, y = 9.8, label = "Remedial Chaos\nTheory", hjust = 1, family = "Rubik") +
  annotate("text", x = 0.65, y = 8.8, label = "Emotional Consequences of\nBroadcast Television", hjust = 0, family = "Rubik") +
  
  geom_curve(data = arrows1, aes(x=x1, y=y1, xend=x2, yend=y2),
             arrow = arrow(length = unit(0.07, "inch")), 
             curvature=0.4, color="grey70") +
  geom_curve(data = arrows2, aes(x=x1, y=y1, xend=x2, yend=y2),
             arrow = arrow(length = unit(0.07, "inch")), 
             curvature=-0.4, color="grey70") +
  ggsave("Community Episodes.png", device = "png", width = 10, height = 9, dpi = 300, type = "cairo-png")