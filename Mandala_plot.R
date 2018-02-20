# Make cool mandala plot using some maths
# Load required packages
library(ggplot2)
library(dplyr)
library(deldir)

# Plotting parameters
iter <- 4 # Depth of layers
points <- 8 # Number of points
radius <- 3.8 # Factor of expansion/compression

# How points should be angled
angles <- seq(0, 2 * pi * (1-1/points), length.out = points) + pi/2

# Initial center
dats <- data.frame(x=0, y=0)

# Iterate over points to generate x & ys
for (k in 1:iter)
{
  temp=data.frame()
  for (i in 1:nrow(dats))
  {
    data.frame(x=dats[i,"x"]+radius^(k-1)*cos(angles), 
               y=dats[i,"y"]+radius^(k-1)*sin(angles)) %>% rbind(temp) -> temp
  }
  dats <- temp
}

# Use Delaunay triangulation to obtain Voronoi regions 
dats %>%
  deldir(sort=TRUE) %>% 
  .$dirsgs -> data

# Plot over regions with geom_segment
data %>% 
  ggplot() +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), alpha=0.5,
               color = sqrt((data$x1-data$x2)^2 + (data$y1-data$y2)^2)+2) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_fixed() +
  theme(legend.position  = "none",
        panel.background = element_rect(fill="#fdfde7"),
        panel.border     = element_rect(colour = "dodgerblue3", fill = NA, size = 3, linetype = 6),
        axis.ticks       = element_blank(),
        panel.grid       = element_blank(),
        axis.title       = element_blank(),
        axis.text        = element_blank()) -> plot

plot  

# Save the result if you want
#ggsave("mathy_mandala.png", height=8, width=8, units='in', dpi=800)


