library(dplyr)
library(faraway)
library(ggmap)
library(ggplot2)
library(grid)
library(gridExtra)

my_circle <- circleGrob(x = 0.5, y = 0.5, r = 0.5,
                        gp = gpar(col = "gray", lty = 3))
grid.draw(my_circle)

my_rect <- rectGrob(x = 0.5, y = 0.5, width = 0.8, height = 0.3)
grid.draw(my_rect)

grid.edit("my_circle", gp = gpar(col = "red", lty = 1))

data("worldcup")
wc_plot <- ggplot(worldcup, aes(x = Time, y = Passes)) +
  geom_point()
grid.draw(wc_plot)

grid.draw(wc_plot)
grid.draw(my_circle)

wc_plot
grid.force()
grid.ls()

grid.edit("geom_point.points.1400", gp = gpar(col = "red"))
grid.edit("GRID.text.1419", gp = gpar(fontface = "bold"))

candy <- circleGrob(r = 0.1, x = 0.5, y = 0.6)
stick <- segmentsGrob(x0 = 0.5, x1 = 0.5, y0 = 0, y1 = 0.5)
lollipop <- gTree(children = gList(candy, stick))
grid.draw(lollipop)

grid.ls(lollipop)

grid.draw(rectGrob())
sample_vp <- viewport(x = 0.5, y = 0.5,
                      width = 0.5, height = 0.5,
                      just = c("left", "bottom"))
pushViewport(sample_vp)
grid.draw(roundrectGrob())
grid.draw(lollipop)
popViewport()

grid.draw(rectGrob())
sample_vp <- viewport(x = 0.5, y = 0.5,
                      width = 0.5, height = 0.5,
                      just = c("center", "center"))
pushViewport(sample_vp)
grid.draw(roundrectGrob())
grid.draw(lollipop)
popViewport()

grid.draw(rectGrob())
sample_vp <- viewport(x = 0.75, y = 0.75,
                      width = 0.25, height = 0.25,
                      just = c("left", "bottom"))
pushViewport(sample_vp)
grid.draw(roundrectGrob())
grid.draw(lollipop)
popViewport()

grid.draw(rectGrob())

sample_vp_1 <- viewport(x = 0.75, y = 0.75,
                        width = 0.25, height = 0.25,
                        just = c("left", "bottom"))
pushViewport(sample_vp_1)
grid.draw(roundrectGrob())
grid.draw(lollipop)
popViewport()

sample_vp_2 <- viewport(x = 0, y = 0,
                        width = 0.5, height = 0.5,
                        just = c("left", "bottom"))
pushViewport(sample_vp_2)
grid.draw(roundrectGrob())
grid.draw(lollipop)
popViewport()

grid.draw(rectGrob())

sample_vp_1 <- viewport(x = 0.5, y = 0.5,
                        width = 0.5, height = 0.5,
                        just = c("left", "bottom"))
sample_vp_2 <- viewport(x = 0.1, y = 0.1,
                        width = 0.4, height = 0.4,
                        just = c("left", "bottom"))

pushViewport(sample_vp_1)
grid.draw(roundrectGrob(gp = gpar(col = "red")))
pushViewport(sample_vp_2)
grid.draw(roundrectGrob())
grid.draw(lollipop)
popViewport(2)

grid.draw(rectGrob())

sample_vp_1 <- viewport(x = 0.5, y = 0.5,
                        width = 0.5, height = 0.5,
                        just = c("left", "bottom"))
pushViewport(sample_vp_1)
grid.draw(roundrectGrob())
grid.draw(lollipop)
popViewport()

grid.ls()

worldcup %>%
  ggplot(aes(x = Time, y = Passes)) +
  geom_point()
grid.force()
grid.ls()

balt_counties <- map_data("county", region = "maryland") %>%
  mutate(our_counties = subregion %in% c("baltimore", "baltimore city"))
balt_map <- get_map("Baltimore County", zoom = 10) %>%
  ggmap(extent = "device") +
  geom_polygon(data = filter(balt_counties, our_counties == TRUE),
               aes(x = long, y = lat, group = group),
               fill = "red", color = "darkred", alpha = 0.2)

maryland_map <- balt_counties %>%
  ggplot(aes(x = long, y = lat, group = group, fill = our_counties)) +
  geom_polygon(color = "black") +
  scale_fill_manual(values = c("white", "darkred"), guide = FALSE) +
  theme_void() +
  coord_map()

grid.draw(ggplotGrob(balt_map))
md_inset <- viewport(x = 0, y = 0,
                     just = c("left", "bottom"),
                     width = 0.35, height = 0.35)
pushViewport(md_inset)
grid.draw(rectGrob(gp = gpar(alpha = 0.5, col = "white")))
grid.draw(rectGrob(gp = gpar(fill = NA, col = "black")))
grid.draw(ggplotGrob(maryland_map))
popViewport()

ex_vp <- viewport(x = 0.5, y = 0.5,
                  just = c("center", "center"),
                  height = 0.8, width = 0.8,
                  xscale = c(0, 100), yscale = c(0, 10))
pushViewport(ex_vp)
grid.draw(rectGrob())
grid.draw(circleGrob(x = unit(20, "native"), y = unit(5, "native"),
                     r = 0.1, gp = gpar(fill = "lightblue")))
grid.draw(circleGrob(x = unit(85, "native"), y = unit(8, "native"),
                     r = 0.1, gp = gpar(fill = "darkred")))
popViewport()

grid.arrange(lollipop, circleGrob(),
             rectGrob(), lollipop,
             ncol = 2)

time_vs_shots <- ggplot(worldcup, aes(x = Time, y = Shots)) +
  geom_point()
player_positions <- ggplot(worldcup, aes(x = Position)) +
  geom_bar()

grid.arrange(time_vs_shots, player_positions, ncol = 2)

grid.arrange(time_vs_shots, player_positions,
             layout_matrix = matrix(c(1, 2, 2), ncol = 3))

grid.arrange(time_vs_shots, player_positions,
             layout_matrix = matrix(c(1, NA, NA, NA, 2, 2),
                                    byrow = TRUE, ncol = 3))

worldcup_table <- worldcup %>%
  filter(Team %in% c("Germany", "Spain", "Netherlands", "Uruguay")) %>%
  group_by(Team) %>%
  dplyr::summarize(`Average time` = round(mean(Time), 1),
                   `Average shots` = round(mean(Shots), 1)) %>%
  tableGrob()

grid.draw(ggplotGrob(time_vs_shots))
wc_table_vp <- viewport(x = 0.22, y = 0.85,
                        just = c("left", "top"),
                        height = 0.1, width = 0.2)
pushViewport(wc_table_vp)
grid.draw(worldcup_table)
popViewport()


## Quiz

# The ggplot2 package is built on top of grid graphics, so the grid graphics system “plays well” with ggplot2 objects.

# Grid graphics and R’s base graphics are two separate systems.
# You cannot easily edit a plot created using base graphics with grid graphics functions.
# If you have to integrate output from these two systems, you may be able to using the gridBase package,

# Possible grobs that can be created using functions in the grid package include circles, rectangles, points, lines,
# polygons, curves, axes, rasters, segments, and plot frames
# The gridExtra package, provide functions that can be used to create addition grobs beyond those provided by the grid package.
# For example, the gridExtra package includes a function called tableGrob to create a table grob that can be added
# to grid graphics objects.

# The grob family of functions also includes a parameter called gp for setting graphical parameters like color, fill, line type, line width, etc.,
# for grob objects. The input to this function must be a gpar object, which can be created using the gpar function
# Aesthetics that you can set by specifying a gpar object for the gp parameter of a grob include color (col), fill (fill),
# transparency (alpha), line type (lty), line width (lwd), line end and join styles (lineend and linejoin, respectively),
# and font elements (fontsize, fontface, fontfamily).

# In many ways, ggplot objects can be treated as grid graphics grobs. For example, you can use the grid.draw function from grid to write
# a ggplot object to the current graphics device

# Viewports are the plotting windows that you can move into and out of to customize plots using grid graphics.
# You can use ggplot objects in plots with viewports.

# he native unit is often the most useful when creating extensions for ggplot2, for example. The npc units are also often
# useful in designing new plots– these set the x- and y-ranges to go from 0 to 1, so you can use these units if you need
# to place an object in, for example, the exact center of a viewport (c(0.5, 0.5) in npc units), or create a viewport in the
# top right quarter of the plot region. Grid graphics also allows the use of some units with absolute values, including
# inches (inches), centimeters (cm), and millimeters (mm).

# The grid.arrange function from the gridExtra package makes it easy to create a plot with multiple grid objects plotted on it.
# For example, you can use it to write out one or more grobs you’ve created to a graphics device:


