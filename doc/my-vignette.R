## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----installation, eval=FALSE--------------------------------------------
#  install.packages("devtools")
#  library(devtools)

## ----library, message = FALSE--------------------------------------------
library(CloseEnough)

## ----readImage-----------------------------------------------------------
## Load image
data(cellmovie)

## Display image
display(cellmovie, method = "browser")

## ----display, message = FALSE--------------------------------------------
## Load cell outline
data(outline)

## Load trajectory
data(trajectory)

## Display image
display(cellmovie, method = "raster")

## Trace outline
lines(outline$x, outline$y, type = "l", col = "white")

## Draw trajectories
ntraj = nlevels(trajectory["trajectory"])
for (i in 1:ntraj) {
  traj_i <- subset(trajectory["data"], trajectory == i)
  lines(traj_i$x, traj_i$y, type="l", col = "white")
}


## ---- message = FALSE----------------------------------------------------
rotated_traj <- rotate_trajectory(trajectory, 90)

## ---- message = FALSE----------------------------------------------------
mirror_traj <- mirror_trajectory(trajectory, axis = "y")

## ------------------------------------------------------------------------
## Load cell mask
data(cellmask)

## Display image
display(cellmask, method = "raster")

## Draw trajectories in black
ntraj = nlevels(trajectory["trajectory"])
for (i in 1:ntraj) {
  traj_i <- subset(trajectory["data"], trajectory == i)
  lines(traj_i$x, traj_i$y, type="l", col = "black")
}

## Add the rotated trajectories in blue
ntraj = nlevels(rotated_traj["trajectory"])
for (i in 1:ntraj) {
  traj_i <- subset(rotated_traj["data"], trajectory == i)
  lines(traj_i$x, traj_i$y, type="l", col = "blue")
}

## And the reflected trajectories in green
ntraj = nlevels(mirror_traj["trajectory"])
for (i in 1:ntraj) {
  traj_i <- subset(mirror_traj["data"], trajectory == i)
  lines(traj_i$x, traj_i$y, type="l", col = "green")
}


## ------------------------------------------------------------------------
## Randomize trajectories using an image mask as reference.
rand_traj <- randomize(trajectory, cellmask)

## Display image
display(cellmovie, method = "raster")

## Trace cell outline
lines(outline$x, outline$y, type = "l", col = "white")

## Draw trajectories in white
ntraj = nlevels(trajectory["trajectory"])
for (i in 1:ntraj) {
  traj_i <- subset(trajectory["data"], trajectory == i)
  lines(traj_i$x, traj_i$y, type="l", col = "white")
}

## Add the randomized trajectories in blue
ntraj = nlevels(rand_traj["trajectory"])
for (i in 1:ntraj) {
  traj_i <- subset(rand_traj["data"], trajectory == i)
  lines(traj_i$x, traj_i$y, type="l", col = "blue")
}


## ------------------------------------------------------------------------
## Load the thresholded mask of the mitochondrial network
data(mitomask)

## Calculate minimal distance between Panx2 foci and mitochondria
# Channel refers to the channel that needs to be used within the binary image. In this case there is only one channel.
Panx2_mito <- min_distance(mitomask, trajectory, channel = 1)

## Calculate minimal distance between randomized Panx2 trajectories and mitochondria
rand_mito <- min_distance(mitomask, rand_traj, channel = 1)

## The value is currently in pixel and can be converted if the size of a pixel was provided. In this case
## the pixel size was provided in nanometer and will be converted to micrometer
Panx2_mito$min_distance <- Panx2_mito$min_distance * trajectory["pixel_size"]/1000
rand_mito$min_distance <- rand_mito$min_distance * trajectory["pixel_size"]/1000


## ----ggplot, message = FALSE---------------------------------------------

options(scipen = 999)
## Let us first group the data 
Panx2_mito$random <- FALSE
rand_mito$random <- TRUE
Panx2_mito <- rbind(Panx2_mito, rand_mito)

graph <- ggplot(data = Panx2_mito) +
  geom_density(aes(x = min_distance, color = random, fill = random),
               alpha = 0.75, size = 0.8) +
  theme_bw() +
  theme(text = element_text(size = 12),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'),
        legend.title = element_blank(),
        legend.position = "top",
        legend.margin = margin(c(5, 5, 5, 0)),
        legend.text = element_text(margin = margin(r = 10, unit = "pt"))) +
  xlab(expression(paste("Distance to mitochondria (in ", mu,"m)"))) +
  ylab("Density") + 
  scale_color_manual(values = c("#d95f02", "#7570b3")) +
  guides(color = FALSE) +
  scale_fill_manual(labels = c("Panx2", "Randomized"), values = c("#d95f02", "#7570b3")) +
  coord_cartesian(xlim = c(0.001, 10))

## Display graph
graph

