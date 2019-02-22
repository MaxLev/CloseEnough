## Methods for objects of class 'Traj'
## Define a series of getter and setter for class 'Traj'
setMethod("[", "Traj", function(x, i, j, ..., drop){
  switch(i,
         data = return(data.frame("trajectory" = x@trajectory,
                                  "frame" = x@frame,
                                  "time" = x@time,
                                  "x" = x@x,
                                  "y" = x@y)),
         trajectory = return(x@trajectory),
         frame = return(x@frame),
         time = return(x@time),
         x = return(x@x),
         y = return(x@y),
         time_unit = return(x@time_unit),
         dim_x = return(x@dim_x),
         dim_y = return(x@dim_y),
         pixel_size = return(x@pixel_size),
         pixel_unit = return(x@pixel_unit))
}
)

## Methods
## 'rotate_trajectory' rotates 'xy' coordinates using the center of the image
## as the center of rotation. Angle must be given in degree.

#' Rotate trajectories
#'
#' Rotates all trajectories 'en bloc' using the centre of the image as origin.
#'
#'
#' @param object An object of class 'Traj'
#' @param angle A numeric value corresponding to the desired angle of rotation.
#' If positive, trajectories are rotated clockwise. If negative, they are rotated counterclockwise.
#'
#' @return
#' @export
#'
#' @examples
#' ## Upload image and trajectories
#' data(cellmask)
#' data(trajectory)
#'
#' rotatedtraj <- rotate_trajectory(trajectory, 90)
#' ## Display image (first frame only)
#' EBImage::display(cellmask, method = "raster")
#'
#' ## Overlay trajectories in black
#' ntraj = nlevels(trajectory["trajectory"])
#' for (i in 1:ntraj) {
#' traj_i <- subset(trajectory["data"], trajectory == i)
#' lines(traj_i$x, traj_i$y, type="l", col = "black")
#' }
#' ## Overlay rotated trajectories in blue
#' nrotatedtraj = nlevels(rotatedtraj["trajectory"])
#' for (i in 1:nrotatedtraj) {
#' traj_i <- subset(rotatedtraj["data"], trajectory == i)
#' lines(traj_i$x, traj_i$y, type="l", col = "blue")
#' }
#'
setGeneric("rotate_trajectory", function(object, angle)
  standardGeneric("rotate_trajectory"))

setMethod("rotate_trajectory", "Traj", function(object, angle) {
  if (length(angle) != 1 || !is.numeric(angle)) {
    stop("The argument angle must be a number.")
  }

  theta <- ifelse(angle <= 0, (360 - angle) * pi/180, -angle * pi/180)

  ## Calculate center of rotation
  half_x <- object@dim_x / 2
  half_y <- object@dim_y / 2

  ## Rotation formula for both axis
  x <- (((object@x - half_x) * cos(theta)) +
          ((object@y - half_y) * sin(theta))) + half_x
  y <- ((-(object@x - half_x) * sin(theta)) +
          ((object@y - half_y) * cos(theta))) + half_y
  object@x <- x
  object@y <- y
  return(object)
}
)

## 'mirror_trajectory' the trajectories. The argument 'axis' defines the
## symetry axis

#' Reflect trajectories
#'
#' Flip all trajectories 'en bloc' along the vertical or horizontal axis.
#'
#' @param object An object of class 'Traj'
#' @param axis If 'x', trajectories are flipped along the horizontal axis. If 'y' they are flipped along the vertical axis.
#'
#' @return
#' @export
#'
#' @examples
#' data(cellmask)
#' data(trajectory)
#' mirroredtraj <- mirror_trajectory(trajectory, "x")
#' ## Display image (first frame only)
#' EBImage::display(cellmask, method = "raster")
#'
#' ## Overlay trajectories in black
#' ntraj = nlevels(trajectory["trajectory"])
#' for (i in 1:ntraj) {
#' traj_i <- subset(trajectory["data"], trajectory == i)
#' lines(traj_i$x, traj_i$y, type="l", col = "black")
#' }
#' ## Overlay mirrored trajectories in blue
#' nmirroredtraj = nlevels(mirroredtraj["trajectory"])
#' for (i in 1:nmirroredtraj) {
#' traj_i <- subset(mirroredtraj["data"], trajectory == i)
#' lines(traj_i$x, traj_i$y, type="l", col = "blue")
#' }
#'
setGeneric("mirror_trajectory", function(object, axis)
  standardGeneric("mirror_trajectory"))

setMethod("mirror_trajectory", "Traj", function(object, axis) {

  ## Reflection formula for both axis
  ifelse(axis == "x",
         object@y <- object@dim_y - object@y,
         object@x <- object@dim_x - object@x)
  return(object)
}
)

## 'frame_image' stores the information from object of class 'Image' into a
## data frame. The first column stores the frame number, the second
## column stores the x coordinates and the third column stores the y coordinates.
## Successive columns store the pixel intensity value for every channel selected.
## 'x' coordinates are read from left to right, 'y' coordinates are read from
## top to bottom, i.e the coordinate (1,1) is at the top left corner.

#' Stores the information of an image into a data frame.
#'
#'
#' @param object An object of class Image
#'
#' @return
#' @import EBImage
#' @export
#' @examples
#'
#' ## Load image
#' data(cellmovie)
#'
#' ## Extract a few frames from the image
#' df_image <- frame_image(cellmovie)
#'
#'
setGeneric("frame_image", function(object)
  standardGeneric("frame_image"))
setMethod("frame_image", "Image", function(object) {
  ## Test arguments validity
  if (!EBImage::is.Image(object)) {
    EBImage::as.Image(object)
  }
  ## Conversion of single-frame, multi-channel color image to data frame
  if (object@colormode == 2 && length(dim(object@.Data)) == 3) {
    dim <- dim(object@.Data)
    channel <- sort(1:dim[3])
    frame <- 1:EBImage::numberOfFrames(object, type = "render")

    ## Total number of rendered pixels per channel
    total_nb_pixel <- dim[1] * dim[2] * length(frame)

    ## Create and fill the data frame
    channel_names <- paste("channel", channel, sep = "")
    image_frame <- matrix(rep(NA, total_nb_pixel * (length(channel) + 3)),
                          nrow = total_nb_pixel, ncol = length(channel) + 3)
    image_frame <- as.data.frame(image_frame)
    names(image_frame) <- c("frame", "x", "y", channel_names)
    image_frame["frame"] <- rep(frame, each = dim[1] * dim[2])
    image_frame["y"] <- as.vector(col(object@.Data[, , 1]))
    image_frame["x"] <- as.vector(row(object@.Data[, , 1]))
    for (i in 1:length(channel)) {
      image_frame[3 + i] <- as.vector(object@.Data[, , channel[i]])
    }
    return(image_frame)
  }

  ## Conversion of multi-frame, multi-channel color image to data frame
  if (object@colormode == 2 && length(dim(object@.Data)) == 4) {
    dim <- dim(object@.Data)
    channel <- sort(1:dim[3])
    frame <- 1:numberOfFrames(object, type = "render")

    ## Total number of rendered pixels per channel
    total_nb_pixel <- dim[1] * dim[2] * length(frame)

    ## Create and fill the data frame
    channel_names <- paste("channel", channel, sep = "")
    image_frame <- matrix(rep(NA, total_nb_pixel * (length(channel) + 3)),
                          nrow = total_nb_pixel, ncol = length(channel) + 3)
    image_frame <- as.data.frame(image_frame)
    names(image_frame) <- c("frame", "x", "y", channel_names)
    image_frame["frame"] <- rep(frame, each = dim[1] * dim[2])
    image_frame["y"] <- rep(as.vector(col(object@.Data[, , 1, 1])),
                            length(frame))
    image_frame["x"] <- rep(as.vector(row(object@.Data[, , 1, 1])),
                            length(frame))
    for (i in 1:length(channel)) {
      image_frame[3 + i] <- as.vector(object@.Data[, , channel[i], ])
    }
    return(image_frame)
  }

  ## Conversion of single-frame, single channel grayscale image to data frame
  if (object@colormode == 0 && length(dim(object@.Data)) == 2) {
    dim <- dim(object@.Data)
    channel <- 1
    frame <- 1:EBImage::numberOfFrames(object, type = "render")

    ## Total number of rendered pixels per channel
    total_nb_pixel <- dim[1] * dim[2] * length(frame)

    ## Create and fill the data frame
    image_frame <- matrix(rep(NA, total_nb_pixel * (4)), nrow = total_nb_pixel,
                          ncol = 4)
    image_frame <- as.data.frame(image_frame)
    names(image_frame) <- c("frame", "x", "y", "channel1")
    image_frame["frame"] <- rep(frame, each = dim[1] * dim[2])
    image_frame["y"] <- as.vector(col(object@.Data[, ]))
    image_frame["x"] <- as.vector(row(object@.Data[, ]))
    image_frame[4] <- as.vector(object@.Data[, ])
    return(image_frame)
  }

  ## Conversion of multi-frame, single-channel grayscale image.
  if (object@colormode == 0 && length(dim(object@.Data)) == 3) {
    dim <- dim(object@.Data)
    channel <- 1
    frame <- 1:EBImage::numberOfFrames(object, type = "render")

    ## Total number of rendered pixels per channel
    total_nb_pixel <- dim[1] * dim[2] * length(frame)

    ## Create and fill the data frame
    image_frame <- matrix(rep(NA, total_nb_pixel * (length(channel) + 3)),
                          nrow = total_nb_pixel, ncol = length(channel) + 3)
    image_frame <- as.data.frame(image_frame)
    names(image_frame) <- c("frame", "x", "y", "channel1")
    image_frame["frame"] <- rep(frame, each = dim[1] * dim[2])
    image_frame["y"] <- rep(as.vector(col(object@.Data[, , 1])),
                            length(frame))
    image_frame["x"] <- rep(as.vector(row(object@.Data[, , 1])),
                            length(frame))
    image_frame[4] <- as.vector(object@.Data[, , ])
    return(image_frame)
  }

  ## Conversion of multi-frame, multi-channel grayscale image.
  if (object@colormode == 0 && length(dim(object@.Data)) == 4) {
    dim <- dim(object@.Data)
    channel <- sort(1:dim[3])
    frame <- 1:EBImage::numberOfFrames(object, type = "render")

    ## Total number of rendered pixels per image
    total_nb_pixel <- dim[1] * dim[2] * length(frame)

    ## Create and fill the data frame
    channel_names <- paste("channel", channel, sep = "")
    image_frame <- matrix(rep(NA, total_nb_pixel * (length(channel) + 3)),
                          nrow = total_nb_pixel, ncol = length(channel) + 3)
    image_frame <- as.data.frame(image_frame)
    names(image_frame) <- c("frame", "x", "y", channel_names)
    image_frame["frame"] <- rep(frame, each = dim[1] * dim[2])
    image_frame["y"] <- rep(as.vector(col(object@.Data[, , 1, 1])),
                            length(frame))
    image_frame["x"] <- rep(as.vector(row(object@.Data[, , 1, 1])),
                            length(frame))
    for (i in 1:length(channel)) {
      image_frame[3 + i] <- as.vector(object@.Data[, , channel[i], ])
    }
    return(image_frame)
  }
}
)

## Helper functions to get number of trajectories or frames --------------------
#' Returns the number of trajectories
#'
#' @param object An object of class Traj
#'
#' @return
#' @export
#' @examples
#'
#' data(trajectory)
#' number_of_trajectory(trajectory)
#'
setGeneric("number_of_trajectory", function(object)
  standardGeneric("number_of_trajectory"))
setMethod("number_of_trajectory", "Traj", function(object) {
  return(length(unique(object@trajectory)))
}
)
#' Returns the number of frames
#'
#' @param object An object of class Traj
#'
#' @return
#' @export
#' @examples
#'
#' data(trajectory)
#' number_of_frame(trajectory)
#'
setGeneric("number_of_frame", function(object) standardGeneric("number_of_frame"))
setMethod("number_of_frame", "Traj", function(object){
  return(length(unique(object@frame)))
}
)

## 'randomize' creates randomized trajectories from a 'Traj' object.
## If a 'mask' image is provided, randomized trajectories are
## restricted to the mask area. Randomized trajectories that extend outside the
## mask are pruned. The mask must be a binary image
## of class 'Image' i.e. pixels within the 'mask' area must have an intensity
## value of 1 while pixels outside the area must be black (intensity = 0). If
## the mask image has only one frame, it is replicated for all frames
## of the 'Traj' object. If no mask is provided, the randomized trajectories
## are generated within the dimension of the 'Traj' object.

#' Randomizes trajectories within a delimited area.
#'
#' This function randomizes the trajectories from an object of class Traj within the white area (intensity = 1)
#' of a binary image.Trajectories are first randomly rotated around their point of origin and then their origin is
#' randomly assigned to a xy coordinate with the mask area. Trajectory points that fall out of the mask are then
#' pruned out.
#'
#' @param trajectory An object of class Traj
#' @param mask An object of class Image. The image should be a binary mask.
#'
#' @return
#' @import EBImage
#' @export
#' @examples
#'
#' ## Upload mask and trajectories
#' data(cellmask)
#' data(trajectory)
#'
#' ## Randomize trajectories
#' randtraj <- randomize(trajectory, mask = cellmask)
#'
#' ## Plot trajectories
#' EBImage::display(cellmask, method = "raster")
#'
#' ## Overlay trajectories in black
#' ntraj = nlevels(trajectory["trajectory"])
#' for (i in 1:ntraj) {
#' traj_i <- subset(trajectory["data"], trajectory == i)
#' lines(traj_i$x, traj_i$y, type="l", col = "black")
#' }
#' ## Overlay randomized trajectories in blue
#' nrandtraj = nlevels(randtraj["trajectory"])
#' for (i in 1:nrandtraj) {
#' traj_i <- subset(randtraj["data"], trajectory == i)
#' lines(traj_i$x, traj_i$y, type="l", col = "blue")
#' }
#'
#'
setGeneric("randomize", function(trajectory, mask)
  standardGeneric("randomize"))
setMethod("randomize", "Traj", function(trajectory, mask) {
  ## Verify argument validity
  if (!is.Traj(trajectory)) {
    stop("'trajectory' must be an object of class 'Traj'.")
  }
  if (!missing(mask) && !EBImage::is.Image(mask)) {
    mask <- EBImage::as.Image(mask)
  }
  if (!missing(mask)) {
    ## Check that 'mask' and 'trajectory' are compatible objects
    ## if 'mask' has more than one frame then 'mask' and 'trajectory' must have
    ## the same frame sequence
    mask_frame <- seq(1, EBImage::numberOfFrames(mask, type = "render"))
    if (mask_frame != 1) {
      trajectory_frame <- sort(unique(trajectory@frame))
      if (all(image_frame != trajectory_frame)) {
        stop("Incompatible objects: 'image' and 'trajectory' do not share the same
             'frame' sequence.")
      }
      }
    ## 'mask' and 'trajectory' must have same number of pixels in x and y
    ## dimensions
    dim <- dim(mask@.Data)
    if (dim[1] != trajectory@dim_x || dim[2] != trajectory@dim_y) {
      stop("Incompatible objects: 'mask' and 'trajectory' do not share the same
           dimensions.")
    }
    }
  traj_data <- trajectory["data"]

  ## initialize randomized trajectories by centering all real trajectories at
  ## origin c(0, 0)
  center_origin <- function(data) {
    data$rand_x <- data$x - data[1, "x"]
    data$rand_y <- data$y - data[1, "y"]
    return(data)
  }
  traj_data <- traj_data %>% dplyr::group_by(trajectory) %>% dplyr::mutate(x = x- x[1], y = y- y[1])

  ## random rotation around origin
  rand_rotate <- function(data) {
    angle <- runif(1, 0, 360)
    theta <- angle * pi/180
    x <- ((data$x * cos(theta)) + (data$y * sin(theta)))
    y <- (-(data$x* sin(theta)) + (data$y * cos(theta)))
    data$x <- x
    data$y <- y
    return(data)
  }
  traj_data <- traj_data %>% dplyr::group_by(trajectory) %>% dplyr::do(rand_rotate(.))

  ## generate grid of possible coordinates
  if (missing(mask)) {
    area <- data.frame("frame" = 1,
                       "x" = as.vector(row(matrix(NA, trajectory@dim_x,
                                                  trajectory@dim_y))),
                       "y" = as.vector(col(matrix(NA, trajectory@dim_x,
                                                  trajectory@dim_y))))
  } else {
    ## Convert 'mask' to data frame
    area <- frame_image(mask)
    ## Only keep pixels above background
    area <- area[area[4] > 0, ]
  }

  ## random translation along both axis
  rand_translate <- function(data, area) {
    initial <- round(runif(1, 1, length(area[area$frame == 1, 1])))
    data[, "x"] <- data[, "x"] + area[initial, "x"]
    data[, "y"] <- data[, "y"] + area[initial, "y"]
    return(data)
  }

  traj_data <- traj_data %>% dplyr::group_by(trajectory) %>%
    dplyr::do(rand_translate(., area = area))

  ## Truncate random trajectory coordinates that fall outside the 'mask' area.
  ## First, generate logical vectors testing whether all the points in the
  ## randomized trajectories are within the 'xy coordinates of the'mask' area
  traj_data$valid_x <- ceiling(traj_data$x) %in% area$x
  traj_data$valid_y <- ceiling(traj_data$y) %in% area$y

  ## Pruning of out of bounds randomized trajectories
  traj_data <- traj_data %>% dplyr::filter(valid_x == TRUE,
                                           valid_y == TRUE)

  return(Traj(trajectory = traj_data$trajectory,
              frame = traj_data$frame,
              time = traj_data$time,
              time_unit = trajectory@time_unit,
              x = traj_data$x,
              y = traj_data$y,
              dim_x = trajectory@dim_x,
              dim_y = trajectory@dim_y,
              pixel_size = trajectory@pixel_size,
              pixel_unit = trajectory@pixel_unit))

    })

### 'min_distance' calculates the minimal distance between every points of a
### 'trajectory' and all non-zero pixels of the 'image' within the same frame.
###

#' Calculates the minimum distance between points of a trajectory and objects from a reference image
#'
#' @param image An object of class Image
#' @param trajectory An object of class Traj
#' @param channel A numeric vector indicating which channel to use in the image
#'
#' @return
#' @export
#' @import EBImage
#'
#' @examples
setGeneric("min_distance", function(image, trajectory, channel)
  standardGeneric("min_distance"))
setMethod("min_distance", signature(image = "Image", trajectory = "Traj"),
          function(image, trajectory, channel) {
            ## Check that 'image' and 'trajectory' are compatible objects
            ## 'image' and 'trajectory' must have same sequence of frames
            image_frame <- seq(1, EBImage::numberOfFrames(image, type = "render"))
            trajectory_frame <- sort(unique(trajectory@frame))
            if (all(image_frame != trajectory_frame)) {
              stop("Incompatible objects! The objects do not share the same frame sequence.")
            }
            ## 'image' and 'trajectory' must have same number of pixels in x and y
            ## dimensions
            dim <- dim(image@.Data)
            if (dim[1] != trajectory@dim_x || dim[2] != trajectory@dim_y) {
              stop("Incompatible objects! The objects do not have the have dimensions.")
            }
            ## Convert 'image' to data frame and select channel(s) if specified.
            image_frame <- frame_image(image)
            if (length(channel) <= (ncol(image_frame) - 3) &&
                max(channel) <= (ncol(image_frame) - 3)) {
              channel <- sort(channel) + 3
              image_frame <- image_frame[c(1:3, channel)]
            } else {
              stop("Invalid 'channel' argument.")
            }

            ## Only keep non-black pixels (i.e. the area to be selected within the mask)
            image_frame <- image_frame %>% dplyr::filter(.[[4:ncol(image_frame)]] > 0)

            ## Calculation of the nearest distance per channel
            output <- vector("list", length(channel))
            names(output) <- paste("channel", channel - 3, sep = "")
            for (i in seq_along(channel)) {
              ## Select only one channel at a time if multiple channel
              if (length(channel) > 1) {
                img_data <- image_frame[c(1:3, 3 + i)]
              } else {
                img_data <- image_frame
              }

              ## Split the image and trajectory by 'frame'
              img_data <- split(img_data, img_data$frame)
              traj_data <- trajectory["data"]
              traj_data <- split(traj_data, traj_data$frame)
              ## Compute the euclidean distance for each pixel of the trajectory
              for (j in seq_along(traj_data)) {
                for (k in seq_along(traj_data[[j]]$x)) {
                  traj_data[[j]]$min_distance[k] <-
                    min(sqrt((img_data[[j]]$x - traj_data[[j]]$x[k])^2 +
                               (img_data[[j]]$y - traj_data[[j]]$y[k])^2))
                }
              }
              traj_data <- do.call(rbind, traj_data)
            }
            output <- TrajEucl(trajectory = traj_data$Trajectory,
                           frame = traj_data$Frame,
                           time = trajectory@time,
                           time_unit = trajectory@time_unit,
                           x = traj_data$x,
                           y = traj_data$y,
                           min_distance = traj_data$min_distance * trajectory@pixel_size,
                           dim_x = trajectory@dim_x,
                           dim_y = trajectory@dim_y,
                           pixel_size = trajectory@pixel_size,
                           pixel_unit = trajectory@pixel_unit)

            return(output)
          })


## Plot

#' Title
#'
#' @param traj
#' @param randtraj
#'
#' @return
#' @export
#' @import ggplot2
#' @examples
#'
setGeneric("profileplot", function(traj, randtraj) standardGeneric("profileplot"))
setMethod("profileplot", signature(traj = "TrajEucl", randtraj = "TrajEucl"), function(traj, randtraj){

  ## Prevent scientific notation
  options(scipen = 999)

  ## Extract pixel unit for labelins axis
  pixel_unit <- traj@pixel_unit

  ## Let us group the data
  traj <- data.frame("min_distance" = c(traj@min_distance, randtraj@min_distance),
                     "random" = c(rep(FALSE, length(traj@min_distance)),
                                  rep(TRUE, length(randtraj@min_distance))))

  graph <- ggplot(data = traj) +
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
    xlab(paste0("Distance (in ", pixel_unit,")")) +
    ylab("Density") +
    scale_color_manual(values = c("#d95f02", "#7570b3")) +
    guides(color = FALSE) +
    scale_fill_manual(labels = c("Original", "Randomized"), values = c("#d95f02", "#7570b3"))
  return(graph)
  })
