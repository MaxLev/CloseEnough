## Class 'Traj', definition and construction
.Traj <- setClass("Traj",
                  representation = representation(
                    trajectory = "factor",
                    frame = "integer",
                    time = "numeric",
                    x = "numeric",
                    y = "numeric",
                    time_unit = "character",
                    dim_x = "integer",
                    dim_y = "integer",
                    pixel_size = "numeric",
                    pixel_unit = "character"))

## Constructor for class 'Traj' ------------------------------------------------
#' Traj class
#'
#' This package uses the Traj class to store trajectory parameters.
#'
#' @param trajectory A vector of unique trajectory identifiers.
#' @param frame A vector of integers corresponding to the image frame of each data point.
#' @param time An integer corresponding to time interval between frames.
#' @param time_unit Name of the unit of time (e.g "second", "s", "min").
#' @param x A numeric vector corresponding to the 'x' coordinates of each data point.
#' @param y A numeric vector corresponding to the 'y' coordinates of each data point.
#' @param dim_x An integer representing the image width (usually in pixels)
#' @param dim_y An integer representing the image height (usually in pixels)
#' @param pixel_size An integer representing the pixel size (usually in microns or nanometers)
#' @param pixel_unit Character(s) for the unit of pixel (e.g. "nanometer", "nm", "micrometer").
#' @param ...
#'
#' @return
#' @export
Traj <- function(trajectory = factor(), frame = integer(), time = numeric(),
                 time_unit = character(), x = numeric(), y = numeric(),
                 dim_x = integer(), dim_y = integer(),
                 pixel_size = numeric(), pixel_unit = character(), ...) {
  ...
  }
#'
#' @examples
#' data(coordinates)
#' traj <- Traj(trajectory = coordinates$Trajectory,
#' frame = coordinates$Frame,
#' time = 0.186,
#' time_unit = "s",
#' x = coordinates$x,
#' y = coordinates$y,
#' dim_x = 512,
#' dim_y = 512,
#' pixel_size = 80.0886,
#' pixel_unit = "nanometer")
#'
Traj <- function(trajectory = factor(), frame = integer(), time = numeric(),
                 time_unit = character(), x = numeric(), y = numeric(),
                 dim_x = integer(), dim_y = integer(),
                 pixel_size = numeric(), pixel_unit = character(), ...) {
  if ((length(x) == 0 || length(y) == 0 || length(dim_x) == 0 ||
       length(dim_y) == 0) &&
      (length(trajectory) != 0 || length(frame) != 0 || length(time) != 0 ||
       length(time_unit) != 0 || length(x) != 0 || length(y) != 0 ||
       length(dim_x) != 0 || length(dim_y) != 0 ||
       length(pixel_size) != 0 || length(pixel_unit) != 0)) {
    stop("Arguments 'x', 'y', 'dim_x' and 'dim_y' must all be initialized
         if at least one other argument is initialized.")
  }
  pixel_unit <- tolower(pixel_unit)
  time_unit <- tolower(time_unit)
  if (length(x) != 0) {
    ## If at least one 'xy' coordinate is provided without assignment for 'frame'
    ## the default number of 'frame' is 1.
    ## Also, frame sequence must start at 1.(Is that essential???)
    if (length(frame) == 0) {
      frame <- rep(1, length(x))
    }
    if (min(frame) != 1) {
      frame <- as.integer((frame - min(frame)) + 1)
    }
    if (!is.integer(frame)) {
      frame <- as.integer(frame)
    }
    ## For data with a single frame and no trajectory assignment
    ## every 'xy' coordinate is considered a unique 'trajectory'.
    ## In other words, 'xy' coordinates becomes points of a single static image.
    if (length(unique(frame)) == 1 && length(trajectory)== 0) {
      trajectory <- as.factor(seq_along(x))
    }
    if (length(trajectory)!= 0 && !is.factor(trajectory)) {
      trajectory <- as.factor(trajectory)
    }
    ## Convert dim_x and dim_y to integer
    if (length(dim_x) != 0 && !is.integer(dim_x)) {
      dim_x <- as.integer(dim_x)
    }
    if (length(dim_y) != 0 && !is.integer(dim_y)) {
      dim_y <- as.integer(dim_y)
    }
    ## Calculate slot time from time interval between frames
    ## The argument time can also accept a vector of length(x) if it respects
    ## the validity assumptions for the slot
    if (length(time) == 1) {
      time <- (frame - min(frame)) * time
    }
    if (length(time) == 0) {
      time <- rep(0, length(x))
    }
  }
  .Traj(trajectory = trajectory, frame = frame, time = time,
        x = x, y = y, time_unit = time_unit, dim_x = dim_x,
        dim_y = dim_y, pixel_size = pixel_size,
        pixel_unit = pixel_unit, ...)
  }
### End of 'Traj' constructor --------------------------------------------------


## Class 'TrajEucl' definition -------------------------------------------------
.TrajEucl <-
  setClass("TrajEucl",
           representation (min_distance = "numeric"),
           contains = "Traj",
           validity = function(object) {
             errors <- character()

             ## 'min_distance' must have the same length as 'trajectory',
             ## "frame', 'time', 'x' and 'y'
             if (length(object@min_distance) != length(object@x)) {
               msg <- paste("'min_distance' does not have the proper length.")
               errors <- c(errors, msg)
             }
             if (length(errors) == 0) TRUE else errors
           }
  )
### End of class 'TrajEucl' ----------------------------------------------------
## Constructor function for class 'TrajEucl' -----------------------------------
TrajEucl <- function(trajectory = factor(),
                     frame = integer(),
                     time = numeric(),
                     time_unit = character(),
                     x = numeric(),
                     y = numeric(),
                     min_distance = numeric(),
                     dim_x = integer(),
                     dim_y = integer(),
                     pixel_size = numeric(),
                     pixel_unit = character(), ...) {

  .TrajEucl(Traj(trajectory = trajectory,
                 frame = frame,
                 time = time,
                 x = x,
                 y = y,
                 time_unit = time_unit,
                 dim_x = dim_x,
                 dim_y = dim_y,
                 pixel_size = pixel_size,
                 pixel_unit = pixel_unit),
            min_distance = min_distance, ...)
}
### End of constructor for class 'TrajEucl' ------------------------------------

## is.Traj ---------------------------------------------------------------------
is.Traj <- function (x) is(x, "Traj")

