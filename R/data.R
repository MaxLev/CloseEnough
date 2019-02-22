#' Sequence of RGB images illustrating the movement of Panx2 foci (in green)
#' and mitochondria (in red).
#'
#' Frames were acquired every 0.186s. Pixel size is 80.0886 nanometer.
#'
#' @format A stack of 512x512 pixel tiff images. This stack contains 3 channels
#' and 50 frames.
#'
"cellmovie"

#' A thresholded binary image representing the area covered by the cell
#' shown in the object 'cellmovie'.
#'
#' White pixel constitutes a mask representing the area covered
#' by the cell. Pixel size is 80.0886 nanometer.
#'
#' @format A 512x512 pixel tiff image. This image contains a single frame.
#'
"cellmask"

#' A stack of thresholded binary images representing the area covered by the mitochondria
#' in the cell shown in the object 'cellmovie'.
#'
#' White pixel constitutes a mask representing the area covered
#' by the mitochondria in each frame. Pixel size is 80.0886 nanometer.
#'
#' @format A stack of 512x512 pixel tiff images. This stack contains a single channel
#' and 50 frames.
#'
"mitomask"

#' A data frame containing the x,y coordinates of the outline of the cell shown
#' in the object 'cellmovie'.
#'
#' @format A data frame with 654 rows and 2 variables.
#' @describe{
#'   \item{x}{x coordinates, in pixel}
#'   \item{y}{y coordinates, in pixel}
#'   ...
#' }
#'
"outline"

#' A data frame containing the trajectory coordinates of the Panx2 foci shown
#' in the object 'cellmovie'.
#'
#' @format A data frame with 5066 rows and 4 variables.
#' @describe{
#'   \item{Trajectory}{The trajectory ID}
#'   \item{Frame}{The frame number for these coordinates}
#'   \item{x}{x coordinates, in pixel}
#'   \item{y}{y coordinates, in pixel}
#'   ...
#' }
#'
"coordinates"

#' An object of class Traj containing the trajectory parameters of the Panx2 foci shown
#' in the object 'cellmovie'.
#'
#' @format An object of class Traj containing the trajectory coordinates as well as
#' additional information.
#' @describe{
#'   \item{Trajectory}{The trajectory ID}
#'   \item{Frame}{The frame number for these coordinates}
#'   \item{time}{time between frame}
#'   \item{time_unit}{unit for slot time}
#'   \item{x}{x coordinates, in pixel}
#'   \item{y}{y coordinates, in pixel}
#'   \item{dim_x}{The width, in pixel, of the reference image}
#'   \item{dim_y}{The height, in pixel, of the reference image}
#'   \item{pixel_size}{The physical size of a pixel}
#'   \item{pixel_unit}{unit for slot pixel_size}
#'   ...
#' }
#'
"coordinates"
