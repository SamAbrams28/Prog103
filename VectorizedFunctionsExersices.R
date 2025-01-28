cylinder_volume <- function (radius, height){
  result <- pi * radius^2 * height / 2
  return(result)
}

cylinder_volume(2 , 3)
cylinder_volume(1, 4)

radii <- c(1, 2, 3)
heights <- c(3, 2, 1)
cylinder_volume(radii, heights)
