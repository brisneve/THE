extract.raster.cortad <- function(nc, var, ext, fxn){
  if(!require(raster)){install.packages("raster"); library(raster)}
  rs <- crop(brick(nc, varname=var),extent(ext), filename=var, datatype=NULL, overwrite=T)
  rs1 <- calc(rs, fun= function(x) {x - 273.15})
  r <- calc(rs1, fun= fxn, na.rm = TRUE)
  r
}
