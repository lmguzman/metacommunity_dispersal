library(SpaDES.tools)
library(raster)

x <- 5
y <- 5
patches <- x*y
landscape_df <- data.frame(patch_id = 1:patches, x_cor = rep(1:x, y), y_cor = rep(1:y, each = x))
coor <- coordinates(landscape_df[,2:3])
colnames(coor) <- c("x", "y")
new_coor <- wrap(coor, extent(coor))
distance_all <- as.matrix(dist(new_coor, upper = TRUE, diag = TRUE))

write.csv(distance_all,"model/distance_all.csv", row.names = FALSE)


