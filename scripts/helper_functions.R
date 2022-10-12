
#function that takes a BBOX and splits it into tiles (defined by n_x and n_y)
# returns a sf dataframe with n_x * n_y tiles, were each tile is a polygon

divide_raster_poly <- function(ext, n_x, n_y){
  
  x_vals <- seq(ext[1], ext[3], length.out = n_x+1)
  y_vals <- seq(ext[2], ext[4], length.out = n_y+1)
  
  x_vals_list <- x_vals[c(1, rep(2:n_x, each = 2), n_x+1)] %>% split(rep(1:n_x, each = 2))
  y_vals_list <- y_vals[c(1, rep(2:n_y, each = 2), n_y+1)] %>% split(rep(1:n_y, each = 2))
  
  crossing(x_vals_list, y_vals_list) %>% 
    apply(1 , function(x) extent(x$x_vals_list, x$y_vals_list)) %>% 
    lapply(function(x) {st_bbox(x) %>% st_as_sfc() %>% st_sf()}) %>% 
    do.call("rbind", .)
  
}
