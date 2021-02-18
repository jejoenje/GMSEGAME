
plot_pop = function(dat, yrange = 10, track_range = TRUE, extinction_message = FALSE) {
  obs = dat[,"obs"]
  t = 1:nrow(dat)
  if(length(obs)<yrange) {
    obs = c(obs, rep(NA, yrange-length(obs)))
    t = c(t, (tail(t,1)+1):yrange)
  }
  if(length(obs)>yrange & track_range == TRUE) {
    obs = tail(obs, yrange)
    t = tail(t, yrange)
  }
  plot(t, obs, ylim = c(0, max(obs, na.rm=T)*2), 
       type = "b", pch = 21, col = "black", bg = "grey", lwd = 2, xaxt = "n",
       xlab = "Time step", ylab = "Observed resource population size")
  axis(1, at = t)
  points(tail(t[!is.na(obs)],1),tail(obs[!is.na(obs)],1), 
         pch = 21, col = "black", bg = "red", lwd = 2, cex = 1.25)
  
  if(extinction_message == TRUE) text(x = length(obs)/2, 
                                      y = max(obs, na.rm=T)*1.8, 
                                      "Population wiped out!", 
                                      col = "red", cex = 4)
  
}

plot_land = function(x, cols = NULL) {
  # Pick colors
  
  if(is.null(cols)) {
    land_cols = grey.colors(length(table(x)))
    land_cols = sample(land_cols, replace = FALSE)  
  } else {
    land_cols = cols
  }
  
  if(sum(x == 1)>0) land_cols[1] = "#FFFFFF"
  
  image(x = x, col = land_cols, yaxt = "n", xaxt = "n")  

}

placeResources = function(res, xd, yd) {
  land_res = matrix(0, nrow = xd, ncol = yd)
  for(i in 1:nrow(res)) {
    land_res[res[i,5]+1,res[i,6]+1] = land_res[res[i,5]+1,res[i,6]+1]+1
  }
  land_res[land_res==0] = NA
  
  return(land_res)
}

plot_land_res = function(land, resources, cols = NULL, extinction_message = FALSE) {
  plot_land(land[,,3], cols = cols)
  if(extinction_message==FALSE) {
    par(new = T)
    res_positions = placeResources(res = resources, xd = dim(land[,,3])[1], yd = dim(land[,,3])[1])
    image(res_positions, col = "darkred", xaxt = "n", yaxt = "n")  
  }
}