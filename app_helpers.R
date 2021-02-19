
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
  
  par(mar = c(5,5,2,1.5))
  
  plot(t, obs, ylim = c(0, max(obs, na.rm=T)*2), 
       type = "b", pch = 21, col = "black", bg = "grey", lwd = 2, xaxt = "n",
       xlab = "Time step", ylab = "Observed population size", cex.axis = 1.5, cex.lab = 2)
  axis(1, at = t, cex.lab = 1.5)
  points(tail(t[!is.na(obs)],1),tail(obs[!is.na(obs)],1), 
         pch = 21, col = "black", bg = "red", lwd = 2, cex = 2)
  
  if(extinction_message == TRUE) text(x = length(obs)/2, 
                                      y = max(obs, na.rm=T)*1.8, 
                                      "Population wiped out!", 
                                      col = "#D35E60", cex = 3.5)
  
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
  
  par(mar = c(5,2,2,2))
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