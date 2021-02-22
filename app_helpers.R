
list.to.df = function(l) {
  return(data.frame(matrix(unlist(l), nrow=length(l), byrow=T)))
}

plot_pop = function(dat, yield_dat = NULL, yrange = 10, track_range = TRUE, extinction_message = FALSE) {
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
  
  par(mar = c(5,5,2,5))
  
  plot(t, obs, ylim = c(0, max(obs, na.rm=T)*2), 
       type = "b", pch = 21, col = "black", bg = "grey", lwd = 2, xaxt = "n",
       xlab = "Time step", ylab = "Observed population size", cex.axis = 1.5, cex.lab = 2)
  axis(1, at = t, cex.lab = 1.5)
  points(tail(t[!is.na(obs)],1),tail(obs[!is.na(obs)],1), 
         pch = 21, col = "black", bg = "red", lwd = 2, cex = 2)
  
  if(!is.null(yield_dat)) {
    par(new = T)
    yield_ylim = c(0, round(max(yield_dat)*1.25,2))
    #yield_pos = seq(yield_ylim[1],yield_ylim[2],(yield_ylim[2]-yield_ylim[1])/6)
    #yield_labs = yield_pos*100
    plot(yield_dat[,1], type = "n", col = "darkgreen", xaxt = "n", yaxt = "n", xlab = "", ylab = "", ylim = yield_ylim)
    apply(yield_dat, 2, function(x) lines(x, col = "darkgreen"))
    #axis(4, at = yield_pos, labels = yield_labs, col.ticks = "darkgreen")
    axis(4, col.ticks = "darkgreen")
    mtext("Yield %",side = 4,cex = 2, line = 3, col = "darkgreen")
    abline(h=1, col = "darkgreen", lty = "dashed")
  }
  
  # The below needs amending for when yield data is added.
  if(extinction_message == TRUE) {
    if(!is.null(yield_dat)) {
      ext_mess_pos = max(yield_dat)*1.1
    } else {
      ext_mess_pos = max(obs, na.rm=T)*1.8
    }

    text(x = length(obs)/2, 
         y = ext_mess_pos,
         "Population wiped out!", 
         col = "#D35E60", cex = 2.5)
  }
  
}

### Takes a list of GMSE apply objects and extracts the mean or total yields across all users
get_init_yields = function(x, type = "mean") {
  if(type == "mean") {
    mn_ylds = lapply(x, function(x) tapply(x$LAND[,,2], x$LAND[,,3],mean))
    mn_ylds = list.to.df(mn_ylds)
    return(mn_ylds)
  }
}

# Takes a GMSE apply object (new_yields), calculates yields per user, and appends to existint "yields"
#  table as output by get_init_yields().
add_yields = function(new_yields, yields) {
  rbind(yields, tapply(new_yields$LAND[,,2],new_yields$LAND[,,3],mean))
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