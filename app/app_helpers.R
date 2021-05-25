
list.to.df = function(l) {
  return(data.frame(matrix(unlist(l), nrow=length(l), byrow=T)))
}

plot_pop = function(dat, yield_dat = NULL, yrange = 10, track_range = TRUE, extinction_message = FALSE) {
  obs = dat[,"obs"]
  t = 1:nrow(dat)
  if(length(obs)<yrange) {
    obs = c(obs, rep(NA, yrange-length(obs)))
    t = c(t, (tail(t,1)+1):yrange)
    
    if(!is.null(yield_dat)) {
      yield_dat = rbind(yield_dat, matrix(NA, ncol = ncol(yield_dat), nrow = yrange-nrow(yield_dat)))
    }
    
  }
  if(length(obs)>yrange & track_range == TRUE) {
    obs = tail(obs, yrange)
    t = tail(t, yrange)
    yield_dat = tail(yield_dat, yrange)
  }
  
  #par(mar = c(5,5,2,5))
  
  plot(t, obs, ylim = c(0, max(obs, na.rm=T)*2), 
       type = "b", pch = 21, col = "black", bg = "grey", lwd = 2, xaxt = "n",
       xlab = "Time step", ylab = "Observed population size", cex.axis = 1, cex.lab = 1, main = "Population & farming yield", cex.main = 1.5)
  axis(1, at = t, cex.lab = 1.25)
  points(tail(t[!is.na(obs)],1),tail(obs[!is.na(obs)],1), 
         pch = 21, col = "black", bg = "red", lwd = 2, cex = 2)
  
  if(!is.null(yield_dat)) {
    par(new = T)
    yield_ylim = c(0, round(max(yield_dat, na.rm=T)*1.25,2))
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

plot_traj = function(dat, yield_dat = NULL) {

  dat$t = 1:nrow(dat)
  if(!is.null(yield_dat)) {
    n_yield = ncol(yield_dat)
    names(yield_dat) = paste("Y",1:n_yield,sep="")
    dat = cbind(dat, yield_dat)
  } else {
    n_yield = 0 
  }
  
  ax = list(title = "Time step", 
            showgrid = FALSE, 
            zeroline = FALSE, 
            showline = TRUE, 
            range = c(-1,51), 
            ticks = "outside", 
            mirror = TRUE, 
            linewidth = 1)
  ay = list(title = "Observed population size", 
            showgrid = FALSE, 
            zeroline = FALSE, 
            showline = TRUE, 
            range = c(0,ceiling(max(dat$obs)*1.1)), 
            ticks = "outside", 
            linewidth = 1)
  ay2 = list(title = "Yield (proportion of max)", 
             showgrid = FALSE, 
             zeroline = FALSE, 
             showline = TRUE, 
             overlaying = "y", 
             ticks = "outside", 
             linewidth = 1, 
             side = "right", 
             range = c(-5,60))
  
  p = plot_ly(dat)
  p = add_trace(p, x = ~t, y = ~obs, mode = "lines+markers",
                line = list(color = "#000000"),
                marker = list(
                  color = "#757575",
                  size = 10,
                  line = list(
                    color = "#000000",
                    width = 2)
                )
  )
  p = add_trace(p, x = tail(dat$t,1), y = tail(dat$obs,1),
                marker = list(
                  color = '#FF0000',
                  size = 15,
                  line = list(
                    color = "#000000",
                    width = 3
                  )
                )
  )
  p = layout(p, xaxis = ax, yaxis = ay)
  p
  
   
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
  
  par(oma = c(2,2,2,2))
  par(mar = c(2,2,2,2))
  image(x = x, col = land_cols, yaxt = "n", xaxt = "n", main = "Farming landscape & animal positions", cex.main = 1.5)
  
  # users = as.numeric(names(table(x)))
  # n_users = length(users)
  # for(i in 1:n_users) {
  #   id_pos = which(x == users[i], arr.ind = T)
  #   id_x = id_pos[,"row"]
  #   id_y = id_pos[,"col"]
  #   xpos = min(id_x)+5
  #   ypos = min(id_x)+5
  #   text(x = xpos/100, y = ypos/100, LETTERS[users[i]-1])
  # }
  
}

placeResources = function(res, xd, yd) {
  land_res = matrix(0, nrow = xd, ncol = yd)
  for(i in 1:nrow(res)) {
    land_res[res[i,5]+1,res[i,6]+1] = land_res[res[i,5]+1,res[i,6]+1]+1
  }
  land_res[land_res==0] = NA
  
  return(land_res)
}

plot_land_res = function(land, resources, cols = NULL, extinction_message = FALSE, show_labels = FALSE) {
  plot_land(land[,,3], cols = cols)
  if(extinction_message==FALSE) {
    par(new = T)
    res_positions = placeResources(res = resources, xd = dim(land[,,3])[1], yd = dim(land[,,3])[1])
    image(res_positions, col = "darkred", xaxt = "n", yaxt = "n")
    
    if(show_labels == TRUE) {
      users = as.numeric(names(table(land[,,3])))
      n_users = length(users)
      for(i in 1:n_users) {
        poss = which(land[,,3]==users[i], arr.ind = T)
        #points(poss[,1]/100,poss[,2]/100,col="red",pch=16)
        xpos = mean(poss[,1])/100
        ypos = mean(poss[,2])/100
        points(x = xpos, y = ypos, pch = 21, col = alpha("black",0.75), bg = alpha("white",0.75), cex = 4)
        text(x = xpos, y = ypos, LETTERS[users[i]-1], col = alpha("black",0.75), cex = 1)
      }
    }
    
  }
}