
plot_pop = function(dat, yrange = 10, track_range = TRUE) {
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
}
