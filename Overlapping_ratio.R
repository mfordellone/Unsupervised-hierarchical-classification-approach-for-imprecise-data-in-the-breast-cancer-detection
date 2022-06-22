# --------------------------------------------------#
#				                    #
#  Overlapped Interval Divergence                   #
#  Author: Dr. Mario Fordellone                     #
#  Reference: Kabir S. et al (2017)                 #                            
#  https://doi.org/# 10.1109/FUZZ-IEEE.2017.8015623 #
#                                                   #
# --------------------------------------------------#
#' 
#' @param Xc center matrix
#' @param Xr radius matrix

OLIDm <- function(Xc, Xr){
  n <- nrow(Xc)
  J <- ncol(Xc)
  o_dist <- matrix(data = 0, nrow = n*2, ncol = J)
  for (i in 1:n){
    for (j in 1:J){
      Ca <- Xc[ii,j]
      Cb <- Xc[i,j]
      Ra <- Xr[ii,j]
      Rb <- Xr[i,j]
      if (abs(Ca-Cb)<=Rb-Ra){
        o_dist[i,j] <- 0
      }
      if (abs(Ca-Cb)<=Ra-Rb){
        o_dist[i,j] <- (abs(Ca-Cb)+Ra-Rb)*(1-((2*Rb)/(2*Ra+1)))
      }
      if (Ra==Rb && Rb==0){
        o_dist[i,j] <- abs(Ca-Cb)
      }
      if (abs(Ra-Rb)<abs(Ca-Cb) && abs(Ca-Cb)<Ra+Rb){
        o_dist[i,j] <- (abs(Ca-Cb)+Ra-Rb)*(1-((Ra+Rb-abs(Ca-Cb))/(2*Ra+1)))
      }
      if (abs(Ca-Cb)>=Ra+Rb){
        o_dist[i,j] <- (abs(Ca-Cb)+Ra-Rb)*(1+((abs(Ca-Cb)-(Ra+Rb))/(2*Ra+1)))
      }
      Ca <- Xc[i,j]
      Cb <- Xc[ii,j]
      Ra <- Xr[i,j]
      Rb <- Xr[ii,j]
      if (abs(Ca-Cb)<=Rb-Ra){
        o_dist[,j] <- 0
      }
      if (abs(Ca-Cb)<=Ra-Rb){
        o_dist[(n+i),j] <- (abs(Ca-Cb)+Ra-Rb)*(1-((2*Rb)/(2*Ra+1)))
      }
      if (Ra==Rb && Rb==0){
        o_dist[(n+i),j] <- abs(Ca-Cb)
      }
      if (abs(Ra-Rb)<abs(Ca-Cb) && abs(Ca-Cb)<Ra+Rb){
        o_dist[(n+i),j] <- (abs(Ca-Cb)+Ra-Rb)*(1-((Ra+Rb-abs(Ca-Cb))/(2*Ra+1)))
      }
      if (abs(Ca-Cb)>=Ra+Rb){
        o_dist[(n+i),j] <- (abs(Ca-Cb)+Ra-Rb)*(1+((abs(Ca-Cb)-(Ra+Rb))/(2*Ra+1)))
      }
    }
  }
  dist <- matrix(data = 0, nrow = n, ncol = 1)
  for (i in 1:n){
    dist[i] <- sqrt(sum((o_dist[i,]+o_dist[(n+i),])^2))
  }
  results <- list()
  results$o_dist <- o_dist
  results$dist   <- dist 
  return(results)
}
