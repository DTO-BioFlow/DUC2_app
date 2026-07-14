#library(ncdf4)
#library(terra) # for test
box::use(ncdf4[nc_open, ncvar_get, nc_close])


# auxillary matrix function 
flip_yaxis <- function(mxy) {
  for (iy in 1:dim(mxy)[2]){
      mxy[,iy] <- rev(mxy[,iy])
      }
  mxy
  }
     

# --------------------------------------------------------------------
# This function loads a SB4S data set on regular 4326 grids
# grid sampling is (lon,lat,time)
# XYT variables (F,U,S,B) are feeding, outbound,spawning and backbound
# with N=F+U+S+B; XY variables with suffix tavg are time-averaged variables
# --------------------------------------------------------------------
load_SB4S_simout <- function(fpath) {
   nc   <- nc_open(fpath)
   lon  <- ncvar_get(nc, "lon")
   lat  <- ncvar_get(nc, "lat")
   time <- ncvar_get(nc, "time")
   nx      <- dim(lon)[1]
   ny      <- dim(lat)[1]
   nframes <- dim(time)[1]
   F   <- ncvar_get(nc, "F") # dim(F) = nx,ny,nframes
   U   <- ncvar_get(nc, "U")
   S   <- ncvar_get(nc, "S")
   B   <- ncvar_get(nc, "B")
   N   <- F + U + S + B
   nc_close(nc)
   Ftavg <- rowMeans(F, dims=2) # mean is over dimensions ‘dims+1, ...’
   Utavg <- rowMeans(U, dims=2)
   Stavg <- rowMeans(S, dims=2)
   Btavg <- rowMeans(B, dims=2)
   Ntavg <- Ftavg + Utavg + Stavg + Btavg
   # return last evaluated expression 
   list(lon=lon, lat=lat, time=time,
        F=F, U=U, S=S, B=B, N=N, 
	Ftavg=Ftavg, Utavg=Utavg, Stavg=Stavg, Btavg=Btavg, Ntavg=Ntavg)
   }

#data  <- load_SB4S_simout("../../../data/sb4s_simout_52w.nc")
#layer <- rast(data$Ntavg, noflip=TRUE) # 
#plot(data$Ntavg)