library(ggplot2)
library(rgl)
library(mgcv)

dataCZKM <- readRDS("/home/sheridan/R/astro/czkm_catalog_final_150114-4.rds")
dataCZKM <- dataCZKM[dataCZKM$corrmag_j < 1000,]
dataCZKM <- dataCZKM[dataCZKM$corrmag_h < 1000,]
dataCZKM <- dataCZKM[dataCZKM$corrmag_k < 1000,]
sampleData<-subset(dataCZKM, 
                  !is.na(ra) & !is.na(dec) & 
                  !is.na(kcorr_u) & !is.na(kcorr_g) & !is.na(kcorr_i) & !is.na(kcorr_z) &
#                  !is.na(kcorr_y) & !is.na(kcorr_j) & !is.na(kcorr_h) & !is.na(kcorr_k) &
#                  !is.na(kcorr_fuv) & !is.na(kcorr_nuv) &
                  !is.na(corrmag_u) & !is.na(corrmag_g) & !is.na(corrmag_i) & !is.na(corrmag_z) &
#                  !is.na(corrmag_y) & !is.na(corrmag_j) & !is.na(corrmag_h) & !is.na(corrmag_k) &
#                  !is.na(corrmag_fuv) & !is.na(corrmag_nuv) &
                  !is.na(z))

dataDeep2 <- readRDS("/home/sheridan/R/astro/tab_deep2_corr.rds")

#sampleCZKM <- dataCZKM[sample(nrow(dataCZKM), 100), ] # выборка 100 случайных

getGradient <- colorRampPalette(c("black", "blue", "green", "red"))

draw4D <- function(drawData,x,y,z,col,mov=FALSE)
{
    open3d(windowRect = c(0, 0, 800, 600), zoom = 1.1)
    #print(fields[,i])
    #print(drawData[fields[3,i]][[1]])
    #colorGradient=rainbow(length(unique(abs(floor((drawData[[fields[3,i]]]+10)*10)))))
    
    colorsCount = length(drawData[[col]])
    drawData$colors=getGradient(colorsCount)[rank(drawData[[col]],ties.method="first")]
    #print(colorGradient)
    plot3d(drawData[[x]], drawData[[y]], drawData[[z]], 
           type="p", col=drawData$colors, 
           xlab=x, ylab=y, zlab=z,
           site=5, lwd=15, alpha=0.5, size=2)
    labels=drawData[order(drawData[[col]]), ][[col]][seq(1, colorsCount, length.out = 10)]
    labelsColors=drawData[order(drawData[[col]]), ]$colors[seq(1, colorsCount, length.out = 10)]
    legend3d("topright", legend=round(labels, 3), pch = 16, 
            col=labelsColors)
    if(mov)
    {
      movie3d( spin3d(rpm=12), duration=5, dir="/home/sheridan/R/astro/k-corrections/out/", 
               movie = sprintf("%s__%s__%s__%s", x, y, z, col), fps=24 )
      rgl.close()
    }
}

draw4DDifference <- function(drawData,firstMag,secondMag,correction,col,mov=FALSE)
{
  open3d(windowRect = c(0, 0, 800, 600), zoom = 1.1)
  #print(fields[,i])
  #print(drawData[fields[3,i]][[1]])
  #colorGradient=rainbow(length(unique(abs(floor((drawData[[fields[3,i]]]+10)*10)))))
  
  colorsCount = length(drawData[[col]])
  drawData$colors=getGradient(colorsCount)[rank(drawData[[col]],ties.method="first")]
  #print(colorGradient)
  plot3d(drawData[[firstMag]]-drawData[[secondMag]], drawData[["z"]], drawData[[correction]], 
         type="p", col=drawData$colors, 
         xlab=sprintf("%s-%s", firstMag, secondMag), ylab=correction, zlab="Z",
         site=5, lwd=15, alpha=0.5, size=2)
  labels=drawData[order(drawData[[col]]), ][[col]][seq(1, colorsCount, length.out = 10)]
  labelsColors=drawData[order(drawData[[col]]), ]$colors[seq(1, colorsCount, length.out = 10)]
  legend3d("topright", legend=round(labels, 5), pch = 16, 
           col=labelsColors)
  if(mov)
  {
    movie3d( spin3d(rpm=12), duration=5, dir="/home/sheridan/R/astro/k-corrections/out/", 
             movie = sprintf("%s__%s__%s__%s", firstMag, secondMag, correction, col), fps=24 )
    rgl.close()
  }
}



draw3DDifference <- function(drawData,firstMag,secondMag,correction,mov=FALSE)
{
  open3d(windowRect = c(0, 0, 800, 600), zoom = 1.1)
  plot3d(drawData[[firstMag]]-drawData[[secondMag]], drawData[["z"]], drawData[[correction]], 
         type="p", col="red", 
         xlab=sprintf("%s-%s", firstMag, secondMag), ylab=correction, zlab="Z",
         site=5, lwd=15, alpha=0.5, size=2)
  if(mov)
  {
    movie3d( spin3d(rpm=12), duration=5, dir="/home/sheridan/R/astro/k-corrections/out/", 
             movie = sprintf("%s__%s__%s__%s", firstMag, secondMag, correction, col), fps=24 )
    rgl.close()
  }
}


sampleCZKM <- subset(sampleData, corrmag_g-corrmag_r > -0.1 & corrmag_g-corrmag_r < 2)
draw4DDifference(sampleCZKM, "corrmag_g", "corrmag_r", "kcorr_g", "dec")
draw4DDifferenceRaDec(sampleCZKM, "corrmag_g", "corrmag_r", "kcorr_g")
draw3DDifference(sampleCZKM, "corrmag_g", "corrmag_r", "kcorr_r")
draw4D(sampleCZKM, "corrmag_u", "corrmag_r", "z", "kcorr_u")
draw4D(sampleCZKM, "corrmag_r", "corrmag_z", "z", "kcorr_z")

draw4DDifferenceRaDec <- function(drawData,firstMag,secondMag,correction,mov=FALSE)
{
  open3d(windowRect = c(0, 0, 800, 600), zoom = 1.1)
  #print(fields[,i])
  #print(drawData[fields[3,i]][[1]])
  #colorGradient=rainbow(length(unique(abs(floor((drawData[[fields[3,i]]]+10)*10)))))
  
  colorsCount = length(drawData$ra)
  drawData$radec = sprintf("%s:%s", drawData$ra, drawData$dec)
  drawData$colors=getGradient(colorsCount)[rank(drawData$radec,ties.method="first")]
  #print(colorGradient)
  plot3d(drawData[[firstMag]]-drawData[[secondMag]], drawData[["z"]], drawData[[correction]], 
         type="p", col=drawData$colors, 
         xlab=sprintf("%s-%s", firstMag, secondMag), ylab=correction, zlab="Z",
         site=5, lwd=15, alpha=0.5, size=2)
  labels=drawData[order(drawData$radec), ]$radec[seq(1, colorsCount, length.out = 10)]
  labelsColors=drawData[order(drawData$radec), ]$colors[seq(1, colorsCount, length.out = 10)]
  legend3d("topright", legend=labels, pch = 16, 
           col=labelsColors)
  if(mov)
  {
    movie3d( spin3d(rpm=12), duration=5, dir="/home/sheridan/R/astro/k-corrections/out/", 
             movie = sprintf("%s__%s__%s__%s", firstMag, secondMag, correction, col), fps=24 )
    rgl.close()
  }
}

draw4DDifferenceRaDec(sampleCZKM, "corrmag_u", "corrmag_r", "kcorr_u")
