library(ggplot2)
library(rgl)
library(mgcv)

dataCZKM <- readRDS("/home/sheridan/R/astrophysics/k-corrections/data/czkm_catalog_final_150114-4.rds")
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

dataDeep2 <- readRDS("/home/sheridan/R/astrophysics/k-corrections/data/tab_deep2_corr.rds")

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

calcK <- function(firstMag, secondMag, z)
{
  a<-matrix(c(-2.45204,4.10188,10.5258,-13.5889,
              56.7969,-140.913,144.572,57.2155,
              -466.949,222.789,-917.46,-78.0591,
              2906.77,1500.8,1689.97,30.889,
              -10453.7,-4419.56,-1011.01,0,
              17568,3236.68,0,0,
              -10820.7,0,0,0), nrow=4)
  a<-t(a)
  print(a)
  kcor = 0.0
  for(x in 1:4)
  {
    for(y in 1:7)
    {
      kcor = kcor + a[y,x] * z^y * (firstMag-secondMag)^x
    }
  }
  return(kcor)
}

draw4DDifference <- function(drawData,firstMag,secondMag,correction,mov=FALSE)
{
  colorsCount = length(drawData$ra)
  drawData$radec = drawData$zerr #sprintf("%s:%s", drawData$ra, drawData$dec)
  drawData$colors=getGradient(colorsCount)[rank(drawData$radec,ties.method="first")]
  
  labels=drawData[order(drawData$radec), ]$radec[seq(1, colorsCount, length.out = 10)]
  labelsColors=drawData[order(drawData$radec), ]$colors[seq(1, colorsCount, length.out = 10)]
  
  open3d(windowRect = c(0, 0, 800, 600), zoom = 1.1)  
  plot3d(drawData[[firstMag]]-drawData[[secondMag]], drawData[["z"]], drawData[[correction]], 
         type="p", col=drawData$colors, 
         xlab=sprintf("%s-%s", firstMag, secondMag), ylab="Z", zlab=correction,
         site=5, lwd=15, alpha=0.5, size=2)
  legend3d("topright", legend=labels, pch = 16, col=labelsColors)
  if(mov)
  {
    movie3d( spin3d(rpm=12), duration=5, dir="/data/sheridan/R/astro/k-corrections/out/", 
             movie = sprintf("%s-%s__%s", firstMag, secondMag, correction), fps=24 )
    rgl.close()
  }
}

sampleCZKM <- subset(sampleData, corrmag_g-corrmag_r > -0.1 & corrmag_g-corrmag_r < 2 & zerr>0 & zerr<0.001)
sampleCZKM$myK = calcK(sampleCZKM$corrmag_g, sampleCZKM$corrmag_r, sampleCZKM$z)
#draw4DDifference(sampleCZKM, "corrmag_g", "corrmag_r", "kcorr_g")
draw4DDifference(sampleCZKM, "corrmag_g", "corrmag_r", "myK")
