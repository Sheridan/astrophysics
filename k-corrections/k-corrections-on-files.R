library(ggplot2)
library(rgl)
library(mgcv)


#udata <- read.table("/home/sheridan/R/astro/czkm_catalog_final_150114-3.csv", header=TRUE, sep=",", dec=".", comment.char="#")
dataCZKM <- readRDS("/home/sheridan/R/astro/czkm_catalog_final_150114-4.rds")
dataCZKM <- dataCZKM[dataCZKM$corrmag_j < 1000,]
dataCZKM <- dataCZKM[dataCZKM$corrmag_h < 1000,]
dataCZKM <- dataCZKM[dataCZKM$corrmag_k < 1000,]

dataDeep2 <- readRDS("/home/sheridan/R/astro/tab_deep2_corr.rds")


differenceData <- data.frame(type=character(), difference=numeric(), Z=numeric(), stringsAsFactors = FALSE)
differenceData<-rbind(differenceData, data.frame(type=c("u-g"), difference=c(dataDeep2$CORRMAG_U-dataDeep2$CORRMAG_G),Z=c(dataDeep2$Z)))
differenceData<-rbind(differenceData, data.frame(type=c("g-r"), difference=c(dataDeep2$CORRMAG_G-dataDeep2$CORRMAG_R),Z=c(dataDeep2$Z)))
differenceData<-rbind(differenceData, data.frame(type=c("r-i"), difference=c(dataDeep2$CORRMAG_R-dataDeep2$CORRMAG_I),Z=c(dataDeep2$Z)))
differenceData<-rbind(differenceData, data.frame(type=c("i-z"), difference=c(dataDeep2$CORRMAG_I-dataDeep2$CORRMAG_Z),Z=c(dataDeep2$Z)))

# цвета в 3d
plot3d(dataDeep2$CORRMAG_U, dataDeep2$CORRMAG_G, dataDeep2$Z, 
       xlim=c(20,30), ylim=c(20,30), zlim=c(0.1,1.5),
       type="p", col="green", 
       xlab="u", ylab="g", zlab="Z", 
       lwd=15, alpha=0.5, size=1, width=1)
plot3d(dataDeep2$CORRMAG_G, dataDeep2$CORRMAG_R, dataDeep2$Z, 
       xlim=c(20,30), ylim=c(20,30), zlim=c(0.1,1.5),
       type="p", col="red", 
       xlab="r", ylab="g", zlab="Z",
       site=5, lwd=15, alpha=0.5, size=2)
plot3d(dataDeep2$CORRMAG_R, dataDeep2$CORRMAG_I, dataDeep2$Z, 
       xlim=c(20,30), ylim=c(20,30), zlim=c(0.1,1.5),
       type="p", col="blue", 
       xlab="r", ylab="i", zlab="Z",
       site=5, lwd=15, alpha=0.5, size=2)
plot3d(dataDeep2$CORRMAG_Z, dataDeep2$CORRMAG_I, dataDeep2$Z, 
       xlim=c(20,30), ylim=c(20,30), zlim=c(0.1,1.5),
       type="p", col="magenta", 
       xlab="z", ylab="i", zlab="Z",
       site=5, lwd=15, alpha=0.5, size=2)
legend3d("topright", legend=c("u-g", "g-r", "r-i", "z-i"), pch = 16, col=c("green", "red", "blue", "magenta"), cex=1, inset=c(0.02))

# Цвет+корректировка
plot3d(dataDeep2$CORRMAG_I, dataDeep2$CORRMAG_G, dataDeep2$Z, 
       xlim=c(20,30), ylim=c(20,30), zlim=c(0.1,1.5),
       type="p", col="red", 
       xlab="i", ylab="g", zlab="Z",
       site=5, lwd=15, alpha=0.5, size=2)

# rainbows Deep2
rnbow=rainbow(length(unique(abs(floor(dataDeep2$Z*10)))))
plot3d(dataDeep2$CORRMAG_R, dataDeep2$CORRMAG_U, dataDeep2$KCORR_R_U, 
       xlim=c(15,35), ylim=c(15,35), zlim=c(-10,10),
       type="p", col=rnbow[abs(floor(dataDeep2$Z*10))], 
       xlab="r", ylab="u", zlab="kcorr r-u",
       site=5, lwd=15, alpha=0.5, size=2)

plot3d(dataDeep2$KCORR_I_G, dataDeep2$CORRMAG_I, dataDeep2$CORRMAG_G, 
       xlim=c(-10,10), ylim=c(15,35), zlim=c(15,35),
       type="p", col=rnbow[abs(floor(dataDeep2$Z*10))], 
       xlab="kcorr i-g", ylab="i", zlab="g",
       site=5, lwd=15, alpha=0.5, size=2)

plot3d(dataDeep2$KCORR_Z_G, dataDeep2$CORRMAG_Z, dataDeep2$CORRMAG_G, 
       xlim=c(-10,10), ylim=c(15,35), zlim=c(15,35),
       type="p", col=rnbow[abs(floor(dataDeep2$Z*10))], 
       xlab="kcorr z-g", ylab="z", zlab="g",
       site=5, lwd=15, alpha=0.5, size=2)

legend3d("topright", legend=sort(unique(abs(floor(dataDeep2$Z*10)))/10), pch = 16, col=rnbow[sort(unique(abs(floor(dataDeep2$Z*10))))], cex=1, inset=c(0.02))
movie3d( spin3d(rpm=12), duration=5, dir="R/astro/k-corrections/out/", movie = "z-g", fps=24 )

# czkm с разницами цвета
# Притянуть данные с z>0.6 и попытаться отфиттить
plot3d(dataCZKM$corrmag_g-dataCZKM$corrmag_r, dataCZKM$z, dataCZKM$kcorr_g, 
       type="p", col="blue", 
       xlab="corrmag_g-corrmag_r", ylab="Z", zlab="kcorr_g",
       site=5, lwd=15, alpha=0.5, size=2)

# - czkm с разницами цвета

# графики из статьи ---- mgcv говорят хорош для фиттинга
cleanCZKM<-subset(dataCZKM, !is.na(z)& 
                    !is.na(corrmag_u) & !is.na(corrmag_g) & !is.na(corrmag_r)& !is.na(corrmag_r) & !is.na(corrmag_z) &
                    !is.na(kcorr_u) & !is.na(kcorr_g) & !is.na(kcorr_r) & !is.na(kcorr_i) & !is.na(kcorr_z) &
                    (corrmag_r-corrmag_z)>=-0.1 & (corrmag_r-corrmag_z)<=1.5)
ggplot() +
  geom_point(data=cleanCZKM, aes(x=z, y=kcorr_z, colour=corrmag_r-corrmag_z), size=1) +
  #geom_point() + stat_smooth(data=cleanCZKM,aes(x=z, y=kcorr_u), method="gam", formula = y~s(x,bs='ps')) +
  scale_colour_hue(l=50) +
  scale_colour_gradientn(colours=a(32)) +
  theme(axis.title = element_blank()) +
  theme(axis.ticks = element_blank(), axis.text = element_text()) + 
  theme(panel.border = element_blank()) + 
  theme(panel.grid.major = element_line(colour="darkgrey")) + 
  ggtitle("sdss z") + 
  theme(plot.title = element_text(size = rel(2))) 
  
ggsave(file="R/astro/k-corrections/out/grj.png", scale=2)
# графики из статьи
# czkm проба поверхностей с 4 измерениями
fields <- cbind(c("corrmag_u", "corrmag_r", "kcorr_u"), 
                c("corrmag_g", "corrmag_r", "kcorr_g"),
                c("corrmag_g", "corrmag_r", "kcorr_r"),
                c("corrmag_g", "corrmag_i", "kcorr_i"),
                c("corrmag_r", "corrmag_z", "kcorr_z"))
for(i in 1:3)
{
  open3d(windowRect = c(0, 0, 640, 480), zoom = 1.1)
  print(fields[,i])
  #print(dataCZKM[fields[3,i]][[1]])
  rnbow=rainbow(length(unique(abs(floor((dataCZKM[[fields[3,i]]]+10)*10)))))
  #print(rnbow)
  plot3d(dataCZKM[[fields[1,i]]], dataCZKM[[fields[2,i]]], dataCZKM$z, 
         type="p", col=rnbow[abs(floor((dataCZKM[[fields[3,i]]]+10)*10))], 
         xlab=fields[1,i], ylab=fields[2,i], zlab="Z",
         site=5, lwd=15, alpha=0.5, size=2)
  legend3d("topright", legend=c(-10:10), pch = 16, 
           col=rnbow[c(0:10)*20], cex=1, inset=c(0.02))
  movie3d( spin3d(rpm=12), duration=5, dir="/home/sheridan/R/astro/k-corrections/out/", 
           movie = sprintf("%s__%s__%s__Z", fields[1,i], fields[2,i], fields[3,i]), fps=24 )
  rgl.close()
}


rnbow=rainbow(length(unique(abs(floor((dataCZKM$kcorr_g+10)*10)))))
plot3d(dataCZKM$corrmag_g, dataCZKM$corrmag_r, dataCZKM$z, 
       type="p", col=rnbow[abs(floor((dataCZKM$kcorr_g+10)*10))], 
       xlab="g", ylab="r", zlab="kcorr_g",
       site=5, lwd=15, alpha=0.5, size=2)
# - czkm проба поверхностей с 4 измерениями
# czkm
plot3d(dataCZKM$kcorr_u, dataCZKM$corrmag_u, dataCZKM$z, 
       type="p", col="blue", 
       xlab="kcorr u", ylab="u", zlab="Z",
       site=5, lwd=15, alpha=0.5, size=2)
plot3d(dataCZKM$kcorr_g, dataCZKM$corrmag_g, dataCZKM$z, 
       type="p", col="#44C419", 
       xlab="kcorr g", ylab="g", zlab="Z",
       site=5, lwd=15, alpha=0.5, size=2)
plot3d(dataCZKM$kcorr_r, dataCZKM$corrmag_r, dataCZKM$z, 
       type="p", col="#FF5E61", 
       xlab="kcorr r", ylab="r", zlab="Z",
       site=5, lwd=15, alpha=0.5, size=2)
plot3d(dataCZKM$kcorr_i, dataCZKM$corrmag_i, dataCZKM$z, 
       type="p", col="#510506", 
       xlab="kcorr i", ylab="i", zlab="Z",
       site=5, lwd=15, alpha=0.5, size=2)
plot3d(dataCZKM$kcorr_z, dataCZKM$corrmag_z, dataCZKM$z, 
       type="p", col="#008080", 
       xlab="kcorr z", ylab="z", zlab="Z",
       site=5, lwd=15, alpha=0.5, size=2)

plot3d(dataCZKM$kcorr_j, dataCZKM$corrmag_j, dataCZKM$z, 
       type="p", col="#FF8000", 
       xlab="kcorr j", ylab="j", zlab="Z",
       site=5, lwd=15, alpha=0.5, size=2)
plot3d(dataCZKM$kcorr_h, dataCZKM$corrmag_h, dataCZKM$z, 
       type="p", col="#2D3B6C", 
       xlab="kcorr h", ylab="h", zlab="Z",
       site=5, lwd=15, alpha=0.5, size=2)
plot3d(dataCZKM$kcorr_k, dataCZKM$corrmag_k, dataCZKM$z, 
       type="p", col="#6C5733", 
       xlab="kcorr k", ylab="k", zlab="Z",
       site=5, lwd=15, alpha=0.5, size=2)
movie3d( spin3d(rpm=12), duration=5, dir="R/astro/k-corrections/out/", movie = "kcorr_k-k-Z", fps=24 )
# - czkm

# поправки на карте
for(zz in 1:6)
{
  zzz = zz/10
  cleanCZKM<-subset(dataCZKM, !is.na(ra) & !is.na(dec) & !is.na(z) & !is.na(kcorr_k) & z>zzz-0.1 & z<=zzz)
  #hcolors=heat.colors(length(unique(floor((cleanCZKM$kcorr_k+10)*10))))
  ggplot() +
    geom_point(data=cleanCZKM, aes(x=ra, y=dec, colour=kcorr_k), size=0.1) +
    scale_colour_hue(l=50) +
    scale_colour_gradientn(colours=rainbow(7)) +
    coord_map("mollweide",xlim=c(0,360),ylim=c(-90,90)) +
    scale_y_continuous(breaks = seq(-90, 90, 30)) +
    scale_x_continuous(breaks = seq(0, 360, 60)) +
    theme(axis.title = element_blank()) +
    theme(axis.ticks = element_blank(), axis.text = element_text()) + 
    theme(panel.border = element_blank()) + 
    theme(panel.grid.major = element_line(colour="darkgrey")) + 
    ggtitle(sprintf("kcorr_k coverage for Z between %s and %s", zzz-0.1, zzz)) + 
    theme(plot.title = element_text(size = rel(2)))
  ggsave(file=sprintf("R/astro/k-corrections/out/map-kcorr_k_z_in_%s_and_%s.png",zzz-0.1,zzz), scale=2)
}
cleanCZKM<-subset(dataCZKM, !is.na(ra) & !is.na(dec) & !is.na(z) & !is.na(kcorr_k))
#hcolors=heat.colors(length(unique(floor((cleanCZKM$kcorr_k+10)*10))))
ggplot() +
  geom_point(data=cleanCZKM, aes(x=ra, y=dec, colour=kcorr_k), size=1) +
  scale_colour_hue(l=50) +
  scale_colour_gradientn(colours=topo.colors(16), breaks=c(-10, -5, 0, 5, 10), labels = format(c(+10,-5,0,5,10))) +
  coord_map("mollweide",xlim=c(0,360),ylim=c(-90,90)) +
  scale_y_continuous(breaks = seq(-90, 90, 30)) +
  scale_x_continuous(breaks = seq(0, 360, 60)) +
  theme(axis.title = element_blank()) +
  theme(axis.ticks = element_blank(), axis.text = element_text()) + 
  theme(panel.border = element_blank()) + 
  theme(panel.grid.major = element_line(colour="darkgrey")) + 
  ggtitle(sprintf("kcorr_k coverage for full Z", zzz-0.1, zzz)) + 
  theme(plot.title = element_text(size = rel(2)))
ggsave(file=sprintf("R/astro/k-corrections/out/map-kcorr_k_z_full.png",zzz-0.1,zzz), scale=2)

print ("w")

scatterplot3d(dataDeep2$CORRMAG_Z, dataDeep2$CORRMAG_I, dataDeep2$Z, 
              xlim=c(20,30), ylim=c(20,30), zlim=c(0.1,1.5),
              type="p", color="#FF00FF22", 
              xlab="z", ylab="i", zlab="Z",lwd=15, pch="*",
              size=1, main="scatterplot3d - 3")




plot3dBig<-function()
{
  plot3d(dataCZKM$corrmag_u, dataCZKM$corrmag_g, dataCZKM$z, 
         xlim=c(20,30), ylim=c(20,30), zlim=c(0.1,1.5),
         type="p", col="green", 
         xlab="u", ylab="g", zlab="Z", 
         site=5, lwd=15, alpha=0.5, size=2)
  plot3d(dataCZKM$corrmag_r, dataCZKM$corrmag_g, dataCZKM$z, 
         xlim=c(20,30), ylim=c(20,30), zlim=c(0.1,1.5),
         type="p", col="red", 
         xlab="r", ylab="g", zlab="Z",
         site=5, lwd=15, alpha=0.5, size=2)
  plot3d(dataCZKM$corrmag_r, dataCZKM$corrmag_i, dataCZKM$z, 
         xlim=c(20,30), ylim=c(20,30), zlim=c(0.1,1.5),
         type="p", col="blue", 
         xlab="r", ylab="i", zlab="Z",
         site=5, lwd=15, alpha=0.5, size=2)
  plot3d(dataCZKM$corrmag_z, dataCZKM$corrmag_i, dataCZKM$z, 
         xlim=c(20,30), ylim=c(20,30), zlim=c(0.1,1.5),
         type="p", col="magenta", 
         xlab="z", ylab="i", zlab="Z",
         site=5, lwd=15, alpha=0.5, size=2)
  legend3d("topright", legend=c("u-g", "g-r", "r-i", "z-i"), pch = 16, col=c("green", "red", "blue", "magenta"), cex=1, inset=c(0.02))
}

plotCalculated<-function()
{
  ggplot(data=differenceData) + 
    theme(panel.background = element_rect(fill = "black")) +
    scale_x_continuous(limits=c(0.001,1.6)) +
    scale_y_continuous(limits=c(-10,10)) +
    geom_point(aes(Z, difference, group = type, color = factor(type)), alpha = 0.3, size=1) 
    #geom_smooth(aes(Z, difference), size=1, method="lm")
}
plotCalculated()


clrBase <- function(clr) {return(paste(clr, "77", sep=""))}
clrLighter <- function(clr) {return(paste(clr, "99", sep=""))}
clrDarker <- function(clr) {return(paste(clr, "55", sep=""))}

plotMag<-function()
{
  p <- ggplot() +
    theme(panel.background = element_rect(fill = "black")) +
    scale_x_continuous(limits=c(0.001,1.6)) +
    scale_y_continuous(limits=c(-10,50)) +

    geom_point(data=dataDeep2, aes(x=Z, y=CORRMAG_U, col="CORRMAG_U"), size=1, color=clrBase("#B9D9FF")) +
    geom_smooth(data=dataDeep2, aes(x=Z, y=CORRMAG_U), fill=clrLighter("#B9D9FF"), colour=clrDarker("#B9D9FF"), size=1, method="lm") +
  
    geom_point(data=dataDeep2, aes(x=Z, y=CORRMAG_G, col="CORRMAG_G"), size=1, color=clrBase("#C4FFCE")) +
    geom_smooth(data=dataDeep2, aes(x=Z, y=CORRMAG_G), fill=clrLighter("#C4FFCE"), colour=clrDarker("#C4FFCE"), size=1, method="lm") +
  
    geom_point(data=dataDeep2, aes(x=Z, y=CORRMAG_R, col="CORRMAG_R"), size=1, color=clrBase("#FFE1C4")) +
    geom_smooth(data=dataDeep2, aes(x=Z, y=CORRMAG_R), fill=clrLighter("#FFE1C4"), colour=clrDarker("#FFE1C4"), size=1, method="lm") +
  
    geom_point(data=dataDeep2, aes(x=Z, y=CORRMAG_I, col="CORRMAG_I"), size=1, color=clrBase("#FFBDB5")) +
    geom_smooth(data=dataDeep2, aes(x=Z, y=CORRMAG_I), fill=clrLighter("#FFBDB5"), colour=clrDarker("#FFBDB5"), size=1, method="lm") +

    geom_point(data=dataDeep2, aes(x=Z, y=CORRMAG_Z, col="CORRMAG_Z"), size=1, color=clrBase("#FF7174")) +
    geom_smooth(data=dataDeep2, aes(x=Z, y=CORRMAG_Z), fill=clrLighter("#FF7174"), colour=clrDarker("#FF7174"), size=1, method="lm")
  print(p)
}
#print(g)
#g + ggplot(dataDeep2, aes(CORRMAG_G, CORRMAG_R)) 
#g + geom_point(size=1, color="red") + 
#  scale_x_continuous(limits=c(0,50)) + 
#  stat_smooth()
#ggplot() +
#   +
#  geom_point(data=dataDeep2, aes(Z, KCORR_I_G), size=1) +
#  geom_point() + stat_smooth(method = "lm")

#  geom_point(data=dataDeep2, aes(x=Z, y=KCORR_Z_G), size=1, col="red") +
 # geom_point()



#hcolors=heat.colors(940)
#ggplot() +
#  geom_point(data=SDSSData, aes(x=ra, y=dec, colour=hcolors[floor(z*10)]), size=1) +
#  scale_colour_hue(l=50) +
#  coord_map("mollweide",xlim=c(0,360),ylim=c(-90,90)) +
#  scale_y_continuous(breaks = seq(-90, 90, 30)) +
#  scale_x_continuous(breaks = seq(0, 360, 60)) +
#  theme(axis.title = element_blank()) +
#  theme(axis.ticks = element_blank(), axis.text = element_text()) + 
#  theme(panel.border = element_blank()) + 
#  theme(panel.grid.major = element_line(colour="darkgrey")) + 
#  ggtitle("SDSS DR12 coverage") + 
#  theme(plot.title = element_text(size = rel(2)))

#stars<-subset(SDSSData, typeStr == "STAR")
#galaxyes <- subset(SDSSData, typeStr == "GALAXY")
#s3d <- scatterplot3d(galaxyes$ra, galaxyes$dec, galaxyes$z, color="blue", pch=4)
#s3d$points3d(stars$ra, stars$dec, stars$z, col="red", pch=3)
#rgl.sphgrid(radius = 10, longtype = "D", radaxis = FALSE)
#rgl.sphpoints(galaxyes$ra, galaxyes$dec, galaxyes$z, col="blue")
#rgl.sphpoints(stars$ra, stars$dec, stars$z, col="red")
#rgl.sphpoints(udata$ra, udata$dec, udata$corrmag_u, col="red")
#rgl.sphpoints(udata$ra, udata$dec, udata$corrmag_g, col="green")
#rgl.sphpoints(udata$ra, udata$dec, udata$corrmag_r, col="blue")

