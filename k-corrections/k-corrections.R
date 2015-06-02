library(RCurl)
library(ggplot2)

cacheRoot="/home/sheridan/R/astro/k-corrections/cache"

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

querySDSS <- function(sql)
{
  Sys.sleep(2); # Требование SDSS - не более 60 запросов в минуту. На всякий случай спим 2 секунд перед каждым запросом. Нам несложно, а им радость :)
  return(getForm("http://skyserver.sdss.org/dr12/en/tools/search/x_sql.aspx", cmd = sql, format = "csv"))
}

# в запросе должна стоять %s в where, в месте, где лучше вставить лимиты по ra, dec 
# для batch выборки данных.
# Так же в функцию передаются имена полей, по которым лимитировать ra и dec. 
# Первое поле - ra, второе - dec, например c("ra","dec"). 
getSDSSData = function(sql, radecFields)
{

  # генерируем имена
  name           = digest::digest(sql,algo='md5', serialize = FALSE)
  folder         = sprintf("%s/sdss/%s"         , cacheRoot, name)
  sqlQueryFile   = sprintf("%s/_.sql"          , folder, name)
  cachedDataFile = sprintf("%s/_.data"         , folder, name)
  
  #Проверяем кеш. Если есть - берем из кеша.
  if(!dir.exists(folder)) { dir.create(folder) }
  if(!file.exists(cachedDataFile))
  {
    if(!file.exists(sqlQueryFile)) {write(sql,sqlQueryFile)}
    sql = trim(gsub(pattern="\n",replacement=" ",x=sql)) # Убираем лишнее в запросе
    chunksFolder=sprintf("%s/chunks", folder);
    if(!dir.exists(chunksFolder)) { dir.create(chunksFolder) }
    message("Дёргаем данные из SDSS по запросу:")
    message(sql)
    message("Ждите...")
    count=0
    
    #raBorders <- 0:359
    #decBorders <- -90:89
    
    raBorders <- 0:10
    decBorders <- -90:89
    
    raBordersLength=length(raBorders)
    decBordersLength=length(decBorders)
    steps=raBordersLength*decBordersLength
    for(ra in raBorders)
    {
      for(dec in decBorders)
      {
        csvChunkFolder = sprintf("%s/ra[%03d_%03d]", chunksFolder, ra, ra+1)
        if(!dir.exists(csvChunkFolder)) { dir.create(csvChunkFolder) }
        csvChunkFile = sprintf("%s/dec[%+03d_%+03d].csv", csvChunkFolder, dec, dec+1)
        if(!file.exists(csvChunkFile))
        {
          chunkSql = sprintf(sql, sprintf("(%s between %d and %d and %s between %d and %d)", 
                                          radecFields[1], ra, ra+1, radecFields[2], dec, dec+1))
          message(sprintf("(Запрос %d из %d) Дёргаем данные для ra (%s) [%03d:%03d] и dec (%s) [%+03d:%+03d]", 
                          count, steps, 
                          radecFields[1], ra, ra+1, radecFields[2], dec, dec+1))
          write(querySDSS(chunkSql), file=csvChunkFile)
        }
        count=count+1;
      }
    }
    message("Готово.")
    message("Парсинг SDSS данных...")
    count=1
    result <- NULL
    for(ra in raBorders)
    {
      dataChunkFile = sprintf("%s/ra[%03d_%03d]/_.data", chunksFolder, ra, ra+1)
      if(!file.exists(dataChunkFile))
      {
        x <- NULL
        for(dec in decBorders)
        {
          csvChunkFile = sprintf("%s/ra[%03d_%03d]/dec[%+03d_%+03d].csv", chunksFolder, ra, ra+1, dec, dec+1)
          message(sprintf("(Шаг %d из %d) Парсим csv для ra (%s) [%03d:%03d] и dec (%s) [%+03d:%+03d]", 
                            count, steps, 
                            radecFields[1], ra, ra+1, radecFields[2], dec, dec+1))
          dx <- read.table(csvChunkFile, header=TRUE, sep=",", dec=".", comment.char="#")
          if(length(dx[[1]]))
          {
            x <- rbind(x, dx)
          }
          rm(dx)
          count=count+1
        }
        saveRDS(x, file = dataChunkFile)
        rm(x)
      }
      else { count=count+length(decBorders)}
      result <- rbind(result, readRDS(dataChunkFile))
    }
    saveRDS(result, file = cachedDataFile)
    rm(result)
    message("Готово.")
  }
  message("Выкапываем SDSS данные из кеша по запросу:")
  message(sql)
  return (readRDS(cachedDataFile))
}


#udata <- read.table("/home/sheridan/R/astro/czkm_catalog_final_150114-3.csv", header=TRUE, sep=",", dec=".", comment.char="#")
#udata <- readRDS("/home/sheridan/R/astro/czkm_catalog_final_150114-4.rds")
#mySqlQuery = 
#  "SELECT objID, htmID, z, ra, dec, cx, cy, cz, class, subClass, type, survey, programname
#FROM SpecPhoto
#WHERE (htmid*37 & 0x000000000000FFFF) < (65) and class='GALAXY'"

SDSSData = getSDSSData("
select 
  PhotoPrimary.objID, PhotoPrimary.htmID, dbo.fPhotoTypeN(PhotoPrimary.type) as typeStr, 
  PhotoPrimary.ra, PhotoPrimary.dec, 
  PhotoPrimary.modelMag_u, PhotoPrimary.modelMag_g, PhotoPrimary.modelMag_r, PhotoPrimary.modelMag_i, PhotoPrimary.modelMag_z, 
  PhotoPrimary.modelMagErr_u, PhotoPrimary.modelMagErr_g, PhotoPrimary.modelMagErr_r, PhotoPrimary.modelMagErr_i, PhotoPrimary.modelMagErr_z,
  SpecPhotoAll.z
from PhotoPrimary 
join SpecPhotoAll on SpecPhotoAll.objID=PhotoPrimary.objID
where 
      PhotoPrimary.type in (dbo.fPhotoType('Star'), dbo.fPhotoType('Galaxy')) 
  and PhotoPrimary.clean=1
  and SpecPhotoAll.z between 0.6 and 10
  and SpecPhotoAll.zWarning = dbo.fSpecZWarning('OK')
 and %s
", c("PhotoPrimary.ra", "PhotoPrimary.dec"))

#str(A)
hcolors=heat.colors(940)
ggplot() +
  geom_point(data=SDSSData, aes(x=ra, y=dec, colour=hcolors[floor(z*10)]), size=1) +
  scale_colour_hue(l=50) +
  coord_map("mollweide",xlim=c(0,360),ylim=c(-90,90)) +
  scale_y_continuous(breaks = seq(-90, 90, 30)) +
  scale_x_continuous(breaks = seq(0, 360, 60)) +
  theme(axis.title = element_blank()) +
  theme(axis.ticks = element_blank(), axis.text = element_text()) + 
  theme(panel.border = element_blank()) + 
  theme(panel.grid.major = element_line(colour="darkgrey")) + 
  ggtitle("SDSS DR12 coverage") + 
  theme(plot.title = element_text(size = rel(2)))

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

