#unsupervised classfication
#unsupervized classification finds patterns in the data without the need for training (like ground truth) dta

#main types of clustering: 
###partitionig like k-means

#detrmining amont of clusters
#####run it until you find the number of flusters that minimizes the error
##### error being the distance from the centroid of clusters, this must be minimized 
#####follow law of diminishing returns 


##WEEK 7 LAB

###SETTING THE WORKING DIRECTORY
###SET THIS FOR EVERY NEW THING, DOWNLOAD THE TIF FILE 
wd = ("C:/Users/tur61275/OneDrive - Temple University/Documents - Robert Hendrickson, PhD/Machine learning spatial analysis/week7")
setwd(wd)
#setting the working directory

dir()
library(terra)
library(sf)
library(dbscan)
##importing raster and plotting for EDA

waterdyn<- rast("aguavarMarialaBaja.tif")
plotRGB(waterdyn, r=1, g=2, b=1)
ext=draw("extent")
zoom(waterdyn, e=ext, maxcell=100000, layer=2, new=FALSE)
click(waterdyn, n=10, cell=TRUE)

#remvoing background pixels
waterdyn[[2]][waterdyn[[1]]==0]=NA 
waterdyn[[1]][waterdyn[[1]]==0]=NA
zoom(waterdyn, e=ext, maxcell=100000, layer=1, new=FALSE)
click(waterdyn, n=10, cell=TRUE)

#conver raster into a data frame
df=as.data.frame(waterdyn)
#exploration of the data frame 
hist(df[,1])
hist(df[,2])
#look at how different the values are, they must be rescaled to have simliar scales for use in the k-means 

#z-score function:
zscore=function(x){
  zval=(x-mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)
  return(zval)
}

scalfun=function(x){
  xscaled=(x-median(x, na.rm=TRUE))/IQR(x, na.rm=TRUE)
  return(xscaled)
}

#applying functions to data
df=apply(df, 2, scalfun) # Use zscore instead of scalfun if you want to scale based on zscores
df=data.frame(df)
summary(df)

#Let's remove any remaining entries with missing values
df=subset(df,complete.cases(df))

#running the k-means analysis using kmeans funciton
clusters <- kmeans(df, centers=5, iter.max=50, nstart=5, algorithm="Lloyd")
head(clusters$cluster)
tail(clusters$cluster)

#rasterizing the results and saving files into disc
coord=terra::xyFromCell(waterdyn,(as.numeric(rownames(df))))
coord=data.frame(coord)
names(coord)=c("x", "y")

# Then create a copy of one of the layers of the original raster.
clustrast=waterdyn[[1]]

#We will use these coordinates to identify the cell number of analyzed pixels 
#and then replace the value of those cells in original raster for the cluster number that was assigned to them in the k-mean analysis:
xypoints=st_as_sf(coord, coords=c("x", "y"), crs=st_crs("EPSG:32618"))
xypoints=vect(xypoints)

cellval=cells(clustrast, xypoints)
clustrast[cellval[,2]]=clusters$cluster
plot(clustrast)
zoom(clustrast, e=ext, maxcell=100000, layer=1, new=FALSE)

#saving the cluster raster
clustlist=list(clustrast, clusters)
save(clustlist, file='cluster_Lloyd.RData')
writeRaster(clustrast, 'cluster_Lloyd.tif')

#interpreting cluster anaylsis
clusters$centers
