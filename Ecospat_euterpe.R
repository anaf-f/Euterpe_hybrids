## written by Olivier Broennimann. Departement of Ecology and Evolution (DEE). 
## 16.01.2019. University of Lausanne. Department of ecology and evolution, Switzerland
## adaptado por Ana F. Francisconi - University of Campinas - Brazil

t1=Sys.time()

#install.packages("devtools")
#library(devtools)
#install_github(repo="ecospat/ecospat/ecospat",ref="3.1")
library(ecospat)

library(ade4)
library(raster)
library(rworldmap)
library(knitr)
library("heatmaply")
###########################
#### data preparation #####
###########################

clim<-getData('worldclim', var='bio', res=10)

# choice of variables
var<-c("bio1","bio2","bio3","bio4","bio5","bio6","bio7",
       "bio8","bio9", "bio10","bio11","bio12","bio13","bio14",
       "bio15","bio16","bio17","bio18","bio19")
clim<-clim[[which(names(clim)%in%var)]]

# create mask for background
data(countriesCoarseLessIslands) #countries from rworldmap
ctry = c("BLZ","BOL","BRA","COL","CRI","ECU","GUY","HND","NIC","PAN","PER","SUR","VEM","GUF","VEM","ARG","PRY")
bkg<-aggregate(countriesCoarseLessIslands[countriesCoarseLessIslands$ADM0_A3%in%ctry,])


# load occurence sites for the species (column names should be x,y)
file<-"euterpe_ok.txt"
occ<-na.exclude(read.delim(file,h=T,sep="\t"))

# extract climate data from the rasters
clim.bkg<-na.exclude(data.frame(extract(clim,bkg)))
clim.occ<-na.exclude(data.frame(extract(clim,occ[,2:3])))

# species list
sp.list<-levels(occ[,1])
sp.nbocc<-c()
for (i in 1:length(sp.list)){
  sp.nbocc<-c(sp.nbocc,length(which(occ[,1] == sp.list[i])))
} #calculate the nb of occurences per species
sp.list<-sp.list[sp.nbocc>4] # remove species with less than 5 occurences
nb.sp<-length(sp.list) #nb of species

################################
#### niche quantifications #####
################################

# calibration of PCA-env 
pca.env <-dudi.pca(clim.bkg, center = T, scale = T, scannf = F, nf = 2)

# selection of species to analyze
sp.choice<- c("Edulis","Oleracea","Precatoria") #CHOOSE THE SET OF SPECIES FOR PAIRWISE ANALYSES
sp.combn<-combn(sp.choice,2) 
nsp<-ncol(sp.combn)

# storage matrices
overlap<-matrix(nrow=nsp,ncol=nsp,dimnames = list(sp.choice,sp.choice))		# store overlap values
equivalency<-matrix(nrow=nsp,ncol=nsp,dimnames = list(sp.choice,sp.choice))	#store p-values for equivalency tests
similarity<-matrix(nrow=nsp,ncol=nsp,dimnames = list(sp.choice,sp.choice))	#store p-values for similarity tests

# loop of niche quantification for each combination of species

for(i in 1:ncol(sp.combn)) {  
  spa<-sp.combn[1,i] #name of species a
  spb<-sp.combn[2,i] #name of species a
  clim.spa<-clim.occ[occ$Species==spa,] #env data for species a
  clim.spb<-clim.occ[occ$Species==spb,] #env data for species a
  
  # PCA scores
  scores.bkg<- pca.env$li	#scores for global climate
  scores.spa<- suprow(pca.env,clim.spa)$lisup				#scores for spa
  scores.spb<- suprow(pca.env,clim.spb)$lisup				#scores for spb

	# calculation of occurence density
  za<- ecospat.grid.clim.dyn(scores.bkg,scores.bkg,scores.spa,100)
  zb<- ecospat.grid.clim.dyn(scores.bkg,scores.bkg,scores.spb,100)
  
  # overlap corrected by availabilty of background conditions
  ecospat.niche.overlap(za,zb,cor=F) 

  
  # test of niche equivalency and similarity
  equ<-ecospat.niche.equivalency.test(za,zb,rep=100) #rep=10 because it's slow, put at least 100 for final analyses
  sim<-ecospat.niche.similarity.test(za,zb,rep=100,rand.type = 1) 
  # both za and zb are randomly shifted in the background (previous versions of ecospat implemented rand.type =2)
  
  #storage of values
  overlap[sp.combn[1,i],sp.combn[2,i]]<-ecospat.niche.overlap(za,zb,cor=T)[[1]] 	#store overlap value
  equivalency[sp.combn[1,i],sp.combn[2,i]]<-equ$p.D					#store equivalency value
  similarity[sp.combn[1,i],sp.combn[2,i]]<-sim$p.D				#store similarity 21 value

  #plot			
  ecospat.plot.niche(za,title=spa,name.axis1="PC1",name.axis2="PC2") 
  ecospat.plot.niche(zb,title=spb,name.axis1="PC1",name.axis2="PC2")
  ecospat.plot.niche.dyn(za, zb, quant=0.25, interest=2,
                         title= "Niche Overlap", name.axis1="PC1",
                         name.axis2="PC2")
  ecospat.shift.centroids(scores.spa, scores.spb, clim.spa, clim.spb)
  ecospat.plot.contrib(pca.env$co,pca.env$eig)
  ecospat.plot.overlap.test(equ,"D","Equivalency")
  ecospat.plot.overlap.test(sim,"D","Similarity")
  
  #counter
  cat(paste(i))
}



## niche position and breadth

niche<-matrix(nrow=nsp,ncol=4,dimnames = list(sp.choice,c("pos1","breadth1","pos2","breadth2")))	#matrix to store niche caracteristics

for(i in 1:length(sp.choice)) { #for each chosen species
  
  R=100 #dimension of gridding of env space
  sp<-sp.choice[i]
  clim.sp<-clim.occ[occ$Species==sp,]
  scores.sp<- suprow(pca.env,clim.sp)$lisup	
  
  z<- ecospat.grid.clim.dyn(scores.bkg,scores.bkg,scores.sp,R) # calculation of occurence density 
  
  c<-sample(1:(R*R),100,prob=values(z$z.uncor)) #row index of 1000 random pixel weighted by density along PC1 and PC2 
  y=(c%/%R)+1;x=c%%R # coordinates of the pixels along CP1 and CP2
  CP.sim<-z$x[x] # scores of random pixels on CP1
  niche[i,1]<-median(CP.sim) # niche position on CP1
  niche[i,2]<-var(CP.sim) # niche breadth on CP1
  CP2.sim<-z$y[y] # scores of random pixels on CP2
  niche[i,3]<-median(CP2.sim) # niche position on CP2
  niche[i,4]<-var(CP2.sim) # niche breadth on CP2
}

t2=Sys.time()
t2-t1 #Time difference of 1.704341 mins

ecospat.niche.dyn.index(za,zb, intersection = NA)




