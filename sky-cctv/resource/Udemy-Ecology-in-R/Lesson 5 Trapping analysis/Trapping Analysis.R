##############################################################
################# Trapping Study Analysis ###############
##############################################################

# set working directory
setwd("C:/Users/Russe/Desktop/Ecology in R/Lesson 5 Trapping analysis")

#citation for dataset:
#Radji R, Gomina M, Amevoin K (2020). Coleoptera of Togo: data of the 
#LEA Insect Collection of the University of Lome. Version 1.4. Université de Lomé. 
#Occurrence dataset https://doi.org/10.15468/6nppyd 
#accessed via GBIF.org on 2020-01-30.

# install necessary packages - don't update packages if you don'r need to
install.packages("viridis")
install.packages("scatterpie")
install.packages("tibble")
install.packages("vegan")
install.packages("BiodiversityR") # if this promps you to install extra packages, proceed with install. 

# open all necessry libraries
library(rgbif)
library(mapview)
library(sp)
library(dplyr)
library(ggplot2)
library(viridis)
library(scatterpie)
library(raster)
library(reshape2)
library(tibble)
library(vegan)
library(BiodiversityR)
library(ggspatial)


# ~~~~~~~~~~~~~~~~~~~~~GET THE DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# use the question mark and occ_search to check examples then copy and paste
# relevant code for what we want to do
?occ_search

# Pull dataset records from GBIF
dat.all <- occ_search(datasetKey = "38fdd8b7-4e48-452c-8c05-673e6f6829af",
                      return = "data", limit = 2000)

dat.all <- dat.all$data
#view all the data that was returned
# so first lets reduce the "dat" data frame to only include necessary
# data
dat <- dplyr::select(dat.all, year, month, eventDate, 
                     family, genus, species, decimalLongitude, decimalLatitude)

#lets change the column names to more convinient labels
colnames(dat) <- c("year","month","datetime",
                   "family", "genus","species","x","y")

# now, lets remove any NA values 
dat <- na.omit(dat)

# convert lat lon to spatialpoints object
splatlong <- SpatialPoints(cbind(dat$x, dat$y),
                           proj4string = crs("+init=epsg:4326"))

mapview(splatlong)

# lets make site names to divide identify sample sites accoring to latitude
# ignore the deprecation warning
dat <- dat %>% 
  mutate(trapID = group_indices_(dat, .dots=c("y"))) 
dat$trapID <- sub("^", "Site", dat$trapID)


table(dat$trapID)

# add the sample site name to the spatial data
splatlong$Site <- dat$trapID



#~~~~~~~~~~~~~~~~~~~~~ Initial visual Analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# check how many sample sites we have in total

#check the locations of the sample sites
mapview(splatlong, map.types = "Esri.NatGeoWorldMap")


# lets explore the beetle data by family and genus
dat.family <- as.data.frame(table(dat$family))
dat.genus <- as.data.frame(table(dat$genus))

head(dat.family)# top 5 families
head(dat.genus)# top 5 genera



# plot the total families data in a barplot
ggplot(dat, aes(x=family,  fill=family))+
         geom_bar(position = "dodge", stat = "count", color = "black", alpha = 0.5)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(face = 2, size = 10, colour = "black",
                                   margin = margin(t = 0, r = 3, b = 0, l = 10)),
        plot.title = element_text(hjust = 0.5, face = 2),
        axis.text.y = element_text(face = 2, size = 12, colour = "black",
                                   margin = margin(t = 0, r = 3, b = 0, l = 10)),
        axis.text.x = element_text(face = 2, size = 9, colour = "black",
                                   margin = margin(t = 3, r = 0, b = 5, l = 0)),
        axis.title.y = element_text(face = 2, size = 12),
        axis.title.x = element_text(face = 2, size = 12),
        axis.ticks = element_line(colour = "black"))+
  scale_fill_viridis_d()+
  guides(fill = guide_legend(reverse=T))+
  coord_flip()



# Plot the families in a panel grid according to site (latitude)
ggplot(dat, aes(x=family,  fill=family))+
  geom_bar(position = "dodge", stat = "count", color = "black", width=1.5, alpha = 0.5)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(face = 1, size = 8, colour = "black",
                                   margin = margin(t = 0, r = 3, b = 0, l = 10)),
        plot.title = element_text(hjust = 0.5, face = 2),
        axis.text.y = element_text(face = 1, size = 8, colour = "black",
                                   margin = margin(t = 0, r = 3, b = 0, l = 10)),
        axis.text.x = element_text(face = 1, size = 8, colour = "black",
                                   margin = margin(t = 3, r = 0, b = 5, l = 0)),
        axis.title.y = element_text(face = 1, size = 8),
        axis.title.x = element_text(face = 1, size = 8),
        axis.ticks = element_line(colour = "black"))+
  scale_fill_viridis_d()+
  guides(fill = guide_legend(reverse=T))+
  facet_wrap(~dat$trapID, scales="free_x")+
  coord_flip()


# ~~~~~~~~~~~~~~~~~~~~~~~ Scatterpie maps ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Using raster get togo map
togomap <- getData("GADM", country = "TGO", level = 1)

# create the data into a long form species site matrix
dat.plot <- dplyr::select(dat, x, y, trapID, family)
dat.melt <- melt(dat.plot, id.vars = c("x", "y", "trapID"))
dat.cast <- dcast(dat.melt, trapID~value)
# create a total value column
dat.cast$total <- rowSums(dat.cast[, -1])
# create a dataframe with associated lat/long and trapname values to add in
latlongTID <- dplyr::select(dat, x, y, trapID)
latlongTID <- unique(latlongTID)
# add trapID back in
piechart.dat <- merge(x=dat.cast,y=latlongTID,by.x="trapID",by.y="trapID")

as.character(dat.family$Var1)

# make the plot
ggplot() +
  geom_spatial_polygon(data = togomap, aes(x = long, y = lat, group = group), size = 1,
                       alpha = 0.2, linetype = 1, color = "grey", fill = NA) +
  geom_scatterpie(data = piechart.dat, 
                  aes(x, y, r = 0.1),
                  cols = as.character(dat.family$Var1), 
                  alpha = 0.5, size = 0.1) +
  labs(title = "Diversity of Beetle Families in Sample Sites",
       subtitle = "latitudinal gradient of beetle family richness",
       fill = NULL) +
  scale_fill_viridis_d()+
  coord_fixed() +
  theme_bw() +
  theme(legend.position = c("bottom"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ Richness, abundance, and diversity analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FAMILY ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# First we will run the analysis on species then on overall families

# much of this comes from the vegan vignette
# Oksanen, J. (2013). Vegan: ecological diversity. R Project.


# we will now recreate our long form dataset for further analysis
# create the data into a long form species site matrix
dat.plot <- dplyr::select(dat, x, y, trapID, species)
dat.plot <- na.omit(dat.plot)
dat.melt <- melt(dat.plot, id.vars = c("x", "y", "trapID"))
dat.cast <- dcast(dat.melt, trapID~value)
rownames(dat.cast) <- dat.cast$trapID
dat.cast <- dat.cast[,-1]

# This next part we will do following the vegan package vignette to create indexes and values
# for our analysis of the trapping data


#change the name to something more appropriate
beetles.spec <- dat.cast


# we will first create diversity indexes for the sample sites
# simpson diversity
simpson <- diversity(beetles.spec, "simpson") #diversity variance
simpson 

# shannon diversity index
shannon <- diversity(beetles.spec, "shannon")
shannon #Higher values = more diversity

# compare the two indices
par(mfrow = c(1, 2)) 
hist(simpson)
hist(shannon)

# calculate difference and similarities between sites
# 0 = similar/same; 1 = different
par(mfrow = c(1, 2))
bray <- vegdist(beetles.spec, "bray") 
gower <- vegdist(beetles.spec, "gower")
hist(bray, xlim = range(0.0,1.0))
hist(gower, xlim = range(0.0,1.0))


# calculate Pielous evenness index
PE <- shannon/log(specnumber(beetles.spec))

# Select all sample sites "k" sample of five sites to calculate
# Renyi diversities
k <- sample(nrow(beetles.spec))
R <- renyi(beetles.spec[k,])
# dots represent values for sites, lines represent
# max, min and median values for the data
plot(R)


par(mfrow=c(1,1))
# we can also use a subset of random sample sites
# by adding a number after "nrow(beetle.spec)"
k <- sample(nrow(beetles.spec), 10)
R <- renyi(beetles.spec[k,])
plot(R)


# calculate Fisher's log series for all selected plots
k <- sample(nrow(beetles.spec))
fisher.log <- fisherfit(beetles.spec[k,])
plot(fisher.log)



#create abundance distance dendrograms using our data
taxdis <- taxa2dist(beetles.spec, varstep=TRUE)
plot(hclust(taxdis), hang = -1)

#clustered dendrogram, non-hanging averages per site
tr <- hclust(taxdis, "aver")
plot(tr)


# we can see that our highest abundance of species is in the 11th plot
# which is the third row of our dataset, we can call this row using the 
# sample fucntion and use the radfit() function to determine the best 
# indices that can be used to analyse the data
# the lowest value of AIC indicates the best-fit model.
RF <- radfit(beetles.spec[3,])
radlattice(RF)

# ~~~~~~~~~~~~~~~~~~~~~~ Rarefaction Curves ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Rarefaction curves allowus to visualize the differences 
# in richness and abundance between sample sites and also 
# to exptrapolate potential unseen species diversity 
# that require more sampling

spec.abund <- rowSums(beetles.spec)  #total individuals found in each plot
spec.abund 

# rarefaction takes the minimum number of observations to extrapolate the 
# expected number of observations for all other samples
rar.min <- min(rowSums(beetles.spec))  
rar.min # view smallest # of obs (site 17)

# extrapolate number of species considering only 1 individual is present
sp.rar <- rarefy(beetles.spec, rar.min) 
sp.rar

par(mfrow=c(1,1))
# plot the rarefaction curve
rarecurve(beetles.spec, col = "blue")

# ~~~~~~~~~~~~ Rank Abundance  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Rank abundance curves show relative species abundance ranks in species data
# where the higher the abundance of an organism is represented by a higher position
# on the y axis

rank.abund <- rankabundance(beetles.spec)
rank.abund
# plot abundance of first 3 most abundant species
rankabunplot(rank.abund, scale='abundance', addit=FALSE, specnames=c(1,2,3))
# plot log abundance of first most abundant species followed by intervals of 5
rankabunplot(rank.abund, scale='logabun', addit=FALSE, specnames=c(1, 5, 10, 15, 20), 
             srt=5, ylim=c(1,300))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ Richness, abundance, and diversity analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ GENERA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# we can also do a simple analysis similar to the first at the family taxonomic level


# we will now recreate our long form dataset for further analysis
# create the data into a long form species site matrix
dat.plot <- dplyr::select(dat, x, y, trapID, genus)
dat.melt <- melt(dat.plot, id.vars = c("x", "y", "trapID"))
dat.cast <- dcast(dat.melt, trapID~value)
rownames(dat.cast) <- dat.cast$trapID
dat.cast <- dat.cast[,-1]

# This next part we will do following the vegan package vignette to create indexes and values
# for our analysis of the trapping data


#change the name to something more appropriate
beetles.gen <- dat.cast


# we will first create diversity indexes for the sample sites
# simpson diversity
simpson <- diversity(beetles.gen, "simpson") #diversity variance
simpson 

# shannon diversity index
shannon <- diversity(beetles.gen, "shannon")
shannon #Higher values = more diversity

# compare the two indices
par(mfrow = c(1, 2)) 
hist(simpson)
hist(shannon)

# calculate difference and similarities between sites
# 0 = similar/same; 1 = different
par(mfrow = c(1, 2))
bray <- vegdist(beetles.gen, "bray") 
gower <- vegdist(beetles.gen, "gower")
hist(bray, xlim = range(0.0,1.0))
hist(gower, xlim = range(0.0,1.0))


# ~~~~~~~~~~~~~~~~~~~~~~ Rarefaction Curves ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Rarefaction curves allowus to visualize the differences 
# in richness and abundance between sample sites and also 
# to exptrapolate potential unseen species diversity 
# that require more sampling

spec.abund <- rowSums(beetles.gen)  #total individuals found in each plot
spec.abund 

# rarefaction takes the minimum number of observations to extrapolate the 
# expected number of observations for all other samples
rar.min <- min(rowSums(beetles.gen))  
rar.min # view smallest # of obs (site 17)

# extrapolate number of species considering only 1 individual is present
sp.rar <- rarefy(beetles.gen, rar.min) 
sp.rar

par(mfrow=c(1,1))
# plot the rarefaction curve
rarecurve(beetles.gen, col = "blue")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Rank Abundance  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Rank abundance curves show relative species abundance ranks in species data
# where the higher the abundance of an organism is represented by a higher position
# on the y axis

rank.abund <- rankabundance(beetles.gen)
rank.abund
# plot the non-scaled abundance to show the three most abundant genera
rankabunplot(rank.abund, scale='abundance', addit=FALSE, specnames=c(1,2,3))

# plot scaled abundance to show the most abundant genera by the first, third and then a sequence of 5
rankabunplot(rank.abund, scale='logabun', addit=FALSE, specnames=c(1, 3, 5, 10, 15, 20, 25), 
             srt=5, ylim=c(1,200))



# there are many other complex methods for community level analysis, but these indices and
# visualization should tell you enough about the study to write results on primary differences
# in richness and abundance. 










