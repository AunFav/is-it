##############################################################
######### Basic Tree Building and Trait Data Analysis ########
##############################################################


# First, as always set your working diretory
setwd("")


# install necessary packges 
remotes::install_github("GuangchuangYu/treeio")
install.packages("BiocManager")
BiocManager::install("ggtree")
install.packages("ape")
install.packages("taxize")
install.packages("rentrez")
install.packages("phytools")
install.packages("Select")
install.packages("treeio")
install.packages("data.tree")
install.packages("tidytree")
install.packages("traits")
install.packages("stringr")

# Open them all in the library
library(ape)
library(taxize)
library(rentrez)
library(phytools)
library(Select)
library(treeio)
library(ggtree)
library(data.tree)
library(tidytree)
library(ggplot2)
library(dplyr)
library(traits)
library(stringr)
library(cowplot)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Prepare your data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# you can get trait data from online sources and query them from R using the 
# "trait" package. However, we will use LD50 trait data pulled from the "Sake Database"
# http://snakedatabase.org/pages/ld50.php
# This data will be in your Lesson 7 folder "Phylogenetics"



# read in your data with species names and traits you would like to use
dat<-read.csv("TBLld50.csv")

# pull the species names column out to create a species list string
sp_list<-as.character(dat$Scientific.Name)

# set your ncbi key. If you don't have one, you can get it 
# by registering an account with ncbi 
set_entrez_key("")
Sys.getenv("ENTREZ_KEY")


# obtain cladistic data on your species list from ncbi
taxise_sp <- classification(sp_list, db = "ncbi")


# ~~~~~~~~~~~~~~~~~~~~~~~~ Build a tree ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Coerce your species list into a classree object
tax_tree <- class2tree(taxise_sp)
#plot the tax tree to se what we've made
plot(tax_tree, cex = 0.5)

# Pull the phylo object from the classtree for plotting in ggtree
tax_phylo <- tax_tree$phylo

# plot your basic tree to see what it looks like in ggplot using ggtree
# note: you will need to change x/y values according to your own data
  ggtree(tax_phylo) + 
  geom_tiplab(size=1) +
  theme_tree2() + 
  xlim(0, 90) 


# Now that the tree works lets write it into a newick file in case
# we need it later or want to use it in an external software for analysis
write.tree(tax_phylo, file = "Snake_Tree")

# import the tree file again just to make sure the function works
# give it an appropriate name for your species
# the tree is imported as a "phylo" object
snake.tree <- read.newick(file = "Snake_Tree")


# The package "ape" also allows us to simulate bramch lengths
# use ?compute.brlen to see diferent variations in the simulated lengths
plot(compute.brlen(snake.tree, power=1), main=expression(rho==1), cex=0.5)
plot(compute.brlen(snake.tree, power=3), main=expression(rho==3), cex=0.5)
plot(compute.brlen(snake.tree, power=0.5), main=expression(rho==0.5), cex=0.5)
plot(compute.brlen(snake.tree, power=0.1), main=expression(rho==0.1), cex=0.5)


# ~~~~~~~~~~~~~~~~~~~ subsetting tree data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# now lets try to subset the tree in case we want to check some
# target species
# check names of tips to use in sample subset
head(snake.tree$tip.label)

# make a subset of the tree and highlight a species
# levels_back argument represents cladistic level (family, superfamily, order, class etc.)
snake.treesub<-tree_subset(snake.tree, "Hydrophis_viperinus", levels_back = 2)

  ggtree(snake.treesub, aes(color = group)) + 
  geom_tiplab(size=4) +
  theme_tree2() + 
  scale_color_manual(values = c(`1` = "red", `0` = "black")) +
  xlim(0, 80)
  

# break the tree into its basal parts as a tibble (table dataframe)
tibtree <- as_tibble(snake.tree)

# remove the "_" from between the genera and specific epithet
tibtree$label <- sub("_", " ", tibtree$label)


# ~~~~~~~~~~~~~~~~~~~~~~ Format trait data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# keep only the necessary species, total body length, and ld50 data 
# we want in the "dat" data frame, have to divide for different trees
dat <- dplyr::select(dat, Scientific.Name, IV_mgkg, TBL_cm)


# change column names for jonining
colnames(dat) <- c("label", "LD50", "TBL")
# make sure labels are formatted as character
dat$label<-as.character(dat$label)


# convert to tibble for each type of trait
tibdat<-tibble(label=paste0(dat$label),
               LD50=dat$LD50, TBL=dat$TBL)


# remove any invisible characters (for some reason this happens when making tibbles)
tibdat$label<-str_replace_all(tibdat$label, "[^[:alnum:]]", " ")
tibtree$label<-str_replace_all(tibtree$label, "[^[:alnum:]]", " ")

# remove leading/trailing spaces from the data
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
###
tibdat$label<-trim(tibdat$label)
tibtree$label<-trim(tibtree$label)

# add traits to the tree tibble we just made and merge using 
# scientific name column "label"
tib_tree <- full_join(tibtree, tibdat, by.x = "label")


# reclassify the tibble columns again (annoying but necessary)
# to make sure they are in proper format for the analysis
tib_tree$parent <- as.integer(tib_tree$parent)
tib_tree$node <- as.integer(tib_tree$node)
colnames(tib_tree) <- c("parent","node","branch.length","label","LD50", "TBL")

# make sure everything is good for conversion
tib_tree # no values should be red "NA"
class(tib_tree) # the class should be:  "tbl_tree" "tbl_df" "tbl" "data.frame"

# remove duplicates (in case there are any)
tib_treeND <- unique(tib_tree)


# ~~~~~~~~~~~~~~~~~~~~~ Trait tree analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# convert into phylo object with trait data as a feature
new.snake.tree<-as.treedata(tib_treeND)

# check names to use for subset
new.snake.tree@phylo$tip.label

# subset the tree data (ignore the warning)
# if you try to plot the whole tree it won't work, likely 
# do to graphics ability
treesub_new<-tree_subset(new.snake.tree, "Crotalus scutulatus", levels_back = 3)

# plot the tree with LD50 values as colors on the branches
ggtree(treesub_new, aes(color=LD50),
       ladderize = TRUE,right = FALSE, 
       continuous = FALSE, size=1) +
  scale_color_distiller(palette = "Reds", direction = 1) +
  geom_tiplab(hjust = -.1, size=1.8, color = "black") + 
  geom_nodelab(aes(x=branch), size=2.5, vjust=-0.5)+
  geom_tippoint() +
  theme(legend.position = c(0.80, .85))+
  xlim(0,70) 

# plot a tree with Total Body Length (TBL) on the branches
ggtree(treesub_new, aes(color=TBL),
       ladderize = TRUE,right = FALSE, 
       continuous = FALSE, size=1) +
  scale_color_distiller(palette = "Reds", direction = 1) +
  geom_tiplab(hjust = -.1, size=1.8, color = "black") + 
  geom_nodelab(aes(x=branch), size=2.5, vjust=-0.5)+
  geom_tippoint() +
  theme(legend.position = c(0.80, .85))+
  xlim(0,70)


# resub and do a sidebyside analysis to see if you can visualize any correlations
# between total body length and LD50
# also lets change the species to some Hydrophis for a smaller subset
treesub_new <- tree_subset(new.snake.tree, "Hydrophis viperinus", levels_back = 2)

#plot the tree with LD50 values as colors on the branches
p1 <- ggtree(treesub_new, aes(color=LD50),
           ladderize = TRUE,right = FALSE, 
           continuous = FALSE, size=1) +
  scale_color_distiller(palette = "Reds", direction = 1) +
  geom_tiplab(hjust = -.1, size=2.5, color = "black") + 
  geom_nodelab(aes(x=branch), size=2.5, vjust=-0.5)+
  geom_tippoint() +
  theme(legend.position = c(0.90, .85))+
  xlim(0,70) 

#plot a tree with Total Body Length (TBL) on the branches
p2 <- ggtree(treesub_new, aes(color=TBL),
           ladderize = TRUE,right = FALSE, 
           continuous = FALSE, size=1) +
  scale_color_distiller(palette = "Reds", direction = 1) +
  geom_tiplab(hjust = -.1, size=2.5, color = "black") + 
  geom_nodelab(aes(x=branch), size=2.5, vjust=-0.5)+
  geom_tippoint() +
  theme(legend.position = c(0.90, .85))+
  xlim(0,70)


#side by side to check any correlation
plot_grid(p1, p2, ncol = 2, nrow = 1, labels = c("LD50", "TBL"))

###########################################################

# here's a fancy thing!?!? Overlapping text though.. don't know about that
ggtree(treesub_new, aes(color=LD50), yscale = "LD50",
       size=0.7) + 
  scale_color_viridis_c() + 
  theme_minimal()+
  geom_tiplab(hjust = -.1, size=3) +
  xlim(0,40) 

#can just fan it out instead 
ggtree(treesub_new, aes(color=LD50), yscale = "NA",
       size = 0.7) + 
  scale_color_viridis_c() + 
  theme_minimal()+
  geom_tiplab(hjust = -.1, size=3) +
  xlim(0,40) 

#or whatever this thing does???
ggtree(treesub_new, aes(color=LD50), layout = "daylight",
       size = 0.7) + 
  scale_color_viridis_c() + 
  theme_minimal()+
  geom_tiplab(hjust = -.1, size=3) 





########################################## End #######################################################

