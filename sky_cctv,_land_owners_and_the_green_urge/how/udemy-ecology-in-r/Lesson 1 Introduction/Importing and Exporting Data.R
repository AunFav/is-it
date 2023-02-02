##############################################################
################### Importing and exporting data #############
##############################################################

# The most common type of data imported into R is .csv files
# usually in the form of an excel spreadsheet
# we will first make a mock data frame, and then import/export it


# the first thing you should do when starting any new project is to set
# your working directory. This means setting your R environment to the
# correct folder on your computer.
# to do this, we use the function "setwd()"
# go to the folder you wish to set your working directory to and copy/paste
# the folder path in setwd(). Remember to 1) encompass the path in quotations
# and 2) change the forward brackets to backwards brackets if necessary \ to /
setwd("C:/Users/cheun/OneDrive/Desktop/Ecology in R/Lesson 1 Introduction")


# now lets set up some dummy data from the last 2 lessons
# we won't change the dates using the lubridate package 
# for this excersise
vector1 <- c(1,2,3,4,5) 
vector2 <- c(6,7,8,9,10)
vector3 <- c("hello", "goodbye", "hi", "bye", "hey") 
combine.columns <- cbind(vector1, vector2, vector3)
new.data.frame <- as.data.frame(combine.columns)
new.data.frame$ones <- 1
vector4 <- c(26, 39, 12, 5, 14)
new.data.frame$vector4 <- vector4
colnames(new.data.frame) <- c("col1", "col2", "col3", "col4", "col5")
dates <- c("13/10/2019", "14/10/2019", "15/10/2019", 
           "16/10/2019", "17/10/2019")
new.data.frame$dates <- dates 

#--------------------------------------------------------------------

# lets save this new data frame we created in our folder as a .csv file
# we do this using the write.csv() function, with our data frame's name
# first, followed by what we want the file name to be called
# NOTE: don't forget to add .csv to the file name!
write.csv(new.data.frame, "dataframe_file.csv")
getwd()

# now if we open our working directory folder, we will find our
# new spreadsheet file!

# let's pretend this file is a new file sent to us by a friend
# and we want to import it into R
# to do this, we use the read.csv() function
# remeber to assign the dataframe a name, we will call it "dat"
# for convinience, lets view the files we have in the folder 
# using the "list.files()" function and copy/paste the file name

list.files() # find the file we just made called "dataframe_file.csv"   
dat <- read.csv("dataframe_file.csv") #copy/paste the data in the function
View(dat) #make sure our import worked properly

#--------------------------------------------------------------

# different packages have different functions for importing and exporting
# data, whether it is an image, video, raster, shapefile, etc.
# however, most of them follow this format. We will go over each of them
# as we go along with the lessons
# NOTE: its important to note that you should try not to name your data
# with a name that corresponds with the functions in R. This can result in 
# ambigous errors, and can turn into a real headache. 





