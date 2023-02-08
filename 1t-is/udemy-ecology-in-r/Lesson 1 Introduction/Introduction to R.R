##############################################################
################### INTRODUCTION TO R ########################
##############################################################

install.packages("dplyr")
library(dplyr)


# IMPORTANT RULES OF R
# 1. R code is CASE-SENSITIVE: A =/= a
# 2. Warnings are not Errors: if you get a warning, your code may
# still have run successfully; if you get an error, it has not.
# 3. If you have a non-English computer, you may have something other than
# a comma delimiting your .csv data. remember to take this into account
# when importing datasets from excel or .txt files. 


# The "#" hashtag allows you to annotate your code
# anything following the hashtag is not run when using the 'run' command


# The shortcut to running code is ctrl + enter
1 + 1 # press ctrl enter to see the answer in your console (window below)
5 + 5 # press ctrl enter to see the answer in your console (window below)


# "Vectors" are combinations of "elements" (words or numbers)
# vectors are initiated by the fucntion c() - think of it as "combine"
c(1,2,3,4,5) # this is a vector of numbers
c("hello", "goodbye", "hi", "bye", "hey") # this is a vector of characters


# assignment arrows and "=" store information in a given name
# the shortcut for assignment arrows is alt + - 
# run all three of these lines below to see them stored in your
# "environment" window on the top right
vector1 <- c(1,2,3,4,5) 
vector2 <- c(6,7,8,9,10)
vector3 <- c("hello", "goodbye", "hi", "bye", "hey") 


#to combine vectors into rows, we use the "rbind()" function
combine.rows <- rbind(vector1, vector2, vector3)
combine.rows # run this to see what happened


#to combine vectors into columns, we use the "cbind()" function
combine.columns <- cbind(vector1, vector2, vector3)
combine.columns # run this to see what happened


# a data frame refers to a combination of rows and columns
# think of a data frame as an excel spreadsheet
# we want the three vectors as a 3 column data frame
# we can create a data frame by using the function "as.data.frame()"
# we can either use the already combined "combine.columns" data, or 
# use cbind() with each of the vectors again
new.data.frame <- as.data.frame(combine.columns)
new.data.frame1 <- as.data.frame(cbind(vector1, vector2, vector3))


# now click on the data frame "objects" in the "Global Environment" window 
# with the blue buttons to view the data frames
# alternatively you can use the "View()" function
View(new.data.frame)


# now say we want to view a specific column of values to our dataframe
# to do this we use the "$" after we call the data frame's name
# then we choose the column name we wish to view
new.data.frame$vector1 # this calls vector1 column
new.data.frame$vector2 # this calls vector2 column
new.data.frame$vector3 # this calls vector3 column


# Say we want to add another column of values to our dataframe
#to do this, we use the "$", name the new column, and assign it values
new.data.frame$ones <- 1 # this makes a column of 1s
View(new.data.frame) # check your work
#create a new vector (of 5 random numbers) to add
vector4 <- c(26, 39, 12, 5, 14)
#add vector4 to the data frame
new.data.frame$vector4 <- vector4
View(new.data.frame)


# Now say we would like to change the column names 
# so they make more sense with our data
# to do this we use the function "colnames()"
# since we have 5 columns now, we must name them all
# remember that all characters (words) must be inside quotations ""
colnames(new.data.frame) <- c("col1", "col2", "col3", "col4", "col5")
View(new.data.frame)


# what if we want to identify specific element(s) from a column and row?
# for this, we use square brackets [] with row number first, followed
# by the column number second
new.data.frame[2,3] #identify element on row 2, column 3 -- "goodbye"
new.data.frame[c(1,2), ] #identify all elements in rows 1 and 2
new.data.frame[ ,c(1,2)] #identify all elements in columns 1 and 2
new.data.frame[c(1:3), ] #identify all elements in rows 1 - 3
new.data.frame[ ,c(1:3)] #identify all elements in columns 1 - 3


# what if we didn't have data in that row (identified as NA)
new.data.frame[2,3] <- NA # assign NA to row, 2 column 3
View(new.data.frame) # see what happened


# what if we didn't want NA (non existing data) in our dataframe?
# how do we remove them?
# we can use the function "na.omit" which deleted the NA and all
# corresponding data (the row with the NA value)
#remove the NA from 
new.data.frame <- na.omit(new.data.frame) #omit the NA
View(new.data.frame) # see what happened

#---------------------------------------------------------------
# one common issue when people are first using R is that they don't convert
# dates when they import their data. If R doesn't recognize dates, they will
# instead be recognized as numerical values.
# one easy way to convert dates in R is to use the package "lubridate"
install.packages("lubridate")
library(lubridate)

# now we add a list of dates to our data frame, lets say we have the 
# format day/month/year
dates <- c("13/10/2019", "14/10/2019", "15/10/2019", 
           "16/10/2019") #make a vector of date values
new.data.frame$dates <- dates #add the vector to the data frame

# to check the type of value in a column, we can use the "class()" function
# they should be identified as Date or "POSIXt"
class(new.data.frame$dates) #we see that our dates are characters
new.data.frame$col1 <- as.numeric(new.data.frame$col1)

# now we can recognize them as dates using 
# the lubridate function as.POSIXct()
# when we call the "format =" argument, we have to use %d %m and %y
# in the correct order, corresponding to how our data is set up.
# also, the delimiter "/" or "-" must also be correct
new.data.frame$dates <- as.POSIXct(new.data.frame$dates, 
                                   format = "%d-%m-%Y")
View(new.data.frame) #OH NO! all of our dates became NA values!
#This happened because we didn't use the proper delimiter.
# instead of using / we used -.


# lets try again with the proper delimeter...
new.data.frame$dates <- dates #add the vector to the data frame again
new.data.frame$dates <- as.POSIXct(new.data.frame$dates, 
                                   format = "%d/%m/%Y")
View(new.data.frame) #perfect! now the dates are in universal format!
#lets check what kind of class they are identified as now
class(new.data.frame$dates) #great! now they're POSIXt (= DATE)


# sometimes we have a huge dataset, and we only want a few 
# specific columns. This is where the dplyr pacage comes in handy
install.packages("dplyr")
library(dplyr)

# in order to select specific columns to keep in a dataframe, we
# can simply use the "select()" function from the dplyr package
# lets only select columns col1, col3, and col5 and store them 
# in a dataframe object called dat.1
dat.1 <- select(new.data.frame, col1, col3, col5)
View(dat.1)# now check what we've done

# now using the colnames() function, lets finish this lesson by renaming
# the columns of our new data frame dat.1 as col1, col2, and col3

#------------ your code here-----------------#

colnames(dat.1) <- c("col1", "col2", "col3")


#------------ your code here-----------------#

#END


