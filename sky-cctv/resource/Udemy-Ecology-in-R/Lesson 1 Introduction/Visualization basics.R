##############################################################
################### VISUALIZATION BASICS #####################
##############################################################

# first, we need to install our new package called "ggplot"2
# to do this, we use the "install.packages()" function
# with this function, remember to put the package name in quotations
# alternatively you can use the GUI buttons on the bottom right window 
# ribbon and press "install" then type in the package name
install.packages("ggplot2")

# now that we've installed the package, it is saved in R and never needs 
# to be installed again! However, we do have to "open" the package by calling
# "library()" this needs to be done each time we open R
# Note: the library() function doesn't require quotations around the package
library(ggplot2)

# next, we will recreate our dataframe from the first lesson
# by copying and pasting the relevant code we learned
vector1 <- c(1,2,3,4,5) 
vector2 <- c(6,7,8,9,10)
vector3 <- c("hello", "goodbye", "hi", "bye", "hey") 
combine.columns <- cbind(vector1, vector2, vector3)
new.data.frame <- as.data.frame(combine.columns)
new.data.frame$ones <- 1
vector4 <- c(26, 39, 12, 5, 14)
new.data.frame$vector4 <- vector4
colnames(new.data.frame) <- c("col1", "col2", "col3", "col4", "col5")


#---------------------------------------------------------------

# Now we would like to visualize our data
# to do this, we can use "Base R" which are the functions
# that come automatically with R, or we can use other packages


# first we will use Base R to make a bar plot
barplot(new.data.frame$col5) # make a simple barplot of col5 values

# what about col3 (x) and col5 (y), for this we use "~"
barplot(new.data.frame$col5 ~ new.data.frame$col3)

# lets give it a title and change the x and y labels
barplot(new.data.frame$col5 ~ new.data.frame$col3,
        main = "this is the plot title",
        xlab = "this is the x-label",
        ylab = "this is the y-label")

# one of our bars seems to be extending over the y limitation
# lets change the y limitation to 50 instead
barplot(new.data.frame$col5 ~ new.data.frame$col3,
        main = "this is the plot title",
        xlab = "this is the x-label",
        ylab = "this is the y-label",
        ylim = c(0,50))

range(new.data.frame$col3)

#now lets use a simple plot function to see what it does
plot(new.data.frame$col5 ~ new.data.frame$col3,
     main = "this is the plot title",
     xlab = "this is the x-label",
     ylab = "this is the y-label",
     ylim = c(0,50))

# now lets plot them side by side by dividing the window into a
# 1 row by 2 column frame using the "par()" function
# and the "mfrow = " argument
par(mfrow = c(1,2))

# plot 1
barplot(new.data.frame$col5 ~ new.data.frame$col3,
        main = "this is the plot title",
        xlab = "this is the x-label",
        ylab = "this is the y-label",
        ylim = c(0,50))

#n plot 2
plot(new.data.frame$col5 ~ new.data.frame$col3,
     main = "this is the plot title",
     xlab = "this is the x-label",
     ylab = "this is the y-label",
     ylim = c(0,50))


# how about visualizing all of our data at once?
# We can plot any data with the simple function plot()
plot(new.data.frame) #meaningless dot plots


# I personally don't like the aesthetic of base R, and will not be using
# it throughout this course. So lets get something better!

#----------------------------------------------------------------

# So now lets go over the use of ggplot. We will start with a barplot
# in order to see how the functions work, we need to use the "help" script
# to view the help script, we can add a question mark "?" before the function
?ggplot() #run this line to see the help script for ggplot

# the easiest way to use functions you aren't familiar with is to check the
# "Examples" section at the bottom of the script

# For the ggplot function, we can see that the first thing we need to do
# is to call our dataframe
# the second is to call the columns we will be using as the x and y axis
# these columns should be named within the aes() or "aesthetic" argument
# lets make a plot with the x axis as col3 elements, and y axis as col5 values
ggplot(new.data.frame, aes(x=col3, y=col5))

# notice that we've created a plot, but have no data plotted? 
# in order to plot data we have to "add" an argument of what kind of plot
# we would like to make
# lets make a barplot first
ggplot(new.data.frame, aes(x=col) + geom_bar(stat = "count")

# OH NO! WE GOT AN ERROR!
# This error signifies that the function "geom_bar" uses a count
# or cumulative sum function for its default. 
# Since our data col5 has values associated col3, we need to set
# the geom bar to "stat = "identity""
ggplot(new.data.frame, aes(x=col3, y=col5)) + geom_bar(stat = "identity")

# much better! now lets add some color by adding our col3 value to the 
# "col" argument in the ggplot aesthetic
ggplot(new.data.frame, aes(x=col3, y=col5, col = col3)) + 
  geom_bar(stat = "identity")

# See how the color only outlines the bars? That's because for geom_bar
# the aesthetic you want to fill the bars is the argument "fill"
ggplot(new.data.frame, aes(x=col3, y=col5, fill = col3)) + 
  geom_bar(stat = "identity")

# So now lets make this a dot plot instead of a barplot. 
# we do this by replacing geom_bar() with geom_point()
ggplot(new.data.frame, aes(x=col3, y=col5, col = col3)) + 
  geom_point(stat = "identity")

#those dots are small, lets make the size bigger
ggplot(new.data.frame, aes(x=col3, y=col5, col = col3, size = 20)) + 
  geom_point(stat = "identity")

# lets add x and y axis bars and eliminate the backrground grid
# we do this by adding a theme_classic function
ggplot(new.data.frame, aes(x=col3, y=col5, col = col3, size = 20)) + 
  geom_point(stat = "identity") +
  theme_classic()

# That black size dot in the legend is ugly, lets remove it
# by setting it to FALSE in the guides
ggplot(new.data.frame, aes(x=col3, y=col5, col = col3, size = 20)) + 
  geom_point(stat = "identity") +
  theme_classic() + 
  guides(size = FALSE)

# now lets add some custom labels by using the theme() function
ggplot(new.data.frame, aes(x=col3, y=col5, col = col3, size = 20)) + 
  geom_point(stat = "identity") +
  theme_classic() + 
  guides(size = FALSE) +
  # this changes the main title
  ggtitle("Number of greetings - dots") +
  #this centers the main title
  theme(plot.title = element_text(hjust = 0.5)) +
  # this changes the y label 
  ylab("quantity") + 
  # this changes the x label 
  xlab("greetings") +
  # this changes the legend title for color
  labs(col = "greeting")


# lets save this plot as an object called p1 for "plot 1"
p1 <- ggplot(new.data.frame, aes(x=col3, y=col5, col = col3, size = 20)) + 
  geom_point(stat = "identity") +
  theme_classic() + 
  guides(size = FALSE) +
  ggtitle("Number of greetings - dots") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("quantity") + 
  xlab("greetings") +
  labs(col = "greeting")

# lets make a bar plot with the same modified code and save it as p2
# remember to change the color "col" aesthetic back to fill and remove the 
# size aesthetic, as it isn't necessary
p2 <- ggplot(new.data.frame, aes(x=col3, y=col5, fill = col3)) + 
  geom_bar(stat = "identity") +
  theme_classic() + 
  guides(size = FALSE) +
  ggtitle("Number of greetings - bars") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("quantity") + 
  xlab("greetings") +
  labs(fill = "greeting")

# to view p2, just run the object 
p2

#----------------------------------------------------------------------


# now lets visualize the two plots side-by-side, to do this with ggplot
# we need to make a grid using the package "cowplot" using the 
# function plot_grid()
install.packages("cowplot")
library(cowplot)

# now we will plot p1 and p2 side-by-side using plot_grid() and label them
# with a, and b, grid labels using the "label = auto" argument
plot_grid(p1, p2, labels = "auto")

# Remember, if we are curious about any other functionality, we can use
# the question mark to read the help script
?plot_grid

# we can see that plpt_grid has an argument to plot by rows "nrow"
# and columns "ncol". Lets plot another grid, but this time with p1
# on top and p2 on the bottom
plot_grid(p1, p2, nrow = 2, ncol = 1, labels = "auto")


# END