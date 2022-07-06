##############################################################################
# R Session	- Visualization					                                                       #
# Date:  10/08/2017									                                         #
# Author: Manish                              							                 #	
##############################################################################

##############################################################################
# This Session will cover:
#     Why Data Visualization
#     https://gramener.com/amitabh/
#     Where data visualization fits in project lifecycle 
#     Delve deeper into different type of charts
#        1. Line Chart         2. Bar Chart          3. Histogram
#        4. Pie Chart          5. Stacked Bar Chart  6. Scatter Plot
#        7. Box Plot           8. Grouped Bar Chart 
#       
#     Base Ploting in R
#     ggplot2
##############################################################################


##Base Plottig in R

#Use Iris Dataset
attach(iris)
iris
Iris_demo <- iris
View(Iris_demo)
str(Iris_demo)
?plot
help(plot)

#Scatter Plot 
#Syntax
#plot(X,Y,....)

#Idea behind ploting scater plot is to understand the 
#correlation between X and Y
plot(Iris_demo$Sepal.Length,
     Iris_demo$Sepal.Width)



points(Iris_demo$Petal.Length,
       Iris_demo$Petal.Width,col="red")

#Identify the three Species 
plot(Iris_demo$Sepal.Length,
     Iris_demo$Sepal.Width,
     col=Iris_demo$Species)



plot(Iris_demo$Sepal.Length,
     Iris_demo$Sepal.Width,
     col=Iris_demo$Species)

legend(6.7,4.5,
       legend=c("Setosa", "Versicolor",
                "Virginica"),
       col=c("black","red", "green"),  
       pch = 1.0)


plot(Iris_demo$Sepal.Length,
     Iris_demo$Sepal.Width,type = "p", 
     col=Iris_demo$Species,
     main = "Demo Plot in Iris Dataset",
     sub = "Plot example",
     xlab = "Length",ylab = "Width")


#Histogram 
#Syntax
#hist(x,...)
?hist
hist(Iris_demo$Sepal.Width)


hist(Iris_demo$Sepal.Width,
     main = "Demo Histogram in Iris Dataset",
     xlab = "Width",
     border = "blue",
     col = "green",
     breaks = c(2,3,4,5)
)



hist(Iris_demo$Sepal.Width,
     main = "Demo Histogram in Iris Dataset",
     xlab = "Width",
     border = "blue",
     col = "green",
     breaks = 1:6
     
)

hist(Iris_demo$Sepal.Width,
     main = "Demo Histogram in Iris Dataset",
     xlab = "Width",
     border = "blue",
     col = "green",
     breaks = 5
)

##density = counts/(n*diff(breaks)
hist(Iris_demo$Sepal.Width,
     main = "Density plot",
     xlab = "Width",
     border = "blue",
     col = "green",
     breaks = 5,
     freq = F
)

##density as parameter (Density of the shading line)
hist(Iris_demo$Sepal.Width,
     main = "Freq plot",
     xlab = "Width",
     border = "blue",
     col = "green",
     density =  100
)

#Boxplot
#Box plots are extremely useful when you want to understand 
#large quantitative information
#Gives you good undestanding of Data distribution
#Very useful if you want to compare data across categories
#Syntax:
#boxplot()
boxplot(Iris_demo$Sepal.Length)
boxplot(Iris_demo)

##Names are not getting fit horizontally in given area.
##I want name to be displayed vertical
##USe "las" parameter
boxplot(Iris_demo,las=2)

#RenameName each variables
boxplot(Iris_demo,las=2, 
        names = c("S_L","S_W",
                  "P_L","P_W",
                  "Species"))


boxplot(Iris_demo,las=2, 
        names = c("S_L","S_W",
                 "P_L","P_W",
                 "Species"),
        col = c("red","green","blue",
                "Black","royalblue2"))


#Grab the outlier
outlier <- boxplot(Iris_demo$Sepal.Width)
outlier

outlier1 <- boxplot(Iris_demo$Sepal.Width,
                    plot = F)$out
outlier1

##Compare each species type and Petal Length
boxplot(Iris_demo$Petal.Length~Iris_demo$Species,
        xlab = "Species Type",
        ylab = "Petal Length", 
        main = "Iris Data",
        col = c("red","green","blue"))





##Horizontal Boxplot
boxplot(Iris_demo$Petal.Length~Iris_demo$Species,
        xlab = "Species Type",
        ylab = "Petal Length", 
        main = "Iris Data",
        col = c("red","green","blue"),
        horizontal = T)


## Pie Chart
#Syntax
#pie(x,labels) 
#x: vector containing the numeric value
#labels: GIve the descriptions

mytable <- table(iris$Species)
pie(mytable,labels = c("Setosa",
                       "Versicolor",
                       "Virginica"))

#Use Pie Percent
pie(mytable,labels = 
      round(100*mytable/sum(mytable), 1))

#Use Legends
pie(mytable,labels = 
      round(100*mytable/sum(mytable), 1),
    col=c("red","green","blue"))
legend("topright",c("Setosa",
                    "Versicolor",
                    "Virginica"),
       cex = .50,
       fill = c("red","green","blue"))



##Limitation of base Package
#Static Graphs
#Can not use to create complex visualization
#Just an image, cant be manipulated.

##ggplot2 - fullfledge R data objects
##Ingredients of ggplot package

install.packages("ggplot2")
## Load the ggplot2 package using install.packages("ggplot2")
library(ggplot2)

##Elements of ggplot: Fundamental design of ggplot
#Essential Attributes
   #1. Data
   #2. Geometries: What type of Plot you want?
       #Example: Point, Line,boxplot.......
       #Tip: A plot must have at least one geom; 
            #there is no upper limit. You can add a geom 
            #to a plot using the + operator
help.search("geom_", package = "ggplot2")

   #3. Aesthetics: Specify parameters which 
   #are specific to that geometry (Wot you can see).
   #Example: Position, Color, Shape, Size etc
   #Each type of geom accepts only a subset of all aesthetics
   #refer to the geom help pages to see what mappings each 
   #geom accepts. Aesthetic mappings are set with the aes() function.

#Optional Attributes
   #1. Facets
   #2. Statistics
   #3. Coordinates
   #4. Themes

##Creating Visualization using ggplot:

attach(mtcars)
library(ggplot2)
ggplot(data = mtcars, 
       aes(x=cyl , y=mpg)) + 
  geom_point()

str(mtcars)


ggplot(data = mtcars, 
       aes(x=factor(cyl) , y=mpg)) + 
  geom_point()







ggplot(data = mtcars, 
       aes(x=as.character(cyl) , y=mpg)) + 
  geom_point()

##Scatter Plots in ggplot
##Add additional parameter called shape in Geometries 
ggplot(data = mtcars, 
       aes(x=factor(cyl) , y=mpg))  +
  geom_point(shape = 1, size = 2)



##size parameter in Aesthetics
  ggplot(data = mtcars, 
         aes(x=factor(cyl) , 
             y=mpg,size = hp)) + 
    geom_point()


##Apply multiple variables in 2 D space
ggplot(data = mtcars,
       aes(x= wt,y = mpg, 
           col = hp,
           size = disp)) + 
  geom_point()

## Size parameter in the geom will override 
#size parameter of aes

ggplot(data = mtcars,
       aes(x= wt,y = mpg, 
           col = hp,size = disp)) + 
  geom_point(size=2)

##USe geom text in plot
ggplot(data = mtcars,
       aes(x = wt, y=mpg)) + 
  geom_text(aes(label=cyl))

##Tip: WHere ever you want to introduced variability 
##use aes parameter.

ggplot(data = mtcars,
       aes(x= wt,y = mpg)) + 
  geom_point() + geom_line(aes(y = qsec))

#############################################################################

#Exercise1:
#1. Use reshape2 package
#2. Use tips data set (from reshape)
#3. Create a scatter plot with 
    #"Total Bill" in x axis and "tip" in Y.
#4. Color ALL the points blue
#5. Make All the points bigger 
#   by setting size = 3
#6. Vary the size of the points based on 
    #the size of the party







#Exercise1: Solution
library(reshape2)
tips
ggplot(tips,aes(x=total_bill,y=tip)) + 
  geom_point()

ggplot(tips,aes(x=total_bill,y=tip)) +
  geom_point(color="blue")

ggplot(tips,aes(x=total_bill,y=tip)) +
  geom_point(color="blue",size=3)

ggplot(tips,aes(x=total_bill,y=tip,
                size = size)) + 
  geom_point()


#############################################################################

##Visualizing the large dataset
attach(diamonds)
str(diamonds)

ggplot(data = diamonds,aes(x=carat,y=price)) + 
  geom_point()

##When the points in the scatter plots are cluttered, 
#a smooth line can be used to visualize the 
#underlying trend or relationship between variables 
#on the x and y axis.

ggplot(data = diamonds,aes(x=carat,y=price)) + 
  geom_point() + geom_smooth()

#Points are not adding much value here 
##so we can remove points
ggplot(data = diamonds,aes(x=carat,y=price)) +
  geom_smooth()

#Add Clarity variable in our graph
temp <- ggplot(data = diamonds,
               aes(x=carat,y=price,
                   col = clarity)) +
  geom_smooth()
temp


#install.packages("directlabels")
#library(directlabels)
#direct.label(temp,list(last.points, 
#                       hjust = .5,
#                       vjust = .5))

##add clarity in our point graph
ggplot(data = diamonds,
       aes(x=carat,y=price,
           col=clarity)) +
  geom_point()

##Add transparency factor using alpha parameter.
ggplot(data = diamonds,
       aes(x=carat,y=price,col=clarity)) +
  geom_point()

ggplot(data = diamonds,
       aes(x=carat,y=price,col=clarity)) +
  geom_point(alpha = 0.4)

##Lets see how can we use ggplot in more powerful way
##Plots as object

diamond_plot <- ggplot(diamonds,
                       aes(x = carat,
                           y=price))
diamond_plot+geom_smooth()
diamond_plot+ geom_point()


##Bar Chart 
cyl_am <- ggplot(mtcars,
                 aes(x=cyl)) 
cyl_am + geom_bar()


cyl_am <- ggplot(mtcars,
                 aes(x=factor(cyl))) 
cyl_am + geom_bar() 

cyl_am

cyl_am + geom_bar() + aes(fill=factor(am))




ggplot(mtcars,aes(x=cyl,fill=am)) + 
  geom_bar(position = "stack")

##We dont see stacked bar for the reason because am(0 and 1) is being 
#considered as numeric
ggplot(mtcars,aes(x=factor(cyl),
                  fill=factor(am))) +
  geom_bar(position = "stack")

#If we are not interested in actual values but in proportion
#Stacked Bar Chart
#proportion
ggplot(mtcars,aes(x=factor(cyl),
                  fill=factor(am)))  + geom_bar(position = "fill")

#Dodge bar graph
ggplot(mtcars,aes(x=factor(cyl),
                  fill=factor(am))) + geom_bar(position = "dodge")

  ##Histogram
ggplot(mtcars,aes(x=mpg)) + 
  geom_histogram(binwidth = 4)

mycolor = "blue"
ggplot(mtcars,aes(x=mpg)) + 
  geom_histogram(binwidth = 3,
                 fill = mycolor)

ggplot(mtcars,aes(x=mpg,
                  fill = factor(cyl))) + 
  geom_histogram(binwidth = 1)


#Working on Time Series Data
attach(economics)

str(economics)
head(economics)

ggplot(economics, 
       aes(x=date,y=unemploy)) + 
  geom_line()


ggplot(economics, aes(x=date,
                      y=unemploy/pop)) +
  geom_line()

ggplot(economics, aes(x=date,
                      y=unemploy/pop,
                      col = psavert)) +
  geom_line()

ggplot(economics, aes(x=date,
                      y=unemploy/pop,
                      col = psavert)) +
  geom_line() + geom_point()

#Statistical Transformation
   #Some plot types (such as scatterplots) do not require transformations
   #each point is plotted at x and y coordinates equal to the original value. 
   #Other plots, such as boxplots, histograms, prediction lines etc. 
   #require statistical transformations:

#Theme
temp <- ggplot(economics,aes(x = date,
                     y = pop)) + 
  theme(legend.position = "top") +
  geom_point(aes(color = unemploy))


temp + scale_color_continuous(name="",
                              breaks = c(3000, 
                                         10000, 
                                         15000),
                              labels = c("3000", 
                                         "10000", 
                                         "15000"))

temp + scale_color_continuous(name="",
                              breaks = 
                                c(3000, 
                                  10000, 
                                  15000),
                              labels = 
                                c("3000", 
                                  "10000", 
                                  "15000"),
                              low = 
                                "blue",
                              high = "red")

##Faceting:
# Create separate graphs for subsets of data
library(reshape2)
temp1 <- ggplot(tips, aes(x = total_bill, 
                          y = tip))
temp1 + geom_line(aes(color = size))

#Not a very good way to represent the data
temp1 + geom_line() +
facet_wrap(~size, ncol = 6)




##############################################################################
# End of this Session you must have understood:
#     Why Data Visualization is so important
#     Where data visualization fits in project lifecycle 
#     What are the different visual representation for us to use
#     When to use which representation
#     Base Plotting in R
#     Plotting in ggplot2
# https://flowingdata.com/2016/03/22/comparing-ggplot2-and-r-base-graphics/
##############################################################################
