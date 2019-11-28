
# Contract Number:	#####
# Client:		        Client Name
# Contract Name:	  Name of contract
# Author: 		      Authors name
# Script purpose:   This is what it does
# Version No:		    v1
# Last updated:	     


###########################################################################
############################## SET-UP #####################################
###########################################################################

rm(list = ls())
old.par <- par() # save plotting parameters so they can be re-set later if necessary

#Create new project!!!
#Set up R, Data and Output folders!!!


###########################################################################
################## INSTALL PACKAGES AND FUNCTIONS #########################
###########################################################################

# Automatically check, install and load a package (useful for scripts intended to be run by a third-party).
# Example uses RODBC package, but could be any package.
Chkinstall <- 0
str(ip <- installed.packages())
for (i in 1:length(ip[,1])){
  if (ip[i,1]=="RODBC") chkinstall <- 1
}
if(chkinstall==0) install.packages("RODBC", repos="http://cran.uk.r-project.org/")
library(RODBC)

#Set-up Packrat
#This will set up a private library to be used for this project, and then place you in packrat mode. 
#While in packrat mode, calls to functions like install.packages and remove.packages will modify the private project library, rather than the user library.
install.packages("packrat");library(packrat) #Install and load packrat
packrat::init() #initialise packrat 

#Load required packages

library(devtools)
packrat::snapshot() #adds currently installed packages (and dependencies) to packrat package library (packrat/src)
packrat::status() # 
packrat::clean() # removes packages no longer needed by script


# Load custom functions
source('R/panel plot.R')
source('R/ggplot multi-plot.R')


###########################################################################
########################### SET PARAMETERS ################################
###########################################################################


theme_set(theme_bw()) # set preferred inbuilt ggplot theme
devtools::install_github('cttobin/ggthemr')
ggthemr("fresh") # set ggthemr theme (fresh and greyscale are good)
#ggthemr_reset() # to remove all ggthemr effects later

#setwd("Y:/Working/AH working/export from R")
#setwd("\\\\filesvr/16666-0/Working/AH working/export from R")


###########################################################################
################################ DATA ENTRY ###############################
###########################################################################

# Opens a dialog box so a file can be selected for import
data<-read.table(file.choose(),header = TRUE, sep = ",")

# Importing a dat file
data<-read.table(file="Data/data.dat",header=F,row.names=NULL,sep="",fill=TRUE) #note fill=TRUE, so missing data replaced by blanks (i.e. rows do not all have to be of same length)
data<-read.table(file="Data/data.dat",header=F,row.names=NULL,sep="\t",fill=TRUE) #use sep="\t" for tab delimited data

# Importing a csv file
data<-read.csv(file="Data/data.csv",header=T,row.names=NULL,sep=",")  

# Importing an Excel worksheet
library(RODBC)
con1<-odbcConnectExcel("Data/FileLocation&Name.xls")
data<-sqlFetch(con1,"SheetName")
odbcClose(con1)

library(readxl)
dataset <- read_excel("Data/FileLocation&Name.xls", sheet = "SheetName")

# Importing an Access database table
# First load package RODBC
con1<-odbcConnectAccess("Data/FileLocation&Name.mdb")
sqlTables(con1)
data<-sqlFetch(con1,"TableName")
odbcClose(con1)

# Import multiple datafiles with a common structure into a single dataframe
datafiles <- list.files(path="D:/Data/Test/") #generates a list of file names
data <- do.call('rbind', lapply(datafiles, function(x) read.table(paste('D:/Data/Test/', x, sep=""), skip=1))) #skip=1 to omit header row from each file

# Import multiple datafiles (with common or different structures)
setwd("D:/Data/Test")
datafiles<-list.files() #generates a list of file names 
data<-vector(mode="list", length=length(datafiles)) 
for (i in 1:length(datafiles)){
	data[[i]]<-read.table(file=paste(datafiles[i]),header=T,row.names=NULL,sep=",")  
	}
data[[1]] #to access the first component datafile
rbind(data[[1]],data[[2]]) #to combine datastes with a common structure 

# Manually creating variables
(x<-c(1,2,3,4,5,6,7,8,9,10)) #note additional paraentheses causes the result of the assignment to be displayed... 
x<-c(1:10)
x<-seq(from=1,to=10,by=1)
x<-seq(from=1,to=10,length.out=10)
y<-rnorm(10,mean=0,sd=1)
z<-rep(1:5,2)
a<-c("A","B","B","B","A","B","A","A","A","B")
b<-rep(c("A","B"),each=5)
date<-c("01/01/2012","02/01/2012","03/01/2012","04/01/2012","05/01/2012","06/01/2012","07/01/2012","08/01/2012","09/01/2012","31/12/2012")

#Create a data frame
data<-data.frame(ColumnName1=x,ColumnName2=y)
data<-cbind.data.frame(x,y,z,a,b,date)

#convert rown names to a new column
data$no <- rownames(data)

###########################################################################
######################### DATA MANIPULATION ###############################
###########################################################################

#examine contents of dataset
str(data)
names(data)
dim(data)
head(data)
fix(data)  # View the table in an editable spreadsheet

# Replace NAs with zeros or mean of dataset
x[is.na(x)] <- 0
x[is.na(x)] <- mean(x,na.rm=TRUE)


#re-format dates
time <- as.POSIXlt(date2, format = "%d/%m/%Y")
library(lubridate)
dec.year<-decimal_date(time) #calculates decimal year
DOY<- as.numeric(format(date, format='%j')) #Day of year from 1-366 where 1= 1st jan

# Creating subsets
x.subset<-subset(x,x>5) # x.subset will include only those where x is greater than 5
x.subset<-subset(x,x==2) # x.subset will include only those where x is equal to 2
x.subset<-subset(x,x!=7) # x.subset will include only those where x is not equal to 7
x.subset<-subset(x,x=7 | x=5 ) #gives records where x=7 OR x=5
x.subset<-subset(x,x %in% c(1,2,4)) # returns records that are contained within the specified set
newdata<-subset(data,x>7 & y<5) #gives records where x>7 AND y<5
newdata<-data[!(data$x<7 & y>5),] #gives recorded EXCEPT where x<7 AND y>5
newdata<-data[grep("texttofind", data$Title),] # find records containing specific text


#Create a new variable based on values of other variables
data$d<-ifelse(data$b=="A",data$y^2,data$y)
data$xy<-data$x*data$y

# Calculate group mean (or other summary stats) and assign to original data
data$meany <- ave(data$y, data$group, FUN = mean) #can have >1 grouping factor

#Rename a data frame column
names(data)[names(data)=="xy"] <- "xyproduct"

# Add xy to the data table as a new column
data<-cbind(data,xy)

# Add a new row to the data table or append another table
data<-rbind(data,c(1,2,2))
data<-rbind(data,data)

# Reorder rows in a table based on column 2 (ascending order)
data.sorted<-data[order(data[,2]),]
# Reorder rows in a table based on column 3 (descending order)
data.sorted<-data[1/order(data[,3]),]
# Reorder rows in a table based on column 2 (ascending order) then column 3 (descending order)
data.sorted<-data[order(data[,2],1/data[,3]),]

# Generate a list of unique values in a column
uniquelist<-unique(data$x)

# Summarise a dataframe by one or more factors (like a pivot table)
aggregate(data,by=list(a),FUN=mean) #numerical data only!
aggregate(Var1 ~ Var2 + Var3 + Var4, data = data, FUN = sum) #can cope with mix of numerical and text?

#OR
library(reshape)...



#linking two data frames on a common field (like vlookup in Excel or sql query in Access)
library(plyr) 
newdata<-join(dataframe1, dataframe2, by = "common.field", type="left") #'type' controls which rows to use from the two data frames

#OR use merge()..
merge(dataframe1,dataframe2) #like cbind, but eliminates duplicates columns
merge(dataframe1,dataframe2,by.x = "FieldName1", by.y = "FieldName2", all.x=TRUE) #if the common field as difefrent names in each dataset


#extract elements of a date
library(lubridate)
month(datevar)
year(datevar)

#Transpose a dataset
library(reshape)
data<-melt(data,id=c("x","y"))

#Change order of factor levels
data$a<-factor(data$a,levels=c("B","A")) #or
data$a<-factor(data$a,levels(data$a)[c(2,1)])
#OR
relevel(a,ref="B")

#loops
ii<-length(fac1)
jj<-length(fac2)
output<-numeric(length(ii*jj))
for(i in 1:ii){  
  for (j in 1:jj) {
    output[jj*(i-1)+j]<-fac1[i]*fac2[j] #or other calcs....
    cat("loop", ii,"-",jj, "\n") #display progress of loop
  }
}
results<-data.frame(cbind(fac1=rep(fac1,each=jj),fac2=rep(fac2,ii),output))

#apply: a more efficient alternative to loops 
data<-matrix(rnorm(n*nsamples),nrow=n,ncol=nsamples) #note that must be formatted as proper matrix
results<-apply(data,1,mean) #the '1' means calculate the mean for each *row* of the matrix.

#this approach can be extended to more complex analyses using user-defined functions to specify what should be done to each row of the matrix, for example:
x<-3
y<-5
myFunction<-function.name(arg1,arg2,arg3,...) {}
results<-apply(data,1,myFunction,arg2=x,arg3=y...) #note that the data matrix should be the first argument to the function.


# Find unique elements in a table (useful to producing a list of site characteristics)
SiteChars <- data[, c("SiteID","SiteFactor1","SiteFactor2","SiteFactor3")] # select columns of interest, plus groupng variable (e.g. site)
SiteChars <- dplyrr::distinct(SiteChars) #produce table of site chaarcteristics

# Convert row names of matrix or data frame to a column
tibble::rownames_to_column(df,var = "NewColName")
tibble::rowid_to_column(df,var = "NewColName")

###########################################################################
################################# GRAPHS ##################################
###########################################################################

# Control multi-panel layout
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), widths=c(1,1), heights=c(1,1)) # one large plot above two smaller ones
attach(mtcars)
hist(wt)
hist(mpg)
hist(disp)

## Add boxplots to a scatterplot (see https://www.statmethods.net/advgraphs/layout.html)
par(fig=c(0,0.8,0,0.8), new=TRUE)
plot(mtcars$wt, mtcars$mpg, xlab="Car Weight",
     ylab="Miles Per Gallon")
par(fig=c(0,0.8,0.55,1), new=TRUE)
boxplot(mtcars$wt, horizontal=TRUE, axes=FALSE)
par(fig=c(0.65,1,0,0.8),new=TRUE)
boxplot(mtcars$mpg, axes=FALSE)
mtext("Enhanced Scatterplot", side=3, outer=TRUE, line=-3) 

# Histogram
ggplot(data=iris, aes(Sepal.Length)) + 
  geom_histogram(position = "stack", binwidth=1) +

# Stacked histogram
ggplot(data=iris, aes(Sepal.Length, fill = Species)) + 
  geom_histogram(position = "stack", binwidth=1) +

# Scatterplot
ggplot(data=iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) + 
  geom_point() +
  geom_abline(slope=1, intercept = 0, size = 1, colour = "red") +
  geom_smooth(stat = "smooth", se = FALSE, formula = y ~ x) 

#general plot settings

  theme_bw +
  labs(title = "Fuel economy declines as weight increases",
     subtitle = "(1973-74)",
     caption = "Data from the 1974 Motor Trend US magazine.", # bottom right footnote
     tag = "Figure 1", # top right corner tag
     x = "Weight (1000 lbs)", # x-axis label
     y = "Fuel economy (mpg)", # x-axis label
     colour = "Gears") # legend title




#to show multiple plots on one page
source('R/ggMultiPlot.R')
multiplot(p1, p2, p3, p4, cols=2) 

#or

library(gridExtra)
grid.arrange(
  grobs = list(g1,g2,g3),
  widths = c(3, 1),
  heights = c(1, 2),
  layout_matrix = rbind(c(1,NA),
                        c(2,3))
)

# plot text as a graph
library(gplots) # for textplot() function
mod1summ <- summary(lm(y~x))
textplot(mod1summ, valign="top",mar=c(0,0,0,0))


###########################################################################
############################### MODELLING #################################
###########################################################################

#Construct a model formula
response <- "y"
predictors  <- " ~ x1 + x2"
m    <- as.formula(paste(response, predictors, sep=""))

#Take list of predictor variables and convert to formula
response <- "y" #user-defined response
predictors  <- c("x1","x2","x3") #user-defined list of predictor variables
n <- length(predictors) - 1
frm <- c("~ ")
for (i in 1:n){
  frm <- paste(frm,predictors[i],"+",sep=" ")
}
frm <- as.formula(paste(response, frm, predictors[nx], sep=" "))
frm



###########################################################################
########################## EXPORTING OUTPUT ###############################
###########################################################################

#Save a plot as a Windows metafile
filename<-paste("Output/name of file_",format(Sys.time(), "%Y%m%d"),".wmf",sep="")
win.metafile(filename)
plot(x,y)
dev.off() 

#Or save active plot window
dev.copy(win.metafile,filename=filename)
dev.off() 

#Save one or more plots to a pdf file
filename<-paste("Output/name of file_",format(Sys.time(), "%Y%m%d"),".pdf",sep="",width=10,height=7)
pdf(filename) 
plot(x,y)
plot(y,x)
dev.off() #turn off pdf plotting

# Export a data table to a csv file
write.csv(data,
          file=paste("Output/name of file_",format(Sys.time(), "%Y%m%d"),".csv",sep=""),
          quote=FALSE,row.names=FALSE)

# Exporting text outputs to a txt or doc file
filename<-paste("Output/name of file_",format(Sys.time(), "%Y%m%d"),".txt",sep="") #for a Word document, change txt to doc.
sink(filename,append = FALSE, type = "output",split = TRUE)

cat("==============================================================\n")
cat("print some text here")
cat("\t") #tab separator
cat(a) #print an object
cat("\n==============================================================")
cat("\n\n") #add 2 blank rows
cat("\n" ) #add a blank row

sink() #to stop writing output

# Save workspace using custom filename
filename<-paste("name of file_",format(Sys.time(), "%Y%m%d"),".RData",sep="")
save.image(file=filename)

rm(list = ls()) #clear current workspace (useful if looping and need to start with empty workspace)