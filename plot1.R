###########################################################################
#
## This is the source code for the Plot1 of the 
## Programming Assignment 1
#
## The structure of the code is as follows:
#
#  1. Set of helper functions used to format strings, 
#     used to construct SQL statement on data. 
#  2. initialize function to set graphic params and load needed libraries
#  3. loaddataset Function to load filtered data from file
#  4  plot1 function to generate Plot1 as required. The rest of functions
#     are used for this one (main function)
#
# Note1: the helper functions, the initialization and load data functions
#        should be written in separate R code files, and used from this file
#        to generate Plot1.
#        That would be the right way to organize the code form a 
#        software programming point of view, reusing code.
#        As the assignment asks for a single code file for each plot
#        I wrote every code file auto-contained, to avoid beign 
#        penalised for strict reviewers.
#
# Note2: the use of message() function is obviously not necessary.
#        this function is used as an indicator of progress of the plot 
#        generation
#
###########################################################################
#
## Section 1: Helper functions:
#       zero2d
#       rmslash
#       Year
#       Month
#       Day
#
# Note: the purpose of these helper functions is to deal with string
#       formatting. Function names are self-explanatory.
#
###########################################################################
zero2d <- function(x) sprintf("substr('0' || %s, -2)", x)

rmSlash <- function(x) sprintf("replace(%s, '/', '')", x)

Year <- function(x) sprintf("substr(%s, -4)", x)

Month <- function(x) {
  y <- sprintf("substr(%s, instr(%s, '/') + 1, 2)", x, x)
  zero2d(rmSlash(y))
}

Day <- function(x) {
  y <- sprintf("substr(%s, 1, 2)", x)
  zero2d(rmSlash(y))
}

fmtDate <- function(x) format(as.Date(x))

###########################################################################
#
## Section 2: Initialize function
#
###########################################################################
initialize <- function(){
  # set bg param transparent as required
  par(bg="transparent")
  # load library sqldf assuming sqldf package installed
  #    sqldf is used for loading only the data desired into R
  #    avoiding to load the whole file. 
  #    There are other methods
  #    to load partial dataset from the file but I chose this 
  #    because of the good performance and the use of SQL as 
  #    query language, that I have worked with previously.
  library(sqldf)
  # load library gsubfn assuming sqldf package installed
  #    gsubfn is used to format params of sql sentence 
  #    before sqldf is executed
  library(gsubfn)  
}

###########################################################################
#
## Section 3: loaddataset function
#
# Note: this function could be optimized loading only data needed for 
#       plotting and filtering the rest. It is not done that way as we are
#       working from an exploratory aata analysis point of view, and thus 
#       not much concerned about performance and optimization
#
###########################################################################
loaddataset <- function(){
  message("loaddataset. Start...................")
  
  # open csv file assuming it is placed on the working directory
  f <- file("household_power_consumption.txt")
  
  # initial date for sql sentence filter
  first_date <- "2007-2-1"       
  
  # end date for sql sentence filter
  second_date <- "2007-2-2"       
  
  # sql sentence using helper functions declared in Section 1
  sql <- "select * from f where
  `Year('Date')` || '-' || 
  `Month('Date')` || '-' || 
  `Day('Date')`
  between '`fmtDate(first_date)`' and '`fmtDate(second_date)`'"
  
  # read data filtered by date
  dataset <- fn$sqldf(sql, stringsAsFactors=TRUE, 
                      file.format=list(sep=";",header=TRUE))
  
  # close file
  close(f)  
  
  message("loaddataset. End.....................")
  # return dataset
  return(dataset)
}

###########################################################################
#
## Section 4: plot1 function
#
# Note: this function generates the plot making use of previously defined
#       functions to load data and initialize graphics and is the only one
#       needed to call from outside to generate it.
#
###########################################################################
plot1 <- function(){
  # first call to initialization function
  message("plot1. call to initialize().......")
  initialize()
  
  # load filtered dataset
  message("plot1. read filtered data.........")
  dataset <- loaddataset()
  
  # closes graphic device befor start plotting
  dev.off()
  
  message("plot1. generate plot...............")
  # opens png graphic device
  png(filename="plot1.png")  
  
  # generate plot1 = histogram
  hist(dataset$Global_active_power,main="Global Active Power",xlab="Global Active Power (kilowatts)",ylab="Frequency",col="red")

  # closes graphic device
  dev.off()
  
  message("plot1. End.........................")
}


