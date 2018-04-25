##-----------------------------------------------------------------------------------------
## Code for Exploring, Analysis, Vis, and Story basis
## For the StoryTeller DATA
## HERE: https://drive.google.com/file/d/1lDrzRNzcND2uG8DJP159huPXnSw2Z2Rf/view?usp=sharing

## Gates, 2018

##################################
## NOTE: Everything I have done  #
## here can be done in another   #
## way. R has many  ways/options #
## for reaching a similar/same   #  
## result.                       #
## Here - I did NOT clean this   # 
## up so that my throught process#
## was preserved.                #
## This code is just a collection#
## of ideas and things to think  #
## about.
##################################

##-----------------------------------------------------------------------------------------

## Download and place the StoryTeller.csv data into the SAME location as your code. If you
## use a different location - you will need to update the path to open the file.

## Don't forget to always set your working directory (this saves headaches)
##   !! This is MY working directory - your will be different - 
setwd("C:/Users/profa/Documents/R/RStudioFolder_1/IST565")

## Check the working dir
(getwd())

## Now, read in the data into a data frame, there IS a header, and replace blanks with NA
StoryTellerDF <- read.csv("StoryTellerData.csv", header=TRUE, na.strings=c("","NA"))
## print out the first 5 rows
(head(StoryTellerDF, 5))

## Next, check the cleanliness and data types - adjust/clean as needed.

(str(StoryTellerDF))

## By doing the above, you can see that School is already a factor - which is good.
## However, Section is shown as int. This is not right. Change this to factor.
StoryTellerDF$Section <- factor(StoryTellerDF$Section)
(str(StoryTellerDF))

## The rest of the variables are fine as they are numerical. 

## Check for missing values
Total <-sum(is.na(StoryTellerDF))
cat("The number of missing values in StoryTeller data is ", Total )

## This dataset is not missing any values. However, you can always remove a few to confirm that 
## the above works.

## To clean this data, we can look through the variables and make sure that the data for each variable is in
## the proper range.
## The data shows the *number of students* in each category.
## This value cannot be negative - so 0 is the min. We do not know the max, but we
## might be suspecious of very large numbers. 

## Let's check each numerical variable to see that it is >= 


for(varname in names(StoryTellerDF)){
  ## Only check numeric variables
  if(sapply(StoryTellerDF[varname], is.numeric)){
    cat("\n", varname, " is numeric\n")
    ## Get median
    (Themedian <- sapply(StoryTellerDF[varname],FUN=median))
    ##print(Themedian)
    ## check/replace if the values are <=0 
    StoryTellerDF[varname] <- replace(StoryTellerDF[varname], StoryTellerDF[varname] < 0, Themedian)
  }
  
}

(StoryTellerDF)

## At this point, we will assume that we have clean data. However, in many cases, 
## several more steps will be required to clean up the data.

####### Now, let's investigate the data using analysis and visual EDA
## Investigating data is a blend of looking at the data and thinking about it
## and exploring it with measures and visualizations

## When you look at this data, you can first think about the total number of 
## sections in each school

(freqSchools=table(StoryTellerDF$School))

## From this, you can see that A and B are comparable, and perhaps C, D, and E can be compared
## with each other. 
## However, we can also first look at the number of students (sum of all sections) for each school


library(plyr)
## do once: install.packages("plyr")

## The following will sum all rows for each "School" and per variable in the data
## Let's save this new aggregated result as a DF
SumBySchoolDF <- ddply(StoryTellerDF, "School", numcolwise(sum))
(SumBySchoolDF)

## The following will do the same thing, but changes the column names to V1....V6 as the col names are not specified:
StudentsSumPerSchool <- aggregate(cbind(StoryTellerDF$VeryAheadby5, StoryTellerDF$Middlingby0, StoryTellerDF$Behindby1to5,
                                        StoryTellerDF$MoreBehindby6to10, StoryTellerDF$VeryBehindMoreThan11, 
                                        StoryTellerDF$Completed), by=list(School=StoryTellerDF$School), FUN=sum)
(StudentsSumPerSchool)






## Now, I want the total number of students for A - E
## I want to sum the columns for each row
## I will start with:

(SumBySchoolDF)

SumOfStudents <- rowSums(SumBySchoolDF[,c("VeryAheadby5", "Middlingby0", "Behindby1to5", 
                                          "MoreBehindby6to10","VeryBehindMoreThan11","Completed")])
(SumOfStudents)

TotalPerSchool <- data.frame("School" = StudentsSumPerSchool$School, 
                  "Total" = SumOfStudents)

(TotalPerSchool)

## Now, after much a do, we have a small data frame with the School (A - E) and its total students.
## We can see that A has the largest collection of students, followed by B. 

## Now we are ready to start thinking about what this data means. 
## First, notice that none of the sections in any of the schools are "Very Ahead" - so this 

## can be noted in any write-up and then that variable becomes uninteresting.

## Let's next look at Middlingby0 - for students on target or a little ahead.
## Let's look at Middlingby0 for each School (A  - E)
## Next, let's place them all in one large plot as subplots
par(mfrow=c(3,2))    # set the plotting area into a 3 row by 2 col grid
hist(StoryTellerDF$Middlingby0[StoryTellerDF$School=="A"])
hist(StoryTellerDF$Middlingby0[StoryTellerDF$School=="B"])
hist(StoryTellerDF$Middlingby0[StoryTellerDF$School=="C"])
hist(StoryTellerDF$Middlingby0[StoryTellerDF$School=="D"])
hist(StoryTellerDF$Middlingby0[StoryTellerDF$School=="E"])

##  !! However, these histograms do not really help us. !!
## It is good to explore ideas and it is important to reject and/or
## build on results that are not helpful yet. 

## Let's try something else...let's change the data to %
## so that we can better compare it between schools. 

print("The total students for each school\n")
(TotalPerSchool)

print("The totals in each categoy are:/n")
(SumBySchoolDF)

## Create a new dataframe with % values for each category
## for A - E
## First, copy the data frame with sums per each cat to a new df
PercentDataFrame <- SumBySchoolDF
## check what you have
(PercentDataFrame)
## Now, update each value to divide it by the correct total.
## In other words, all items in School A should be divided by 932
## which is the Total value for A in (TotalPerSchool)
# and School B items should be divided by 446, and so on....
## First - here is another method to get the totals and to create a new DF that includes this column
PercentDataFrame <- transform(PercentDataFrame, Total= VeryAheadby5+Middlingby0+Behindby1to5+MoreBehindby6to10+
                                VeryBehindMoreThan11+Completed)
(PercentDataFrame)

## Now we need to divide the values in our variables by the corresponding total
## I will first create a TEMP_DF that has only the numerical columns
(TempDF <- SumBySchoolDF)
## Now, remove the School column
(drops <- c("School"))
TempDF <- TempDF[ , !(names(TempDF) %in% drops)]
(TempDF)
#(rowSums(TempDF))
TempDF_perc <- TempDF/rowSums(TempDF) 
## This following dataframe contains the percentages
## This is also a form of normalization as now all values are between 0 and 1
## and are normalized based on their own student total
(TempDF_perc)

## Now, let's put this back together with Schools A - E
(PercentageDF <- data.frame(School = SumBySchoolDF$School,TempDF_perc))

## Finally, we have a useful and clear comparison. 
## We can say that visually (although we could run ANOVA tests to be sure) that
## all Schools A - E do not appear sig different per their student progress. 
## Again, this is a visual view and NOT a statistical result
## We can also see that School B is the best of the group with the most ahead, the fewest behind
## and the most completed

## Let's create a collection of pies to compare all the results
## Notice what this line of code gives you....
## It is the decimal value for the first rwo (School A), column Middlingby0
(TempDF_perc[1,]$Middlingby0)
## Use the above concept to build a set of pie graphs
## Create one pie graph for school A
par(mfrow=c(1,1))
slices <- c(TempDF_perc[1,]$Middlingby0, TempDF_perc[1,]$Behindby1to5,TempDF_perc[1,]$MoreBehindby6to10, 
            TempDF_perc[1,]$VeryBehindMoreThan11, TempDF_perc[1,]$Completed)
lbls <- c("Very Ahead", "Met or Better", "Behind 1-5", "Behind 6-10", "Behind > 11", "Completed")
pie(slices, labels = lbls, main="School A")

## SIDE NOTE: These two lines of code will create dynamic names or tiles....
items <- c("School", as.character(PercentageDF$School[1]) )
(title <-paste(items, collapse = " "))


par(mfrow=c(3,2))
## Now that you see that the above works - try to place this into a loop
## This will create a collection of pies - one for each School
for (i in 1:nrow(TempDF_perc)){
  #print(i)
  slices <- c(TempDF_perc[i,]$Middlingby0, TempDF_perc[i,]$Behindby1to5,TempDF_perc[i,]$MoreBehindby6to10, 
              TempDF_perc[i,]$VeryBehindMoreThan11, TempDF_perc[i,]$Completed)
  lbls <- c("Very Ahead", "Met or Better", "Behind 1-5", "Behind 6-10", "Behind > 11", "Completed")
  items <- c("School", as.character(PercentageDF$School[i]) )
  (title <-paste(items, collapse = " "))
  pie(slices, labels = lbls, main=title)
  
}

## To get a better measure of performance, I will combine some of the attributes
## and will re-plot to compare.
## I will not use VeryAheadby5 as the values here are 0 for all Schools
## New variable: AheadOrOnTarget =  Completed +  Middlingby0
## New Variable 2:  Behind = Behindby1to5 + MoreBehind6to10
## I will keep VeryBehind as it is

## I will use my dataframe that contains the percentages as this one is
#normalized by the total in each school and so allows us to compare directly

(PercentageDF)

## Copy this over to a new DF
GoodBadDF <- PercentageDF
(GoodBadDF)
GoodBadDF <- data.frame("School" = PercentageDF$School,"Good" = PercentageDF$Middlingby0+PercentageDF$Completed,
                        "Behind" = PercentageDF$Behindby1to5+PercentageDF$MoreBehindby6to10,
                        "VeryBehind" = PercentageDF$VeryBehindMoreThan11)
(GoodBadDF)


(View(GoodBadDF))

## NOw, we can really see which schools are better and which are not, given
## that the measure of "good" is completed, above, or met and the measure of bad is behind.

## Create a set of pies for this 

par(mfrow=c(3,2))
## Now that you see that the above works - try to place this into a loop
## This will create a collection of pies - one for each School
for (i in 1:nrow(GoodBadDF)){
  #print(i)
  slices <- c(GoodBadDF[i,]$Good, GoodBadDF[i,]$Behind,GoodBadDF[i,]$VeryBehind)
  lbls <- c(paste("Good\n    ",round(  GoodBadDF[i,]$Good * 100, digits = 1), "%"), 
            paste("Behind  \n", round(  GoodBadDF[i,]$Behind * 100, digits = 1), "%"), 
            paste("  VeryBehind\n", round(GoodBadDF[i,]$VeryBehind * 100, digits = 1), "%"))
  items <- c("School", as.character(GoodBadDF$School[i]) )
  (title <-paste(items, collapse = " "))
  pie(slices, labels = lbls, main=title)
  
}

## Now you can see that School B is doing the best of the Schools overall.
