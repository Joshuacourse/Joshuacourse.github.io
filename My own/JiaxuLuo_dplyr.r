## Question #1:
#For the first two questions, we study the data mpg in the ggplot2 package.
#(a) Load ggplot2 package first and then type data(mpg). Quickly go through the dataset and the help file.
library(ggplot2)
data(mpg)
?mpg

#(b) Obtain a subset of data including: year, cyl, cty, hwy, and renames these variables as V1, V2, V3, V4.
library(dplyr)
Question1_2=select(mpg,V1=year,V2=cyl,V3=cty,V4=hwy)

#(c) In mpg data, obtain the average of city miles per gallon and highway miles per gallon 
# for different numbers of cylinders. (Hint: the mean function calculates the average of a vector.)
Question1_3=summarise(group_by(mpg,cyl),Meancty=mean(cty),Meanhwy=mean(hwy))

#(d) For manufacturer, identify the car(s) that have the largest city miles per gallon .
Question1_4=select(filter(mpg,mpg$cty==max(mpg$cty)),manufacturer)


## Question #2:
# We want to know the relationship between three variables: engine displacement , city miles per gallon , and highway miles per gallon .

# 1. Create a new variable ratioHVE showing the ratio between highway miles per gallon and engine displacement .
Question2_1=mutate(mpg,ratioHVE=hwy/displ)

# 2. Create new variables rationCVE showing the ratio between city miles per gallon and engine displacement .
Question2_2=mutate(Question2_1,ratioCVE=cty/displ)

# 3. Obtain the average ratioHVE and ratioCVE by different years and manufacturers.
Question2_3=summarise(group_by(Question2_2,year,manufacturer),mean(ratioHVE),mean(ratioCVE))

# 4. Find the biggest ratioHVE by different years and drv.
Question2_4=summarise(group_by(Question2_2,year,drv),max(ratioHVE))

## Question #3
# For this question, you are going to explore an online dataset and try to answer the
# questions. You can find the dataset here:
# 1. What are the mean and median beginning and ending salaries for each agency?
#Note that salaries can be annual, hourly, or daily. You need to convert all of them to annual.
NYC_Jobs=read.csv("NYC_Jobs.csv",stringsAsFactors = F)
NYC_Jobs_split=split(NYC_Jobs,NYC_Jobs$Salary.Frequency)

str(NYC_Jobs_split[[2]])
NYC_Jobs_split[[2]]$Salary.Frequency="Annual"
NYC_Jobs_split[[2]]$Salary.Range.To=NYC_Jobs_split[[2]]$Salary.Range.To*365
NYC_Jobs_split[[2]]$Salary.Range.From=NYC_Jobs_split[[2]]$Salary.Range.From*365

str(NYC_Jobs_split[[3]])
NYC_Jobs_split[[3]]$Salary.Frequency="Annual"
NYC_Jobs_split[[3]]$Salary.Range.To=NYC_Jobs_split[[3]]$Salary.Range.To*365*24
NYC_Jobs_split[[3]]$Salary.Range.From=NYC_Jobs_split[[3]]$Salary.Range.From*365*24

NYC_unsplit=unsplit(NYC_Jobs_split,NYC_Jobs$Salary.Frequency)

summarise(group_by(NYC_unsplit,Agency),mean(Salary.Range.From),mean(Salary.Range.To))
summarise(group_by(NYC_unsplit,Agency),median(Salary.Range.From),median(Salary.Range.To))
# 2. Which agency has the highest average starting salary?
Question3_2=summarise(group_by(NYC_unsplit,Agency),max(Salary.Range.From))
Question3_2[Question3_2$`max(Salary.Range.From)`==max(Question3_2$`max(Salary.Range.From)`),1]

# 3. Does the type of posting (internal or external) have a big impact on the average salary range?
summarise(group_by(NYC_unsplit,Posting.Type),Avgrange=(mean(Salary.Range.To)+mean(Salary.Range.From))/2)

# 4. Rank the levels according to the average salary range for each level.
Question3_4=arrange(NYC_unsplit,Level)

# 5. Find out the range of money currently being spent by each agency for new hires. 
#Note that the X..Of.Positions column shows how many positions are available.
summarise(group_by(mutate(NYC_unsplit,Moneyspent=(Salary.Range.From+Salary.Range.To)/2*X..Of.Positions),Agency),sum(Moneyspent))

# 6. What civil service title has the largest salary range?
arrange(summarise(group_by(NYC_unsplit,Civil.Service.Title),Avgrange=(mean(Salary.Range.To)+mean(Salary.Range.From))/2),Avgrange)
