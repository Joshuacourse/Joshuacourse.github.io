## Question #1
## Use the TimesSquareSignage.csv in the homework folder and import it into R. 
## Then check the following features of the dataset:
TimesSquareSignage=read.csv("TimesSquareSignage.csv")

## 1. The number of observations and the number of variables
## 2. The type of each variable
str(TimesSquareSignage)
### Solution: 	184 obs. of  18 variables
#$ Screen.Name..LED...Vinyl.Signs.: Factor w/ 175 levels "1 Times Sq south of Walgreens (Too Big to Fail)",..: 126 149 23 84 156 138 162 161 75 157 ...
#$ Building.Address               : Factor w/ 55 levels "1 Times Sq","1460 Broadway",..: 35 35 36 37 39 42 1 1 1 1 ...
#$ Location.Description           : Factor w/ 117 levels "","2 panels",..: 1 65 70 16 12 15 27 102 56 29 ...
#$ Location                       : Factor w/ 4 levels "42nd/Below","Bowtie",..: 1 1 1 1 1 1 2 2 2 2 ...
#$ Height                         : Factor w/ 4 levels "","Above 65 Ft",..: 3 3 4 4 4 2 2 2 2 4 ...
#$ Type                           : Factor w/ 2 levels "LED","Vinyl": 1 1 1 1 1 1 1 1 1 1 ...
#$ X.                             : int  1 3 1 1 1 1 1 1 1 1 ...
#$ Width                          : int  30 NA 11 24 4 18 51 40 35 35 ...
#$ X__Height                      : int  10 NA 11 18 10 13 53 53 50 40 ...
#$ SF                             : int  300 44 121 432 40 234 2703 2120 1750 1400 ...
#$ Note.Photo                     : Factor w/ 103 levels "","IMG_8862",..: 91 1 96 92 86 84 1 1 65 1 ...
#$ X_.                            : int  NA NA NA NA NA NA NA NA NA NA ...
#$ X_Width                        : int  NA NA NA NA NA NA NA NA NA NA ...
#$ X_Height                       : int  NA NA NA NA NA NA NA NA NA NA ...
#$ X_SF                           : int  0 0 0 NA 0 0 0 0 0 0 ...
#$ TOTAL                          : int  1 3 1 1 1 1 1 1 1 1 ...
#$ TOTAL.SF                       : int  300 44 121 432 40 234 2703 2120 1750 1400 ...
#$ TOTAL.BY.TYPE                  : Factor w/ 3 levels "","112,376","274,896": 1 1 1 1 1 1 1 1 1 1 )

##3. How many missing values are there in the dataset?
summary(TimesSquareSignage)
9+9+167+167+167+1
### Solution: 520
#                                    Screen.Name..LED...Vinyl.Signs.      Building.Address
 # Broadway Spire W Hotel (1567 Broadway btw 46th & 47th):  3            1 Times Sq   : 11    
 # Morgan Stanley Building (750 7th Avenue)              :  3            1535 Broadway: 10    
 # 1552 Broadway @ 46th st                               :  2            1567 Broadway: 10    
 # Above former Hawaiian Tropic (ESPN draft)             :  2            2 Times Sq   :  9    
 # Winter Garden Theatre (7th Avenue between 50th & 51st):  2            7 Times Sq   :  9    
 # Winter Garden Theatre (Broadway @ 50th)               :  2            1530 Broadway:  8    
 # (Other)                                               :170            (Other)      :127    
 #                                 Location.Description       Location          Height      Type           X.        
  #                                          : 47       42nd/Below:33              :23   LED  : 45   Min.   : 1.000  
 # North side of 42nd facing south            :  7       Bowtie    :94   Above 65 Ft:59   Vinyl:139   1st Qu.: 1.000  
 # South side of 42nd facing north            :  5       Upper Bway:28   Marquee    :15               Median : 1.000  
 # Above the Footlocker                       :  3       Upper Sev :29   Under 65 Ft:87               Mean   : 1.234  
 # NW corner of Broadway and 42nd, facing east:  3                                                    3rd Qu.: 1.000  
 # Turkey tourism                             :  3                                                    Max.   :13.000  
 # (Other)                                    :116                                                                    
 #    Width          X__Height            SF           Note.Photo       X_.            X_Width          X_Height    
 # Min.   :  4.00   Min.   :  4.00   Min.   :   40           : 41   Min.   :0.0000   Min.   :  0.00   Min.   : 0.00  
 # 1st Qu.: 24.00   1st Qu.: 24.00   1st Qu.:  560   IMG_8915:  6   1st Qu.:0.0000   1st Qu.:  0.00   1st Qu.: 0.00  
 # Median : 35.00   Median : 35.00   Median : 1188   IMG_8929:  4   Median :1.0000   Median : 25.00   Median :29.00  
 # Mean   : 43.11   Mean   : 39.42   Mean   : 2013   IMG_8900:  3   Mean   :0.7647   Mean   : 24.18   Mean   :26.29  
 # 3rd Qu.: 45.00   3rd Qu.: 49.00   3rd Qu.: 2419   IMG_8912:  3   3rd Qu.:1.0000   3rd Qu.: 28.00   3rd Qu.:42.00  
 # Max.   :320.00   Max.   :200.00   Max.   :16000   IMG_8923:  3   Max.   :2.0000   Max.   :130.00   Max.   :95.00  
 # NA's   :9        NA's   :9                        (Other) :124   NA's   :167      NA's   :167      NA's   :167    
 #     X_SF             TOTAL           TOTAL.SF     TOTAL.BY.TYPE
 # Min.   :   0.00   Min.   : 1.000   Min.   :   40          :182  
 # 1st Qu.:   0.00   1st Qu.: 1.000   1st Qu.:  600   112,376:  1  
 # Median :   0.00   Median : 1.000   Median : 1249   274,896:  1  
 # Mean   :  91.99   Mean   : 1.304   Mean   : 2105                
# 3rd Qu.:   0.00   3rd Qu.: 1.000   3rd Qu.: 2556                
 # Max.   :6240.00   Max.   :15.000   Max.   :18200                
 # NA's   :1            
 
 ##4. Which rows (people) have missing value? Which columns (variables) include missing value?
 complete.cases(TimesSquareSignage)
 ### Solution 1
  #[1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE
 #[20] FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
 #[39] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#[58] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
 #[77] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE
 #[96] FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#[115] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE
#[134] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE
#[153] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE
#[172] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE

### Width, X__Height, X_., X_Width, X_Height, X_SF







## Question #2
#From the Time Square dataset, we'd like to extract specific information about advertising
# in Midtown Manhattan. Obtain the following data frames and save them in CSV files:

##1. Observations from Upper Broadway
Upper_Broadway=subset(TimesSquareSignage,Location=="Upper Bway")

##2. Observations with greater-than-average square footage
Greater_than_Aver_SF=subset(TimesSquareSignage,SF>mean(TimesSquareSignage$SF))

##3. The name, address and location of the top observations in terms of total square footage
TopObservation=head(TimesSquareSignage[order(TimesSquareSignage$TOTAL.SF,decreasing = T),][,1:3])




## Question #3
##1. From your RStudio, import the built-in data by running data(cars)
data(cars)
cars

##2. Print the first 5 lines from cars .
head(cars,n=5)

##3. Randomly generate a vector as long as the the number of rows in cars , and have
# elements NY, CA or CT . Call the vector state.
vector_state=sample(c("NY","CA","CT"),size=nrow(cars),replace = T)

##4. Add state to the data frame cars as a new column. Again name the column state.
NewCars=cbind(cars,state=vector_state)

##5. Create a new column ratio whose value is the ratio dist/speed. Then compute the average and standard deviation.
NewCars$ratio=NewCars$dist/NewCars$speed
NewCars
mean(NewCars$ratio)
sd(NewCars$ratio)