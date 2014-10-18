# >=============== Quiz WEEK 2 ==================<
# QUESTION 2
library('sqldf')
url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(url2,"/tmp/data.csv",method = "wget")
acs <- read.csv("/tmp/data.csv")

# sqldf("select * from acs")
# sqldf("select * from acs where AGEP < 50 and pwgtp1")
# sqldf("select pwgtp1 from acs")
sqldf("select pwgtp1 from acs where AGEP < 50")


#QUESTION 3
str(unique(acs$AGEP))
# sqldf("select unique * from acs")
# sqldf("select distinct pwgtp1 from acs")
# sqldf("select AGEP where unique from acs")
str(sqldf("select distinct AGEP from acs"))


#QUESTION 4
library('httr')
library('XML')
url4 <- "http://biostat.jhsph.edu/~jleek/contact.html"
con = url(url4)
htmlCode = readLines(con)
close(con)
nchar(htmlCode[10])
nchar(htmlCode[20])
nchar(htmlCode[30])
nchar(htmlCode[100])


#QUESTION 5
url5 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"
download.file(url5,"/tmp/data5.for",method = "wget")
data5 <- read.fwf("/tmp/data5.for",skip=4,widths=c(12, 7,4, 9,4, 9,4, 9,4))
