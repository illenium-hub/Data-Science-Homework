
### 
library(tidyverse)
library(RSQLite)
library(DBI)
library(readxl)


### extract data
all<-read_excel("/Applications/BU/BU/615 Data Science in R/hw/hw5 SQL/Top MA Donors 2016-2020(1).xlsx",sheet = 1)
contribution<- read_excel("/Applications/BU/BU/615 Data Science in R/hw/hw5 SQL/Top MA Donors 2016-2020(1).xlsx",sheet = 2)
jfc<-read_excel("/Applications/BU/BU/615 Data Science in R/hw/hw5 SQL/Top MA Donors 2016-2020(1).xlsx",sheet = 3)


### see variables
names(all)
names(contribution)
names(jfc)

### creat series of new tables
#database1
da_1<-select(contribution,cycle,contribid,fam,date,amount,recipid,type,fectransid,cmteid)
money<-distinct(da_1)

#database2
da_2<-select(contribution,contribid,fam,contrib,City,State,Zip,Fecoccemp,orgname,lastname)
give<-distinct(da_2)

#database3
da_3<-select(contribution,recipid,recipient,party,recipcode)
receive<-distinct(da_3)

#chart4
da_4<-na.omit(select(contribution,orgname,ultorg))
employer<-distinct(da_4)


### build database
database<-dbConnect(SQLite(),"Shichenghw5.sqlite")
dbWriteTable(conn = database,value=money,name="money")
dbWriteTable(conn = database,value=give,name="give")
dbWriteTable(conn = database,value=receive,name="receive")
dbWriteTable(conn = database,value=employer,name="employer")







