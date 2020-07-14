#Reading Data

#if package is not installed, install package using install.packages('<Package Name>')

#From Databases----
require(RODBC)
db <-  odbcConnect('<DSN Connection>')
db
#Simple Query
ordersTable <- sqlQuery(db, 'select * from Orders', stringsAsFactors = FALSE)
head(ordersTable)

detailsTable <- sqlQuery(db, 'Select *from [Order Details]', stringsAsFactors = FALSE)
head(detailsTable)

#Writing a long Query
longQuery <- 'Select * From Order, [Order Details] where Orders.OrderID = [Order Details].OrderID'
longQuery

#Joined Table
joinedTable <-  sqlQuery(db, longQuery, stringsAsFactors = FALSE)
joinedTable

#From Other Statistical Packages----

library('foreign')
#1 from SPSS
read.spss('C:/Users/user/...')

#2 from STATA
read.dta('C:/Users/user/...')

#3 from SAS
read.ssd('C:/Users/user/...')

#4 from Octave and MATLAB
read.octave('C:/Users/user/...')

#5 from MiniTab
read.mtp('C:/Users/user/...')

#6 from systat
read.systat('C:/Users/user/...')

#Web Scraping----
library(XML)

theurl <- www.data.gov/...
data <- readHTMLTable(url, which= , header = FALSE, stringsAsFactors = FALSE)
data
