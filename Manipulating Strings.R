#Dealing with unstructured data

#paste function and sprintf

paste('Hello', 'Jared','and others', sep=',')
paste('Hello', 'Jared','and others', sep='/')
paste('Hello', 'Jared','and others', sep='+/')
paste('Hello', 'Jared','and others')

paste(c('Hello','Hey','Howdy'),c('Jared','Bob', 'David'))
paste('Hello',c('Jared','Bob','David'))

#collapsing a vector into a single text

vectorOfText <- c('Hello','everyone','out there','.')
paste(vectorOfText)
paste(vectorOfText, collapse = ' ')

person <- 'Jared'
partSize <- 8
waitTime <- 25
paste('Hello',person, ', your party of size,',partSize,
      'will be seated in ', waitTime,'minutes.', sep=' ')

#Another function is sprintf
sprintf('your party of %s would start in %s minutes, Dear %s', partSize, waitTime,person)


#Regular Expressions manipulation
require(XML)
durl <- 'http://www.loc.gov/rr/print/list/057_chron.html'
presds <-  readHTMLTable(durl, which=3, as.data.frame=T, skip.rows=1, header=T, stringsAsFactors=F)
head(presds)
tail(presds)
tail(presds$YEAR)
presds <- presds[1:65,]
str(presds)


require(stringr)
yearlist <- stringr::str_split(string=presds$YEAR ,pattern ='-')
head(yearlist)
yearMatrix <- data.frame(Reduce(rbind,yearlist))
head(yearMatrix)
names(yearMatrix) <- c('Start','End')

#Binding the yearmatrix back to the original table
presds <- cbind(presds, yearMatrix)
head(presds)

stringr::str_sub(string=presds$PRESIDENT, start=1, end=3)
str_sub(presds$PRESIDENT, start=4, end=8)

presid1= presds[(str_sub(presds$Start, start =4, end=4)==1),c(1,2,5,6)]
presid1

str_detect(presds$PRESIDENT, 'john')
presds$PRESIDENT =str_to_lower(presds$PRESIDENT)
str_detect(presds$PRESIDENT, 'john')

#OR
#Ignore case
str_detect(presid1$PRESIDENT, ignore.case('john'))



#Regular Expressions are powerful

con <- url('http://www.jaredlander.com/data/warTimes.rdata')
load(con)
close(con)
head(warTimes)
warTimes[str_detect(warTimes,'-')]
theTimes <- str_split(warTimes, pattern = '(ACAEA)|-',  n=2)
head(theTimes)

#start = 1L
#end = -1L
theStart <- sapply(theTimes,FUN = function(x){ x[1]} )
head(theStart)

theStart <- str_trim(theStart)

theStart[str_detect(theStart, 'January')]

str_extract(theStart,'January')


#Extracting year

head(str_extract(theStart, pattern = '[0-9][0-9][0-9][0-9]'),4)

head(str_extract(theStart, pattern = '[0-9]{4}'),5)

head(str_extract(theStart, pattern = '\\d'{4}),4) #othe languages might be \d, but in R it is \\d. where the 4 is curly
#braces represents number of digits to be found

head(str_extract(theStart, pattern = '\\d{1,3}'),4)

head(str_extract(theStart, pattern = '^\\d{4}'),4) #^ The caret means to search the beginning of the line
head(str_extract(theStart, pattern = '^\\d{4}$'),7) #The only number on the line


#Replacing strings with strings

head(str_replace(theStart, '\\d', replacement = 'x'),20)
head(str_replace_all(theStart, '\\d', replacement = 'x'),20)

head(str_replace(theStart, '\\d{1,4}', replacement = 'x'),20)
head(str_replace_all(theStart, '\\d{1,4}', 'x'),20)

#Replace

commands <- c('<a href=index.html>The link is here</a>','<b> This is a bold text </b>')
str_replace(commands, '<.+?>(.+?)<.+>', replacement = '\\1')
