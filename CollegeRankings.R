require(XML)
require(RCurl)
library(rvest)
library(car)
library(pls)


#To get the a list containing the top 500 colleges and the links to Forbe's individual college page I downloaded the
#first five pages of their list.  I downloaded the sites manually and named them Forbes1.html, Forbes2.html and so on.  
#The first page is has the colleges ranked 1-100, second page is 101-200...  The links for these sites are below.
#"http://www.forbes.com/top-colleges/list/"
#"http://www.forbes.com/top-colleges/list/#page:2_sort:0_direction:asc_search:_filter:All%20states" 
#"http://www.forbes.com/top-colleges/list/#page:3_sort:0_direction:asc_search:_filter:All%20states" 
#"http://www.forbes.com/top-colleges/list/#page:4_sort:0_direction:asc_search:_filter:All%20states" 
#"http://www.forbes.com/top-colleges/list/#page:5_sort:0_direction:asc_search:_filter:All%20states" 
  

#name of downloaded sites that have Forbes' top colleges lists.
listofcolleges <- c("Forbes1.html", "Forbes2.html", "Forbes3.html", "Forbes4.html", "Forbes5.html")

topcolleges <- NULL

for(i in listofcolleges){
  
tables <- readHTMLTable(i)
df <- data.frame(tables[27])

parsed <- htmlParse(i)
xpath <- '//*[@id="listbody"]/tr/td/a/@href'
nodes <- getNodeSet(parsed, xpath)
links <- sapply(nodes, toString)


top <- cbind(df, links)
topcolleges <- rbind(topcolleges, top)
}

topcolleges <- topcolleges[,c(-4,-5)]
names(topcolleges) <- c("rank", "name", "state", "full_link")

topcolleges$full_link <- gsub("\\?list=top-colleges", "", topcolleges$full_link)


#saving topcolleges object:
#save(topcolleges, file="list of top colleges")
#load("list of top colleges")


##test code for extract data function
#urlcollege <- 'http://www.forbes.com/colleges/swarthmore-college/'
#rawcollege <- getURL(urlcollege)
#parsed <- htmlParse(rawcollege)

#urlcollege <- 'http://www.forbes.com/colleges/texas-tech-university/'
#rawcollege <- getURL(urlcollege)
#parsed <- htmlParse(rawcollege)


#Extractdata function takes in the htmlParsed data of one indivudal college's raw URL 
#It returns a vector with 39 variables.
extractdata <- function(data) {

  vect <- c() 
  
  #total cost-in state
  xpath <- '//*[@id="tuition"]/ul/li/table[3]/tbody/tr[2]/td[2]'
  nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
  if(!is.null(value)){
    vect[1] <- toString(value)
  }else {vect[1] <- NA}

  #total cost-out state
  xpath <- '//*[@id="tuition"]/ul/li/table[3]/tbody/tr[2]/td[3]'
  nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
  if(!is.null(value)){
    vect[2] <- toString(value)
  }else {vect[2] <- NA}
  
  #student population
  xpath <- '//*[@id="academics"]/ul/li/table[1]/tbody/tr[4]/td[2]'
  nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
  if(!is.null(value)){
    vect[3] <- toString(value)
  }else {vect[3] <- NA}
  
  #Undergrad Population
  xpath <- '//*[@id="academics"]/ul/li/table[1]/tbody/tr[5]/td[2]'
  nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
  if(!is.null(value)){
    vect[4] <- toString(value)
  }else {vect[4] <- NA}
  
  #Student to faculty Ratio
  xpath <- '//*[@id="academics"]/ul/li/table[1]/tbody/tr[3]/td[2]'
  nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
  if(!is.null(value)){
    vect[5] <- toString(value)
  }else {vect[5] <- NA}
  
  #In-state tuition
  xpath <- '//*[@id="tuition"]/ul/li/table[1]/tbody/tr[1]/td[2]'
  nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
  if(!is.null(value)){
    vect[6] <- toString(value)
  }else {vect[6] <- NA}
  
  #out-of-state tuition
  xpath <- '//*[@id="tuition"]/ul/li/table[1]/tbody/tr[2]/td[2]'
  nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
  if(!is.null(value)){
    vect[7] <- toString(value)
  }else {vect[7] <- NA}
  
  #% on financial aid
  xpath <- '//*[@id="finaid"]/ul/li/table[1]/tbody/tr[2]/td[2]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
  if(!is.null(value)){
    vect[8] <- toString(value)
  }else {vect[8] <- NA}
  
  #% admitted
  xpath <- '//*[@id="admissions"]/ul/li/table[2]/tbody/tr[2]/td[3]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
  if(!is.null(value)){
    vect[9] <- toString(value)
  }else {vect[9] <- NA}
  
  #% submitting SAT score 
  xpath <- '//*[@id="admissions"]/ul/li/table[3]/tbody/tr[2]/td[2]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
  if(!is.null(value)){
    vect[10] <- toString(value)
  }else {vect[10] <- NA}
  
  #Forbes rank
  xpath <- '//*[@id="collegesTop"]/div[2]/div[3]/div[2]/div[1]/text()'
  nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
  if(!is.null(value)){
    vect[11] <- toString(value)
  }else {vect[11] <- NA}
  
  
  #% full time
  xpath <- '//*[@id="academics"]/ul/li/table[2]/tbody/tr[3]/td[2]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
  if(!is.null(value)){
    vect[12] <- toString(value)
  }else {vect[12] <- NA}
  
  #4 year graduation rate
  xpath <- '//*[@id="academics"]/ul/li/table[2]/tbody/tr[2]/td[6]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
  if(!is.null(value)){
    vect[13] <- toString(value)
  }else {vect[13] <- NA}
  
  #% female
  xpath <- '//*[@id="academics"]/ul/li/table[3]/tbody/tr[3]/td[2]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
  if(!is.null(value)){
    vect[14] <- toString(value)
  }else {vect[14] <- NA}
  
 #NEEDS CLEANING
  #Race- % Native
  xpath <- '//*[@id="academics"]/ul/li/table[4]/tbody/tr[2]/td[2]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
 if(!is.null(value)){
   vect[15] <- toString(value)
 }else {vect[15] <- NA}
  
 #NEEDS CLEANING
  #Race- Asian
  xpath <- '//*[@id="academics"]/ul/li/table[4]/tbody/tr[3]/td[2]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
 if(!is.null(value)){
   vect[16] <- toString(value)
 }else {vect[16] <- NA}
  
 #NEEDS CLEANING
  #Race- Black
  xpath <- '//*[@id="academics"]/ul/li/table[4]/tbody/tr[4]/td[2]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
 if(!is.null(value)){
   vect[17] <- toString(value)
 }else {vect[17] <- NA}
 
 #NEEDS CLEANING
  #Race- Hispanic
  xpath <- '//*[@id="academics"]/ul/li/table[4]/tbody/tr[5]/td[2]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
 if(!is.null(value)){
   vect[18] <- toString(value)
 }else {vect[18] <- NA}
  
 #NEEDS CLEANING
  #Race- White
  xpath <- '//*[@id="academics"]/ul/li/table[4]/tbody/tr[6]/td[2]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
 if(!is.null(value)){
   vect[19] <- toString(value)
 }else {vect[19] <- NA}
  
 #NEEDS CLEANING
  #Race- Two or more
  xpath <- '//*[@id="academics"]/ul/li/table[4]/tbody/tr[7]/td[2]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
 if(!is.null(value)){
   vect[20] <- toString(value)
 }else {vect[20] <- NA}
  
 #NEEDS CLEANING
  #Race- Unknown
  xpath <- '//*[@id="academics"]/ul/li/table[4]/tbody/tr[8]/td[2]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
 if(!is.null(value)){
   vect[21] <- toString(value)
 }else {vect[21] <- NA}
  
 #NEEDS CLEANING
  #Race- Alien
  xpath <- '//*[@id="academics"]/ul/li/table[4]/tbody/tr[9]/td[2]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
 if(!is.null(value)){
   vect[22] <- toString(value)
 }else {vect[22] <- NA}
  
 #NEEDS CLEANING
  #On campus room and board
  xpath <- '//*[@id="tuition"]/ul/li/table[2]/tbody/tr[2]/td[2]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
 if(!is.null(value)){
   vect[23] <- toString(value)
 }else {vect[23] <- NA}
  
 #NEEDS CLEANING
  #off campus room and board
  xpath <- '//*[@id="tuition"]/ul/li/table[2]/tbody/tr[3]/td[2]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
 if(!is.null(value)){
   vect[24] <- toString(value)
 }else {vect[24] <- NA}
  
 #NEEDS CLEANING
  #Average Grant
  xpath <- '//*[@id="finaid"]/ul/li/table[2]/tbody/tr[7]/td[2]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
 if(!is.null(value)){
   vect[25] <- toString(value)
 }else {vect[25] <- NA}
  
 #NEEDS CLEANING
  #% receiving any loans
  xpath <- '//*[@id="finaid"]/ul/li/table[3]/tbody/tr[4]/td[2]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
 if(!is.null(value)){
   vect[26] <- toString(value)
 }else {vect[26] <- NA}
  
 #NEEDS CLEANING
  #Average Loan
  xpath <- '//*[@id="finaid"]/ul/li/table[3]/tbody/tr[4]/td[4]'    
 nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
 if(!is.null(value)){
   vect[27] <- toString(value)
 }else {vect[27] <- NA}
  
 #NEEDS CLEANING
  #Application Fee
  xpath <- '//*[@id="admissions"]/ul/li/table[1]/tbody/tr/td[2]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
 if(!is.null(value)){
   vect[28] <- toString(value)
 }else {vect[28] <- NA}
  
 
  #Total amount of applicants
  xpath <- '//*[@id="admissions"]/ul/li/table[2]/tbody/tr[2]/td[2]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
 if(!is.null(value)){
   vect[29] <- toString(value)
 }else {vect[29] <- NA}
  

  #% Admitted
  xpath <- '//*[@id="admissions"]/ul/li/table[2]/tbody/tr[2]/td[3]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
 if(!is.null(value)){
   vect[30] <- toString(value)
 }else {vect[30] <- NA}
  
  #% admitted that enrolled
  xpath <- '//*[@id="admissions"]/ul/li/table[2]/tbody/tr[2]/td[4]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
 if(!is.null(value)){
   vect[31] <- toString(value)
 }else {vect[31] <- NA}
  
 #NEEDS CLEANING
  #Composite SAT 25th %
  xpath <- '//*[@id="admissions"]/ul/li/table[4]/tbody/tr[2]/td[2]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
 if(!is.null(value)){
   vect[32] <- toString(value)
 }else {vect[32] <- NA}
  
 #NEEDS CLEANING
  #Composite SAT 75th %
  xpath <- '//*[@id="admissions"]/ul/li/table[4]/tbody/tr[3]/td[2]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
 if(!is.null(value)){
   vect[33] <- toString(value)
 }else {vect[33] <- NA}
  
 #NEEDS CLEANING
  #Calendar Type
  xpath <- '//*[@id="stulife"]/ul/li/table[1]/tbody/tr[1]/td[2]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
 if(!is.null(value)){
   vect[34] <- toString(value)
 }else {vect[34] <- NA}
  
 #NEEDS CLEANING
  #Campus housing offered
  xpath <- '//*[@id="stulife"]/ul/li/table[1]/tbody/tr[2]/td[2]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
 if(!is.null(value)){
   vect[35] <- toString(value)
 }else {vect[35] <- NA}
  
 #NEEDS CLEANING
  #Religion affiliation
  xpath <- '//*[@id="stulife"]/ul/li/table[1]/tbody/tr[3]/td[2]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
 if(!is.null(value)){
   vect[36] <- toString(value)
 }else {vect[36] <- NA}
  
  #Campus Setting
  xpath <- '//*[@id="stulife"]/ul/li/table[1]/tbody/tr[4]/td[2]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
 if(!is.null(value)){
   vect[37] <- toString(value)
 }else {vect[37] <- NA}
 
 #NEEDS CLEANING
  #% Student that is varsity athelete
  xpath <- '//*[@id="athletics"]/ul/li/table[1]/tbody/tr[2]/td[2]'
    nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
 if(!is.null(value)){
   vect[38] <- toString(value)
 }else {vect[38] <- NA}
  
 #NEEDS CLEANING
  #Grand total sports revenues
  xpath <- '//*[@id="athletics"]/ul/li/table[4]/tbody/tr[5]/td[4]'
  nodes <- getNodeSet(data, xpath)
  value <- sapply(nodes, xmlValue)
 if(!is.null(value)){
   vect[39] <- toString(value)
 }else {vect[39] <- NA}
  
 vect <- gsub("\n", "", vect)
 vect <- gsub("\t", "", vect)
  
  vect
} #End of extract data function

#Download all of the individual college websites.
mat <- matrix(NA, nrow=500, ncol=39)
i <- 1
while(i <= nrow(topcolleges)){
  
  collegeurl <- topcolleges$full_link[i]
  rawcollege <- getURL(collegeurl)
  nameoffile <- paste0(toString(topcolleges$name[i]), ".Rdata")
  save(rawcollege, file=nameoffile)

  parsed <- htmlParse(rawcollege)
  mat[i,] <- extractdata(parsed)
  
i <- i + 1 
} #End of while loop

#save(mat, file="variables.Rdata")
#load("variables.Rdata")

mat <- data.frame(mat, stringsAsFactors=FALSE)
names(mat) <- c("totalcostinstate", "totalcostoutstate", "studentpopulation",
               "undergradpopulation", "studenttofacultyratio", "instatetuition", "outofstatetuition",
               "onfinancialaid", "admitted", "submitttingsat", "rank",
               "fulltime", "fouryeargrad", "female", 
               "native", "asian", "black", "hispanic", "white", "twoormore", "raceunknown", "alien",
               "oncamphouse", "offcamphouse", "averagegrant", "recevingloan", "averageloan",
               "applicationfee", "amountofapplicants", "accepted", "acceptedenrolled",
               "sat25", "sat75", "calendar", "campushousing", "religion", "setting",
               "athlete", "sportsrevenue")



mat$rank <- gsub("#", "", mat$rank)
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
mat$rank <- trim(mat$rank)

topcolleges$rank <- sapply(topcolleges$rank, toString)

mat <- mat[order(mat$rank),]
topcolleges <- topcolleges[order(topcolleges$rank),]

final <- merge(topcolleges, mat, by="rank")

final <- sapply(final, function(x)sub("\\$","", x))
final[,5:42] <- sapply(final[,5:42], function(x)sub("\\,","", x))
final[,5:42] <- sapply(final[,5:42], function(x)sub("\\,","", x))

percents <- c(12,13,14,15,16,17,18,19, 20,21,22,23,24,25,29 ,33,34,41)

final[,percents] <- sapply(final[,percents], function(x)sub("\\%","", x))

arenumbers <- c(1, 5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,41,42)
arefactors <- c(1, 2,3,37,38,39,40)

final <- as.data.frame(final, stringsAsFactors = FALSE)
final[,arenumbers] <- sapply(final[,arenumbers], as.numeric)
final[,percents] <- sapply(final[,percents], function(x) x/100)

final <- final[order(final$rank),]
final[,arefactors] <- sapply(final[,arefactors], factor)

withoutrank <- c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,41,42)

covariance <- cov(final[,withoutrank],  use = "complete", method = c("pearson", "kendall", "spearman"))
correlationmatrix <- cor(final[,withoutrank],  use="complete",method = c("pearson", "kendall", "spearman"))


for(i in withoutrank){
  final[,i] <- scale(final[,i])
}

final$calendar <- sapply(final$calendar, as.factor)
final$setting <- sapply(final$setting, as.factor)
final$religion <- sapply(final$religion, as.factor)
final$state <- sapply(final$state, as.factor)
final$rank <- sapply(final$state, as.numeric)

#save(final, file="finaltable.Rdata")
#load("finaltable.Rdata")

boxplot(totalcostinstate ~ calendar, data=final)
boxplot(totalcostinstate ~ setting, data=final)
boxplot(totalcostinstate ~ religion, data=final)
boxplot(totalcostinstate ~ state, data=final)

final_vars <- final[,c(1, 3, 5,7, 9, 12,13, 14,15,16,17,22,28,29,30,31,32,33,34,35,36,37,40,41,42)]

final_complete <- final_vars[!is.na(final_vars$totalcostinstate),]
correlationvars <- c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,24,25)

#correlationmatrix <- cor(final_complete[,correlationvars],  use="complete",method = c("pearson", "kendall", "spearman"))


allmodel <- lm(totalcostinstate ~ ., data=final_complete, na.action=na.omit)

nostate <- lm(totalcostinstate ~ . -nostate , data=final_complete, na.action=na.omit)

basemodel <- lm(totalcostinstate ~ rank + acceptedenrolled +  studentpopulation * amountofapplicants + submitttingsat +
                  studenttofacultyratio * fouryeargrad +  onfinancialaid * recevingloan + fulltime + female + white +
                  averageloan * averagegrant + applicationfee + state + 
                sat25 * admitted + calendar+ setting + athlete + sportsrevenue, data=final_complete, na.action=na.omit)

basenostate <- lm(totalcostinstate ~ rank + acceptedenrolled +  studentpopulation * amountofapplicants + submitttingsat +
                  studenttofacultyratio * fouryeargrad +  onfinancialaid * recevingloan + fulltime + female + white +
                  averageloan * averagegrant + applicationfee + 
                  sat25 * admitted + calendar+ setting + athlete + sportsrevenue, data=final_complete, na.action=na.omit)

summary(allmodel)
summary(basenostate)

completecases <- final_complete[complete.cases(final_complete),]

basemodel2 <- lm(totalcostinstate ~ rank + acceptedenrolled +  studentpopulation * amountofapplicants + submitttingsat +
                  studenttofacultyratio * fouryeargrad +  onfinancialaid * recevingloan + fulltime + female + white +
                  averageloan * averagegrant + applicationfee + state + 
                  sat25 * admitted + calendar+ setting + athlete + sportsrevenue, data=completecases)


step(basemodel2,direction="both")

bestbackward<- lm(formula = totalcostinstate ~ acceptedenrolled + amountofapplicants + 
                    studenttofacultyratio + fouryeargrad + onfinancialaid + averageloan + 
                    averagegrant + sat25 + admitted + athlete + rank + studenttofacultyratio:fouryeargrad + 
                    averageloan:averagegrant + sat25:admitted, data = final_complete, na.action=na.omit)


AIC(allmodel, basemodel, basenostate, bestbackward)

vif(bestbackward)
vif(basenostate)

qqPlot(basenostate, main="QQ Plot")
resid <- resid(basenostate)

par(mfrow=c(2,2))
plot(basenostate)

temp1 <- final_complete[,correlationvars]
temp2 <- complete.cases(temp1)
complete_continous <- temp1[temp2,]

prinModel = prcomp(x=complete_continous[,-1], na.rm=TRUE, rotation="varimax")
print(prinModel)
par(mfrow=c(1,1))
plot(prinModel, type = "l")

pcrModel = pcr(totalcostinstate ~ ., 
               data=complete_continous, rotation='Varimax')

plsrModel = plsr(totalcostinstate ~ ., 
                 data=complete_continous, rotation='Varimax')

summary(pcrModel)
summary(plsrModel)

cross <- crossval(plsrModel, segments=5)
plot(MSEP(cross))


loadings <- as.data.frame(plsrModel[1])
names(loadings) <- c("PC1", "PC2", "PC3", "PC4", "PC5",
                     "PC6", "PC7", "PC8", "PC9", "PC10",
                     "PC11", "PC12", "PC13", "PC14", "PC15",
                     "PC16", "PC17", "PC18", "PC19", "PC20")              
loadings[,1:5]


