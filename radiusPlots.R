###########
## load and install packages
###########

reqPackages <- c("ggplot2","devtools","scales","gridExtra") #list of required packages#

#install packages if not installed, then oad packages
for (i in c(1:length(reqPackages))) {
  if (reqPackages[i] %in% rownames(installed.packages())==FALSE) {
    install.packages(reqPackages[i])
  }
  library(reqPackages[i],character.only=TRUE)
}

##install and load a package from github
devtools::install_github("jcheng5/bubbles")
library(bubbles)

#########
## load files
#########

fillRate <- read.csv("derivedData//fillRate.csv",stringsAsFactors=FALSE)
fillRateClean <- read.csv("derivedData//fillRateClean.csv",stringsAsFactors=FALSE)
fillRateScrubbed <- read.csv("derivedData//fillRateScrubbed.csv",stringsAsFactors=FALSE)

cardinality <- read.csv("derivedData//cardinality.csv",stringsAsFactors=FALSE)

# read in levenshtein distances for repeated phones
ldRepeat <- read.csv("derivedData/ld_repeats.csv",stringsAsFactors=FALSE)

# read in levenshtein distances for unique phones
ldUnique <- read.csv("derivedData//ld_unique.csv",stringsAsFactors=FALSE)

# read in levenshtein distances on common misspelled words
ldCommon <- read.csv("derivedData//misspelledWordsLD.csv",stringsAsFactors=FALSE)

#read in jaccard distances for repeated phones
jaccardRepeat <- read.csv("derivedData//jaccard_repeat.csv",stringsAsFactors=FALSE)

# read in jaccard distances for unique phones
jaccardUnique <- read.csv("derivedData//jaccard_unique.csv",stringsAsFactors=FALSE)

missing <- read.csv("derivedData//reviewMissing.csv",stringsAsFactors=FALSE)

##########
## functions
##########

prepData <- function(data) {
  # Cleans csv files from the radiusAnalysis.py script
  # Args:
  #   data: A data frame with the names of attributes stored in the first column
  # Returns:
  #  A data frame with attribute names in the first column cleaned and standardized
  
  names(data)[1] <- "ColumnName"
  data[,1] <- gsub("_"," ",fillRate[,1])
  data[,1] <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", data[,1], perl=TRUE)
  data <- within(data, ColumnName <- factor(ColumnName, 
                                                    levels=(sort(ColumnName, decreasing=TRUE))))
  return(data)
}

summaryStats <- function(column,columnName) {
  # Reports on the average, 5th percentile, and 9th percentile of data in column
  # Args:
  #   column: The data for one specific attribute
  #   columnName: The name of the attribute
  # Returns:
  #  Nothing. Prints results to console.
  
  print(paste("Summary statistics on",columnName))
  print(paste("Mean: ",mean(column,na.rm=TRUE)))
  
  columnNoNA <- sort(column[is.na(column)==FALSE])
  print(paste("5th Percentile: ",columnNoNA[0.05*length(columnNoNA)]))
  print(paste("95th Percentile: ",columnNoNA[0.95*length(columnNoNA)]))
}

##########
## assess fill rates on original data set
##########


fillRate <- prepData(fillRate)
fillRate$type <- "Fill Rate on Original Data"

fillRatePlot <-ggplot(fillRate, aes(ColumnName, fillRate))
fillRatePlot <- fillRatePlot+ geom_bar(stat = "identity",fill="#67a9cf") + labs(x="Attribute",y="Fill Rate",title="Fill Rate on Original Data Set")
fillRatePlot <- fillRatePlot + scale_y_continuous(labels = scales::percent) + coord_flip()
ggsave("images//fillRate.png", plot = fillRatePlot, width = 7, height = 4, units = "in")

##########
## assess fill rates after inconsistent missing values are scrubbed
##########


fillRateClean <- prepData(fillRateClean)
fillRateClean$type <- "Stringent"

##compare variance in fill rates between original data set and data set scrubbed of inconsistent values
fillComparison <- merge(fillRate,fillRateClean,by="ColumnName")
fillComparison$variance <- (fillComparison$fillRate.x - fillComparison$fillRate.y)/fillComparison$fillRate.x
fillComparison$variance <- fillComparison$variance * 100

fillRateVariance <- ggplot(fillComparison, aes(ColumnName, variance)) + geom_bar(stat="identity",fill="#7fbf7b")
fillRateVariance <- fillRateVariance + scale_y_continuous(labels = scales::percent) + coord_flip()
fillRateVariance <- fillRateVariance + labs(x="Attribute",y="Variance in Fill Rate",title="Scrubbing Inconsistent Missing Values Lowers Fill Rates")
ggsave("images//fillRateVariance.png", plot = fillRateVariance, width = 7, height = 4, units = "in")

##########
## assess fill rates after non-conforming values in zip, state, and phone are removed
##########


fillRateScrubbed <- prepData(fillRateScrubbed)
fillRateScrubbed$type <- "True Fill Rate on Scrubbed Data"

##compare variance in fill rates between data set scrubbed of inconsistent values and data set scrubbed of non-conforming values
fillComparison <- merge(fillRateClean,fillRateScrubbed,by="ColumnName")
fillComparison$variance <- (fillComparison$fillRate.x - fillComparison$fillRate.y)/fillComparison$fillRate.x
fillComparison$variance <- fillComparison$variance * 100
fillComparison <- fillComparison[fillComparison$ColumnName %in% c("Zip","Phone","State"),]

fillRateVarianceScrub <- ggplot(fillComparison, aes(ColumnName, variance)) + geom_bar(stat="identity",fill="#7fbf7b")
fillRateVarianceScrub <- fillRateVarianceScrub + scale_y_continuous(labels = scales::percent) + coord_flip()
fillRateVarianceScrub <- fillRateVarianceScrub + labs(x="Attribute",y="Variance in Fill Rate",title="Scrubbing Non-Conforming Values Reduces Fill Rates for Zip and State")
ggsave("images//fillRateVarianceScrub.png", plot = fillRateVarianceScrub, width = 7, height = 4, units = "in")

##########
## compare fill rate on original data set and true fill rate on final data set
##########

comparison <- rbind(fillRate,fillRateScrubbed)
comparison <- within(comparison, type <- factor(type, levels=c("True Fill Rate on Scrubbed Data","Fill Rate on Original Data")))

# make graph comparing fill rates under original and scrubbed data sets
compareFillRates <- ggplot(comparison, aes(ColumnName, fillRate, fill = type)) + geom_bar(position = "dodge",stat="identity")
compareFillRates <- compareFillRates + labs(x="Attribute",y="Fill Rate",title="True Fill Rates are Slightly Below Original Fill Rates")
compareFillRates <- compareFillRates + scale_y_continuous(labels = scales::percent) + coord_flip()
compareFillRates <- compareFillRates + scale_fill_manual(name="Data Set",values=c( "#ef8a62","#67a9cf"))
compareFillRates <- compareFillRates + geom_text(aes(x=ColumnName, y=fillRate, ymax=fillRate, label=format(round(fillRate*100,2),nsmall=2), hjust=ifelse(sign(fillRate)>0, 1, 0)), 
               position = position_dodge(width=1),colour="white")
compareFillRates <- compareFillRates+ theme(legend.position="bottom")
ggsave("images//compareFillRates.png", plot = compareFillRates, width = 7, height = 4, units = "in")

# make table listing fill rates under original and scrubbed data sets
prettyCompareFillRate <- cbind(fillRate,fillRateScrubbed)
names(prettyCompareFillRate)[names(prettyCompareFillRate)=="fillRate"] <- c("Fill Rate % on Original Data","True Fill Rate % on Scrubbed Data")
names(prettyCompareFillRate)[names(prettyCompareFillRate)=="ColumnName"] <- "Attribute"
prettyCompareFillRate[,names(prettyCompareFillRate)=="Fill Rate % on Original Data"] <- round(prettyCompareFillRate[,names(prettyCompareFillRate)=="Fill Rate % on Original Data"],5)*100
prettyCompareFillRate[,names(prettyCompareFillRate)=="True Fill Rate % on Scrubbed Data"] <- round(prettyCompareFillRate[,names(prettyCompareFillRate)=="True Fill Rate % on Scrubbed Data"],5)*100

compareFillTable <- qplot(1:nrow(prettyCompareFillRate), 1:nrow(prettyCompareFillRate), geom = "blank") + theme_bw() + theme(line = element_blank(), panel.border = element_blank(), text = element_blank()) +
  annotation_custom(grob = tableGrob(prettyCompareFillRate[,c(1,2,5)]))
ggsave("images//compareFillTable.png", plot = compareFillTable, width = 7, height = 4, units = "in")




##########
## cardinality
##########


cardinality <- prepData(cardinality)

prettyCardinality <- cardinality
names(prettyCardinality) <- c("Attribute","Cardinality")
cardinalityTable <- qplot(1:nrow(prettyCardinality), 1:nrow(prettyCardinality), geom = "blank") + theme_bw() + theme(line = element_blank(), text = element_blank()) +
annotation_custom(grob = tableGrob(prettyCardinality[,1:2]))
cardinalityTable <- cardinalityTable + theme(panel.border = element_blank())
ggsave("images//cardinalityTable.png", plot = cardinalityTable, width = 7, height = 4, units = "in")

##assign variables into different cardinality types
cardinality$attributeType[cardinality[,1] %in% c("Zip","City")] <- "Geographic"
cardinality$attributeType[cardinality[,1] %in% c("Revenue","Headcount","Time In Business")] <- "Descriptive"
cardinality$attributeType[cardinality[,1] %in% c("Name","Phone")] <- "Unique"
cardinality$attributeType[cardinality[,1] %in% c("Category Code","State","Address")] <- "Other"

##plot of descriptive attributes
card_descrip <- ggplot(cardinality[cardinality$attributeType=="Descriptive",], aes(ColumnName, cardinality)) + geom_bar(stat="identity",fill="#998ec3")
card_descrip <- card_descrip + coord_flip() + scale_y_continuous(labels = function (x) floor(x))
card_descrip <- card_descrip + labs(x="Attribute",y="Cardinality",title="Cardinality of Descriptive Attributes")
card_descrip <- card_descrip + geom_text(aes(x=ColumnName, y=cardinality, ymax=cardinality, label=cardinality, hjust=ifelse(sign(cardinality)>0, 1, 0)), 
                   position = position_dodge(width=1),colour="white")
ggsave("images//card_descrip.png", plot = card_descrip, width = 7, height = 4, units = "in")

##plot of geographic attributes
card_geo <- ggplot(cardinality[cardinality$attributeType=="Geographic",], aes(ColumnName, cardinality)) + geom_bar(stat="identity",fill="#998ec3")
card_geo <- card_geo  + coord_flip() + scale_y_continuous(labels = function (x) comma(floor(x)))
card_geo <- card_geo  + labs(x="Attribute",y="Cardinality",title="Cardinality of Geographic Attributes")
card_geo <- card_geo + geom_text(aes(x=ColumnName, y=cardinality, ymax=cardinality, label=comma(cardinality), hjust=ifelse(sign(cardinality)>0, 1, 0)), 
                   position = position_dodge(width=1),colour="white")
ggsave("images//card_geo.png", plot = card_geo, width = 7, height = 4, units = "in")

##transform data set to assess cardinality of attributes expected to be unique
cardinalityUnique <- cardinality[cardinality$attributeType=="Unique",]
names(cardinalityUnique)[names(cardinalityUnique)=="cardinality"] <- "x"
names(cardinalityUnique)[names(cardinalityUnique)=="attributeType"] <- "type"
cardinalityUnique$type <- "Cardinality"

fillRateUnique <- fillRateScrubbed[fillRateScrubbed$ColumnName %in% c("Name","Phone"),]
fillRateUnique$x <- fillRateUnique$fillRate * 1000000
fillRateUnique$type <- "Count of Populated Scrubbed Values"
cardinalityUniqueReview <- rbind(cardinalityUnique[,names(cardinalityUnique) %in% c("type","x","ColumnName")],fillRateUnique[,names(fillRateUnique) %in% c("type","x","ColumnName")])

card_unique <- ggplot(cardinalityUniqueReview, aes(ColumnName, x, fill = type)) + geom_bar(position = "dodge",stat="identity")
card_unique <- card_unique+ labs(x="Attribute",y="",title="Comparison of Cardinality and Populated Scrubbed Values")
card_unique <- card_unique+ coord_flip() +  scale_y_continuous(label=comma)
card_unique <- card_unique+ scale_fill_manual(name="",values=c( "#998ec3","#67a9cf"))
card_unique <- card_unique + geom_text(aes(x=ColumnName, y=x, ymax=x, label=comma(x), hjust=ifelse(sign(x)>0, 1, 0)), 
                   position = position_dodge(width=1),colour="white")
card_unique <- card_unique + theme(legend.position="bottom")
ggsave("images//card_unique.png", plot = card_unique, width = 7, height = 4, units = "in")

##########
## assess misspelled words with Levenshtein Distances
##########

ldRepeat$type <- "Distances from Repeated Phone Numbers"
ldUnique$type <- "Distances from Unique Phone Numbers"
ldCommon$type <- "Distances on Commonly Misspelled Words"

summaryStats(ldRepeat$normalized,"Normalized Levenshtein Distances for Repeated Phones")
summaryStats(ldUnique$normalized,"Normalized Levenshtein Distances for Unique Phones")
summaryStats(ldCommon$normalized,"Normalized Levenshtein Distances for Commonly Misspelled Words")

ldCombined <- rbind(ldRepeat[,names(ldRepeat) %in% c("normalized","type")], ldUnique[,names(ldUnique) %in% c("normalized","type")],ldCommon[,names(ldCommon) %in% c("normalized","type")])
combinedLDPlot <- ggplot(ldCombined, aes(normalized, fill = type)) + geom_density(alpha = 0.5)
combinedLDPlot <-combinedLDPlot + scale_fill_manual(name="",values=c( "#b2e2e2","#238b45","#af8dc3"))
combinedLDPlot <-combinedLDPlot + labs(x="Normalized Levenshtein Distance",y="Density",title="String Changes Are Similar between Businesses with Duplicated Phones vs. Unique Phones")
combinedLDPlot <- combinedLDPlot + theme(legend.position="bottom")

ggsave("images//combinedLD.png", plot = combinedLDPlot, width = 9, height = 4, units = "in")

##########
## assess jaccard similarities
##########


jaccardRepeat$type <- "Distances from Repeated Phone Numbers"
jaccardUnique$type <- "Distances from Unique Phone Numbers"


jaccardRepeatPlot <- ggplot(data=jaccardRepeat, aes(jaccard)) + geom_histogram(fill="#d8b365") + expand_limits(x=c(0,1))
jaccardRepeatPlot <- jaccardRepeatPlot+ labs(x="Jaccard Similarity",y="Frequency",title="Few Businesses with Same Phone Number Share Tokens")
jaccardRepeatPlot <- jaccardRepeatPlot+ scale_y_continuous(label=comma)
ggsave("images//jaccardRepeatPlot.png", plot = jaccardRepeatPlot, width = 6, height = 3, units = "in")

summaryStats(jaccardRepeat$jaccard,"Jaccard Similarities for Repeated Phones")
summaryStats(jaccardUnique$jaccard,"Jaccard Similarities for Unique Phones")

##########
## missing value coding
##########

##identify most common missing values across attributes
mostCommonMissingAcross <- aggregate(missing$frequency, by=(list(level=missing$level)), FUN=sum)
mostCommonMissingAcross <- mostCommonMissingAcross[order(mostCommonMissingAcross$x,decreasing=TRUE),]

focus <- mostCommonMissingAcross[mostCommonMissingAcross$level %in% c("0","none","null",""),]
focus$level[focus$level==""] <- "[space]"

bubbleChart <- bubbles(value = focus$x, label = paste(focus$level,": ",focus$x,sep=""),
                       color = "steelblue",textColor="white"
)

##must take a screenshot of this bubble chart; cannot save widget images
