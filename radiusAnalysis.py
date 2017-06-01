#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu May 25 19:35:26 2017

@author: elenasmith
"""

from collections import Counter
import editdistance
import itertools
import math
import numpy as np
import pandas as pd
import random

###########
## functions
###########

def fillRate(data):
    '''
    Calculates the fill rate for each column in a data frame as the % of cells that are 
    not stored as np.NaN values
    Args:a
        data: A data frame
    Returns:
        fillRateDF: A data frame with the fill rate for each column in data     
    '''
    fillData = pd.isnull(data).sum() / data.shape[0]
    fillRateDF = 1-fillData
    return (fillRateDF)
   
def exploreMissingValues(data):
    '''
    Counts the number of times values appear in each column of a data frame
    Prints the top 10 most frequent values per column for review,
    as missing values tend to repeat and may appear within the top 10 values.
    
    Args:
        data: A data frame
    Returns:
        valueCount: A data frame describing the frequency of values in data.
            The columns are the columns in data.
            The rows are all unique values in the data set
            Each cell [i,j] contains the number of times value i appears in column j
    '''
    
    valueCount = data.apply(pd.Series.value_counts)
    colNames = valueCount.columns.values.tolist()
    
    frequencyTable = pd.DataFrame()
    
    for i in range(len(colNames)):
        valueCount = valueCount.sort_values(by=[colNames[i]], ascending=[False])
        
        thisVarData = valueCount.ix[1:20,i]
        thisVarData = thisVarData.dropna()
        
        counts = list(thisVarData)
        attribute = [colNames[i]] * len(thisVarData)
        
        thisVarTable = pd.DataFrame({'level': thisVarData.index,'frequency':counts,'attribute':attribute})
        
        frequencyTable = frequencyTable.append(thisVarTable)
        
    return(frequencyTable)
    
def trueFillRate(data,missingDictionary,missingList):
    '''
    Args:
        data : A data frame 
        missingDictionary: A dictionary where keys are column names and values are lists with row values that 
            are considered missing  values in that column
        missingList: A list of values considered to be missing values in all columns
    Returns:
        df2: A data frame where white spaces have been stripped and missing values replaced with NaN
    '''
    ##remove spaces or strings that say none and have a space
    df2 = data.apply(lambda x: x.str.strip())
    df2 = df2.replace(missingList,np.NaN) 
    df2 = df2.replace(missingDictionary,np.NaN) 
    return([df2,fillRate(df2)])
    
def cardinality(data):
    '''
    Counts the number of unique values in each column of a data frame.
    The value of np.NaN is not counted toward the cardinality.
    Args:
        data: A data frame
    Returns: 
        cardinalityCount: A data frame with the number of unique values in each column of data
    '''
    cardinalityCount = data.apply(lambda x: len(set(x.dropna())),axis=0)
    return(cardinalityCount)


def cleanPhones(column):
    '''
    Removes any non-numeric character from a column
    Args:
            Column: A series 
    Returns:
            The series with non-numeric characters removed
    '''
    column  = column.astype(str)
    column = column.str.replace(r'[^0-9]', '')
    return(column)   

def exportResults(data,columnNames,fileName):
    '''
    Exports a data frame to a csv file
    Args:
        data: The data frame to export 
        columnNames: A list with the names of columns in data frame
        fileName: The filepath and name of the export file
    Returns:
        Nothing
    '''  
    df = pd.DataFrame(data)
    df.columns = columnNames
    df.to_csv(fileName,sep=",")    

def findIncorrectLength(column,desiredLength):
    '''
    Looks for indices of a column that do not match the specified length
    Args: 
        column: A series that may continue values with incorrect lengths
        desiredLength: The expected length of each element of the series
    Returns:
        wrongIndices: A list with the indices of elements in column that have incorrect lengths
    '''
    lengths = column.apply(lambda x: len(str(x)))
    
    ##this line is slow
    potentialWrongIndices = [i for i in range(len(lengths)) if lengths[i] != desiredLength]
    
    ##remove NAN values that were returned as potential incorrect indices
    wrongIndices = [x for x in potentialWrongIndices if str(column[x])!='nan']                         
    return(wrongIndices)
    
def jaccard(x,y):
    '''
    Calculates the jaccard similarity between strings x and y
    Args:
        x, y: Strings
    Returns
        The jaccard distance between strings x and y,
        where the jaccard distance = (# tokens in both x and y)/ (# unique tokens in x and y)
    '''
    common = len(set(x.split(" ")).intersection(set(y.split(" "))))
    all = len(set(x.split(" ")).union(set(y.split(" "))))
    return(common/all)    

def normalizedLD(data,name1,name2,ldCol):
    '''
    Derives the normalized Levenshtein distances 
    Arguments:
        Data: A data frame with columns for string 1, string 2, and the Levenshtein distance of string 1 and string 2
        name1: The name of the column storing string 1
        name2: The name of the column storing string 2
        ldCol: The name of the column with the Levenshtein distance of strings 1 and 2
    Returns:
        A data frame with appended columns for the maximum string length and normalized Levenshtein distance
    '''    
    data['maxLength'] = data[[name1, name2]].apply(lambda x: max(len(x[0]),len(x[1])),axis=1)
    data['normalized'] = data[ldCol]/data['maxLength']
    return(data)

       
#######
## Load data
#######  
    
# load sample file
df=pd.read_json('data_analysis.json')

# load data frame of commonly misspelled words from scrapeMisspell.py
commonMisspell = pd.read_csv('derivedData/misspelledWordsLD.csv')
    
     
#######
## Variables: Customize when appropriate
#######

# store variables that are considered missing values in every column
generalMissing = ['none','null','','nan']

# store missing values specific to each column
# key=column name; value = list of missing values
missingValues = {'time_in_business':['0'], 'phone':['0'], 'headcount':['0'],'revenue':['0']}

# store list of valid state abbreviations
validStates = ["AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DC", "DE", "FL", "GA", 
          "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
          "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
          "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
          "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY","PR", "VI"]

# store the expected lengths of columns where values should be a certain length          
columnLengthDict = {'zip':5, 'phone':10,'state':2}

# store the names of columns with string values
stringCols = ['address', 'city', 'state','name', 'revenue', 'time_in_business']

#######
## Explore Data
#######

# Identify values that appear frequently in each column
# Review frequent values to identify missing value proxies
reviewMissing = exploreMissingValues(df)
exportResults(reviewMissing,reviewMissing.columns.values,"derivedData/reviewMissing.csv")

# Assess Fill Rate of Original Data Set
exportResults(fillRate(df),['fillRate'],"derivedData/fillRate.csv")

#######
## Clean Data
#######

# Assess True Fill Rate of Data Set where missing values are scrubbed
cleanedDF, fillRateClean = trueFillRate(df,missingValues,generalMissing)
exportResults(fillRateClean,['fillRate'],"derivedData/fillRateClean.csv")

# Scrub Phone Numbers that are not 10 digits long
cleanedDF['phone'] = cleanPhones(cleanedDF['phone'] )
possibleWrongPhones= findIncorrectLength(cleanedDF['phone'],10)
cleanedDF['phone'][possibleWrongPhones] = np.NaN

# Clean Zip Code data
# If Zips are 4 digits long and Business is in a State with 0-Prefix Zips, Insert 0 Prefix
# Otherwise, scrub zips of incorrect length
possiblyWrongZips = findIncorrectLength(cleanedDF['zip'],5)
states0PrefixZip = ['CT','MA','ME','NH','NJ','RI','VT']
fixedZips = ['0'+x if y in states0PrefixZip else np.NaN for x,y in zip(cleanedDF['zip'][possiblyWrongZips],cleanedDF['state'][possiblyWrongZips])]
cleanedDF['zip'][possiblyWrongZips] = fixedZips
wrongZips = findIncorrectLength(cleanedDF['zip'],5)
cleanedDF['zip'][wrongZips] = np.NaN

# Scrub states that are not in the list of valid state abbreviations
cleanedDF['state'] = [np.NaN if x not in validStates else x for x in cleanedDF['state']]

# Standardize strings to upper case so that cardinality is not case-sensitive
for i in range(len(stringCols)):
    cleanedDF[stringCols[i]] =cleanedDF[stringCols[i]].str.upper()

#######
## Assess cleaned vs. original data 
#######

# Measure cardinality of scrubbed data
cleanCardinality = cardinality(cleanedDF)
exportResults(cleanCardinality,['cardinality'],"derivedData/cardinality.csv")

# How many name + address entries are unique?
uniqueBusinesses= pd.Series(cleanedDF['name']).str.cat(cleanedDF['address'], sep='')
uniqueBusinessesDF = pd.DataFrame({'name+address':uniqueBusinesses,'name':cleanedDF['name']})
print("*****")
print("Unique Name + Address Combination Strings: do businesses Repeat due to multiple locations?")
print(cardinality(uniqueBusinessesDF))
      
# How many name + phone entries are unique?
uniqueBusinessPhones= pd.Series(cleanedDF['name']).str.cat(cleanedDF['phone'], sep='')
uniqueBusinessPhonesDF = pd.DataFrame({'name+phone':uniqueBusinessPhones,'name':cleanedDF['name']})
print("Unique Name + Address Combination Strings: do phones repeat due to central company hotlines? ")
print(cardinality(uniqueBusinessPhonesDF))
   
# Assess True Fill Rate
cleanedDF, fillRateScrubbed = trueFillRate(cleanedDF,missingValues,generalMissing)
exportResults(fillRateScrubbed,['fillRate'],"derivedData/fillRateScrubbed.csv")

##########
## Identify Phones Associated with Multiple Businsesses
###########

phoneCounts = cleanedDF['phone'].value_counts()
atLeast2xRepeatPhones = list(phoneCounts[phoneCounts > 1].index)

# Focus on phones that appear at least 3 times
repeatedPhones = list(phoneCounts[phoneCounts > 2].index)

##########
## Assess Levenshtein Distances on Business Namess from Repeated Phones
###########

print("Analyzing Levenshtein Distances of Repeated Phone Numbers....")
print("Percent Complete....")
repeatNameDist = []
for i in range(len(repeatedPhones)):
    if i % 100 == 0:
        print(str(math.floor((i/len(repeatedPhones))*100))+"%")
    thisPhoneData = cleanedDF[cleanedDF['phone']==repeatedPhones[i]]
    nameCombos = list(itertools.combinations(thisPhoneData['name'], 2))
       
    distances = [[repeatedPhones[i],x[0],x[1], editdistance.eval(x[0], x[1])] for x in nameCombos]
    repeatNameDist.extend(distances)
    ##each element of nameCombos is (name1, name2)
    ##each element of distances is [name1,name2,distance]


# save levenshtein distance data into a data frame
header = ['phone','name1','name2','levenshteinDistance']
repeatNameDF = pd.DataFrame(repeatNameDist)                         
repeatNameDF.columns = header

# Derive normalized Levenshtein distance
repeatNameDF = normalizedLD(repeatNameDF,'name1','name2','levenshteinDistance')
exportResults(repeatNameDF,repeatNameDF.columns.values,"derivedData/ld_repeats.csv")


##########
## Assess Levenshtein Distances on Business Namess with Unique Phones
###########

# pull data set with unique phone numbers
uniquePhones = list(phoneCounts[phoneCounts ==1].index)
uniquePhonesDF = cleanedDF[cleanedDF['phone'].isin(uniquePhones)]

# pick random sample of 1000 unique phone numbers
random.seed(123)
indices = random.sample(range(0,len(uniquePhones)), 1000)
uniquePhonesSample = uniquePhonesDF.iloc[indices] 

# Calculate Levenshtein distances between businesses (b_i, b_j) with different phone numbers
nameCombos = list(itertools.combinations(uniquePhonesSample['name'], 2))
distances = [[x[0],x[1], editdistance.eval(x[0], x[1])] for x in nameCombos]
header = ['name1','name2','levenshteinDistance']
uniquePhoneDist = pd.DataFrame(distances)                         
uniquePhoneDist.columns = header

# Derive normalized Levenshtein distance
uniquePhoneDist = normalizedLD(uniquePhoneDist,'name1','name2','levenshteinDistance')
exportResults(uniquePhoneDist,uniquePhoneDist.columns.values,"derivedData/ld_unique.csv")

##########
## Assess Levenshtein Distances on commonly misspelled words
###########

# Derive normalized Levenshtein distance
commonMisspell = normalizedLD(commonMisspell,'correct','incorrect','levenshteinDistance')
exportResults(uniquePhoneDist,uniquePhoneDist.columns.values,"derivedData/misspelledWordsLD.csv")


##########
## Compute Jaccard Similarity
###########

# Calculate Jaccard similarities
uniquePhoneDist['jaccard'] = uniquePhoneDist[['name1','name2']].apply(lambda x: jaccard(x[0],x[1]),axis=1)
repeatNameDF['jaccard'] = repeatNameDF[['name1','name2']].apply(lambda x: jaccard(x[0],x[1]),axis=1)

# Export results
exportResults(uniquePhoneDist,uniquePhoneDist.columns.values,"derivedData/jaccard_unique.csv")
exportResults(repeatNameDF,repeatNameDF.columns.values,"derivedData/jaccard_repeat.csv")

