#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed May 31 19:32:53 2017

@author: elenasmith
"""

from bs4 import BeautifulSoup
import editdistance
import pandas as pd
import urllib

##load webpage with commonly misspelled words
url = "https://en.oxforddictionaries.com/spelling/common-misspellings"
content = urllib.request.urlopen(url)
soup = BeautifulSoup(content)

##extract misspelled words and correct version
scrapedWords = []
for row in soup.findAll('tr'): #each set (misspelled,correct) is stored in a tr element
    thisRow = []
    for word in row.findAll('a'):
        #extract correct word from end of URL link to dictionary definition of word
        strippedWord = word['href'].replace("http://www.oxforddictionaries.com/definition/english/","")
        #some tr elements have two words; only extract the first one
        if (len(strippedWord.split(",")) > 1):
            strippedWord = strippedWord.split(",")[0]
        thisRow.append(strippedWord)
    
    # each misspelled word is stored in the third td element of the tr
    tds = [x for x in row.findAll('td')]  
    if (len(tds)> 2):
        incorrectWord = tds[2].getText()
        if (len(incorrectWord.split(",")) > 1):
            #some td elements have two words; only extract the first one
            incorrectWord = incorrectWord.split(",")[0]
        thisRow.append(incorrectWord)
    # if two elements have been gathered (correct and misspelled), add to data set
    if (len(thisRow)==2):
        scrapedWords.append(thisRow)
        
#drop the header row
scrapedWords = scrapedWords[1:len(scrapedWords)]
                            
# calculate levenshtein distances                        
distances = [[x[0],x[1], editdistance.eval(x[0], x[1])] for x in scrapedWords]        

##convert list of [correct,misspelled] lists into a data frame
header = ['correct','incorrect','levenshteinDistance']
distanceDF = pd.DataFrame(distances)                         
distanceDF.columns = header

#export to CSV
distanceDF.to_csv("derivedData/misspelledWordsLD.csv",sep=",",index=False) 