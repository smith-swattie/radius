# radius

This file contains scripts used to analyze a sample file of business records. The sample file (data_analysis.json) is not saved in this directory, but it is 
available for download at https://github.com/RadiusIntelligence/datascience-cc-1.

This repository includes:
1) radiusAnalysis.py: A script to analyze the sample business file and export out derived data to the /derivedData folder.
2) scrapeMisspell.py: A script to download misspelled words from the web and export out derived data to the /derivedData folder.
3) radiusPlots.R: An R script that takes the exported derived data, creates plots, and saves plots to the /images folder.
4) report.rmd: An R Markdown file summarizing insights from the analysis.
5) smith_report.pdf: A PDF version of the R Markdown report.

Please note:
--One image in the R Markdown report (missingValues.png) cannot be saved directly by running the radiusPlots.R. 
This image is created by a widget, and widget images cannot be saved in R. 
--The files ld_unique.csv and jaccard_unique.csv are not saved on github due to space limitations. Re-run the radiusAnalysis.py script to create the files.
