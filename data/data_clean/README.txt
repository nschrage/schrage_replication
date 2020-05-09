###readme.txt

This folder contains the replication files for "What the Demolition of Public Housing Teaches Us About the Impact of Racial Threat on Political Behavior", Ryan D. Enos, American Journal of Political Science.
 
***You have two options in replicating this manuscript***:
1) Produce the Figures and Tables only.  To do this, run "create_tables_and_figures_only.r" by typing "source('create_tables_and_figures_only.r')" into the R terminal.  This will use processed data to output all figures and tables in the manuscript and appendix.

2) Replicate estimation and produce the figures and tables. This will recreate all estimates used to create the tables and figures. To do this, run "replication_master.r" by typing "source('replication_master.r')" into the R terminal. This will call the other necessary scripts in the folder.

****WARNING****** This estimation is computationally intensive.  The dataset is large and the estimation requires considerable processing power.  It is only recommended that you do this option if you have a powerful computer and reasonable patience.  
 
**Output will be in two directories: 
	*"output": 1 table and 7 figures.  These are the tables and figures in the article.
	*"output_appendix": 19 Figures.  These are the figures from the online appendix.
	
**Prior to executing the scripts, the R terminal should be pointed to the folder "Enos_Chicago_Replication", which can be accomplished by typing "setwd('../Enos_Chicago_Replication')" where ".." is the local file structure where "Enos_Chicago_Replication" is located.

These files require the R programming language, which can be downloaded here http://www.r-project.org/

You may need to install the packages loaded in analysis_master.r, this can be accomplished by typing the following in the R terminal:
install.packages('ei') 
install.packages('MatchIt')
install.packages('weights')
install.packages('simpleboot')
install.packages('Zelig')
install.packages('apsrtable')

Questions? Please contact Ryan Enos (renos@gov.harvard.edu).

***There are three raw data files included in the 'indata_static' directory.  Descriptions of the files and the variables are below.  The other files in that directory are the estimation output used to create the output when "create_tables_and_figures_only.r" is executed. 

***'data.turnout.csv'***
This is file of voters in Chicago, supplemented with data from the U.S. Census and the Cook County Registrar, as described in the article. This file has been anonymized to protect individuals.  
'vote1996': did the voter vote in the 1996 General Election? 0/1
,'vote1998': did the voter vote in the 1998 General Election? 0/1
,'vote2000': did the voter vote in the 2000 General Election? 0/1
'vote2002': did the voter vote in the 2002 General Election? 0/1
'vote2004': did the voter vote in the 2004 General Election? 0/1
'vote.change': change in turnout status between 2000 and 2004, -1/0/1
'reg': date of registration
'p': Party ID, 1 = Republican, 0 = unaligned, -1 = Democrat
's': gender, M/F
'age': age of voter at time of 2004 election
'age.squared': age^2
'demo.distance': Euclidean distance from nearest demolished housing project in 2004
'nondemo.distance': Euclidean distance from nearest non-demolished housing project in 20004
'whitename': probability of being white, based on name and location, as described in article
,'blackname': probability of being black, based on name and location, as described in article
'medianincome': median income of Census Block Group in 2000
'prior.avg.value': value of dwelling place, in dollars, as an average of possible dwelling places, when more than one can be matched to the voter
'deeded.strict': is the deed of the dwelling place of the voter in the name of the voter (using strict name matching criteria)? 0/1
'demo.gid': identifier of the nearest demolished project to the voter in 2004
'pctblack': percent Black in the voters Census Block in 2000
'context_black': percent of the Black population within 1000 meters of the demolished housing project, nearest to the voter in 2004, which lived in the demolished project in 2000.  
id: a unique id of voters


***data.votechoice.2000.csv and data.votechoice.2010.csv***
These are files containing data on electoral wards in Chicago in 1996, 2000, 2004, and 2008.  Depending on the election, the files have been supplemented with the most recent Census data using a process of spatial interpolation when necessary.  Some wards have been renamed for consistency across redistricting and some wards are missing when redistricting created wards which could not be matched across elections.
***data.votechoice.2000.csv***
'ward_pre': unique identifier for the ward
'dole_pct': percent of votes cast for Bob Dole in 1996
'bush2000_pct': percent of votes cast for George Bush in 2000
'votes_cast_1996': total votes cast in 1996 General Election
'votes_cast_2000': total votes cast in 2000 General Election
'nondemo.distance': Euclidean distance of ward to nearest non-demolished project in 2004
'demo.distance': Euclidean distance of ward to nearest demolished project in 2004
'white_median_income': median income of white voters in the ward
'black_median_income': median income of black voters in the ward
'white_name': average probability of being white, based on name and location, as described in article
'black_name': average probability of being black, based on name and location, as described in article
'registrants': total number of registered voters 

***data.votechoice.2010.csv***
'ward_pre': unique identifier for the ward
'obama_sen_primary_pct':  percent of votes cast for Barack Obama in 2004 Senate Primary
'keyes_pct': percent of votes cast for Alan Keyes in 2004 Senate General
'bush2004_pct': percent of votes cast for George Bush in 2000
'obama_pres_primary_pct': percent of votes cast for Barack Obama in 2008 Primary
'mccain_pct': percent of votes cast for John McCain in 2008
'votes_cast_2004_sen_dem_primary': total votes cast in 2004 Democratic Senate Primary
'votes_cast_2004_senate': total votes cast in 2004 Senate General race
'votes_cast_2004_president': : total votes cast in 2004 Presidential race
'votes_cast_2008_president_dem_primary': : total votes cast in 2004 Democratic Presidential Primary
'votes_cast_2008_president': total votes cast in 2008 Presidential race
'nondemo.distance': Euclidean distance of ward to nearest non-demolished project in 2004
'demo.distance': Euclidean distance of ward to nearest demolished project in 2004
'white_median_income': median income of white voters in the ward
'black_median_income': median income of black voters in the ward
'white_name': average probability of being white, based on name and location, as described in article
'black_name': average probability of being black, based on name and location, as described in article
'registrants': total number of registered voters 



