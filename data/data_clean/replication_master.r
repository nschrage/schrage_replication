##replication_master.r
###replication code for Enos 'What the Demolition of Public Housing Teaches Us About the Impact of Racial Threat on Political Behavior' 
####June 2014, R.D. Enos


rm(list = ls())

##load necessary libraries
###THE PACKAGES BELOW MAY NEED TO BE INSTALLED USING install.packages('x'), WHERE X IS THE PACKAGE NAME
#install.packages("ei",repos = NULL, type="source")
library(ei) 
library(MatchIt)
library(weights)
library(simpleboot)
library(Zelig)
library(apsrtable)


##This portion of the code produces the estimates of voter turnout
cat('################################# \n Beginning turnout estimation... \n ######################## \n \n')
source('scripts/turnout.r')  ##revert back to original numbers before releasing

cat('################################# \n Beginning vote choice estimation... \n ######################## \n \n')
##This portion of the code produces the estimates of vote choice
source('scripts/vote_choice.r')  

cat('################################# \n Creating output... \n ######################## \n \n')
source('scripts/output_create.r')