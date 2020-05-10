# Replication of Enos (2016)



## Introduction

This repository contains my attempt at replicating a published paper by a respected academic. It is the final for [Gov 1006](https://www.davidkane.info/files/gov_1006_spring_2020.html), a class that has taught me a seemingly endless amount about data science, R, and life's most pressing questions. All analysis for this paper is available in my [repository](https://github.com/nschrage/schrage_replication). The paper I have selected is by [Ryan D. Enos](http://ryandenos.com/) entitled ["What the Demolition of Public Housing Teaches Us about the Impact of Racial Threat on Political Behavior."](https://onlinelibrary.wiley.com/doi/abs/10.1111/ajps.12156) Thank you to Professor Enos for providing easily accessible [replication data](http://dvn.iq.harvard.edu/dvn/dv/ajps) through the [Harvard Dataverse](https://dataverse.harvard.edu/).

**A note on reproducibility:** The data set involved with this paper/replication is quite large. If you are going to try to reproduce my results, you will have to set up [Github Large File Sharing](https://git-lfs.github.com/). 

## Abstract

Enos (2016) measures the shift in voter turnout for white voters living in Chicago near demolished public housing, occupied predominantly by African Americans, as compared to white voters living farther away; observing that white voters living in close proximity to demolished public housing had a 10 percentage point drop in voter turnout between 2000 and 2004, Enos concludes that this change in behavior was the result of the decline in race threat from the change in size and proximity of the outgroup population. The results of my replication effort were largely successful, although there were some challenges. For my extension, I expanded the parallel trends robustness check that Enos presents in his appendix; my results were consistent with his findings. These results are significant in two important ways: first, they illustrate the strength of the robustness checks that Enos conducted and, second, they suggest that his conclusions about the effect of racial threat on voting are even more robust than his paper suggests.

## Contents
* schrage_replication: replication of pdf
* schrage_bib: bibliography
* data 
  * data_clean
     * untouched dataverse files
     * copy of enos (2016)
  * data_commented
    * commented dataverse files
  * images
    * image of fig one
    * image of fig a.two




