##vote_choice.r
###estimate the quantities for the vote choice section of Enos Chicago AJPS
####RdE June 2014

##use king's ei method to estimate vote choice by race
##do it separately by year

##load data, one data set for each Census/redistricting period
data.2000 = read.csv('indata_static/data.votechoice.2000.csv')
data.2010 = read.csv('indata_static/data.votechoice.2010.csv')

cat('Begin EI estimation \n')

years = c('2000','2010')
##cycle through each data set and estimate the black and white vote for candidates
for(year in years){
	if(year == '2000'){
		ts = c('dole_pct','bush2000_pct') ##dependent variables
		ns = c('votes_cast_1996','votes_cast_2000') ##turnout
		ei.white = as.data.frame(data.2000[,'ward_pre']) ##divide data into black and white
		ei.black = as.data.frame(data.2000[,'ward_pre'])		
		use.data = data.2000
		}
	if(year == '2010'){
		ts = c('obama_sen_primary_pct', 'keyes_pct',	'bush2004_pct', 'obama_pres_primary_pct','mccain_pct')
		ns = c('votes_cast_2004_sen_dem_primary', 'votes_cast_2004_senate',	'votes_cast_2004_president', 'votes_cast_2008_president_dem_primary','votes_cast_2008_president')
		ei.white = as.data.frame(data.2010[,'ward_pre'])
		ei.black = as.data.frame(data.2010[,'ward_pre'])	
		use.data = data.2010		
		}	
	colnames(ei.white) = 'ward_pre'	
	colnames(ei.black) = 'ward_pre'	


	for(i in 1:length(ts)){
		##use Enos name estimations to approximate number of white and black residents in each precinct
		ei.data.king.white = use.data[,c('white_name',ts[i],ns[i],'ward_pre')]
		ei.data.king.black = use.data[,c('black_name',ts[i],ns[i],'ward_pre')]
	
		##limit to complete cases
		ei.data.king.white = ei.data.king.white[complete.cases(ei.data.king.white),]
		ei.data.king.black = ei.data.king.black[complete.cases(ei.data.king.black),]
	
		##formulas for EI estimation
		white.formula = as.formula(paste(ts[i], ' ~ white_name',sep = ''))
		black.formula = as.formula(paste(ts[i], ' ~ black_name',sep = ''))
	
		##perform EI using King's method
		res.king.white = ei(formula = white.formula,total = ns[i],
			data=ei.data.king.white)		
		res.king.black = ei(formula = black.formula,total = ns[i],
			data=ei.data.king.black)
	
		##extract vote estimations and recombine with wards, name new variable
		ei.data.king.white$votes = res.king.white$betab		
		output.white = ei.data.king.white[,c('ward_pre','votes')]			
		colnames(output.white) = c('ward_pre',paste(ts[i],'ei',sep = '_'))
	
		ei.data.king.black$votes = res.king.black$betab		
		output.black = ei.data.king.black[,c('ward_pre','votes')]			
		colnames(output.black) = c('ward_pre',paste(ts[i],'ei',sep = '_'))
		
		##merge back with existing data for use later
		ei.white = merge(ei.white,output.white, 
			by = 'ward_pre',
			all.x = T, all.y = F)
		ei.black = merge(ei.black,output.black, 
			by = 'ward_pre',
			all.x = T, all.y = F)
		}
	##name for use below
	if(year == '2000'){
		ei.white.2000 = ei.white
		ei.black.2000 = ei.black
		}		
	if(year == '2010'){
		ei.white.2010 = ei.white
		ei.black.2010 = ei.black		
		}
	}
	
time.elapsed = proc.time() - ptm  ##end time
cat(paste('total elapsed time:', time.elapsed[3],'\n \n',sep = ' '))
	
##########################################################
#####now use these data to estimate differences in voting between precincts near and far from demolished projects
#################################################
cat('Begin estimation of vote differences between treated and control \n')

distance.subset = 1000 ##SETS THE DISTANCE UNDER WHICH TO ANALYZE THE RELATIONSHIP

##mats for storing results
outmat.distance = matrix(ncol = 10,nrow = 0)
outmat.demolished = matrix(ncol = 10,nrow = 0)

##variables needed for estimation for white and black subjects, will vary between them
whitevars = c('demo.distance','nondemo.distance','white_median_income','white.weight')
blackvars = c('demo.distance','nondemo.distance','black_median_income','black.weight')


###create formulas for below
white.treated.form = 'treated ~ white_median_income'
black.treated.form = 'treated ~ black_median_income'



##again cycle through datasets
for(year in years){
	if(year == 2000){
		usedata.black = merge(data.2000, ei.black.2000,by = 'ward_pre', all = F)
		usedata.white = merge(data.2000, ei.white.2000,by = 'ward_pre', all.x=F,all.y=F)
		deps = c('dole_pct_ei','bush2000_pct_ei')		
		}		
	if(year == 2010){
		usedata.black = merge(data.2010, ei.black.2010,by = 'ward_pre', all = F)
		usedata.white = merge(data.2010, ei.white.2010,by = 'ward_pre', all.x=F,all.y=F)
		deps = c('obama_sen_primary_pct_ei','keyes_pct_ei','bush2004_pct_ei','obama_pres_primary_pct_ei','mccain_pct_ei')
		}		


	##drop O'Hare precincts, this can be left in with no substantive consequence
	usedata.black = usedata.black[usedata.black$ward_pre != '41 27',]
	usedata.white = usedata.white[usedata.white$ward_pre != '41 27',]

	##DROP WHITE PRECINCTS WITH 0 WHITE INCOME AND SAME FOR BLACK, THESE PRECINCTS DO NOT HAVE THESE GROUPS
	usedata.black = usedata.black[usedata.black$black_median_income>0,]
	usedata.white = usedata.white[usedata.white$white_median_income>0,]


		##weights
		usedata.black$black.weight = usedata.black$registrants*usedata.black$black_name 
		usedata.white$white.weight = usedata.white$registrants*usedata.white$white_name 
		##force mean of weights to 1, this helps with sensible weighting below		
#		usedata.black$black.weight = usedata.black$black.weight/mean(usedata.black$black.weight, na.rm = T)
#		usedata.white$white.weight = usedata.white$white.weight/mean(usedata.white$white.weight, na.rm = T)
		
	
	##cycle through each dependent variable, in each year and find average vote by racial group
	for(i in 1:length(deps)){					
		for(j in c('distance','demolished')){ ##define treatment by distance and demolished/non-demolished, estimate for each
			thesevars = c(whitevars,deps[i])
			usedata = usedata.white[,thesevars]
			usedata = usedata[complete.cases(usedata),]	
			if(j == 'demolished'){
				usedata = usedata[usedata$demo.distance<=distance.subset | usedata$nondemo.distance<=distance.subset,]
				}		
			usedata$treated = ifelse(usedata$demo.distance<=distance.subset,1,0)
		
			##define subset for use below that does not include precincts near intact projects, do the same below, but exclude demolished 	
			m.out = matchit(as.formula(white.treated.form),
				data=	usedata,
				method = 'nearest',
				replace = F)  ##can replaced with replace = T to demonstrate robustness
			m.data = match.data(m.out)
			m.data$white.weight = m.data$white.weight/mean(m.data$white.weight, na.rm = T)
			
			##weighted t-test		
			out.t.test = wtd.t.test(x = m.data[m.data$treated==1,deps[i]], 
				y = m.data[m.data$treated==0,deps[i]],
				weight =  m.data$white.weight[m.data$treated==1],
				weighty =  m.data$white.weight[m.data$treated==0])
				
			out.N = nrow(m.data[m.data$treated==1,])	
			##put together results for output in matrix
			outres = c(deps[i],'white',out.t.test$coefficients,out.t.test$additional,out.N)		
			if(j == 'distance'){
				outmat.distance = rbind(outmat.distance,outres)	
				}
			if(j == 'demolished'){
				outmat.demolished = rbind(outmat.demolished,outres)	
				}
		###for black vote		
			thesevars = c(blackvars,deps[i])
		
			usedata = usedata.black[,thesevars]
			usedata = usedata[complete.cases(usedata),]
			if(j == 'demolished'){
				usedata = usedata[usedata$demo.distance<=distance.subset | usedata$nondemo.distance<=distance.subset,]
				}			
			usedata$treated = ifelse(usedata$demo.distance<=distance.subset,1,0)
			##define subset for use below that does not include precincts near intact projects, do the same below, but exclude demolished 	
				m.out = matchit(as.formula(black.treated.form),
					data=	usedata,
					method = 'nearest',
					replace = F)  ##can replaced with replace = T to demonstrate robustness
				m.data = match.data(m.out)
			m.data$black.weight = m.data$black.weight/mean(m.data$black.weight, na.rm = T)

			
			out.t.test = wtd.t.test(x = m.data[m.data$treated==1,deps[i]], 
				y = m.data[m.data$treated==0,deps[i]],
				weight =  m.data$black.weight[m.data$treated==1],
				weighty =  m.data$black.weight[m.data$treated==0])
				
			out.N = nrow(m.data[m.data$treated==1,])	
			##put together results for output in matrix
			outres = c(deps[i],'black',out.t.test$coefficients,out.t.test$additional,out.N)		
			if(j == 'distance'){
				outmat.distance = rbind(outmat.distance,outres)	
				}
			if(j == 'demolished'){
				outmat.demolished = rbind(outmat.demolished,outres)	
				}	
			}
			}		
		}
	
##save results
rownames(outmat.distance) = NULL
outmat.distance = as.data.frame(outmat.distance)
colnames(outmat.distance) = c('election','group','t.value','df','p','diff','x.mean','y.mean','sd','treated.N')
outmat.distance$t.value = as.numeric(levels(outmat.distance[,'t.value']))[outmat.distance[,'t.value']]
outmat.distance$df = as.numeric(levels(outmat.distance[,'df']))[outmat.distance[,'df']]
outmat.distance$p = as.numeric(levels(outmat.distance[,'p']))[outmat.distance[,'p']]
outmat.distance$diff = as.numeric(levels(outmat.distance[,'diff']))[outmat.distance[,'diff']]
outmat.distance$x.mean = as.numeric(levels(outmat.distance[,'x.mean']))[outmat.distance[,'x.mean']]
outmat.distance$y.mean = as.numeric(levels(outmat.distance[,'y.mean']))[outmat.distance[,'y.mean']]
outmat.distance$sd = as.numeric(levels(outmat.distance[,'sd']))[outmat.distance[,'sd']]
outmat.distance$treated.N = as.numeric(levels(outmat.distance[,'treated.N']))[outmat.distance[,'treated.N']]

rownames(outmat.demolished) = NULL
outmat.demolished = as.data.frame(outmat.demolished)
colnames(outmat.demolished) = c('election','group','t.value','df','p','diff','x.mean','y.mean','sd','treated.N')
outmat.demolished$t.value = as.numeric(levels(outmat.demolished[,'t.value']))[outmat.demolished[,'t.value']]
outmat.demolished$df = as.numeric(levels(outmat.demolished[,'df']))[outmat.demolished[,'df']]
outmat.demolished$p = as.numeric(levels(outmat.demolished[,'p']))[outmat.demolished[,'p']]
outmat.demolished$diff = as.numeric(levels(outmat.demolished[,'diff']))[outmat.demolished[,'diff']]
outmat.demolished$x.mean = as.numeric(levels(outmat.demolished[,'x.mean']))[outmat.demolished[,'x.mean']]
outmat.demolished$y.mean = as.numeric(levels(outmat.demolished[,'y.mean']))[outmat.demolished[,'y.mean']]
outmat.demolished$sd = as.numeric(levels(outmat.demolished[,'sd']))[outmat.demolished[,'sd']]
outmat.demolished$treated.N = as.numeric(levels(outmat.demolished[,'treated.N']))[outmat.demolished[,'treated.N']]

##rename for plotting
distance.vote.differences = outmat.distance
demolished.vote.differences = outmat.demolished

	
time.elapsed = proc.time() - ptm  ##end time
cat(paste('total elapsed time:', time.elapsed[3],'\n \n',sep = ' '))
	
	