##turnout.r
###estimate the quantities for the turnout section of Enos Chicago AJPS
####RdE June 2014

ptm <- proc.time()  ##start time of analysis


data = read.csv('indata_static/data.turnout.csv')
##set some data as factors for use below
data$reg = as.Date(data$reg)
data$p = as.factor(data$p)
data$s = as.factor(data$s)

##distances used repeatedly in estimation below
dists = seq(from = 100, to = 1000, by = 100)


##basic diff in diffs in paper, estimated across multiple definitions of white and distances
cat('begin basic difference-in-differences estimation \n')

namepcts = c(seq(from = .91, to = .96, by = .01),.975,.99,1)

##matrices for stroing results
res.mat = matrix(nrow=length(namepcts),ncol=length(dists))

white.treat.N = res.mat
white.treat.effect.mean.boot = res.mat
white.treat.effect.conf.boot.lower = res.mat
white.treat.effect.conf.boot.upper = res.mat

black.treat.N = res.mat
black.treat.effect.mean.boot = res.mat
black.treat.effect.conf.boot.lower = res.mat
black.treat.effect.conf.boot.upper = res.mat
####################################

###registration is Illionis is cutoff 27 days prior to election day, limit to these individuals
use.data = data[data$reg<"2000-10-10"&is.na(data$reg)==F,]

##loop through definitions of white and distances and estimate at each combination
for(j in 1:length(namepcts)){
	##define a treatment and control group for each name percent
	useW = use.data[use.data$whitename>=namepcts[j],]
   useB = use.data[use.data$blackname>=namepcts[j],]
  
    for(h in 1:length(dists)){
      	Wtreat = useW[useW$demo.distance<=dists[h],]
      	Btreat = useB[useB$demo.distance<=dists[h],]
      	Wcont = useW[useW$demo.distance>dists[h],]
      	Bcont = useB[useB$demo.distance>dists[h],]     		
	
      	white.treat.N[j,h] = nrow(Wtreat)
      	black.treat.N[j,h] = nrow(Btreat)
	      	
	   ##for white and black subjects, perform t test of differences of means with boostrapped standard errors  	
		if(white.treat.N[j,h] > 0){
			white.boot = two.boot((Wtreat$vote2004-Wtreat$vote2000),(Wcont$vote2004-Wcont$vote2000),mean, R = 1000, na.rm=T)
			white.treat.effect.mean.boot[j,h] = white.boot$t0
			white.boot.ci = boot.ci(white.boot, type = 'basic')
			white.treat.effect.conf.boot.lower[j,h] = white.boot.ci$basic[4]
			white.treat.effect.conf.boot.upper[j,h] = white.boot.ci$basic[5]
		      		}
		      		
		if(black.treat.N[j,h] > 0){
			black.boot = two.boot((Btreat$vote2004-Btreat$vote2000),(Bcont$vote2004-Bcont$vote2000),mean, R = 1000, na.rm=T)
			black.treat.effect.mean.boot[j,h] = black.boot$t0
			black.boot.ci = boot.ci(black.boot, type = 'basic')
			black.treat.effect.conf.boot.lower[j,h] = black.boot.ci$basic[4]
			black.treat.effect.conf.boot.upper[j,h] = black.boot.ci$basic[5]		
			 }
			 }
	}
		
	
time.elapsed = proc.time() - ptm  ##end time
cat(paste('total elapsed time:', time.elapsed[3],'\n \n',sep = ' '))
		
################################################################
################################################################
##parallel trends tests
###change in turnout overtime for black and white, treatment and control
########################################

cat('beginning parallel trends test \n')

##these are the elections to look at
elections = c('vote1996','vote1998','vote2000','vote2002','vote2004')

##matrices for storing results
outmat = matrix(nrow=length(elections), ncol=4)
colnames(outmat) = c('white.treatment','white.control','black.treatment','black.control')
##use different registration cutoff here because going all the way back to 1996
use.data = data[data$reg<"1996-10-08"&is.na(data$reg)==F,]

##define a treatment and control group for each name percent
useW = use.data[use.data$whitename>=.975,]
useB = use.data[use.data$blackname>=.975,]

##set distance for parallel trends test to 200 meters, can be tested at other distances too  
Wtreat = useW[useW$demo.distance<=200,]
Btreat = useB[useB$demo.distance<=200,]
Wcont = useW[useW$demo.distance>200,]
Bcont = useB[useB$demo.distance>200,]     		

WtreatN = nrow(Wtreat)
BtreatN = nrow(Btreat)
WcontN = nrow(Wcont)
BcontN = nrow(Bcont)
     
##test turnout across difference elections     
for(i in 1:length(elections)){
		election = elections[i]
		outmat[i,'white.treatment'] = sum(Wtreat[election],na.rm=T)/WtreatN
	   outmat[i,'black.treatment'] = sum(Btreat[election],na.rm=T)/BtreatN
	   outmat[i,'white.control'] = sum(Wcont[election],na.rm=T)/WcontN
	   outmat[i,'black.control'] = sum(Bcont[election],na.rm=T)/BcontN  
	 }
	    
parallel.trends = outmat


time.elapsed = proc.time() - ptm  ##end time
cat(paste('total elapsed time:', time.elapsed[3],'\n \n',sep = ' '))
   
####################################################################
##now test for matched white subjects
##################################################################
cat('beginning tests with matched white subjects \n')

###mats for storage
outmat = matrix(ncol=4,nrow = length(dists))
colnames(outmat) = c('coefficient','stdev','N.treatment','N.control')

##define data that will bs used for series of tests below
white.data = data[data$reg<"2000-10-10"&is.na(data$reg)==F,]

##only need subjects who qualify by name pcts
white.data = white.data[white.data$whitename>=.975,]

##only can use complete cases for matching, so extract those, first extract needed columns
use.data = white.data[,c('vote.change','demo.distance','p','s','age','age.squared','medianincome')]
use.data = use.data[complete.cases(use.data),]

##cycle through distances and match subjects from control groups defined at progressively smaller distances 
for(i in 1:length(dists)){
	use.data$demolished = ifelse(use.data$demo.distance<=dists[i],1,0) ##define treatment group by distance from projects
	##perform match
	m.out = matchit(demolished ~ p + s + age  + medianincome,
		data=use.data,
		method = 'nearest')
	m.data = match.data(m.out) ##extract matched data
	
	##linear regression with the treatment variable and controls
	out.reg = lm(vote.change~demolished + p + s + age + age.squared + medianincome, 
		data = m.data)
	
	##now estimate bootstrapped confidence intervals
	N.R = 1000
	out.boot = lm.boot(out.reg,R = N.R)
	##sometimes bad draws on the sparse factors cause trouble on estimating standard errors, so put in try functions
	##reduce size of simulation for sparse data to point that is needed	
	repeat {
	res.try = try(summary(out.boot), silent = T)
		if(class(res.try) == 'try-error'){
			N.R = N.R - 1
			out.boot = lm.boot(out.reg,R = N.R)		 
		if(N.R < 50){N.R. = 1000}
			} 	
		if(class(res.try) != 'try-error') break()
		}


	outmat[i,'coefficient'] = out.reg$coefficients['demolished']
	outmat[i,'stdev'] = summary(out.boot)$stdev.params['demolished']
	outmat[i,'N.treatment'] =  nrow(m.data[m.data$demolished==T,])
	outmat[i,'N.control'] = nrow(m.data[m.data$demolished==F,])

	}

##rename matrix for use in printing later
white.match.basic = outmat

time.elapsed = proc.time() - ptm  ##end time
cat(paste('total elapsed time:', time.elapsed[3],'\n \n',sep = ' '))


##############################################################
##now test for matched white subjects
##now using property controls
#######################################
cat('beginning tests with matched white subjects using property controls \n')


###mats for storage
outmat = matrix(ncol=4,nrow = length(dists))
colnames(outmat) = c('coefficient','stdev','N.treatment','N.control')

##only can use complete cases for matching, so extract those, first extract needed columns
use.data = white.data[,c('vote.change','demo.distance','p','s','age','age.squared','medianincome', 'prior.avg.value','deeded.strict')]
use.data = use.data[complete.cases(use.data),]

##use same process as above, except control for property ownership and value of property
for(i in 1:length(dists)){
	use.data$demolished = ifelse(use.data$demo.distance<=dists[i] ,1,0)
	m.out = matchit(demolished ~ p + s + age + medianincome+ prior.avg.value +deeded.strict,
		data=use.data,
		method = 'nearest')
	m.data = match.data(m.out)
	
	out.reg = lm(vote.change~demolished+ p + s +age+age.squared+medianincome+ prior.avg.value +deeded.strict, data = m.data)

	##bootstrap the standard errors
	N.R = 1000
	out.boot = lm.boot(out.reg,R = N.R)
	##sometimes bad draws on the sparse factors cause trouble on estimating standard errors, so put in try functions
	repeat {
	res.try = try(summary(out.boot), silent = T)
		if(class(res.try) == 'try-error'){
			N.R = N.R - 1
			out.boot = lm.boot(out.reg,R = N.R)		 
		if(N.R < 50){N.R. = 1000}
			} 	
		if(class(res.try) != 'try-error') break()
		}

	outmat[i,'coefficient'] = out.reg$coefficients['demolished']
	outmat[i,'stdev'] = summary(out.boot)$stdev.params['demolished']
	outmat[i,'N.treatment'] =  nrow(m.data[m.data$demolished==T,])
	outmat[i,'N.control'] = nrow(m.data[m.data$demolished==F,])
	}

##rename matrix for use in printing later
white.match.basic.property = outmat

time.elapsed = proc.time() - ptm  ##end time
cat(paste('total elapsed time:', time.elapsed[3],'\n \n',sep = ' '))



##############################################################
##now test for matched white subjects against other whites near non-demolished projects
#######################################
cat('beginning tests with matched white subjects near non-demolished projects \n')

outmat = matrix(ncol=4,nrow = length(dists))
outmat.diffs = matrix(ncol=4,nrow = length(dists))
colnames(outmat) = c('coefficient','stdev','N.treatment','N.control')
colnames(outmat.diffs) = c('mean.diff','low.ci','high.ci','N')


use.data = white.data[,c('vote.change','demo.distance','nondemo.distance','p','s','age','age.squared','medianincome')]
use.data = use.data[complete.cases(use.data),]

for(i in 1:length(dists)){
	##define treatment group as living near demolished or non-demolished project
	this.data = use.data[use.data$demo.distance<=dists[i]|use.data$nondemo.distance<=dists[i],]

	##create variable separating demolished from non-demolished
	this.data$demolished = ifelse(this.data$demo.distance<=dists[i] ,1,0)

	this.data$combo.distance = ifelse(this.data$demolished == 1, this.data$demo.distance, this.data$nondemo.distance)
	m.out = matchit(demolished ~ combo.distance + p + s + age + age.squared + medianincome,
		data=this.data,
		method = 'nearest',
		replace = T)  ##can replaced with replace = T to demonstrate robustness
	m.data = match.data(m.out)
	
	out.reg = lm(vote.change~demolished+combo.distance+ p + s +age+age.squared+medianincome, data = m.data)

	##bootstrap the standard errors
	N.R = 1000
	out.boot = lm.boot(out.reg,1000)
	##sometimes bad draws on the sparse factors cause trouble on estimating standard errors, so put in try functions
	repeat {
	res.try = try(summary(out.boot), silent = T)
		if(class(res.try) == 'try-error'){
			N.R = N.R - 1
			out.boot = lm.boot(out.reg,R = N.R)		 
		if(N.R < 50){N.R. = 1000}
			} 	
		if(class(res.try) != 'try-error') break()
		}

	##bootstrap diff in diff
	mean.boot = two.boot(sample1 = m.data$vote.change[m.data$demolished==1],
	 		sample2 = m.data$vote.change[m.data$demolished==0],	
		FUN = mean,
		R = 10000, ##need many draws because ci's below require them
		na.rm = T)
	ci.boot = boot.ci(mean.boot)
	
	outmat[i,'coefficient'] = out.reg$coefficients['demolished']
	outmat[i,'stdev'] = summary(out.boot)$stdev.params['demolished']
	outmat[i,'N.treatment'] =  nrow(m.data[m.data$demolished==T,])
	outmat[i,'N.control'] = nrow(m.data[m.data$demolished==F,])

	outmat.diffs[i,'mean.diff'] = mean.boot$t0
	outmat.diffs[i,'low.ci'] = ci.boot$normal[2]##extracting the normal approximation, rather than regular, for 95% confidence interval
	outmat.diffs[i,'high.ci'] = ci.boot$basic[3]
	outmat.diffs[i,'N'] = length(mean.boot$data)   
	
	}

##rename matrix for use in printing later
white.match.nondemolished = outmat
white.match.nondemolished.diffs = outmat.diffs

time.elapsed = proc.time() - ptm  ##end time
cat(paste('total elapsed time:', time.elapsed[3],'\n \n',sep = ' '))





##############################################################
##now test for matched white subjects against other whites near non-demolished projects
##controlling for property
#######################################
cat('beginning tests with matched white subjects near non-demolished projects with property controls \n')

outmat = matrix(ncol=4,nrow = length(dists))
outmat.diffs = matrix(ncol=4,nrow = length(dists))
colnames(outmat) = c('coefficient','stdev','N.treatment','N.control')
colnames(outmat.diffs) = c('mean.diff','low.ci','high.ci','N')


use.data = white.data[,c('vote.change','demo.distance','nondemo.distance','p','s','age','age.squared','medianincome','prior.avg.value','deeded.strict')]
use.data = use.data[complete.cases(use.data),]


for(i in 1:length(dists)){
	this.data = use.data[use.data$demo.distance<=dists[i]|use.data$nondemo.distance<=dists[i],]

	##create variable separating demolished from non-demolished
	this.data$demolished = ifelse(this.data$demo.distance<=dists[i],1,0)

	this.data$combo.distance = ifelse(this.data$demolished == 1, this.data$demo.distance, this.data$nondemo.distance)
	m.out = matchit(demolished ~ combo.distance + p + s + age + age.squared + medianincome + prior.avg.value +deeded.strict,
		data=this.data,
		method = 'nearest',
		replace = T)  ##can replaced with replace = T to demonstrate robustness
	m.data = match.data(m.out)

	out.reg = lm(vote.change~demolished+combo.distance+ p + s + age+ age.squared+prior.avg.value+deeded.strict, data = m.data)
	
##bootstrap the standard errors
	##bootstrap the standard errors
	out.boot = lm.boot(out.reg,1000)
	N.R = 1000
	##sometimes bad draws on the sparse factors cause trouble on estimating standard errors, so put in try functions
	repeat {
	res.try = try(summary(out.boot), silent = T)
		if(class(res.try) == 'try-error'){
			N.R = N.R - 1
			out.boot = lm.boot(out.reg,R = N.R)		 
		if(N.R < 50){N.R. = 1000}
			} 	
		if(class(res.try) != 'try-error') break()
		}
	
	##bootstrap diff in diff
	mean.boot = two.boot(sample1 = m.data$vote.change[m.data$demolished==1],
	 		sample2 = m.data$vote.change[m.data$demolished==0],	
		FUN = mean,
		R = 10000, ##need many draws because ci's below require them
		na.rm = T)
	ci.boot = boot.ci(mean.boot)
	
	##store values
	outmat[i,'coefficient'] = out.reg$coefficients['demolished']
	outmat[i,'stdev'] = summary(out.boot)$stdev.params['demolished']
	outmat[i,'N.treatment'] =  nrow(m.data[m.data$demolished==T,])
	outmat[i,'N.control'] = nrow(m.data[m.data$demolished==F,])

	outmat.diffs[i,'mean.diff'] = mean.boot$t0
	outmat.diffs[i,'low.ci'] = ci.boot$normal[2] ##extracting the normal approximation, rather than regular, for 95% confidence interval
	outmat.diffs[i,'high.ci'] = ci.boot$normal[3]
	outmat.diffs[i,'N'] = length(mean.boot$data)   
	
	}

##rename matrix for use in printing later
white.match.nondemolished.property = outmat
white.match.nondemolished.diffs.property = outmat.diffs

time.elapsed = proc.time() - ptm  ##end time
cat(paste('total elapsed time:', time.elapsed[3],'\n \n',sep = ' '))



##############################################################
##now test for matched white subjects against other whites near non-demolished projects
##controlling for local racial context
#######################################
cat('beginning tests with matched white subjects near non-demolished projects with local race context controls \n')

outmat = matrix(ncol=4,nrow = length(dists))
outmat.diffs = matrix(ncol=4,nrow = length(dists))
colnames(outmat) = c('coefficient','stdev','N.treatment','N.control')
colnames(outmat.diffs) = c('mean.diff','low.ci','high.ci','N')

use.data = white.data[,c('vote.change','demo.distance','nondemo.distance','p','s','age','age.squared','medianincome','pctblack')]
use.data = use.data[complete.cases(use.data),]

for(i in 1:length(dists)){
	this.data = use.data[use.data$demo.distance<=dists[i]|use.data$nondemo.distance<=dists[i],]

	##create variable separating demolished from non-demolished
	this.data$demolished = ifelse(this.data$demo.distance<=dists[i],1,0)

	this.data$combo.distance = ifelse(this.data$demolished == 1, this.data$demo.distance, this.data$nondemo.distance)
	m.out = matchit(demolished ~ combo.distance + p + s + age + age.squared + medianincome + pctblack,
		data=this.data,
		method = 'nearest',
		replace = T)  ##can replaced with replace = T to demonstrate robustness
	m.data = match.data(m.out)

	
	out.reg = lm(vote.change~demolished+combo.distance+ p + s +age+age.squared+medianincome + pctblack, data = m.data)

	##bootstrap the standard errors
	N.R = 1000
	out.boot = lm.boot(out.reg,1000)
	##sometimes bad draws on the sparse factors cause trouble on estimating standard errors, so put in try functions
	repeat {
	res.try = try(summary(out.boot), silent = T)
		if(class(res.try) == 'try-error'){
			N.R = N.R - 1
			out.boot = lm.boot(out.reg,R = N.R)		 
		if(N.R < 50){N.R. = 1000}
			} 	
		if(class(res.try) != 'try-error') break()
		}

	##bootstrap diff in diff
	mean.boot = two.boot(sample1 = m.data$vote.change[m.data$demolished==1],
	 		sample2 = m.data$vote.change[m.data$demolished==0],	
		FUN = mean,
		R = 10000, ##need many draws because ci's below require them
		na.rm = T)
	ci.boot = boot.ci(mean.boot)
	
	outmat[i,'coefficient'] = out.reg$coefficients['demolished']
	outmat[i,'stdev'] = summary(out.boot)$stdev.params['demolished']
	outmat[i,'N.treatment'] =  nrow(m.data[m.data$demolished==T,])
	outmat[i,'N.control'] = nrow(m.data[m.data$demolished==F,])

	outmat.diffs[i,'mean.diff'] = mean.boot$t0
	outmat.diffs[i,'low.ci'] = ci.boot$normal[2] ##extracting the normal approximation, rather than regular, for 95% confidence interval
	outmat.diffs[i,'high.ci'] = ci.boot$normal[3]
	outmat.diffs[i,'N'] = length(mean.boot$data)   
	}

##rename matrix for use in printing later
white.match.nondemolished.localrace = outmat
white.match.nondemolished.diffs.localrace = outmat.diffs

time.elapsed = proc.time() - ptm  ##end time
cat(paste('total elapsed time:', time.elapsed[3],'\n \n',sep = ' '))


##########################################################
##match white subjects with black subjects
###########################################################
cat('beginning tests with matched black subjects \n')

outmat = matrix(ncol=4,nrow = length(dists))
outmat.diffs = matrix(ncol=4,nrow = length(dists))
colnames(outmat) = c('coefficient','stdev','N.treatment','N.control')
colnames(outmat.diffs) = c('mean.diff','low.ci','high.ci','N')

white.black.data = data[data$reg<"2000-10-10"&is.na(data$reg)==F,]
white.black.data$white = ifelse(white.black.data$whitename>=.975,T,F)
white.black.data$black = ifelse(white.black.data$blackname>=.975,T,F)

##only need subjects who qualify by name pcts
white.black.data = white.black.data[white.black.data$white==T|white.black.data$black==T,]

##only can use complete cases for matching, so extract those, first extract needed columns
use.data = white.black.data[,c('vote.change','white','demo.distance','p','s','age','age.squared','medianincome','demo.gid'
)]
use.data = use.data[complete.cases(use.data),]

for(i in 1:length(dists)){
	##define treatment group by race
	this.data = use.data[use.data$demo.distance<=dists[i],]
	m.out = matchit(white ~ demo.distance + p + s + age + age.squared + medianincome + as.factor(demo.gid),
		data=this.data,
		method = 'nearest')
	m.data = match.data(m.out)

	out.reg = lm(vote.change~white+demo.distance+ p + s +age+age.squared+medianincome, data = m.data)

	##bootstrap the standard errors
	N.R = 1000
	out.boot = lm.boot(out.reg,R = N.R)
	##sometimes bad draws on the sparse factors cause trouble on estimating standard errors, so put in try functions
	repeat {
		res.try = try(summary(out.boot), silent = T)
		if(class(res.try) == 'try-error'){
			N.R = N.R - 1
			out.boot = lm.boot(out.reg,R = N.R)		 
		if(N.R < 50){N.R. = 1000}
			} 	
		if(class(res.try) != 'try-error') break()
		}
	##bootstrap diff in diff
	N.R = 10000
	mean.boot = two.boot(sample1 = m.data$vote.change[m.data$white==1],
	 		sample2 = m.data$vote.change[m.data$white==0],
		FUN = mean,
		R = N.R, ##need many draws because ci's below require them
		na.rm = T)
	###need to increase number of ci's sometimes, so use try function
	repeat{
		ci.try = try(boot.ci(mean.boot), silent = T)
			if(class(ci.try) == 'try-error'){
				N.R = N.R + 10000
			mean.boot = two.boot(sample1 = m.data$vote.change[m.data$white==1],
	 				sample2 = m.data$vote.change[m.data$white==0],
					FUN = mean,
					R = N.R, ##need many draws because ci's below require them
					na.rm = T)
				}
			if(class(ci.try) != 'try-error'){
				ci.boot = boot.ci(mean.boot)
				 break()
				 }
		}	

	outmat[i,'coefficient'] = out.reg$coefficients['whiteTRUE']
	outmat[i,'stdev'] = summary(out.boot)$stdev.params['whiteTRUE']
	outmat[i,'N.treatment'] =  nrow(m.data[m.data$white==T,])
	outmat[i,'N.control'] = nrow(m.data[m.data$white==F,])

	outmat.diffs[i,'mean.diff'] = mean.boot$t0
	outmat.diffs[i,'low.ci'] = ci.boot$normal[2] ##extracting the normal approximation, rather than regular, for 95% confidence interval
	outmat.diffs[i,'high.ci'] = ci.boot$normal[3]
	outmat.diffs[i,'N'] = length(mean.boot$data)   
	}

##rename matrix for use in printing later
white.match.black = outmat
white.match.black.diffs = outmat.diffs

time.elapsed = proc.time() - ptm  ##end time
cat(paste('total elapsed time:', time.elapsed[3],'\n \n',sep = ' '))



##########################################################
##match white subjects with black subjects
##use property
###########################################################
cat('beginning tests with matched black subjects and property controls \n')

outmat = matrix(ncol=4,nrow = length(dists))
outmat.diffs = matrix(ncol=4,nrow = length(dists))
colnames(outmat) = c('coefficient','stdev','N.treatment','N.control')
colnames(outmat.diffs) = c('mean.diff','low.ci','high.ci','N')

##only can use complete cases for matching, so extract those, first extract needed columns
use.data = white.black.data[,c('vote.change','white','demo.distance','p','s','age','age.squared','medianincome',
'prior.avg.value','deeded.strict','demo.gid')]
print(nrow(use.data))
use.data = use.data[complete.cases(use.data),]

for(i in 1:length(dists)){
	print(i)
   ptm <- proc.time()  ##start time
	
	this.data = use.data[use.data$demo.distance<=dists[i],]
m.out = matchit(white ~ demo.distance + p + s + age + age.squared + medianincome+prior.avg.value+deeded.strict+as.factor(demo.gid),
		data=this.data,
		method = 'nearest')
	m.data = match.data(m.out)

	out.reg = lm(vote.change~white+demo.distance + p + s +age+age.squared+prior.avg.value+deeded.strict, data = m.data)

##bootstrap the standard errors
##bootstrap the standard errors
	N.R = 1000
	out.boot = lm.boot(out.reg,R = N.R)
	##sometimes bad draws on the sparse factors cause trouble on estimating standard errors, so put in try functions
	repeat {
		res.try = try(summary(out.boot), silent = T)
		if(class(res.try) == 'try-error'){
			N.R = N.R - 1
			out.boot = lm.boot(out.reg,R = N.R)		 
		if(N.R < 50){N.R. = 1000}
			} 	
		if(class(res.try) != 'try-error') break()
		}
	##bootstrap diff in diff
	N.R = 10000
	mean.boot = two.boot(sample1 = m.data$vote.change[m.data$white==1],
	 				sample2 = m.data$vote.change[m.data$white==0],
		FUN = mean,
		R = N.R, ##need many draws because ci's below require them
		na.rm = T)
	###need to increase number of ci's sometimes, so use try function
	repeat{
		ci.try = try(boot.ci(mean.boot), silent = T)
			if(class(ci.try) == 'try-error'){
				N.R = N.R + 10000
				mean.boot = two.boot(sample1 = m.data$vote.change[m.data$white==1],
	 				sample2 = m.data$vote.change[m.data$white==0],
					FUN = mean,
					R = N.R, ##need many draws because ci's below require them
					na.rm = T)
				}
			if(class(ci.try) != 'try-error'){
				ci.boot = boot.ci(mean.boot)
				 break()
				 }
	}

	outmat[i,'coefficient'] = out.reg$coefficients['whiteTRUE']
	outmat[i,'stdev'] = summary(out.boot)$stdev.params['whiteTRUE']
	outmat[i,'N.treatment'] =  nrow(m.data[m.data$white==T,])
	outmat[i,'N.control'] = nrow(m.data[m.data$white==F,])

	outmat.diffs[i,'mean.diff'] = mean.boot$t0
	outmat.diffs[i,'low.ci'] = ci.boot$normal[2] ##extracting the normal approximation, rather than regular, for 95% confidence interval
	outmat.diffs[i,'high.ci'] = ci.boot$normal[3]
	outmat.diffs[i,'N'] = length(mean.boot$data)   

	}

##rename matrix for use in printing later
white.match.black.property = outmat
white.match.black.diffs.property = outmat.diffs

time.elapsed = proc.time() - ptm  ##end time
cat(paste('total elapsed time:', time.elapsed[3],'\n \n',sep = ' '))



#################################################################
##create predicted effects for distance and context changes
##########################################################
cat('beginning turnout predictions with changing context and distance \n')

##use data created above for whites
usedata = white.data


##create chunks for estimation
distances = seq(from = 10, to = 2000, by = 10)
areas = seq(from = 0, to = 1, length.out = length(distances))
			      
##storage bin
outmat.s = matrix(ncol = 5, nrow = 0)
outmat.d = matrix(ncol = 5, nrow = 0)

out.reg = zelig(vote2004~log(demo.distance)+log(context_black)+vote2000,data = usedata, model = 'ls', cite = F)

##begin simulations across variable values
#for(i in seq(1:length(distances))){
for(i in seq(1:length(distances))){
	print(i)
	ptm <- proc.time()  ##start time

	use.distance = distances[i]
	out.d.1 = setx(out.reg,
		vote2000 = 1,
		demo.distance = use.distance)

	out.d.sims = sim(out.reg,
		x = out.d.1)
	
	use.area = areas[i]	
	out.s.1 = setx(out.reg,
		vote2000 = 1,
		demo.distance = 100,
		context_black = use.area)

	out.s.sims = sim(out.reg,
		x = out.s.1)

	outstats.d = summary(out.d.sims)$stats[[1]]
	outstats.s = summary(out.s.sims)$stats[[1]]
	
	outmat.d = rbind(outmat.d,outstats.d)
	outmat.s = rbind(outmat.s,outstats.s)

	print(proc.time() - ptm)  ##end time
	}

##store results for graphics and table	
predicted.results.distance.vary.context = outmat.d
predicted.results.area.vary.context = outmat.s

out.reg.predictions = out.reg

time.elapsed = proc.time() - ptm  ##end time
cat(paste('total elapsed time:', time.elapsed[3],'\n \n',sep = ' '))
