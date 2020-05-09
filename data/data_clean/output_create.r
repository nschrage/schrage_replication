##output_create.r
###create output for Enos 'What the Demolition of Public Housing Teaches Us About the Impact of Racial Threat on Political Behavior' 
###RdE June 2014


cat('creating output \n')

###master graphic parameters for graphics 
ylims = c(-.35,.1)
ylims.2 = c(-.45,.1)
xlims = c(.5,11)
dists = seq(from = 1000, to = 100, by = -100) ###DELETE THIS LATER
xs = seq(1:length(dists))
ys = seq(from = -.35, to = .1, by = .05)
ys.lab = c('-0.35','-0.30', '-0.25','-0.20','-0.15','-0.10','-0.05','0.00','0.05','0.10')
ys.2 = seq(from = -.45, to = .1, by = .05)
ys.lab.2 = c('-0.45','-0.40','-0.35','-0.30', '-0.25','-0.20','-0.15','-0.10','-0.05','0.00','0.05','0.10')

offsets = .15
text.offsets = .025
cex.axis = .9
cex.N = .7
top.text.adj = c(1.3,1.3) ##offsets on labels to reduce crowding
bottom.text.adj = c(-.15,-.85)
point.size = 2
line.offset = .0175

###########################################
###basic diff in diff graphs################
###Figure 1 and Appendix Figure A1
###################################

##load data
wtreat = white.treat.effect.mean.boot 
wtreat.lower = white.treat.effect.conf.boot.lower 
wtreat.upper = white.treat.effect.conf.boot.upper 
Nwtreat = white.treat.N
btreat = black.treat.effect.mean.boot 
btreat.lower = black.treat.effect.conf.boot.lower 
btreat.upper = black.treat.effect.conf.boot.upper 
Nbtreat = black.treat.N
##letters for marking graphs, one is not used
use.letters = c('a','b','c','d','e','f','skip','g','h')

##cycle through each line of data, each of which are groups defined by diferent namepcts
for(i in 1:nrow(wtreat)){ ##turning into matrices helps below with segment function
	use.wtreat = as.matrix(wtreat[i,])
	use.wlower = as.matrix(wtreat.lower[i,])
	use.wupper = as.matrix(wtreat.upper[i,])
	use.Nwtreat = as.matrix(Nwtreat[i,])
	
	use.btreat = as.matrix(btreat[i,])
	use.blower = as.matrix(btreat.lower[i,])
	use.bupper = as.matrix(btreat.upper[i,])
	use.Nbtreat = as.matrix(Nbtreat[i,])
	

##name graphs
	if(i == 7){
		pdf('output/Figure_1.pdf')
		}
	else{
		pdf(paste('appendix_output/Figure_A1',use.letters[i],'.pdf',sep=''))
		}			
	par(las = 1)
	par(mar = c(5.1, 4.1, .5, .5))
	plot(xs, use.wtreat,
		ylim = ylims,
		xlim = xlims,
		type = 'n',
		ylab = 'Treatment Effect',
		xlab = 'Treated Group Distance from Projects',
		xaxt = 'n',
		yaxt = 'n')
	abline(h = 0, lty = 2)

	###draw lines first because I want them to be covered by points
	####create spaces in lines using the offset (this allows the N to be displayed with the text() function)
	##black lines are offset to the left, white lines to the right	
  segments(x0= xs[1:2]+offsets, x1 = xs[1:2]+offsets, ##only do it for low N blacks because otherwise lines look funny
         y0 = use.btreat[,1:2], y1 =	use.blower[,1:2])
  segments(x0= xs[1:2]+offsets, x1 = xs[1:2]+offsets,
         y0 = use.btreat[,1:2] + line.offset, 	y1 =	use.bupper[,1:2])
  ##now the others
  segments(x0= xs[3:10]+offsets, x1 = xs[3:10]+offsets,
         y0 = use.blower[,3:10], 	y1 =	use.bupper[,3:10])

		
	segments(x0= xs-offsets, x1 = xs-offsets, ##bottomlines
		y0 = use.wtreat - line.offset, 	y1 =	use.wlower)
	segments(x0= xs-offsets, x1 = xs-offsets, ##toplines
		y0 = use.wtreat, 	y1 =	use.wupper)

  
  ##points and N descriptions
	points(xs-offsets, use.wtreat,
	       cex = point.size,
	       pch = 21, 
	       bg = 'white',
	       col = 'black')
	text(xs-offsets,use.wtreat,
	     paste('(',use.Nwtreat,')',sep = ''),
	     cex = cex.N,
	     #adj = top.text.adj
	     pos = 1
	    )
	
	points(xs+offsets, use.btreat,
	       pch = 16,
	       cex = point.size)
	text(xs+offsets,use.btreat,
	     paste('(',use.Nbtreat,')',sep = ''),
	     cex = cex.N,
	     #adj = bottom.text.adj
	     pos = 3
	    )
	
	axis(side = 1,
		at = xs,
		label = seq(100,1000,100),
		cex.axis = cex.axis
		)
	axis(side = 2,
		at = ys,
		label = ys.lab,
		cex.axis = cex.axis
		)	

	dev.off()
	}


###########################################
###matched groups################
###Figures 2 and 3 and Appendix Figure A3-A12
###################################

##this cycles thorugh a bunch of dataframes, each of which is needed for a different graph
for(figure in c('white.basic.main','white.demo.main','white.demo.property','white.demo.localrace','blackmain','blackcensus')){
	if(figure == 'white.basic.main'){
		##this group is different than the rest because the second set is not actually a diff in diff, but calling it "diffs" for consistency
		treat = white.match.basic
		treat.2 = white.match.basic.property
		fig.nums = c('A3','A4') ##figure names
		pchs = c(17,17) ##point types
		}
	if(figure == 'white.demo.main'){
		treat = white.match.nondemolished
		diffs = white.match.nondemolished.diffs
		fig.nums = c('2','A5')
		pchs = c(17,22)
		}
	if(figure == 'white.demo.property'){
		treat = white.match.nondemolished.property
		diffs = white.match.nondemolished.diffs.property
		fig.nums = c('A6','A7')	
		pchs = c(17,22)
		}	
	if(figure == 'white.demo.localrace'){
		treat = white.match.nondemolished.localrace
		diffs = white.match.nondemolished.diffs.localrace
		fig.nums = c('A8','A9')			
		pchs = c(17,22)
		}
	if(figure == 'blackmain'){
		treat = white.match.black.property
		diffs = white.match.black.diffs.property
		fig.nums = c('3','A12')
		pchs = c(17,21)			
		}
	if(figure == 'blackcensus'){
		treat = white.match.black
		diffs = white.match.black.diffs
		fig.nums = c('A10','A11')
		pchs = c(17,21)	
		}


	##define axis for different graphs
	if(figure %in% c('white.basic.main','white.demo.main','blackmain')){
			use.ylims = ylims
			use.ys.lab = ys.lab
			use.ys = ys
		}
	else{
			use.ylims = ylims.2
			use.ys.lab = ys.lab.2
			use.ys = ys.2
		}

	##go through pairs for each pair of dataframe
	for(i in 1:2){
		if(i == 1){ 	
			use.treat = treat[,'coefficient']			
			clower = use.treat-(1.96*treat[,'stdev'])
			cupper = use.treat+(1.96*treat[,'stdev'])
			use.N.treat = treat[,'N.treatment'] + treat[,'N.control']
			}			
		if(i == 2 & figure != 'white.basic.main'){	
			use.treat = diffs[,'mean.diff']			
			clower = diffs[,'low.ci']
			cupper = diffs[,'high.ci']
			use.N.treat = diffs[,'N']
			}	
		if(i == 2 & figure == 'white.basic.main'){	##white.basic.main figures have slightly different structure 
			use.treat = treat.2[,'coefficient']			
			clower = use.treat-(1.96*treat.2[,'stdev'])
			cupper = use.treat+(1.96*treat.2[,'stdev'])
			use.N.treat = treat.2[,'N.treatment'] + treat.2[,'N.control']
			}
		if(figure %in% c('white.demo.main','blackmain') & i ==1){
				pdf(paste('output/Figure_',fig.nums[i],'.pdf',sep=''))
				}
		else{	
			pdf(paste('appendix_output/Figure_',fig.nums[i],'.pdf',sep=''))
			}
			par(las = 1)
			par(mar = c(5.1, 4.1, .5, .5))
			plot(xs, use.treat,
				ylim = use.ylims,
				xlim = xlims,
				type = 'n',
				ylab = 'Treatment Effect',
				xlab = 'Treated Group Distance from Projects',
				xaxt = 'n',
				yaxt = 'n')
			abline(h = 0, lty = 2)
				
			segments(x0=xs,x1=xs,
						y0= use.treat+line.offset,y1=cupper)
			segments(x0=xs,x1=xs,
						y0= use.treat,y1=clower)

		### Treatment Effects
			points(xs, use.treat, 
				pch = pchs[i], 
				cex = point.size,
					bg = 'white',
       			col = 'black')
			text(xs,use.treat,
			     paste('(',use.N.treat,')',sep = ''),
			     cex = cex.N,
			     pos = 3
			  )
			axis(side = 1,
					at = xs,
					label = seq(100,1000,100),
					cex.axis = cex.axis
					)
			axis(side = 2,
					at = use.ys,
					label = use.ys.lab,
					cex.axis = cex.axis
					)	
			
			dev.off()
		}
	}	


###########################################
###regression of turnout on distance and population size################
###Figure 4
###################################
##write out regression to latex table
out.model = apsrtable(out.reg.predictions,
	coef.names = c( 'Intercept','log(distance)','log(percent of local black population)','2000 turnout'),
	digits = 3
	)
writeLines(out.model,
	'output/Table_1.tex')
	

###########################################
###predicted effects graphs################
###Figure 4
###################################

distdat =  predicted.results.distance.vary.context
areadat = predicted.results.area.vary.context

##new ylims for these graphs
ylims.predict = c(.6,.75)

datas = list(distdat,areadat)##put data in a list to cycle through
##parameters to be used in graphs below
xs = list(seq(from = 10, to = 2000, by = 10), seq(from = 45000, to = 1004000, by = 4800)/1000)
use.letters = c('a','b')
xlabs = c('Distance from Project','Percent of Local Black Population in Demolished Project')
ylabs = c(expression(Pr(vote[2004])),'')
vlines = list(seq(from = 0, to = 2000, by = 200),seq(from = 0, to = 1000, by = 100))
axis.labs = list(as.character(seq(from = 0, to = 2000, by = 200)),
	as.character(c('0','10%','20%','30%','40%','50%','60%','70%','80%','90%','100%')))

for(i in 1:2){
	colnames(datas[[i]]) = c("mean","sd","50%","2.5%","97.5%") ##saving renames columns, so name back

	pdf(paste('output/Figure_4',use.letters[i],'.pdf',sep=''))
		par(las = 1)
		par(mar = c(5.1, 4.1, .5, .5))
		plot(xs[[i]],datas[[i]][,'mean'],
			type = 'l',
			xlab = xlabs[i],
			ylab = ylabs[i],
			ylim = ylims.predict,
			xaxt = 'n',
			cex.axis = cex.axis,
			lwd = 4
		)
	##put horizontal and vertical lines on plots
	abline(h = seq(from = min(ylims.predict), to = max(ylims.predict), by = .025),
	       lty = 2,
	       col = 'gray',
	       lwd = 1)
	abline(v = vlines[[i]], 
	       lty = 2,
	       col = 'gray',
	       lwd = 1)
	lines(xs[[i]],datas[[i]][,'2.5%'],
			lty = 3,
			lwd = 2.5)
	lines(xs[[i]],datas[[i]][,'97.5%'],
			lty = 3,
			lwd = 2.5)
	axis(side = 1, 
		at = vlines[[i]], 
		labels = axis.labs[[i]],
		cex.axis = cex.axis)
		
		dev.off()
	}



###########################################
###vote choice graphs################
###Figures 5 and 6
###################################
pres.elections = c('dole_pct_ei','bush2000_pct_ei','bush2004_pct_ei','mccain_pct_ei')
obama.elections = c('obama_sen_primary_pct_ei','keyes_pct_ei','obama_pres_primary_pct_ei')

dists = distance.vote.differences
demos = demolished.vote.differences

graphs = c('5a','5b','6')

for(i in graphs){

	if(i == '5a'){dat = dists}
	else{dat = demos}
		
		
	if(i %in% c('5a','5b')){
		xlims = c(.75,4.25)
		ylims = c(-.1,.2)	
		}
	else{
		xlims = c(.75,3.25)
		ylims = c(-.1,.25)
		}
		
	##recode Keyes to Obama general for presentation purposes
	dat[dat$election == 'keyes_pct_ei','x.mean'] = 1 - dat[dat$election == 'keyes_pct_ei','x.mean']
	dat[dat$election == 'keyes_pct_ei','y.mean'] = 1 - dat[dat$election == 'keyes_pct_ei','y.mean']
	dat[dat$election == 'keyes_pct_ei','diff'] =dat[dat$election == 'keyes_pct_ei','y.mean'] - dat[dat$election == 'keyes_pct_ei','x.mean']
	
		pdf(paste('output/Figure_',i,'.pdf',sep=''),
		width = 7, height = 8)
		par(las = 1)
		par(mar = c(5.1, 4.1, .5, 1.5))
		plot(seq(1:4),
			rep(1,4),
			ylim = c(-.1,.2),
			xlim = xlims, 
			type = 'n',
			xaxt = 'n',
			yaxt = 'n',
			xlab = 'Election',
			ylab = ifelse(i == '5b','','Treatment Effect')
			)
		abline(h=0, lty = 2)
		
		if(i %in% c('5a','5b')){
			segments(
				x0= seq(1:4)-offsets, 
				x1 = seq(1:4)-offsets,
				y0 = dat[dat$group == 'white'&dat$election %in% pres.elections,'diff']-(1.96*dat[dat$group == 'white'&dat$election %in% pres.elections,'sd']),
				y1 =	dat[dat$group == 'white'&dat$election %in% pres.elections,'diff']+(1.96*dat[dat$group == 'white'&dat$election %in% pres.elections,'sd'])	
					)
			points(seq(1:4)-offsets,
				dat[dat$group == 'white'&dat$election %in% pres.elections,'diff'],
					pch = 21, 
					bg = 'white',
					col = 'black',
					cex = 2
				)
			segments(
				x0= seq(1:4)+offsets, 
				x1 = seq(1:4)+offsets,
				y0 = dat[dat$group == 'black'&dat$election %in% pres.elections,'diff']-(1.96*dat[dat$group == 'black'&dat$election %in% pres.elections,'sd']),
				y1 =	dat[dat$group == 'black'&dat$election %in% pres.elections,'diff']+(1.96*dat[dat$group == 'black'&dat$election %in% pres.elections,'sd'])	
					)
			points(seq(1:4)+offsets,
				dat[dat$group == 'black'&dat$election %in% pres.elections,'diff'],
					pch = 16,
					cex = 2
				)
			axis(side = 1, at = seq(1:4), 
				c('1996','2000','2004','2008'), 
				tick = F,
				cex.axis = cex.axis)		
			}
		else{
			segments(
				x0= seq(1:3)-offsets, 
				x1 = seq(1:3)-offsets,
				y0 = dat[dat$group == 'white'&dat$election %in% obama.elections,'diff']-(1.96*dat[dat$group == 'white'&dat$election %in% obama.elections,'sd']),
				y1 =	dat[dat$group == 'white'&dat$election %in% obama.elections,'diff']+(1.96*dat[dat$group == 'white'&dat$election %in% obama.elections,'sd'])	
					)
			points(seq(1:3)-offsets,
				dat[dat$group == 'white'&dat$election %in% obama.elections,'diff'],
					pch = 21, 
					bg = 'white',
					col = 'black',
					cex = 2
				)
			segments(
				x0= seq(1:3)+offsets, 
				x1 = seq(1:3)+offsets,
				y0 = dat[dat$group == 'black'&dat$election %in% obama.elections,'diff']-(1.96*dat[dat$group == 'black'&dat$election %in% obama.elections,'sd']),
				y1 =	dat[dat$group == 'black'&dat$election %in% obama.elections,'diff']+(1.96*dat[dat$group == 'black'&dat$election %in% obama.elections,'sd'])	
					)
  			points(seq(1:3)+offsets,
				dat[dat$group == 'black'&dat$election %in% obama.elections,'diff'],
					pch = 16,
					cex = 2
				)
		axis(side = 1, at = seq(1:3), 
					c('2004 \n Senate Primary','2004 \n Senate General','2008 \n President Primary'),
					tick = F,
					cex.axis = cex.axis
					)
		
			}	
		axis(side = 2,
			at = seq(from = -.1, to = .3, by = .05),
			label = c('-0.10','-0.05','0.00','0.05','0.10','0.15','0.20','0.25','0.30'),
			cex.axis = cex.axis
			)			
		dev.off()
	}		
				

###########################################
###parallel trends graph################
###Appendix Figures A2
###################################

groups = parallel.trends


###plot elections votes
	pdf('appendix_output/Figure_A2.pdf')
		par(las = 1)
		par(mar = c(5.1, 4.1, .5, .5))
		plot(1,
			.5,
			ylim = c(.5,.9),
			xlim = c(1,5),
			type = 'n',
			xaxt = 'n',
			ylab = 'Percent Voter Turnout',
			xlab = 'Year'
			)
 			text(seq(1:5),groups[,'white.control'], expression('W'['C']), cex = 1.5)
			text(seq(1:5),groups[,'white.treatment'],expression('W'['T']), cex = 1.5)
			text(seq(1:5),groups[,'black.control'], expression('B'['C']), cex = 1.5)
			text(seq(1:5),groups[,'black.treatment'],expression('B'['T']), cex = 1.5)
				
			lines(seq(1:5),groups[,'white.control'], lty = 2)
			lines(seq(1:5),groups[,'white.treatment'], lty = 2)
			lines(seq(1:5),groups[,'black.control'], lty = 2)
			lines(seq(1:5),groups[,'black.treatment'], lty = 2)

		axis(side = 1, at = seq(1:5), labels = c('1996','1998','2000','2002','2004'))
		axis(side = 2, at = seq(from = .5, to = .9, by = .05), labels = seq(from = .5, to = .9, by = .05))
			
		dev.off()
	


##############################################################
time.elapsed = proc.time() - ptm  ##end time
cat(paste('total elapsed time:', time.elapsed[3],'\n \n',sep = ' '))

cat("Done! \n Figures should be in the directories 'output' and 'appendix_output'.")
