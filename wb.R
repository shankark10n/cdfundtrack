require(dplyr)
lt <- function(x) {length(x)}
lu <- function(x) {length(unique(x))}
adf <- function(x) {as.data.frame(x)}
convert_currency_to_number <- function(x){as.numeric(gsub("[$,]", "", as.character(x)))}
cand_disc <- list()
setwd('~/work/box/projects/fec')
cand_disc[[1996]] <- read.delim('data/webl96.txt', sep="|", header=F)
cand_disc[[1998]] <- read.delim('data/webl98.txt', sep="|", header=F)
cand_disc[[2000]] <- read.delim('data/webl00.txt', sep="|", header=F)
cand_disc[[2002]] <- read.delim('data/webl02.txt', sep="|", header=F)
cand_disc[[2004]] <- read.delim('data/webl04.txt', sep="|", header=F)
cand_disc[[2006]] <- read.delim('data/webl06.txt', sep="|", header=F)
cand_disc[[2008]] <- read.delim('data/webl08.txt', sep="|", header=F)
cand_disc[[2010]] <- read.delim('data/webl10.txt', sep="|", header=F)
cand_disc[[2012]] <- read.delim('data/webl12.txt', sep="|", header=F)
cand_disc[[2014]] <- read.delim('data/webl14.txt', sep="|", header=F)
years <- seq(1996, 2014, 2)
for (i in years) {
	names(cand_disc[[i]]) <- c('candidate_id', 'candidate_name', 'candidate_incum_chall_status',
		'party_code', 'party_affil', 
		'total_receipts', 'trans_from_authorised_comms', 'total_disburs', 
		'trans_to_authorised_comms', 'start_cash', 'end_cash', 
		'contribs_from_candidate', 'loans_from_candidate', 'other_loans', 'candidate_loan_repay', 'other_loan_repay',
		'debts_owed_by', 'total_indiv_contribs', 
		'candidate_state', 'candidate_dist',
		'spl_election_status', 'prim_election_status', 'runoff_election_status', 'gen_election_status', 
		'gen_election_percent', 'contribs_from_other_pol_comms', 'contribs_from_party_comms', 'coverage_end_date',
		'refunds_to_indivs', 'refunds_to_comms')
	cand_disc[[i]]$candidate_id <- as.character(cand_disc[[i]]$candidate_id)
	cand_disc[[i]]$candidate_name <- as.character(cand_disc[[i]]$candidate_name)
	cand_disc[[i]] <- subset(cand_disc[[i]], strtrim(candidate_id, 1)=='H')
}

# most expensive district races each year
house_races_l <- list()
for (i in years) {
	if (i == 2012) {
		require(plyr)
		house_races_l[[i]] <- adf(arrange(ddply(cand_disc[[i]], .(candidate_state, candidate_dist), summarise, 
			total_indiv_cont=sum(total_indiv_contribs), 
			total_incumbent_indiv_cont=sum(total_indiv_contribs[candidate_incum_chall_status=='I']),
			incumbent=candidate_name[candidate_incum_chall_status=='I'][1],
			incumbent_winner=(candidate_name[gen_election_status=='W'][1]==candidate_name[candidate_incum_chall_status=='I'])[1],
			incumbent_gen_election_percent=gen_election_percent[candidate_incum_chall_status=='I'][1]), -total_indiv_cont))
	}
	else {
		house_races_l[[i]] <- adf(arrange(summarise(group_by(cand_disc[[i]], candidate_state, candidate_dist), 
			total_indiv_cont=sum(total_indiv_contribs), 
			total_incumbent_indiv_cont=sum(total_indiv_contribs[candidate_incum_chall_status=='I']),
			incumbent=candidate_name[candidate_incum_chall_status=='I'][1],
			incumbent_winner=(candidate_name[gen_election_status=='W'][1]==candidate_name[candidate_incum_chall_status=='I'])[1],
			incumbent_gen_election_percent=gen_election_percent[candidate_incum_chall_status=='I'][1]), 
		-total_indiv_cont))
	}
	nas <- which(is.na(house_races_l[[i]]$total_indiv_cont)|(house_races_l[[i]]$candidate_state==""))
	if (lt(nas) > 0) {
		house_races_l[[i]]$total_indiv_cont[nas] <- 0
		house_races_l[[i]]$total_incumbent_indiv_cont[nas] <- 0
		house_races_l[[i]]$frac_incumbent_winners[nas] <- 0
	}
}
agg_house_races <- data.frame(year=years)
k <- 1
for (i in years[1:lt(years)-1]) {
	agg_house_races$total_indiv_cont[k] <- sum(subset(house_races_l[[i]], !is.na(total_indiv_cont))$total_indiv_cont)
	agg_house_races$total_incumbent_indiv_cont[k] <- sum(subset(house_races_l[[i]], !is.na(total_incumbent_indiv_cont))$total_incumbent_indiv_cont)
	agg_house_races$total_incumbent_indiv_cont_top50_costliest[k] <- sum(subset(house_races_l[[i]], !is.na(total_incumbent_indiv_cont))$total_incumbent_indiv_cont[1:50])
	agg_house_races$median_total_indiv_cont[k] <- median(subset(house_races_l[[i]], !is.na(total_incumbent_indiv_cont))$total_incumbent_indiv_cont)
	agg_house_races$mean_total_indiv_cont[k] <- mean(subset(house_races_l[[i]], !is.na(total_incumbent_indiv_cont))$total_incumbent_indiv_cont)
	agg_house_races$frac_incumbent_winners[k] <- sum(subset(house_races_l[[i]], !is.na(incumbent_winner))$incumbent_winner)/dim(subset(house_races_l[[i]], !is.na(incumbent_winner)))[1]
	agg_house_races$frac_incumbent_winners_top50_costliest[k] <- sum(subset(house_races_l[[i]], !is.na(incumbent_winner))$incumbent_winner[1:50])/50
	agg_house_races$frac_incumbent_winners_top100_costliest[k] <- sum(subset(house_races_l[[i]], !is.na(incumbent_winner))$incumbent_winner[1:100])/100
	k <- k+1
}

# incumbents are winning fewer elections but incumbents are spending more in aggregate
par(mfrow=c(2,1))
with(agg_house_races, plot(x=year, y=frac_incumbent_winners))
with(agg_house_races, plot(x=year, y=total_incumbent_indiv_cont))
# controls for outliers: incumbents are winning fewer elections but raising more in median 
par(mfrow=c(2,1))
with(agg_house_races, plot(x=year, y=frac_incumbent_winners))
with(agg_house_races, plot(x=year, y=median_total_indiv_cont))
# (TODO: inflation-adjusted?)


# state-wise trends for incumbents running for re-election
states <- unique(cand_disc[[2008]]$candidate_state)
agg_house_races <- data.frame(
	state=rep(states, each=lt(years)), 
	year=rep(years, lt(states)),
	total_indiv_cont=NA,
	total_incumbent_indiv_cont=NA,
	median_total_indiv_cont=NA,
	mean_total_indiv_cont=NA,
	frac_incumbent_winners=NA)
for (year in years[1:lt(years)-1]) {
	# g <- group_by(house_races_l[[year]], candidate_state)
	# state_wise_house_races <- adf(arrange(summarise(g,
	# 	total_indiv_cont=sum(total_indiv_cont),
	# 	total_incumbent_indiv_cont=sum(total_incumbent_indiv_cont),
	# 	median_total_indiv_cont=median(subset(house_races_l[[year]]$total_indiv_cont, T)),
	# 	mean_total_indiv_cont=mean(subset(house_races_l[[year]]$total_indiv_cont, T)),
	# 	frac_incumbent_winners=(sum(incumbent_winner[(!is.na(incumbent_winner))&(!is.na(incumbent_gen_election_percent))])/(lt(incumbent_winner[(!is.na(incumbent_winner))&(!is.na(incumbent_gen_election_percent))])))[1]
	# 	), candidate_state))
	state_wise_house_races <- adf(arrange(ddply(house_races_l[[year]], .(candidate_state), summarise,
		total_indiv_cont=sum(total_indiv_cont),
		total_incumbent_indiv_cont=sum(total_incumbent_indiv_cont),
		median_total_indiv_cont=median(subset(house_races_l[[year]]$total_indiv_cont, T)),
		mean_total_indiv_cont=mean(subset(house_races_l[[year]]$total_indiv_cont, T)),
		frac_incumbent_winners=(sum(incumbent_winner[(!is.na(incumbent_winner))&(!is.na(incumbent_gen_election_percent))])/(lt(incumbent_winner[(!is.na(incumbent_winner))&(!is.na(incumbent_gen_election_percent))])))[1]
	), candidate_state))
	state_wise_house_races <- subset(state_wise_house_races, candidate_state!='')
	swhr_states <- sort(unique(state_wise_house_races$candidate_state))
	agg_house_races[(agg_house_races$year==year)&(agg_house_races$state %in% swhr_states), 'total_indiv_cont'] <- state_wise_house_races$total_indiv_cont
	agg_house_races[(agg_house_races$year==year)&(agg_house_races$state %in% swhr_states), 'total_incumbent_indiv_cont'] <- state_wise_house_races$total_incumbent_indiv_cont
	agg_house_races[(agg_house_races$year==year)&(agg_house_races$state %in% swhr_states), 'median_total_indiv_cont'] <- state_wise_house_races$median_total_indiv_cont
	agg_house_races[(agg_house_races$year==year)&(agg_house_races$state %in% swhr_states), 'mean_total_indiv_cont'] <- state_wise_house_races$mean_total_indiv_cont
	agg_house_races[(agg_house_races$year==year)&(agg_house_races$state %in% swhr_states), 'frac_incumbent_winners'] <- state_wise_house_races$frac_incumbent_winners
}
# for converting to geojson
state_names<- c('00', 'Alaska', 'Alabama', 'Arkansas', 'American Samoa', 'Arizona', 'California', 'Colorado', 'Connecticut', 'District of Columbia', 'Delaware', 'Florida', 'Georgia', 'Guam', 'Hawaii', 'Iowa', 'Idaho', 'Illinois', 'Indiana', 'Kansas', 'Kentucky', 'Louisiana', 'Massachusetts', 'Maryland', 'Maine', 'Michigan', 'Minnesota', 'Missouri', 'Northern Mariana Islands', 'Mississippi', 'Montana', 'North Carolina', 'North Dakota', 'Nebraska', 'New Hampshire', 'New Jersey', 'New Mexico', 'Nevada', 'New York', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Puerto Rico', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Virginia', 'Virgin Islands', 'Vermont', 'Washington', 'Wisconsin', 'West Virginia', 'Wyoming')
agg_house_races$state_name<-state_names[agg_house_races$state]
agg_house_races <- subset(agg_house_races, !is.na(frac_incumbent_winners))
write.table(agg_house_races, 'allstates_agg_house_races.csv', quote=F, sep=',', row.names=F, col.names=T)
# t <- arrange(summarise(group_by(agg_house_races, state_name), frac_incumbent_winners=mean(frac_incumbent_winners)), state_name)
t <- arrange(ddply(agg_house_races, .(state_name), summarise, frac_incumbent_winners=mean(frac_incumbent_winners)), state_name)
write.table(t, 'allstates_mean_incumbent_winfraction.csv', quote=F, sep=',', row.names=F, col.names=T)
# t <- arrange(summarise(group_by(agg_house_races, state_name), total_indiv_cont=sum(total_indiv_cont)), state_name)
t <- arrange(ddply(agg_house_races, .(state_name), summarise, total_indiv_cont=sum(total_indiv_cont)), state_name)
write.table(t, 'allstates_total_indiv_contribs.csv', quote=F, sep=',', row.names=F, col.names=T)
# years <- c(2008, 2010, 2012, 2014)
# cand_disc <- list()
# cand_disc[[2008]] <- read.csv('candidatesummary2008.csv')
# cand_disc[[2010]] <- read.csv('candidatesummary2010.csv')
# cand_disc[[2012]] <- read.csv('candidatesummary2012.csv')
# cand_disc[[2014]] <- read.csv('candidatesummary2014.csv')
# for (i in years) {
# 	for (j in 14:48) {
# 		cand_disc[[i]][, j] <- convert_currency_to_number(cand_disc[[i]][, j])
# 	}
# 	cand_disc[[i]]$can_nam <- as.character(cand_disc[[i]]$can_nam)
# 	cand_disc[[i]]$cov_sta_dat <- as.Date(as.character(cand_disc[[i]]$cov_sta_dat), '%Y-%m-%d')
# 	cand_disc[[i]]$cov_end_dat <- as.Date(as.character(cand_disc[[i]]$cov_end_dat), '%Y-%m-%d')
# 	cand_disc[[i]]$can_off_dis <- as.numeric(as.character(cand_disc[[i]]$can_off_dis))
# 	cand_disc[[i]]$can_par_aff <- toupper(cand_disc[[i]]$can_par_aff)
# 	# print(sum(subset(cand_disc[[i]], !is.na(ind_ite_con))$ind_ite_con))
# }
# # add a column if incumbents had less money than challengers in House for election
# inc_cha_contribs <- as.data.frame(arrange(summarise(group_by(subset(cand_disc[[2008]], can_off=='H'), can_off_sta, can_off_dis), 
# 	inc=can_nam[which(can_inc_cha_ope_sea=='INCUMBENT')][1], 
# 	inc_par_aff=can_par_aff[which(can_inc_cha_ope_sea=='INCUMBENT')][1],
# 	cha_dem=lu(which((can_inc_cha_ope_sea=='CHALLENGER')&(can_par_aff=='DEM'))),
# 	cha_rep=lu(which((can_inc_cha_ope_sea=='CHALLENGER')&(can_par_aff=='REP'))),
# 	inc_net_con=sum(net_con[which((can_inc_cha_ope_sea=='INCUMBENT')&!is.na(net_con))]),
# 	cha_net_con=sum(net_con[which((can_inc_cha_ope_sea=='CHALLENGER')&!is.na(net_con))]),
# 	cha_dem_net_con=sum(net_con[which((can_inc_cha_ope_sea=='CHALLENGER')&(can_par_aff=='DEM')&(!is.na(net_con)))]),
# 	cha_rep_net_con=sum(net_con[which((can_inc_cha_ope_sea=='CHALLENGER')&(can_par_aff=='REP')&(!is.na(net_con)))])), 
# can_off_sta, can_off_dis))
# inc_cha_contribs$year <- 2008
# for (i in years[2:4]) {
# 	t <- as.data.frame(arrange(summarise(group_by(subset(cand_disc[[i]], can_off=='H'), can_off_sta, can_off_dis), 
# 	inc=can_nam[which(can_inc_cha_ope_sea=='INCUMBENT')][1], 
# 	inc_par_aff=can_par_aff[which(can_inc_cha_ope_sea=='INCUMBENT')][1],
# 	cha_dem=lu(which((can_inc_cha_ope_sea=='CHALLENGER')&(can_par_aff=='DEM'))),
# 	cha_rep=lu(which((can_inc_cha_ope_sea=='CHALLENGER')&(can_par_aff=='REP'))),
# 	inc_net_con=sum(net_con[which((can_inc_cha_ope_sea=='INCUMBENT')&!is.na(net_con))]),
# 	cha_net_con=sum(net_con[which((can_inc_cha_ope_sea=='CHALLENGER')&!is.na(net_con))]),
# 	cha_dem_net_con=sum(net_con[which((can_inc_cha_ope_sea=='CHALLENGER')&(can_par_aff=='DEM')&(!is.na(net_con)))]),
# 	cha_rep_net_con=sum(net_con[which((can_inc_cha_ope_sea=='CHALLENGER')&(can_par_aff=='REP')&(!is.na(net_con)))])), 
# can_off_sta, can_off_dis))
# 	t$year <- i
# 	inc_cha_contribs <- rbind(inc_cha_contribs, t)
# }
# inc_cha_contribs <- as.data.frame(arrange(inc_cha_contribs, can_off_sta, can_off_dis, year))
# write.table(inc_cha_contribs, 'incumbent_challenger_contributions.csv', quote=T, col.names=T, row.names=F, sep=',')
# for(i in years) {
# 	write.table(cand_disc[[i]], paste('cand_disc', i, '.csv', sep=''), quote=T, col.names=T, sep=',')
# }
# statewise_incumbent_challenger_contribs <- arrange(summarise(group_by(incumbent_challenger_contributions, can_off_sta, year), 
# 	rep_incs=lu(which(inc_par_aff=='REP')),
# 	dem_incs=lu(which(inc_par_aff=='DEM')),
# 	dem_chas=sum(cha_dem), 
# 	rep_chas=sum(cha_rep), 
# 	net_inc_con=sum(inc_net_con), 
# 	net_cha_con=sum(cha_net_con), 
# 	net_cha_dem_con=sum(cha_dem_net_con), 
# 	net_cha_rep_con=sum(cha_rep_net_con)), 
# can_off_sta, year)

