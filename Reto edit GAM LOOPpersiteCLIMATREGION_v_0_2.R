rm(list=ls())

library(RODBC)
library(quantmod)
library(mgcv)

if(Sys.info()[["sysname"]] =='Linux'){
usrname <- readline("Enter your username to connect the database LOLAdb\n")
pswd <- readline(paste("Enter your password for",usrname,"\n"))
ch.lola <- odbcConnect("LOLAdb", uid=usrname, pwd=pswd,case="postgresql") # at MNHN
} else {
usrname <- readline("Enter your username to connect the database LOLAdb\n")
pswd <- readline(paste("Enter your password for",usrname,"\n"))
ch.lola <- odbcConnect("LOLA_BMSdb", uid=usrname, pwd=pswd,case="postgresql")} # on my MAC


if(Sys.info()[["sysname"]] =='Linux'){
setwd("/home/reto/Documents/LOLA_BMS/")
} else {
setwd("/Users/retoschmucki/Desktop/LOLA_occupancy_model")
}

origin_wd <- getwd()

# country <- readline("Which country are you looking for (UK,NL,FR,ES,DE,FI,IL)")

# here we can set the first year to account for in a speciefic country.
# first_year <- 

# load functions

source ('dennis_gam_script.R')

# climate_zone <- "K. Warm temperate and mesic"
# climate_zone_name <- "Warm_temperate_mesic"

# climate_zone <- "J. Cool temperate and moist"
# climate_zone_name <- "Cool_temperate_moist"
# country <- "UK" # country to be excluded from the zone for Germany.

# climate_zone <- "H. Cool temperate and dry"
# climate_zone_name <- "Cool_temperate_dry"
# country <- "FI" # country to be excluded from the zone.

# climate_zone <- "G. Cold and mesic"
# climate_zone_name <- "Cold_and_mesic"
# country <- "FI" # country to be excluded from the zone.

###############################################################

 climate_zone_all <- c("E. Cold and wet","F. Extremely cold and mesic","G. Cold and mesic","H. Cool temperate and dry","J. Cool temperate and moist","K. Warm temperate and mesic","L. Warm temperate and xeric","N. Hot and dry","P. Extremely hot and arid")
 climate_zone_name_all <- c("Cold_wet","Extremely_cold_mesic","Cold_mesic","Cool_temperate_dry","Cool_temperate_moist","Warm_temperate_mesic","Warm_temperate_xeric","Hot_dry","Extremely_hot_arid")
 country_all <- c("ALL", "FI","FI","ALL","UK","ALL","ALL","ALL","ALL")


# "FR";"E. Cold and wet"
# "UK";"E. Cold and wet"

# "ES";"F. Extremely cold and mesic"
# "FI";"F. Extremely cold and mesic"
# "UK";"F. Extremely cold and mesic"

# "DE";"G. Cold and mesic"
# "ES";"G. Cold and mesic"
# "FI";"G. Cold and mesic" EXCLUDE
# "FR";"G. Cold and mesic"
# "UK";"G. Cold and mesic"

# "DE";"H. Cool temperate and dry"

# "DE";"J. Cool temperate and moist"
# "ES";"J. Cool temperate and moist"
# "FR";"J. Cool temperate and moist"
# "NL";"J. Cool temperate and moist"
# "UK";"J. Cool temperate and moist" EXCLUDE

# "ES";"K. Warm temperate and mesic"
# "FR";"K. Warm temperate and mesic"

# "ES";"L. Warm temperate and xeric"
# "IL";"L. Warm temperate and xeric"

# "IL";"N. Hot and dry"
# "IL";"P. Extremely hot and arid"

################################################################

# climate_zone_all <- c("J. Cool temperate and moist","H. Cool temperate and dry","G. Cold and mesic","K. Warm temperate and mesic")
# climate_zone_name_all <- c("Cool_temperate_moist","Cool_temperate_dry","Cold_and_mesic","Warm_temperate_mesic")
# country_all <- c("UK","FI","FI","ALL")



# START CLIMATE ZONE LOOP

for (z in 1:length(climate_zone_all)){

climate_zone <- climate_zone_all[z]
climate_zone_name <- climate_zone_name_all[z]
country <- country_all[z]

cat("\t\t START",climate_zone,"no",country_all[z], format(Sys.time(), "%d/%m/%Y %H:%M:%S"),"\n")

site_visit_query<- paste("SELECT DISTINCT
transect_id,
visit_year,
visit_month,
visit_day
FROM
lola_bms.species_count_visit
WHERE
transect_id IN (SELECT transect_id FROM lola_bms.bms_transects_coord WHERE eco_zone_name = \'",as.name(climate_zone),"\') AND
substring(transect_id from 1 for 2) != \'",as.name(country),"\' AND
visit_year IS NOT NULL AND
visit_month > 2 AND
visit_month < 10
ORDER BY
transect_id,
visit_year,
visit_month,
visit_day",sep="")

site_visit<- sqlQuery(ch.lola,site_visit_query,rows_at_time=1)

year_sampled <- unique(site_visit$visit_year)
year_sampled <- year_sampled[year_sampled > 1989 & year_sampled < 2013 & !is.na(year_sampled)]
year_sampled <- year_sampled[order(year_sampled)]
year_sampled <- c(min(year_sampled):max(year_sampled))

site_sampled <- unique(site_visit$transect_id)
site_sampled <- site_sampled[order(as.numeric(substr(as.character(site_sampled),7,nchar(as.character(site_sampled)))))]

# Generate a sampling date dataframe with Julian day
site_visit$julian_day <- strptime(paste(site_visit$visit_day,site_visit$visit_month,site_visit$visit_year,sep='/'), "%d/%m/%Y")$yday+1

species_list_query <- paste("SELECT DISTINCT
fauna_europea_species,
visit_year,
count(distinct transect_id) as nbr_transect
FROM
lola_bms.species_count_visit
WHERE
transect_id IN (SELECT transect_id FROM lola_bms.bms_transects_coord WHERE eco_zone_name = \'",as.name(climate_zone),"\') AND
substring(transect_id from 1 for 2) != \'",as.name(country),"\' AND
visit_year IS NOT NULL AND
fauna_europea_species != \'no_butterfly_obs\'
GROUP BY
fauna_europea_species,
visit_year
ORDER BY
visit_year,
nbr_transect",sep='')

species_list<- sqlQuery(ch.lola,species_list_query,rows_at_time=1)

species_unique <- as.character(unique(species_list$fauna_europea_species))
species_unique <- species_unique[order(species_unique)]

# print(species_unique)

###############################################################
# FOR SUBSET OF SPECIES
###############################################################
# species_interest <- c('Papilio machaon',
# 'Gonepteryx rhamni',
# 'Anthocharis cardamines',
# 'Polyommatus coridon',
# 'Polyommatus icarus',
# 'Argynnis paphia',
# 'Issoria lathonia',
# 'Vanessa atalanta',
# 'Vanessa cardui',
# 'Aglais io',
# 'Nymphalis antiopa',
# 'Aglais urticae',
# 'Polygonia c-album',
# 'Araschnia levana',
# 'Pararge aegeria',
# 'Lasiommata megera',
# 'Coenonympha pamphilus',
# 'Aphantopus hyperantus',
# 'Maniola jurtina',
# 'Melanargia galathea')

# species_no <- match(species_interest,species_unique)
# species_no <- species_no[!is.na(species_no)]

############################################################
# FOR ALL SPECIES
############################################################

species_no <- match(species_unique,species_unique)

object_to_keep <-ls()
object_to_keep <-ls()

#####################
# Species loop start
#####################

# to test sp_i <- 1

for (sp_i in species_no[1:length(species_no)]) {

speciesGAM <- species_unique[sp_i]

cat("\t\t",speciesGAM, format(Sys.time(), "%d/%m/%Y %H:%M:%S"),"\n")

			# get species count from the DB
			species_visit_query <- paste("SELECT DISTINCT
			  fauna_europea_species,
			  transect_id,
			  visit_year,
			  visit_month,
			  visit_day,
			  individual_count
			FROM
			  lola_bms.species_count_visit
			WHERE
			  transect_id IN (SELECT transect_id FROM lola_bms.bms_transects_coord WHERE eco_zone_name = \'",as.name(climate_zone),"\') AND
			  substring(transect_id from 1 for 2) != \'",as.name(country),"\' AND
			  fauna_europea_species = \'",as.name(speciesGAM),"\' AND
			  visit_year IS NOT NULL AND
			  visit_month > 2 AND
			  visit_month < 10
			ORDER BY
			  transect_id,
			  visit_year,
			  visit_month,
			  visit_day",sep='')

			species_visit<- sqlQuery(ch.lola,species_visit_query,rows_at_time=1)

#############################################
# YEAR LOOP
#############################################

# y <- year_sampled[19]

for (y in year_sampled){
year_run <- y
year_lims = year_run


if(length(species_list[species_list$visit_year == year_run & as.character(species_list$fauna_europea_species) == speciesGAM,'nbr_transect']) != 0){

if(species_list[species_list$visit_year == year_run & as.character(species_list$fauna_europea_species) == speciesGAM,'nbr_transect'] > 4)
				
				# if species at least in 5 transects this year
				{ 

				cat("\t\tYear", year_lims,speciesGAM,climate_zone,"no",country_all[z],"-", format(Sys.time(), "%d/%m/%Y %H:%M:%S"),"\n")

				# for year y go!
				# subset site visit for year y
				dataset <- site_visit[site_visit$visit_year==year_run,]
				dataset$species <- as.character(speciesGAM)
				dataset$spp_count <- 0

				# add the observed count for this year
				species_visit_year <- species_visit[species_visit$visit_year==year_run,]
				count_index <- match(paste(species_visit_year$transect_id,species_visit_year$visit_year,species_visit_year$visit_month,species_visit_year$visit_day,sep='_'),paste(dataset$transect_id,dataset$visit_year,dataset$visit_month,dataset$visit_day,sep='_'))
				dataset$spp_count[count_index] <- species_visit_year$individual_count
				dataset <- dataset[,c("species","transect_id","visit_year","visit_month","visit_day","julian_day","spp_count")]
				dataset <- data.frame(dataset, stringsAsFactors = FALSE)

				#keep the country name in the transect
				# SCRIPT FOR MODELLING
				
				spp_lims = as.character(speciesGAM)
				sp_data = dataset
				
				# Rename columns to standard names
				names(sp_data) = c("SPECIES","SITE","YEAR","MONTH","DAY","DAYNO","COUNT")

				# Determine missing days and add to sp_data	
				sp_data_all <- year_day_func(sp_data)

				# Trim the data to keep only the site with visit 
				# NOTE: sites with no observation contain no information about flight period 
				observation_data <- aggregate(sp_data_all$COUNT[!is.na(sp_data_all$COUNT)],by=list(SITE=sp_data_all$SITE[!is.na(sp_data_all$COUNT)], YEAR =sp_data_all$YEAR[!is.na(sp_data_all$COUNT)]),FUN=sum,na.rm=TRUE)
				site_tokeep <- observation_data$SITE[observation_data$x > 0]
				sp_data <- sp_data_all[sp_data_all$SITE %in% site_tokeep,]

				site_tokeep_z <- observation_data$SITE[observation_data$x == 0]
				sp_data_z <- sp_data_all[sp_data_all$SITE %in% site_tokeep_z,]

				###################################
				# here we are for the weekly count
				###################################

				# remove dates outside the flight season defined by the anchors (0) and initalize the days to start at 1
				sp_data_trimmed <- sp_data[sp_data$DAYNO>=min(sp_data$DAYNO[sp_data$ANCHOR==1]) & sp_data$DAYNO<=max(sp_data$DAYNO[sp_data$ANCHOR==1]),]

				sp_data_trimmed$trimDAYNO <- sp_data_trimmed$DAYNO-min(sp_data_trimmed$DAYNO)+1

				# Fit a model of count function of days and site for this year

				gam_obj_site <- try(gam(COUNT~ s(trimDAYNO,bs="cr") + SITE ,data=sp_data_trimmed,family = poisson(link="log"),optimizer=c("perf")), silent=TRUE)

	if(class(gam_obj_site)[1] == "try-error"){

#########

	cat("\t\tTry to fit GAM without performance fitting", year_lims,speciesGAM,climate_zone,"-", format(Sys.time(), "%d/%m/%Y %H:%M:%S"),"\n")

	gam_obj_site <- try(gam(COUNT~ s(trimDAYNO,bs="cr") + SITE ,data=sp_data_trimmed,family = poisson(link="log")), silent=TRUE)
		
		if(class(gam_obj_site)[1] == "try-error"){
		cat("\t\tUnable to fit GAM even without performance optimizer", year_lims,speciesGAM,climate_zone,"-", format(Sys.time(), "%d/%m/%Y %H:%M:%S"),"\n")
		} else {
		
			# Generate a list of values for all days from the aditive model and use these value to fill the missing obserations
			sp_data_trimmed[,"FITTED"] <- predict.gam(gam_obj_site, newdata = sp_data_trimmed, type="response")
			sp_data_trimmed[,"COUNT_IMPUTED"] <- sp_data_trimmed$COUNT
			sp_data_trimmed[is.na(sp_data_trimmed$COUNT),"COUNT_IMPUTED"] <- sp_data_trimmed$FITTED[is.na(sp_data_trimmed$COUNT)]

			# Define the flight curve from the fitted values and append them over years (this is one flight curve per year for all site)

			site_sums = aggregate(sp_data_trimmed$FITTED, by = list(SITE = sp_data_trimmed$SITE), FUN = sum)
			# Rename sum column
			names(site_sums)[names(site_sums) == "x"]  = "SITE_YR_FSUM"
			# Add data to sp_data data.frame (ensure merge does not sort the data!)
			sp_data_trimmed = merge(sp_data_trimmed, site_sums, by = c("SITE"), all = TRUE, sort = FALSE)
			# Calculate normalised values
			sp_data_trimmed[,"NM"] = sp_data_trimmed$FITTED/sp_data_trimmed$SITE_YR_FSUM

			sp_data_filled <- sp_data_trimmed[! paste(sp_data_trimmed$DAY_WEEK,sp_data_trimmed$COUNT)%in%c("1 NA","2 NA","3 NA","5 NA","6 NA","7 NA"),]

			flight_curve <- data.frame(species=sp_data_filled$SPECIES, year=sp_data_filled$YEAR, week=sp_data_filled$WEEK, DAYNO=sp_data_filled$DAYNO,DAYNO_adj=sp_data_filled$trimDAYNO, nm=sp_data_filled$NM)[!duplicated(paste(sp_data_filled$YEAR,sp_data_filled$DAYNO,sep="_")),]

			flight_curve <- flight_curve[order(flight_curve$DAYNO),]


				plot(flight_curve$DAYNO,flight_curve$nm,type='l')
				points(flight_curve$DAYNO,flight_curve$nm,col='red')
				abline(v=152,col="blue")
				abline(v=60,col="grey")
				abline(v=273,col="grey")
				text(152,max(flight_curve$nm),"June")
				text(250,max(flight_curve$nm),year_lims)


				# bind if exist else create
				if("flight_pheno" %in% ls()){
				flight_pheno <- rbind(flight_pheno,flight_curve)
				} else {flight_pheno <- flight_curve}

			glm_obj <- glm(COUNT~ factor(SITE) + offset(log(NM)) - 1, data = sp_data_filled, family = poisson(link="log")) 
			sp_data_filled[,"FULL_FIT"] = predict.glm(glm_obj, newdata=sp_data_filled, type="response")

			sp_data_filled[,"IMP"] = sp_data_filled[,"COUNT"]
			sp_data_filled[is.na(sp_data_filled$COUNT),"IMP"] = sp_data_filled[is.na(sp_data_filled$COUNT),"FULL_FIT"]

			indices_site <- data.frame(trap_index(sp_data_filled, data_col = "IMP", time_col = "DAYNO", by_col = c("SPECIES","SITE","YEAR")),climate_zone=climate_zone)

			indices_site_count <- trap_index(sp_data_filled, data_col = "COUNT", time_col = "DAYNO", by_col = c("SPECIES","SITE","YEAR"))
			indices_site$proportion_count <- indices_site_count$SINDEX / indices_site$SINDEX

			#####################

			sp_data_filled_noanchors <- sp_data_filled[sp_data_filled$ANCHOR != 1,]

			nbr_of_week_site <- aggregate(sp_data_filled_noanchors$WEEK,by=list(SITE=sp_data_filled_noanchors$SITE),function(x) length(unique(x)))
			names(nbr_of_week_site) <- c("SITE","total_nbr_week")

			sp_data_filled_noanchors_noNA <- sp_data_filled[sp_data_filled$ANCHOR != 1 & !is.na(sp_data_filled$COUNT),]
			nbr_of_sampled_week_site <- aggregate(sp_data_filled_noanchors_noNA$WEEK,by=list(SITE=sp_data_filled_noanchors_noNA$SITE),function(x) length(unique(x)))
			names(nbr_of_sampled_week_site) <- c("SITE","nbr_sampled_week")

			week_sampling <- merge(nbr_of_week_site,nbr_of_sampled_week_site,by=c("SITE"),all=TRUE,sort=FALSE)
			week_sampling$nbr_missing_week <- week_sampling$total_nbr_week - week_sampling$nbr_sampled_week

			sampling_week_report <- week_sampling

			indices_site <- merge(indices_site,sampling_week_report,by=c("SITE"),all=TRUE,sort=FALSE)

			peak_week <- sp_data_filled$WEEK[sp_data_filled$NM==max(sp_data_filled$NM)][1]
			obs_peak <- sp_data_filled[paste(sp_data_filled$SITE,sp_data_filled$WEEK,sep="_")%in%unique(c(paste(sp_data_filled$SITE,peak_week-1,sep="_"),paste(sp_data_filled$SITE,peak_week,sep="_"),paste(sp_data_filled$SITE,peak_week+1,sep="_"))),c("SITE","COUNT")]

			max_imput <- aggregate(sp_data_trimmed$COUNT_IMPUTED, by = list(SITE = sp_data_trimmed$SITE), function(x) max(x,na.rm=T)); names(max_imput) <- c("SITE","max_imput")
			max_count <- aggregate(sp_data_trimmed$COUNT, by = list(SITE = sp_data_trimmed$SITE), function(x) max(x,na.rm=T)); names(max_count) <- c("SITE","max_count")
			imput_vs_count <- merge(max_imput,max_count,by=c("SITE"),all=TRUE,sort=FALSE)
			imput_vs_count$larger_imput_5pct <- (imput_vs_count$max_count - imput_vs_count$max_imput) <= (imput_vs_count$max_count - (imput_vs_count$max_count*1.05))

				if(dim(obs_peak)[1]>0){
				peak_obs <- aggregate(obs_peak$COUNT,by = list(SITE = obs_peak$SITE),function(x) sum(!is.na(x)) > 0)
				names(peak_obs)[names(peak_obs) == "x"]  = "around_peak_obs"
				} else { peak_obs <- data.frame(SITE=unique(sp_data_filled$SITE),peak_obs=FALSE)}

			indices_site <- merge(indices_site,peak_obs,by=c("SITE"),all=TRUE,sort=FALSE)
			indices_site <- merge(indices_site, imput_vs_count[,-c(2,3)],by=c("SITE"),all=TRUE,sort=FALSE)

			indices_site$peak_week <- peak_week


			###############################
			# START FOR ZEROS OBSERVATIONS
			###############################

			if(dim(sp_data_z)[1]>0){

			sp_data_zeros <- sp_data_z

			# remove dates outside the flight season in site with only Zeros and remove the anchors (0)
			sp_data_zeros_trimmed <- sp_data_zeros[sp_data_zeros$DAYNO>=min(sp_data_zeros$DAYNO[sp_data_zeros$ANCHOR==1]) & sp_data_zeros$DAYNO<=max(sp_data_zeros$DAYNO[sp_data_zeros$ANCHOR==1]),]
			sp_data_zeros_trimmed$trimDAYNO <- sp_data_zeros_trimmed$DAYNO-min(sp_data_zeros_trimmed$DAYNO)+1
			sp_data_zeros_trimmed$FITTED <- NA
			sp_data_zeros_trimmed$COUNT_IMPUTED <- NA
			sp_data_zeros_trimmed$NM <- NA

			#####################
			sp_data_zero_filled_noanchors <- sp_data_zeros_trimmed[sp_data_zeros_trimmed$ANCHOR != 1,]

			nbr_of_week_site <- aggregate(sp_data_zero_filled_noanchors$WEEK,by=list(SITE=sp_data_zero_filled_noanchors$SITE),function(x) length(unique(x)))
			names(nbr_of_week_site) <- c("SITE","total_nbr_week")

			sp_data_zero_filled_noanchors_noNA <- sp_data_zeros_trimmed[sp_data_zeros_trimmed$ANCHOR != 1 & !is.na(sp_data_zeros_trimmed$COUNT),]
			nbr_of_sampled_week_site <- aggregate(sp_data_zero_filled_noanchors_noNA$WEEK,by=list(SITE=sp_data_zero_filled_noanchors_noNA$SITE),function(x) length(unique(x)))
			names(nbr_of_sampled_week_site) <- c("SITE","nbr_sampled_week")

			week_sampling <- merge(nbr_of_week_site,nbr_of_sampled_week_site,by=c("SITE"),all=TRUE,sort=FALSE)
			week_sampling$nbr_missing_week <- week_sampling$total_nbr_week - week_sampling$nbr_sampled_week

			sampling_week_report_zeros <- week_sampling

			##########################

			# FOR site with zeros only
			indices_sites_zeros <- unique(data.frame(sp_data_zeros_trimmed[,c(2,1,3)],SINDEX = 0 ,climate_zone=climate_zone,proportion_count=1))
			indices_sites_zeros <- merge(indices_sites_zeros,sampling_week_report_zeros,by=c("SITE"),all=TRUE,sort=FALSE)

			obs_peak <- sp_data_zeros_trimmed[paste(sp_data_zeros_trimmed$SITE,sp_data_zeros_trimmed$WEEK,sep="_")%in% unique(c(paste(sp_data_zeros_trimmed$SITE,peak_week-1,sep="_"),paste(sp_data_zeros_trimmed$SITE,peak_week,sep="_"),paste(sp_data_zeros_trimmed$SITE,peak_week+1 ,sep="_"))),c("SITE","COUNT")]

			if(dim(obs_peak)[1]>0){
			peak_obs <- aggregate(obs_peak$COUNT,by = list(SITE = obs_peak$SITE),function(x) sum(!is.na(x)) > 0)
			names(peak_obs)[names(peak_obs) == "x"]  = "around_peak_obs"
			} else { peak_obs <- data.frame(SITE=unique(sp_data_zeros_trimmed$SITE),peak_obs=FALSE)}

			indices_sites_zeros <- merge(indices_sites_zeros,peak_obs,by=c("SITE"),all=TRUE,sort=FALSE)
			indices_sites_zeros$larger_imput_5pct <- FALSE
			indices_sites_zeros$peak_week <- peak_week
			indices_site <- rbind(indices_site,indices_sites_zeros)
			indices_site <- indices_site[order(indices_site$SITE),]
			} else { cat("\t\tno zeros index for ",year_lims ,"!\n")}

			###############################
			# END FOR ZEROS OBSERVATIONS
			###############################

				# bind if exist else create
				if("site_count_year" %in% ls()){
				site_count_year <- rbind(site_count_year,indices_site)
				} else {site_count_year <- indices_site}

			} # END of computation after gam FITTING without performance optimization

########

	} else {

		# Generate a list of values for all days from the aditive model and use these value to fill the missing obserations
		sp_data_trimmed[,"FITTED"] <- predict.gam(gam_obj_site, newdata = sp_data_trimmed, type="response")
		sp_data_trimmed[,"COUNT_IMPUTED"] <- sp_data_trimmed$COUNT
		sp_data_trimmed[is.na(sp_data_trimmed$COUNT),"COUNT_IMPUTED"] <- sp_data_trimmed$FITTED[is.na(sp_data_trimmed$COUNT)]

		# Define the flight curve from the fitted values and append them over years (this is one flight curve per year for all site)

		site_sums = aggregate(sp_data_trimmed$FITTED, by = list(SITE = sp_data_trimmed$SITE), FUN = sum)
		# Rename sum column
		names(site_sums)[names(site_sums) == "x"]  = "SITE_YR_FSUM"
		# Add data to sp_data data.frame (ensure merge does not sort the data!)
		sp_data_trimmed = merge(sp_data_trimmed, site_sums, by = c("SITE"), all = TRUE, sort = FALSE)
		# Calculate normalised values
		sp_data_trimmed[,"NM"] = sp_data_trimmed$FITTED/sp_data_trimmed$SITE_YR_FSUM

		sp_data_filled <- sp_data_trimmed[! paste(sp_data_trimmed$DAY_WEEK,sp_data_trimmed$COUNT)%in%c("1 NA","2 NA","3 NA","5 NA","6 NA","7 NA"),]

		flight_curve <- data.frame(species=sp_data_filled$SPECIES, year=sp_data_filled$YEAR, week=sp_data_filled$WEEK, DAYNO=sp_data_filled$DAYNO,DAYNO_adj=sp_data_filled$trimDAYNO, nm=sp_data_filled$NM)[!duplicated(paste(sp_data_filled$YEAR,sp_data_filled$DAYNO,sep="_")),]

		flight_curve <- flight_curve[order(flight_curve$DAYNO),]


			plot(flight_curve$DAYNO,flight_curve$nm,type='l')
			points(flight_curve$DAYNO,flight_curve$nm,col='red')
			abline(v=152,col="blue")
			abline(v=60,col="grey")
			abline(v=273,col="grey")
			text(152,max(flight_curve$nm),"June")
			text(250,max(flight_curve$nm),year_lims)


			# bind if exist else create
			if("flight_pheno" %in% ls()){
			flight_pheno <- rbind(flight_pheno,flight_curve)
			} else {flight_pheno <- flight_curve}

		glm_obj <- glm(COUNT~ factor(SITE) + offset(log(NM)) - 1, data = sp_data_filled, family = poisson(link="log")) 
		sp_data_filled[,"FULL_FIT"] = predict.glm(glm_obj, newdata=sp_data_filled, type="response")

		sp_data_filled[,"IMP"] = sp_data_filled[,"COUNT"]
		sp_data_filled[is.na(sp_data_filled$COUNT),"IMP"] = sp_data_filled[is.na(sp_data_filled$COUNT),"FULL_FIT"]

		indices_site <- data.frame(trap_index(sp_data_filled, data_col = "IMP", time_col = "DAYNO", by_col = c("SPECIES","SITE","YEAR")),climate_zone=climate_zone)

		indices_site_count <- trap_index(sp_data_filled, data_col = "COUNT", time_col = "DAYNO", by_col = c("SPECIES","SITE","YEAR"))
		indices_site$proportion_count <- indices_site_count$SINDEX / indices_site$SINDEX

		#####################

		sp_data_filled_noanchors <- sp_data_filled[sp_data_filled$ANCHOR != 1,]

		nbr_of_week_site <- aggregate(sp_data_filled_noanchors$WEEK,by=list(SITE=sp_data_filled_noanchors$SITE),function(x) length(unique(x)))
		names(nbr_of_week_site) <- c("SITE","total_nbr_week")

		sp_data_filled_noanchors_noNA <- sp_data_filled[sp_data_filled$ANCHOR != 1 & !is.na(sp_data_filled$COUNT),]
		nbr_of_sampled_week_site <- aggregate(sp_data_filled_noanchors_noNA$WEEK,by=list(SITE=sp_data_filled_noanchors_noNA$SITE),function(x) length(unique(x)))
		names(nbr_of_sampled_week_site) <- c("SITE","nbr_sampled_week")

		week_sampling <- merge(nbr_of_week_site,nbr_of_sampled_week_site,by=c("SITE"),all=TRUE,sort=FALSE)
		week_sampling$nbr_missing_week <- week_sampling$total_nbr_week - week_sampling$nbr_sampled_week

		sampling_week_report <- week_sampling

		indices_site <- merge(indices_site,sampling_week_report,by=c("SITE"),all=TRUE,sort=FALSE)

		peak_week <- sp_data_filled$WEEK[sp_data_filled$NM==max(sp_data_filled$NM)][1]
		obs_peak <- sp_data_filled[paste(sp_data_filled$SITE,sp_data_filled$WEEK,sep="_")%in%unique(c(paste(sp_data_filled$SITE,peak_week-1,sep="_"),paste(sp_data_filled$SITE,peak_week,sep="_"),paste(sp_data_filled$SITE,peak_week+1,sep="_"))),c("SITE","COUNT")]

		max_imput <- aggregate(sp_data_trimmed$COUNT_IMPUTED, by = list(SITE = sp_data_trimmed$SITE), function(x) max(x,na.rm=T)); names(max_imput) <- c("SITE","max_imput")
		max_count <- aggregate(sp_data_trimmed$COUNT, by = list(SITE = sp_data_trimmed$SITE), function(x) max(x,na.rm=T)); names(max_count) <- c("SITE","max_count")
		imput_vs_count <- merge(max_imput,max_count,by=c("SITE"),all=TRUE,sort=FALSE)
		imput_vs_count$larger_imput_5pct <- (imput_vs_count$max_count - imput_vs_count$max_imput) <= (imput_vs_count$max_count - (imput_vs_count$max_count*1.05))

		if(dim(obs_peak)[1]>0){
		peak_obs <- aggregate(obs_peak$COUNT,by = list(SITE = obs_peak$SITE),function(x) sum(!is.na(x)) > 0)
		names(peak_obs)[names(peak_obs) == "x"]  = "around_peak_obs"
		} else { peak_obs <- data.frame(SITE=unique(sp_data_filled$SITE),peak_obs=FALSE)}

		indices_site <- merge(indices_site,peak_obs,by=c("SITE"),all=TRUE,sort=FALSE)
		indices_site <- merge(indices_site, imput_vs_count[,-c(2,3)],by=c("SITE"),all=TRUE,sort=FALSE)

		indices_site$peak_week <- peak_week


		###############################
		# START FOR ZEROS OBSERVATIONS
		###############################

		if(dim(sp_data_z)[1]>0){

		sp_data_zeros <- sp_data_z

		# remove dates outside the flight season in site with only Zeros and remove the anchors (0)
		sp_data_zeros_trimmed <- sp_data_zeros[sp_data_zeros$DAYNO>=min(sp_data_zeros$DAYNO[sp_data_zeros$ANCHOR==1]) & sp_data_zeros$DAYNO<=max(sp_data_zeros$DAYNO[sp_data_zeros$ANCHOR==1]),]
		sp_data_zeros_trimmed$trimDAYNO <- sp_data_zeros_trimmed$DAYNO-min(sp_data_zeros_trimmed$DAYNO)+1
		sp_data_zeros_trimmed$FITTED <- NA
		sp_data_zeros_trimmed$COUNT_IMPUTED <- NA
		sp_data_zeros_trimmed$NM <- NA

		#####################
		sp_data_zero_filled_noanchors <- sp_data_zeros_trimmed[sp_data_zeros_trimmed$ANCHOR != 1,]

		nbr_of_week_site <- aggregate(sp_data_zero_filled_noanchors$WEEK,by=list(SITE=sp_data_zero_filled_noanchors$SITE),function(x) length(unique(x)))
		names(nbr_of_week_site) <- c("SITE","total_nbr_week")

		sp_data_zero_filled_noanchors_noNA <- sp_data_zeros_trimmed[sp_data_zeros_trimmed$ANCHOR != 1 & !is.na(sp_data_zeros_trimmed$COUNT),]
		nbr_of_sampled_week_site <- aggregate(sp_data_zero_filled_noanchors_noNA$WEEK,by=list(SITE=sp_data_zero_filled_noanchors_noNA$SITE),function(x) length(unique(x)))
		names(nbr_of_sampled_week_site) <- c("SITE","nbr_sampled_week")

		week_sampling <- merge(nbr_of_week_site,nbr_of_sampled_week_site,by=c("SITE"),all=TRUE,sort=FALSE)
		week_sampling$nbr_missing_week <- week_sampling$total_nbr_week - week_sampling$nbr_sampled_week

		sampling_week_report_zeros <- week_sampling

		##########################

		# FOR site with zeros only
		indices_sites_zeros <- unique(data.frame(sp_data_zeros_trimmed[,c(2,1,3)],SINDEX = 0 ,climate_zone=climate_zone,proportion_count=1))
		indices_sites_zeros <- merge(indices_sites_zeros,sampling_week_report_zeros,by=c("SITE"),all=TRUE,sort=FALSE)

		obs_peak <- sp_data_zeros_trimmed[paste(sp_data_zeros_trimmed$SITE,sp_data_zeros_trimmed$WEEK,sep="_")%in% unique(c(paste(sp_data_zeros_trimmed$SITE,peak_week-1,sep="_"),paste(sp_data_zeros_trimmed$SITE,peak_week,sep="_"),paste(sp_data_zeros_trimmed$SITE,peak_week+1 ,sep="_"))),c("SITE","COUNT")]

			if(dim(obs_peak)[1]>0){
			peak_obs <- aggregate(obs_peak$COUNT,by = list(SITE = obs_peak$SITE),function(x) sum(!is.na(x)) > 0)
			names(peak_obs)[names(peak_obs) == "x"]  = "around_peak_obs"
			} else { peak_obs <- data.frame(SITE=unique(sp_data_zeros_trimmed$SITE),peak_obs=FALSE)}

		indices_sites_zeros <- merge(indices_sites_zeros,peak_obs,by=c("SITE"),all=TRUE,sort=FALSE)
		indices_sites_zeros$larger_imput_5pct <- FALSE
		indices_sites_zeros$peak_week <- peak_week
		indices_site <- rbind(indices_site,indices_sites_zeros)
		indices_site <- indices_site[order(indices_site$SITE),]
		} else { cat("\t\tno zeros index for ",year_lims ,"!\n") }

		###############################
		# END FOR ZEROS OBSERVATIONS
		###############################

			# bind if exist else create
			if("site_count_year" %in% ls()){
			site_count_year <- rbind(site_count_year,indices_site)
			} else {site_count_year <- indices_site}
			
	} # END of computation whith GAM fitted using performance optimization

	# IF less than 5 transects for that year
	} else { year_lims = year_run
		# Print progress to screen
		cat("\t\tYear", year_lims,speciesGAM,climate_zone,"LESS THEN 5 TRANSECTS -", format(Sys.time(), "%d/%m/%Y %H:%M:%S"),"\n")}
	
	# IF no observation at all for that year
	} else {year_lims = year_run 		
		# Print progress to screen
		cat("\t\tYear", year_lims,speciesGAM,climate_zone,"NO OBSERVATION -", format(Sys.time(), "%d/%m/%Y %H:%M:%S"),"\n")}

} # END OF YEAR LOOP

#######################

species_name <- gsub("/"," ",speciesGAM)

if("site_count_year" %in% ls())
{

		if (!file.exists(file.path(origin_wd,"indices",climate_zone_name,paste("no",country,sep="_")))){

		dir.create(file.path(origin_wd,"indices",climate_zone_name,paste("no",country,sep="_")),recursive =TRUE)
		write.csv(site_count_year,file=paste("indices/",climate_zone_name,"/",paste("no",country,sep="_"),"/site_count_year_",species_name,".csv",sep=""))
		write.csv(flight_pheno,file=paste("indices/",climate_zone_name,"/",paste("no",country,sep="_"),"/flight_curve_",species_name,".csv",sep=""))	

		} else {cat(paste("file",file.path(origin_wd,"indices",climate_zone_name,paste("no",country,sep="_")),"exist! \n\n"))
			write.csv(site_count_year,file=paste("indices/",climate_zone_name,"/",paste("no",country,sep="_"),"/site_count_year_",species_name,".csv",sep=""))
			write.csv(flight_pheno,file=paste("indices/",climate_zone_name,"/",paste("no",country,sep="_"),"/flight_curve_",species_name,".csv",sep=""))}

} else {
		if (!file.exists(file.path(origin_wd,"indices",climate_zone_name,paste("no",country,sep="_")))){
		dir.create(file.path(origin_wd,"indices",climate_zone_name,paste("no",country,sep="_")),recursive =TRUE) 
		sink(paste("indices/",climate_zone_name,"/",paste("no",country,sep="_"),"/",species_name,"_diagnostic.txt",sep=""))
		cat("\t\tNot enough observation for -", speciesGAM, format(Sys.time(), "%d/%m/%Y %H:%M:%S"),"\n")
		sink()
		} else {
		sink(paste("indices/",climate_zone_name,"/",paste("no",country,sep="_"),"/",species_name,"_diagnostic.txt",sep=""))
		cat("\t\tNot enough observation for -", speciesGAM, format(Sys.time(), "%d/%m/%Y %H:%M:%S"),"\n")
		sink()
		}
	}

rm(list=ls()[!ls() %in% object_to_keep])

} # end of species loop
} # end climate zone loop

############## - END OF CODE - ########################

