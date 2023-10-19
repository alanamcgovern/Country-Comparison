library(tidyverse)
library(stringr)
library(gridExtra)
library(readxl)
library(tidyverse)
library(scales)
library(rgdal)
library(ggpubr)
library(ggplotify)
library(data.table)


# Load in IGME NMR results ----
igme.ests.nmr.raw <- read.csv('/Users/alanamcgovern/Desktop/Research/UN_Estimates/Country Comparison/igme2022_nmr.csv')
igme.ests.nmr <- data.frame(igme.ests.nmr.raw)
igme.ests.nmr <- gather(igme.ests.nmr,year,estimate,X2000.5:X2022.5)
igme.ests.nmr <- igme.ests.nmr %>% select(iso,year,Quantile,estimate)
igme.ests.nmr <- spread(igme.ests.nmr,Quantile,estimate)
igme.ests.nmr$year <-  as.numeric(stringr::str_remove(igme.ests.nmr$year,'X')) - 0.5
igme.ests.nmr$median <- igme.ests.nmr$Median/1000
igme.ests.nmr$lower <- igme.ests.nmr$Lower/1000
igme.ests.nmr$upper <- igme.ests.nmr$Upper/1000
iso.codes.raw <- read.csv('/Users/alanamcgovern/Desktop/Research/UN_Estimates/Country Comparison/iso_codes.csv')
iso.codes <- iso.codes.raw %>% select(name,alpha.3,sub.region,intermediate.region)
igme.ests.nmr <- merge(iso.codes,igme.ests.nmr,by.x='alpha.3',by.y='iso')

igme.ests.nmr <- igme.ests.nmr %>% select(alpha.3,name,sub.region,intermediate.region,year,lower,median,upper) %>% rename(iso=alpha.3,country=name)
#change some country names to match up
igme.ests.nmr[igme.ests.nmr$country=="Lao People's Democratic Republic",]$country <- 'Laos'
igme.ests.nmr[igme.ests.nmr$country=="Sierra Leone",]$country <- 'Sierra_Leone'
igme.ests.nmr[igme.ests.nmr$country=="Tanzania, United Republic of",]$country <- 'Tanzania'
igme.ests.nmr[igme.ests.nmr$intermediate.region=='',]$intermediate.region <- igme.ests.nmr[igme.ests.nmr$intermediate.region=='',]$sub.region

# load in year of last survey and intermediate region key ------
analysis.info <- read_excel('/Users/alanamcgovern/Desktop/Research/UN_Estimates/UN-Subnational-Estimates/UN Priority Countries 2022.xlsx',sheet=2)
survey.info <- analysis.info[,c(2,11)]
colnames(survey.info) <- c('country','last_year')
survey.info <- survey.info[!is.na(survey.info$last_year),]
survey.info[survey.info$country=="United Republic of Tanzania",]$country <- 'Tanzania'
survey.info[survey.info$country=="Sierra Leone",]$country <- 'Sierra_Leone'

igme.ests.nmr <- igme.ests.nmr %>% filter(country %in% survey.info$country)
region_key <- igme.ests.nmr %>% select(country,sub.region,intermediate.region) %>% unique()
region_key <- right_join(region_key,survey.info,by='country')

# load in map data -------
setwd("/Users/alanamcgovern/Desktop/Research/UN_Estimates/UN-Subnational-Estimates")
admin_key <- read_excel("/Users/alanamcgovern/Desktop/Research/UN_Estimates/UN-Subnational-Estimates/Data/shapeFiles_gadm/Admin1_Admin2_Key.xlsx")

adm1poly_dat_t <- adm2poly_dat_t <- NULL
for(country_t in region_key$country){
  # load country info
  load(paste0('Info/',country_t,"_general_info.Rdata"))
  
  # # load admin1 shapefiles
  if(str_detect(poly.layer.adm1,'gadm')){
    folder.name <- str_split(poly.layer.adm1,'_')[[1]][1:2]
    poly.path <- paste0('Data/shapeFiles_gadm/',folder.name[1],'_',folder.name[2],'_shp')
    poly.adm1 <- readOGR(dsn = poly.path, encoding = "UTF-8", layer = poly.layer.adm1)
  }else{
    poly.path.new <- paste0('Data/shapeFiles_alt/',country_t,'/',poly.path)
    poly.adm1 <- readOGR(dsn = poly.path.new, encoding = "UTF-8", layer = poly.layer.adm1)
  }
  geo <- ggplot2::fortify(poly.adm1, region = strsplit(poly.label.adm1, "\\$")[[1]][2])
  geo$country <- country_t
  adm1poly_dat_t <- rbind(adm1poly_dat_t,geo)
  
  #load admin2 shapefiles
  if(exists('poly.label.adm2') & !(country_t %in% c('Ghana','Kenya','Laos','Lesotho','Nigeria','Togo'))){
    if(str_detect(poly.layer.adm1,'gadm')){
      folder.name <- str_split(poly.layer.adm2,'_')[[1]][1:2]
      poly.path <- paste0('Data/shapeFiles_gadm/',folder.name[1],'_',folder.name[2],'_shp')
      poly.adm2 <- readOGR(dsn = poly.path, encoding = "UTF-8", layer = poly.layer.adm2)
    }else{
      poly.path.new <- paste0('Data/shapeFiles_alt/',country_t,'/',poly.path)
      poly.adm2 <- readOGR(dsn = poly.path.new, encoding = "UTF-8", layer = poly.layer.adm2)
    }
    geo <- ggplot2::fortify(poly.adm2, region = strsplit(poly.label.adm2, "\\$")[[1]][2])
    geo$country <- country_t
    geo <- left_join(geo,admin_key %>% filter(country==country_t) %>% rename(id=admin2.name),by='id')
    adm2poly_dat_t <- rbind(adm2poly_dat_t,geo)
  }
  
}

adm1poly_dat <- adm1poly_dat_t
adm2poly_dat <- adm2poly_dat_t

#remove disputed areas (Pakistan)
adm1poly_dat <- adm1poly_dat[!(adm1poly_dat$id %in% c('Gilgit-Baltistan','Azad Kashmir')),]
adm2poly_dat <- adm2poly_dat[!(adm2poly_dat$id %in% c('Northern Areas','Azad Kashmir')),]

#remove body of water (Tanzania)
adm2poly_dat$admin2.name <- paste0(adm2poly_dat$id,', ',adm2poly_dat$admin1.name)
adm2poly_dat <- adm2poly_dat %>% filter(admin2.name!='Lake Rukwa, Rukwa')

# change some names to match with data
for(num in 1:7){
  adm1poly_dat[adm1poly_dat$id==paste0(num),]$id <- paste0('District #', num)
}

# Load in National stratified NMR results -----
natl_nmr_files_list <- list.files(path = "/Users/alanamcgovern/Desktop/Research/UN_Estimates/Country Comparison/Final All Countries/NMR/National (stratified only)")
natl_strat_nmr_results_t <- NULL

for(file in natl_nmr_files_list){
  obj_name <- load(paste0('/Users/alanamcgovern/Desktop/Research/UN_Estimates/Country Comparison/Final All Countries/NMR/National (stratified only)/', file))
  res_t <- eval(str2lang(obj_name))$stratified
  res_t$country <- str_split(file,'_res')[[1]][1]
  res_t <- res_t %>% select(country, strata, years.num, median, mean, lower, upper, variance)
  natl_strat_nmr_results_t <- rbind(natl_strat_nmr_results_t, res_t)
}


# Load in Admin1 NMR results and draw posterior samples -----
adm1_nmr_files_list <- list.files(path = "/Users/alanamcgovern/Desktop/Research/UN_Estimates/Country Comparison/Final All Countries/NMR/Admin 1")
admin_key <- read_excel("/Users/alanamcgovern/Desktop/Research/UN_Estimates/UN-Subnational-Estimates/Data/shapeFiles_gadm/Admin1_Admin2_Key.xlsx")
years_vt <- c(beg.year:2021)
n_years <- length(years_vt)
n_samp <- 1000

adm1_nmr_results_t <- NULL
adm1_nmr_ranks_t <- NULL
for(file in adm1_nmr_files_list){
  # load results object
  obj_name <- load(paste0('/Users/alanamcgovern/Desktop/Research/UN_Estimates/Country Comparison/Final All Countries/NMR/Admin 1/', file))
  obj_t <- eval(str2lang(obj_name))
  #determine country
  country_t <- str_split(file,'_res')[[1]][1]
  #subset admin areas
  admin1_key_t <- admin_key %>% filter(country==country_t) %>% select(admin1.name,admin1.char) %>% unique()
  
  # get posterior estimates and organize
  res_t <- obj_t$overall
  res_t$country <- country_t
  res_t <- res_t %>% select(country, region, years.num, median, mean, lower, upper, variance) %>% rename(year=years.num)
  res_t <- merge(admin1_key_t,res_t,by.x='admin1.char',by.y='region')
  adm1_nmr_results_t <- rbind(adm1_nmr_results_t, res_t)
  
  # get posterior draws and organize
  draws_list <- obj_t$draws.est.overall
  # excludes disputed areas not in admin key (Pakistan)
  cond <- lapply(draws_list,function(x){x$region %in% admin1_key_t$admin1.char})
  draws_list <- draws_list[unlist(cond)]
  n_admin <- length(draws_list)/n_years 
  if(n_admin!=nrow(admin1_key_t) & country_t!='Pakistan'){
    stop('Number of admin areas does not match up.')
  }
  postsamp_mt_list <- vector(mode = "list", length = n_years)
  for (i in 1:n_years){
    # i <- 1
    postsamp_mt_list[[i]]$years <- years_vt[i]
    postsamp_mt <- matrix(0, nrow = n_admin, ncol = n_samp)
    
    for(j in 1:n_admin){
      # j <- 1
      postsamp_mt[j, ] <- draws_list[[n_years*(j-1)+i]]$draws
    }
    
    postsamp_mt_list[[i]]$postsamp_mt <- postsamp_mt
  }
  
  # get rank probabilites and organize
  for(year in years_vt){
    
    cond <- lapply(postsamp_mt_list, function(x){x$years == year})
    postsamp_mt <- postsamp_mt_list[unlist(cond)][[1]]$postsamp_mt
    
    # prepare the posterior sample for rank plot
    rank_mt <- apply(postsamp_mt, 2, rank)
    
    rank_dt <- matrix(NA,n_admin,n_admin)
    for(rank in 1:nrow(admin1_key_t)){
      rank_dt[,rank] <- apply(rank_mt, 1, function(x){sum(I(x==rank))/1000})
    }
    
    rank_dt <- data.frame(cbind(admin1_key_t,rank_dt))
    rank_dt_long <- gather(rank_dt,rank,prob,X1:str2lang(paste0('X',n_admin)))
    rank_dt_long$rank <- as.numeric(str_remove(rank_dt_long$rank,'X'))
    rank_dt_long$year <- year
    rank_dt_long$country <- country_t
    
    adm1_nmr_ranks_t <- rbind(adm1_nmr_ranks_t,rank_dt_long)
  }
}

adm1_nmr_results <- adm1_nmr_results_t
adm1_nmr_ranks <- adm1_nmr_ranks_t

# Load in Admin2 NMR results -----
adm2_nmr_files_list <- list.files(path = "/Users/alanamcgovern/Desktop/Research/UN_Estimates/Country Comparison/Final All Countries/NMR/Admin 2")
adm2_nmr_results_t <- NULL

for(file in adm2_nmr_files_list){
  obj_name <- load(paste0('/Users/alanamcgovern/Desktop/Research/UN_Estimates/Country Comparison/Final All Countries/NMR/Admin 2/', file))
  res_t <- eval(str2lang(obj_name))$overall
  res_t$country <- str_split(file,'_res')[[1]][1]
  res_t <- res_t %>% select(country, region, years.num, median, mean, lower, upper, variance)
  adm2_nmr_results_t <- rbind(adm2_nmr_results_t, res_t)
}

adm2_nmr_results <- adm2_nmr_results_t %>% rename(year=years.num)
# add admin names
adm2_nmr_results <- right_join(admin_key %>% unique(),adm2_nmr_results,join_by('country'=='country','admin2.char'=='region'))
# remove disputed areas in Pakistan
adm2_nmr_results <- adm2_nmr_results[!is.na(adm2_nmr_results$admin1.name),]
adm2_nmr_results$admin2.fullname <- paste0(adm2_nmr_results$admin2.name,', ',adm2_nmr_results$admin1.name)

# Summarize data ----

natl_strat_nmr_results <- left_join(natl_strat_nmr_results_t,region_key,by='country')
# between country variation (using IGME national estimates)
between_nmr <- igme.ests.nmr %>% group_by(year) %>% summarise(IQR = quantile(median, 0.75) - quantile(median, 0.25),
                                                                   scaled_IQR = (quantile(median, 0.75) - quantile(median, 0.25))/quantile(median,0.5),
                                                                   coeff_var=sd(median)/mean(median))

# within country variation (using admin1 estimates)
within_nmr_adm1 <- adm1_nmr_results %>% group_by(country,year) %>% summarise(IQR = quantile(median, 0.75) - quantile(median, 0.25),
                                                                             scaled_IQR = (quantile(median, 0.75) - quantile(median, 0.25))/quantile(median,0.5),
                                                                             coeff_var=sd(median)/mean(median))
diffs_nmr_adm1 <- merge(within_nmr_adm1,between_nmr,by='year') %>% rename(within_IQR=IQR.x, between_IQR=IQR.y,
                                                                         scaled_within_IQR=scaled_IQR.x, scaled_between_IQR=scaled_IQR.y,
                                                                         within_coefvar=coeff_var.x, between_coefvar=coeff_var.y)
diffs_nmr_adm1 <- left_join(diffs_nmr_adm1,region_key,by='country')
adm1_nmr_results <- left_join(adm1_nmr_results,region_key,by='country')
adm1_nmr_results <- adm1_nmr_results[order(adm1_nmr_results$country),]
adm1_nmr_results$natlmedian <- NA

for(country_t in unique(adm1_nmr_results$country)){
  for(year_t in unique(adm1_nmr_results$year)){
    adm1_nmr_results[adm1_nmr_results$country==country_t & adm1_nmr_results$year==year_t,]$natlmedian <- (igme.ests.nmr %>% filter(country==country_t,year==year_t))$median
  }
}
adm1_nmr_results$ratio_to_natl <- adm1_nmr_results$median/adm1_nmr_results$natlmedian

# within country variation (using admin2 estimates)
within_nmr_adm2 <- adm2_nmr_results %>% group_by(country,year) %>% summarise(IQR = quantile(median, 0.75) - quantile(median, 0.25),
                                                                             scaled_IQR = (quantile(median, 0.75) - quantile(median, 0.25))/quantile(median,0.5),
                                                                             coeff_var=sd(median)/mean(median))
diffs_nmr_adm2 <- merge(within_nmr_adm2,between_nmr,by='year') %>% rename(within_IQR=IQR.x, between_IQR=IQR.y,
                                                                          scaled_within_IQR=scaled_IQR.x, scaled_between_IQR=scaled_IQR.y,
                                                                          within_coefvar=coeff_var.x, between_coefvar=coeff_var.y)
diffs_nmr_adm2 <- left_join(diffs_nmr_adm2,region_key,by='country')
adm2_nmr_results <- left_join(adm2_nmr_results,region_key,by='country')
adm2_nmr_results <- adm2_nmr_results[order(adm2_nmr_results$country),]
adm2_nmr_results$natlmedian <- NA

for(country_t in unique(adm2_nmr_results$country)){
  for(year_t in unique(adm2_nmr_results$year)){
    adm2_nmr_results[adm2_nmr_results$country==country_t & adm2_nmr_results$year==year_t,]$natlmedian <- (igme.ests.nmr %>% filter(country==country_t,year==year_t))$median
  }
}
adm2_nmr_results$ratio_to_natl <- adm2_nmr_results$median/adm2_nmr_results$natlmedian

# Plots comparing within and between IQR ----

pdf("/Users/alanamcgovern/Desktop/Research/UN_Estimates/Country Comparison/Figures/Boxplot Summary of Variation NMR.pdf")
{
  g1 <- diffs_nmr_adm1 %>% ggplot() + geom_boxplot(aes(y=scaled_within_IQR)) + geom_hline(aes(yintercept=scaled_between_IQR),col='blue') + 
    facet_grid(~year) + ggtitle('Scaled IQR of Admin1 level NMR for each country across time')
  print(g1)
  
  g2 <- diffs_nmr_adm1 %>% ggplot() + geom_boxplot(aes(y=within_coefvar)) + geom_hline(aes(yintercept=between_coefvar),col='blue') + 
    facet_grid(~year) + ggtitle('Coefficient of Variation of Admin1 level NMR for each country across time')
  print(g2)
  
  g3 <- diffs_nmr_adm2 %>% ggplot() + geom_boxplot(aes(y=scaled_within_IQR)) + geom_hline(aes(yintercept=scaled_between_IQR),col='blue') + 
    facet_grid(~year) + ggtitle('Scaled IQR of Admin2 level NMR for each country across time')
  print(g3)
  
  g4 <- diffs_nmr_adm2 %>% ggplot() + geom_boxplot(aes(y=within_coefvar)) + geom_hline(aes(yintercept=between_coefvar),col='blue') + 
    facet_grid(~year) + ggtitle('Coefficient of Variation of Admin2 level NMR for each country across time')
  print(g4)
}
dev.off()

pdf("/Users/alanamcgovern/Desktop/Research/UN_Estimates/Country Comparison/Figures/Dot Plot Summary of Variation NMR.pdf")
{
f1 <- diffs_nmr_adm1 %>% ggplot() + geom_point(aes(x=year, y=scaled_within_IQR, col=intermediate.region),size=1) + 
  geom_point(aes(x=year, y=scaled_between_IQR),col='black') + 
 ggtitle('Scaled IQR of Admin1 level NMR for each country across time')
print(f1)

f2 <- diffs_nmr_adm1 %>% ggplot() + geom_point(aes(x=year, y=within_coefvar, col=intermediate.region),size=1) + 
  geom_point(aes(x=year, y=between_coefvar),col='black') + 
  ggtitle('Coefficient of Variation of Admin1 level NMR for each country across time')
print(f2)

f3 <- diffs_nmr_adm2 %>% ggplot() + geom_point(aes(x=year, y=scaled_within_IQR, col=intermediate.region),size=1) + 
  geom_point(aes(x=year, y=scaled_between_IQR),col='black') + 
  ggtitle('Scaled IQR of Admin2 level NMR for each country across time')
print(f3)

f4 <- diffs_nmr_adm2 %>% ggplot() + geom_point(aes(x=year, y=within_coefvar, col=intermediate.region),size=1) + 
  geom_point(aes(x=year, y=between_coefvar),col='black') + 
  ggtitle('Coefficient of Variation of Admin2 level NMR for each country across time')
print(f4)
}
dev.off()

# Plots examining within country variation of NMR ----

pdf("/Users/alanamcgovern/Desktop/Research/UN_Estimates/Country Comparison/Figures/Country Specific Admin1 NMR.pdf")

for(country_t in unique(adm1_nmr_results$country)){
  subdat <- adm1_nmr_results[adm1_nmr_results$country==country_t,]
  
  # which region has highest median (on average, across all years) and which has lowest
  region.avgs <- subdat %>% group_by(admin1.char) %>% summarise(mean(median))
  max.region <- region.avgs[which.max(region.avgs$`mean(median)`),]$admin1.char
  min.region <- region.avgs[which.min(region.avgs$`mean(median)`),]$admin1.char
  extreme.regions <- c(min.region,max.region)
  
  r1 <- subdat %>% filter(admin1.char %in% extreme.regions) %>% ggplot(aes(x=year)) + 
    # estimates for highest and lowest regions
    geom_line(aes(y=median,group=admin1.char,color=admin1.char)) + 
    # credible intervals of estimates
    geom_ribbon(aes(ymin=lower,ymax=upper,group=admin1.char,fill=admin1.char),alpha=0.25) + 
    # indicates where prediction starts
    geom_vline(aes(xintercept=last_year),lty=2,col='grey50') +
    # indicates SDG goal
    geom_hline(yintercept=12/1000,col='limegreen',alpha=0.75,lwd=0.75,lty=2) +
    # national IGME estimate
    geom_line(data=igme.ests.nmr[igme.ests.nmr$country==country_t,], aes(x=year, y = median)) +
    ggtitle(paste0(country_t)) + ggtitle('NMR estimates and CI in most extreme admin1 regions')
  

  r2 <- adm1_nmr_results %>% filter(country==country_t) %>% ggplot(aes(x=year)) + 
    # estimates for each region
    geom_line(aes(y=median,group=admin1.char,color=admin1.char)) + 
    # national estimate
    geom_line(data=igme.ests.nmr[igme.ests.nmr$country==country_t,], aes(x=year, y = median)) +
    # credible interval for national estimate
    geom_ribbon(data=igme.ests.nmr[igme.ests.nmr$country==country_t,], aes(ymin=lower, ymax = upper), alpha=0.25) + 
    # indicates where prediction starts
    geom_vline(aes(xintercept=last_year),lty=2,col='grey50') +
    # indicates SDG goal
    geom_hline(yintercept=12/1000,col='limegreen',alpha=0.5,lwd=0.75, lty=2) +
    ggtitle(paste0(country_t)) + theme(legend.position = 'none') + ggtitle(paste0(country_t, ' Admin 1 NMR Estimates'))
  
  grid.arrange(r2,r1)
}

dev.off()

pdf("/Users/alanamcgovern/Desktop/Research/UN_Estimates/Country Comparison/Figures/Country Specific Admin2 NMR.pdf")

for(country_t in unique(adm2_nmr_results$country)){
  subdat <- adm2_nmr_results[adm2_nmr_results$country==country_t,]
  
  region.avgs <- subdat %>% group_by(admin2.char) %>% summarise(mean(median))
  max.region <- region.avgs[which.max(region.avgs$`mean(median)`),]$admin2.char
  min.region <- region.avgs[which.min(region.avgs$`mean(median)`),]$admin2.char
  extreme.regions <- c(min.region,max.region)
  
  r1 <- adm2_nmr_results %>% filter(country==country_t, admin2.char %in% extreme.regions) %>% ggplot(aes(x=year)) + 
    geom_line(aes(y=median,group=admin2.char,color=admin2.char)) + 
    geom_ribbon(aes(ymin=lower,ymax=upper,group=admin2.char,fill=admin2.char),alpha=0.25) + 
    # indicates where prediction starts
    geom_vline(aes(xintercept=last_year),lty=2,col='grey50') +
    # indicates SDG goal
    geom_hline(yintercept=12/1000,col='limegreen',alpha=0.75,lwd=0.75,lty=2) +
    geom_line(data=igme.ests.nmr[igme.ests.nmr$country==country_t,], aes(x=year, y = median)) +
    ggtitle(paste0(country_t)) + ggtitle('NMR estimates and CI in most extreme admin2 regions')
  
  r2 <- adm2_nmr_results %>% filter(country==country_t) %>% ggplot(aes(x=year)) + 
    geom_line(aes(y=median,group=admin2.char,color=admin2.char)) + 
    geom_line(data=igme.ests.nmr[igme.ests.nmr$country==country_t,], aes(x=year, y = median)) +
    geom_ribbon(data=igme.ests.nmr[igme.ests.nmr$country==country_t,], aes(ymin=lower, ymax = upper), alpha=0.25) + 
    # indicates where prediction starts
    geom_vline(aes(xintercept=last_year),lty=2,col='grey50') +
    # indicates SDG goal
    geom_hline(yintercept=12/1000,col='limegreen',alpha=0.75,lwd=0.75,lty=2) +
    ggtitle(paste0(country_t)) + theme(legend.position = 'none') + ggtitle(paste0(country_t, ' Admin 2 NMR Estimates'))
  
  grid.arrange(r2,r1)
}

dev.off()

pdf("/Users/alanamcgovern/Desktop/Research/UN_Estimates/Country Comparison/Figures/NMR Subnational Estimates by Area.pdf")
{
# Sub-Saharan Africa  
subdat <- adm1_nmr_results[adm1_nmr_results$sub.region=='Sub-Saharan Africa',]
  subdat_nat <- igme.ests.nmr[igme.ests.nmr$country %in% subdat$country,]
  
  g <- subdat %>% ggplot(aes(x=year)) + 
    # estimates for each region
    geom_line(aes(y=median,group=admin1.char,color=admin1.char)) + 
    # national estimate
    geom_line(data=subdat_nat, aes(x=year, y = median)) +
    # indicates where prediction starts
    geom_vline(aes(xintercept=last_year),lty=2,col='grey50') +
    # indicates SDG goal
    geom_hline(yintercept=12/1000,col='limegreen',alpha=0.5,lwd=0.75, lty=2) +
    ylim(c(0.005,0.09)) +
    facet_wrap(~country) + theme(legend.position = 'none',axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle('Admin 1 level Subnational NMR in Sub-Saharan African Countries') + ylab('Median NMR Estimate')
  
  print(g)
  
# Other areas
  subdat <- adm1_nmr_results[adm1_nmr_results$sub.region!='Sub-Saharan Africa',]
  subdat_nat <- igme.ests.nmr[igme.ests.nmr$country %in% subdat$country,]
  
  g <- subdat %>% ggplot(aes(x=year)) + 
    # estimates for each region
    geom_line(aes(y=median,group=admin1.char,color=admin1.char)) + 
    # national estimate
    geom_line(data=subdat_nat, aes(x=year, y = median)) +
    # indicates where prediction starts
    geom_vline(aes(xintercept=last_year),lty=2,col='grey50') +
    # indicates SDG goal
    geom_hline(yintercept=12/1000,col='limegreen',alpha=0.5,lwd=0.75, lty=2) +
    facet_wrap(~country) + theme(legend.position = 'none', axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle('Admin 1 level Subnational NMR in countries outside of Sub-Saharan Africa')+ ylab('Median NMR Estimate')
  
  print(g)

# Sub-Saharan Africa  
  subdat <- adm2_nmr_results[adm2_nmr_results$sub.region=='Sub-Saharan Africa',]
  subdat_nat <- igme.ests.nmr[igme.ests.nmr$country %in% subdat$country,]
  
  g <- subdat %>% ggplot(aes(x=year)) + 
    # estimates for each region
    geom_line(aes(y=median,group=admin2.char,color=admin2.char)) + 
    # national estimate
    geom_line(data=subdat_nat, aes(x=year, y = median)) +
    # indicates where prediction starts
    geom_vline(aes(xintercept=last_year),lty=2,col='grey50') +
    # indicates SDG goal
    geom_hline(yintercept=12/1000,col='limegreen',alpha=0.5,lwd=0.75, lty=2) +
    ylim(c(0.005,0.15)) +
    facet_wrap(~country) + theme(legend.position = 'none',axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle('Admin 2 level Subnational NMR in Sub-Saharan African Countries') + ylab('Median NMR Estimate')
  
  print(g)
  
  # Other areas
  subdat <- adm2_nmr_results[adm2_nmr_results$sub.region!='Sub-Saharan Africa',]
  subdat_nat <- igme.ests.nmr[igme.ests.nmr$country %in% subdat$country,]
  
  g <- subdat %>% ggplot(aes(x=year)) + 
    # estimates for each region
    geom_line(aes(y=median,group=admin2.char,color=admin2.char)) + 
    # national estimate
    geom_line(data=subdat_nat, aes(x=year, y = median)) +
    # indicates where prediction starts
    geom_vline(aes(xintercept=last_year),lty=2,col='grey50') +
    # indicates SDG goal
    geom_hline(yintercept=12/1000,col='limegreen',alpha=0.5,lwd=0.75, lty=2) +
    facet_wrap(~country) + theme(legend.position = 'none', axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle('Admin 2 level Subnational NMR in countries outside of Sub-Saharan Africa')+ ylab('Median NMR Estimate')
  
  print(g)
}
dev.off()

pdf("/Users/alanamcgovern/Desktop/Research/UN_Estimates/Country Comparison/Figures/NMR Subnational Estimates by Area.pdf")
{
  g <- adm1_nmr_results %>% ggplot(aes(x=year)) + 
    # estimates for each region
    geom_line(aes(y=median,group=admin1.char,color=admin1.char)) + 
    # national estimate
    geom_line(data=igme.ests.nmr[igme.ests.nmr$country %in% unique(adm1_nmr_results$country),], aes(x=year, y = median)) +
    # indicates where prediction starts
    geom_vline(aes(xintercept=last_year),lty=2,col='grey50') +
    # indicates SDG goal
    geom_hline(yintercept=12/1000,col='limegreen',alpha=0.5,lwd=0.75, lty=2) +
    ylim(c(0.005,0.09)) +
    facet_wrap(~country) + theme(legend.position = 'none',axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle('Admin 1 level Subnational NMR') + ylab('Median NMR Estimate')
  
  print(g)
  
  g <- adm2_nmr_results %>% ggplot(aes(x=year)) + 
    # estimates for each region
    geom_line(aes(y=median,group=admin2.char,color=admin2.char)) + 
    # national estimate
    geom_line(data=igme.ests.nmr[igme.ests.nmr$country %in% unique(adm2_nmr_results$country),], aes(x=year, y = median)) +
    # indicates where prediction starts
    geom_vline(aes(xintercept=last_year),lty=2,col='grey50') +
    # indicates SDG goal
    geom_hline(yintercept=12/1000,col='limegreen',alpha=0.5,lwd=0.75, lty=2) +
    ylim(c(0.005,0.15)) +
    facet_wrap(~country) + theme(legend.position = 'none',axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle('Admin 2 level Subnational NMR ') + ylab('Median NMR Estimate')
  
  print(g)
  
}
dev.off()

# Plots examining stratified national NMR -----
pdf("/Users/alanamcgovern/Desktop/Research/UN_Estimates/Country Comparison/Figures/NMR Stratified National Estimates.pdf")
{
  g <- natl_strat_nmr_results %>% ggplot(aes(x=years.num,y=median)) + geom_line(aes(group=strata,color=strata)) +
    # indicates where prediction starts
    geom_vline(aes(xintercept=last_year),lty=2,col='grey50') +
    # indicates SDG goal
    geom_hline(yintercept=12/1000,col='limegreen',alpha=0.5,lwd=0.75, lty=2) + xlab('Year') +
    facet_wrap(~country) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(g)
}
dev.off()

# Map plots trying to capture discrepancy between areas ------

pdf("/Users/alanamcgovern/Desktop/Research/UN_Estimates/Country Comparison/Figures/Comparing Admin 1 NMR Estimates to National.pdf")
for(country_t in unique(adm1_nmr_results$country)){
  adm1poly_subdat <- adm1poly_dat %>% filter(country==country_t)
  map_dat <- right_join(adm1_nmr_results %>% filter(country == country_t,year %in% c(2005,2010,2015,2021)) %>% rename(id=admin1.name), adm1poly_subdat, by = 'id',relationship = 'many-to-many')
  
  #settings for all plots
  g_map <- ggplot() + geom_polygon(data = map_dat, aes(x = long, y = lat, group = group, fill = ratio_to_natl), color = 'grey60') +
    scale_fill_gradient2(name='Ratio of Subnational\n to National NMR', trans = 'log10',
                         low = 'darkblue',mid = 'white',high = 'red', midpoint = 0) + facet_grid(~year) +
    coord_map() + ggplot2::theme_bw() + ggplot2::theme(legend.title = ggplot2::element_text(size = ggplot2::rel(0.7)), 
                                                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(), 
                                                       axis.ticks.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(), 
                                                       axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(), 
                                                       panel.grid.major = ggplot2::element_blank(), 
                                                       panel.grid.minor = ggplot2::element_blank()) +
    theme(legend.position = 'left')
  
  # Adds area labels
  # geo2 <- by(geo, geo$id, function(x) {sp::Polygon(x[c("long", "lat")])@labpt})
  # centroids <- stats::setNames(do.call("rbind.data.frame", geo2), c("long", "lat"))
  # centroids$name <- names(geo2)
  # g <- g + shadowtext::geom_shadowtext(data = centroids, ggplot2::aes(x = long, y = lat, label = name), check.overlap = TRUE, 
  #                                      size = 2, color = 'black', bg.colour = "white")
  
  
  
  adm1_nmr_results$admin1 <- as.numeric(str_remove(adm1_nmr_results$admin1.char,'admin1_'))
  blue_to_red <- colorRampPalette(c('darkblue','white','red'))(max(adm1_nmr_results[adm1_nmr_results$country==country_t,]$admin1))
  subdat_nat <- igme.ests.nmr[igme.ests.nmr$country == country_t,]
  g <- adm1_nmr_results %>% filter(country==country_t) %>% ggplot(aes(x=year)) + 
    # estimates for each region
    geom_line(aes(y=median,group=admin1.char,col=ratio_to_natl)) + 
    # national estimate
    geom_line(data=subdat_nat, aes(x=year, y = median)) +
    # indicates where prediction starts
    geom_vline(aes(xintercept=last_year),lty=2,col='grey50') +
    # indicates SDG goal
    geom_hline(yintercept=12/1000,col='limegreen',alpha=0.5,lwd=0.75, lty=2) +
    scale_color_gradient2(name='Ratio of Subnational\n to National NMR', 
                         low = 'darkblue',mid = 'grey80',high = 'red', midpoint =1) +
    theme(legend.position = 'none',axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle(paste0(country_t,' Admin 1 level Subnational NMR')) + ylab('Median NMR Estimate') + xlab('Year')
  
 maps.grob <- as.grob(g_map)
 lines.grob <- as.grob(g)
 grid.arrange(lines.grob,maps.grob)
 
}
dev.off()

pdf("/Users/alanamcgovern/Desktop/Research/UN_Estimates/Country Comparison/Figures/Comparing Admin 2 NMR Estimates to National.pdf")
for(country_t in unique(adm2_nmr_results$country)){
  adm2poly_subdat <- adm2poly_dat %>% filter(country.x==country_t)
  
  #settings for all plots
  g1 <- g2 <- g3 <- g4 <- ggplot() + coord_map() + ggplot2::theme_bw() + ggplot2::theme(legend.title = ggplot2::element_text(size = ggplot2::rel(0.7)), 
                                                                                        axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(), 
                                                                                        axis.ticks.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(), 
                                                                                        axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(), 
                                                                                        panel.grid.major = ggplot2::element_blank(), 
                                                                                        panel.grid.minor = ggplot2::element_blank())
  
  map_dat <- right_join(adm2_nmr_results %>% filter(country == country_t & year == 2005,), adm2poly_subdat %>% rename(admin2.fullname=admin2.name), by = 'admin2.fullname')
  g1 <- g1 + geom_polygon(data = map_dat, aes(x = long, y = lat, group = group, fill = ratio_to_natl), color = 'grey60') +
    scale_fill_gradient2(name='Ratio of Subnational to National NMR', trans = 'log10',
                         low = 'darkblue',mid = 'white',high = 'red', midpoint = 0) + ggtitle(paste0(country_t,', 2005'))
  
  map_dat <- right_join(adm2_nmr_results %>% filter(country == country_t & year == 2010,), adm2poly_subdat %>% rename(admin2.fullname=admin2.name), by = 'admin2.fullname')
  g2 <- g2 + geom_polygon(data = map_dat, aes(x = long, y = lat, group = group, fill = ratio_to_natl), color = 'grey60') +
    scale_fill_gradient2(name='Ratio of Subnational to National NMR', trans = 'log10',
                         low = 'darkblue',mid = 'white',high = 'red', midpoint = 0) + ggtitle(paste0(country_t,', 2010'))
  
  map_dat <- right_join(adm2_nmr_results %>% filter(country == country_t & year == 2015,), adm2poly_subdat %>% rename(admin2.fullname=admin2.name), by = 'admin2.fullname')
  g3 <- g3 + geom_polygon(data = map_dat, aes(x = long, y = lat, group = group, fill = ratio_to_natl), color = 'grey60') +
    scale_fill_gradient2(name='Ratio of Subnational to National NMR', trans = 'log10',
                         low = 'darkblue',mid = 'white',high = 'red', midpoint = 0) + ggtitle(paste0(country_t,', 2015'))
  
  map_dat <- right_join(adm2_nmr_results %>% filter(country == country_t & year == 2021,), adm2poly_subdat %>% rename(admin2.fullname=admin2.name), by = 'admin2.fullname')
  g4 <- g4 + geom_polygon(data = map_dat, aes(x = long, y = lat, group = group, fill = ratio_to_natl), color = 'grey60') +
    scale_fill_gradient2(name='Ratio of Subnational to National NMR', trans = 'log10',
                         low = 'darkblue',mid = 'white',high = 'red', midpoint = 0) + ggtitle(paste0(country_t,', 2021'))
  
  # Adds area labels
  # geo2 <- by(geo, geo$id, function(x) {sp::Polygon(x[c("long", "lat")])@labpt})
  # centroids <- stats::setNames(do.call("rbind.data.frame", geo2), c("long", "lat"))
  # centroids$name <- names(geo2)
  # g <- g + shadowtext::geom_shadowtext(data = centroids, ggplot2::aes(x = long, y = lat, label = name), check.overlap = TRUE, 
  #                                      size = 2, color = 'black', bg.colour = "white")
  print(ggarrange(g1,g2,g3,g4,ncol=2,nrow=2,common.legend = T))
  
}
dev.off()

# Rank probability plots ----------
pdf("/Users/alanamcgovern/Desktop/Research/UN_Estimates/Country Comparison/Figures/Admin 1 NMR Subnational Rank Probability.pdf")
for(country_t in unique(adm1_nmr_ranks$country)){
  adm1_nmr_ranks_t <- adm1_nmr_ranks %>% filter(country==country_t)
  n_admin <- length(unique(adm1_nmr_ranks_t$admin1.name))
  max_prob <- round(max(adm1_nmr_ranks_t$prob),2) + 0.01
  plot1 = adm1_nmr_ranks_t %>%
    ggplot(aes(year, factor(rank,levels = rev(1:n_admin))))+
    geom_tile(aes(fill=prob))+
    facet_wrap(~admin1.name)+
    ylab("Ranking")+ xlab(" ")+ ggtitle(paste0('Rank Probability of Admin 1 NMR in ',country_t))+
    ggthemes::theme_few() +
    scale_fill_gradient2(
      low = "white", high = 'darkblue',
      limits = c(0, max_prob),
      breaks = c(0, max_prob),
      labels = c("Low probability", "High probability"),
      guide = "colourbar",
      name = NULL,
    ) +
    guides(fill = guide_colourbar(title = "Ranking Probability", title.hjust = 0.5, title.position = 'top', barwidth = unit(15, 'lines'), ticks = FALSE)) +
    theme(legend.position = 'bottom',
                        panel.spacing.x = unit(1, 'lines'),
                        legend.title = element_text( size=6), 
                        legend.text=element_text(size=6),
                        plot.margin = unit(c(1, 1, 1, 1), 'lines'),
                        axis.text.y = element_text(size = 6),
                        axis.text.x = element_text(angle = 90,vjust=0.5, hjust = 1,size=6),
                        strip.text = element_text(size = 8),
                        legend.key.size = unit(0.25, "cm"),
                        axis.title.y = element_text(size = 8),
                        legend.margin=margin(0,0,0,0),
                        legend.box.margin=margin(-12,-12,-12,-12))
  if(n_admin>20){
    plot1 <- plot1 + scale_y_discrete(breaks=seq(1,n_admin,5)) }
  if(country_t %in% c('Ethiopia','Kenya','Nigeria')){
    plot1 <- plot1 + theme(strip.text = element_text(size=6))
  }
  print(plot1)
}
dev.off()
