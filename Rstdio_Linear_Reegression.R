fip_flar_data<-read.csv("C:/Users/prasa/Documents/Data_Science_project/COUNTY_ANNUAL_EXPOSURE.csv",sep=',',header=TRUE)
sorted_fips_well<-fip_flar_data[order(fip_flar_data$year),]
#2005-2015
corn_10_yr<-read.csv("C:/Users/prasa/Documents/Data_Science_project/corn 2005-2015 nd.csv",sep=',',header=TRUE)
#Sort 
corn_10_yr_sorted<-corn_10_yr[order(corn_10_yr$year),]
names(sorted_fips_well)[names(sorted_fips_well) == 'fips_code'] <- "fips"

ten_years_corn_flare<- merge(sorted_fips_well, corn_10_yr_sorted, by=c("fips","year"))

precip_allyr<-read.csv("C:/Users/prasa/Documents/Data_Science_project/precip2015-2005.csv",sep=',',header=TRUE)
sorted_precip_allyr<-precip_allyr[order(precip_allyr$year),]
names(sorted_precip_allyr)[names(sorted_precip_allyr) == 'Value'] <- "precipitation"

lr_model_lot_obs=lm(yield ~ county_flare_exposure+county_oil_exposure, data=ten_years_corn_flare)

summary(lr_model_lot_obs)



ten_years_corn_flare_precep<- merge(ten_years_corn_flare, sorted_precip_allyr, by=c("fips","year"))

lr_model_lot_obs_well=lm(yield ~ county_flare_exposure+county_oil_exposure+county_well_exposure, data=ten_years_corn_flare)
summary(lr_model_lot_obs_well)


lr_model_lot_obs_well_precep=lm(yield ~ county_flare_exposure+county_oil_exposure+county_well_exposure+precipitation, data=ten_years_corn_flare_precep)
summary(lr_model_lot_obs_well_precep)

maxtemp_allyr<-read.csv("C:/Users/prasa/Documents/Data_Science_project/max_temp-05-15.csv",sep=',',header=TRUE)
sorted_maxtemp_allyr<-maxtemp_allyr[order(maxtemp_allyr$year),]
names(sorted_maxtemp_allyr)[names(sorted_maxtemp_allyr) == 'Value'] <- "max_Temp"

ten_years_corn_flare_precep_maxtemp<- merge(ten_years_corn_flare_precep, sorted_maxtemp_allyr, by=c("fips","year"))

lr_model_lot_obs_well_precep_mxtemp=lm(yield ~ county_flare_exposure+county_oil_exposure+county_well_exposure+precipitation+max_Temp, data=ten_years_corn_flare_precep_maxtemp)
summary(lr_model_lot_obs_well_precep_mxtemp)

