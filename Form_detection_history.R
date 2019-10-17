fulldata = read.csv("CleanEvents.csv") # read data here
dates = as.Date( fulldata$Date) # date can have different name
years = format(dates,"%Y") # need the year
posi  = as.POSIXlt(dates) # date
j_day = as.POSIXlt(dates)$yday # Julian day of the year
years = as.numeric(years) # year, again
accu_day = (years-min(years,na.rm = F))*365 + j_day # accumulated day begin from the first Janurary first
accu_day = accu_day-min(accu_day)+1 # let it start from 1
fulldata = data.frame(fulldata,accu_day) 
write.csv(fulldata,"CleanEvents_accuday.csv",row.names = F) # write the data in disc with accumulated day

fulldata = read.csv("CleanEvents_accuday.csv") 
unique_sites = read.csv("unique_sites.csv") # list of site names 
unique_sites = as.character(unique_sites$sites)
spp_list = c("Coyote","Fox_red","Bobcat","Fisher","Marten") # spp list

dethis_sum = data.frame(site = unique_sites,day_min = NA,day_max=NA,Coyote = NA,Fox_red = NA,Bobcat=NA)
row.names(dethis_sum) = unique_sites

for(i in unique_sites){
  data_site = fulldata[fulldata$Location==i,]
  dethis_sum[i,"day_min"]=min(data_site$accu_day)
  dethis_sum[i,"day_max"]=max(data_site$accu_day)
  for(j in spp_list){
    dethis_sum[i,j] = sum(data_site$Species==j)
  }
}

write.csv(dethis_sum,"detection_num_uniquesites.csv",row.names = F) # form a summary chart for certain species


Event = read.csv("CleanEvents_accuday.csv") # events with accumulated day (can be julian day for a certain year, then years should have different files)
Event$Species=as.character(Event$Species)# can be named differently 
Event$Location = as.character(Event$Location) # this should be a subset of site list
num = read.csv("detection_num_uniquesites_fixed.csv") # read the summary, can be prepared when setting the trap grid 
num$site = as.character(num$site) # site name again, should be a subset of site list
min_day = 1 # start from 1
max_day = max(num$day_max,na.rm = T)

spp_list = c("Coyote","Fox_red","Bobcat")
spp_list = c("Fisher","Marten")

occa_length = 15 # how many days as a period

num_occa_year = floor(max_day/occa_length)

# it will take some time but need to be only once thus for is fine
for(spp in spp_list){
	dethist = matrix(-1,nrow(num),num_occa_year) # for ram
	rownames(dethist) = as.character(num[,1]) # site's name as row name
	for(l in 1:nrow(num)){ # number of sites
	  if( is.na( num$day_min[l])) { # if no any detection,assumed to be not survied
	    dethist[l,]=NA
	    next
	  }
	  temp = Event[Event$Species==spp & Event$Location==num$site[l],]
	  yday_low = (1:num_occa_year-1)*occa_length
	  for(j in 1:num_occa_year){ # loop through time period
	    low = yday_low[j] # lower bound
	    high = low + occa_length # higher bound
	    
	    if(low > num$day_max[l] | high<num$day_min[l]){ # no survey at certain time period
	      dethist[l,j]=NA
	      next
	    }
	    event_temp = temp[(temp$accu_day<=high & temp$accu_day>low),]
	    dethist[ rownames(dethist) %in% event_temp$Location,j]=1 # detections 
	  }
	}
	write.csv(dethist,paste0(spp,occa_length,"dayfull.csv")) # detection history for the species
}
