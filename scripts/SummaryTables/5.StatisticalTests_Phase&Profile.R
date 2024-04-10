### CODE to apply statistical tests comparisons (Mann-Whitney U test ) for TBF and ODBA (top vs Below, Descents vs Ascents)

# Set timezone to GMT
Sys.setenv(TZ='GMT')

## PACKAGES 
pack <- c('data.table','pastecs', 'dplyr')
# lapply('pals', install.packages)
lapply(pack, require, character.only = TRUE)

## OPTIONS 
# path folderS
folder<-'D:/_Papers/O2RecordingTag/'
fileDir<- paste0(folder,'/files/')
tableDir<- paste0(folder,'/tables/')

# files generated from IGOR analysis with acceleration-derived metrics - 3 sharks: blue2 (S1), blue4 (S3), blue5 (S4)
files<-list.files(folder, pattern= '-accdata2.csv', recursive = TRUE)	

# depth threshold used
depth_threshold<- 85 # for comparison

# list to save metrics
statList<- list()

for(i in 1:length(files)){
  
  #i=1
  # get shark id
  sharkID<- gsub('-accdata2.csv','', basename(files[i]))
  
  # read file
  envdata<- read.csv(paste0(folder,files[i]), header = TRUE)
  
  # format file
  colnames(envdata)<- c('time', 'depth', 'temperature', 'oxygen', 'TBF', 'pitch1Hz', 'roll1Hz', 'odba1Hz', 'glides')
  
  # remove negative depths for blue 4
  if(sharkID == 'blue4'){ envdata$depth[envdata$depth <0]<- 0}
  
  # time in correct format
  envdata$time<- as.POSIXct(envdata$time)
  
  ## DEFINE DIVING PHASES
  
  # first smoothed the trace using a 10 s running mean and calculate vertical velocity
  envdata$depth_avg<- frollsum(envdata$depth, n = 60, align = 'center')
  envdata$vvelocity <- c(NA, diff(envdata$depth_avg)/ unique(diff(envdata$time)))
  
  # identify diving phases: ascend, descend and level
  dat<- envdata[,c('time','depth', 'vvelocity','depth_avg')]
  dat$vvelocity_abs<- abs(dat$vvelocity)
  
  # define descend time 
  run_d <- rle(dat$vvelocity > 1)
  dat$run_d <- rep(run_d$lengths, run_d$lengths)
  dat$cond_d <- rep(run_d$values, run_d$lengths)
  dat$descend<-  ifelse(dat$run_d>1 & dat$cond_d == TRUE, 'D', NA)
  
  # define ascent time 
  run_a <- rle(dat$vvelocity < -1)
  dat$run_a <- rep(run_a$lengths, run_a$lengths)
  dat$cond_a <- rep(run_a$values, run_a$lengths)
  dat$ascend<-  ifelse(dat$run_a>1 & dat$cond_a == TRUE, 'A', NA)
  
  # define level time
  run_l <- rle(dat$vvelocity_abs < 1)
  dat$run_l <- rep(run_l$lengths, run_l$lengths)
  dat$cond_l <- rep(run_l$values, run_l$lengths)
  dat$level<-  ifelse(dat$run_l>1 & dat$cond_l == TRUE, 'L', NA)
  
  # coalesce phases into one column
  dat$phase<-  coalesce(dat$descend, dat$ascend, dat$level)
  
  # save in table
  envdata$phase<- dat$phase
  
  # save also id
  envdata$id<- sharkID
  
  # DEFINE EACH INDIVIDUAL DIVE PHASE (1s descent, 2nd descent,..)
  seq_size<- c(0,abs(diff(sequence(rle(as.character(envdata$phase))$lengths))))
  lst <- split(envdata, cumsum(as.numeric(seq_size > 1)))

  # interaction for correct names (always run this before loop)
  D_id<-1 
  L_id<- 1
  A_id<- 1
  
  
  # lst to save each individual dive phase
  diveList<- list()
  
  # loop through all ind dive phases and assign correct name
  for(i in 1:length(lst)){
    
    # get only ind dive phase 
    ind_phase<- as.data.frame(lst[i])
    colnames(ind_phase)<- c('time', 'depth', 'temperature', 'oxygen', 'TBF', 'pitch1Hz', 'roll1Hz', 'odba1Hz', 'glides', 'depth_avg','vvelocity', 'phase','id')
    
    # remove phases with 2 sec/NA
    dive_phase<- ind_phase$phase[nrow(ind_phase)]
    ind_phase<- ind_phase[ind_phase$phase == dive_phase,]
    ind_phase<- ind_phase[!is.na(ind_phase$phase),]
    
    # iteration to assign the next name correctly
    if(!is.na(dive_phase) & dive_phase == 'D'){
      dive_name<- paste0(dive_phase,D_id)
      D_id<- D_id + 1}
    
    if(!is.na(dive_phase) & dive_phase == 'A'){
      dive_name<- paste0(dive_phase,A_id)
      A_id<- A_id + 1}   
    
    if(!is.na(dive_phase) & dive_phase == 'L'){
      dive_name<- paste0(dive_phase,L_id)
      L_id<- L_id + 1} 
    
    if(is.na(dive_phase)){
      dive_name<- NA}  
    
    # last part of the track 
    if(i == length(lst) & nrow(ind_phase) == 0){
      
      print('End of track')
      
      next
      
    }  
    
    
    print(dive_name)
    
    # define recovery time
    if(sharkID == 'blue2'){ recovery_hour<-envdata$time[1] + (2.16*60*60) } else{ recovery_hour<-envdata$time[1] + (5.08*60*60)}
    
    
    
    # name and save 
    ind_phase$phase_id<- as.character(dive_name)
    ind_phase<- ind_phase[,c('time','phase_id')]
    diveList[[i]]<- ind_phase
    
  }
  
  # rbind all data again
  dive_tbl<- do.call(rbind,diveList)
  
  # merge with raw data
  envdata<- merge(envdata, dive_tbl, by = 'time', all.x = TRUE)
  
  # calculate TBF and ODBA 95th percentiles (burst swimming definition)
  tbf_95<- quantile(envdata$TBF, 0.95, na.rm =TRUE )
  odba_95<- quantile(envdata$odba1Hz, 0.95, na.rm =TRUE )
  envdata$burst<-  ifelse(envdata$odba1Hz >=odba_95 & envdata$TBF >=tbf_95 , 1,0)
  envdata$burst[is.na(envdata$burst)]<- 0
  
  # save on list
  statList[[i]]<- envdata 
  
  print(sharkID)
  
}


# merge data
stat_metrics<- do.call(rbind,statList)

# remove lines with depth above 1 meter
stat_metrics<- stat_metrics[stat_metrics$depth>1,]

# dataframe for top vs below
stat_metrics$depth_limit<- ifelse(stat_metrics$depth<= depth_threshold, 'top', 'belw')

# another dataframe for descent/ascents
stat_metrics_phase<- stat_metrics[stat_metrics$phase == 'D'|stat_metrics$phase == 'A',]
stat_metrics_phase<- stat_metrics_phase[complete.cases(stat_metrics_phase$phase),]



## STATISTICAL TESTS 


# ASCENTS VS DESCENTS - tail-beat and ODBA
# aggregate TBF and ODBA data for individual dive phases
mean_tbl<- aggregate(cbind( TBF, odba1Hz) ~  id + phase_id, data = stat_metrics_phase, mean, na.rm =TRUE)

# get dive phase
mean_tbl$phase<- gsub('[0-9]+','',mean_tbl$phase_id)
unique(mean_tbl$phase)


# overall report TBF (no groupping)
m3<-wilcox.test(TBF ~ phase , data=mean_tbl, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(m3)
length(mean_tbl$TBF[mean_tbl$phase =='D']) + length(mean_tbl$TBF[mean_tbl$phase =='A'])
boxplot(stat_metrics_phase$TBF[stat_metrics_phase$phase == 'D'],stat_metrics_phase$TBF[stat_metrics_phase$phase == 'A'], names=c("Des","Asc"), ylim = c(0,1))

# report ODBA (no groupping)
m4<-wilcox.test(odba1Hz ~ phase, data=mean_tbl, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(m4)
length(mean_tbl$odba1Hz[mean_tbl$phase =='D']) + length(mean_tbl$odba1Hz[mean_tbl$phase =='A'])
boxplot(stat_metrics_phase$odba1Hz[stat_metrics_phase$phase == 'D'],stat_metrics_phase$odba1Hz[stat_metrics_phase$phase == 'A'], names=c("Des","Asc"),ylim = c(0,0.1))


# now, select and run individually for each shark id
mean_tbl_id<- mean_tbl[mean_tbl$id == 'blue4',]

# individually (TBF)
m5<-wilcox.test(TBF ~ phase , data=mean_tbl_id, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(m5)
length(mean_tbl_id$TBF[mean_tbl_id$phase =='D']) + length(mean_tbl_id$TBF[mean_tbl_id$phase =='A'])
boxplot(mean_tbl_id$TBF[mean_tbl_id$phase == 'D'],mean_tbl_id$TBF[mean_tbl_id$phase == 'A'], names=c("Des","Asc"), ylim = c(0,1))

# individually (ODBA)
m6<-wilcox.test(odba1Hz ~ phase, data=mean_tbl_id, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(m6)
length(mean_tbl_id$odba1Hz[mean_tbl_id$phase =='D']) + length(mean_tbl_id$odba1Hz[mean_tbl_id$phase =='A'])
boxplot(mean_tbl_id$odba1Hz[mean_tbl_id$phase == 'D'],mean_tbl_id$odba1Hz[mean_tbl_id$phase == 'A'], names=c("Des","Asc"),ylim = c(0,0.1))



# TOP VS BELOW 50 METERS - burst swimming

# get tracking day
stat_metrics$day<-  as.Date(stat_metrics$time)

# aggregate by id, day and depth threshold (top and below)
burst_agg<- aggregate(burst ~  id + day + depth_limit, data = stat_metrics, sum, na.rm =TRUE)

# tranform percentage to minutes
burst_agg$burst<- burst_agg$burst/60
burst_agg<- burst_agg[-7,]
row.names(burst_agg)<- 1:10

# time burst swimming on top vs below
m7<-wilcox.test(burst ~ depth_limit, data=burst_agg, paired=FALSE)
wilcox.test(burst ~ depth_limit, data=burst_agg, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)





