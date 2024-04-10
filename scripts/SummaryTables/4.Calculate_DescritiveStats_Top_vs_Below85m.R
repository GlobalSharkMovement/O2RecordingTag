## CODE to calculate descriptive statistics of swimming metrics for high DO (top 85m) vs below

# Set timezone to GMT
Sys.setenv(TZ='GMT')

## PACKAGES 
pack <- c('data.table','pastecs', 'dplyr')
# lapply('pals', install.packages)
lapply(pack, require, character.only = TRUE)

## OPTIONS 
# path folders
folder<-'D:/_Papers/O2RecordingTag/'
fileDir<- paste0(folder,'/files/')
tableDir<- paste0(folder,'/tables/')

# files generated from IGOR analysis with acceleration-derived metrics - 3 sharks: blue2 (S1), blue4 (S3), blue5 (S4)
files<-list.files(folder, pattern= '-accdata2.csv', recursive = TRUE)	

# depth threshold (higher DO region - 85 meters)
depth_threshold<- 85

# list to save metrics
profile_overall<- list()
profile_descent<- list()
profile_ascent<- list()

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
  
  # define ascend time
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
  
  #check dive phases on time series
  #ggplot(envdata,aes(x=time,y=-depth,col=phase)) + geom_point()+  scale_colour_manual(name = 'Dive Phase',values = c("#D7A31A", "#002458","#5E81C2")) 
  
  # calculate TBF and ODBA 95th percentiles (burst swimming definition)
  tbf_95<- quantile(envdata$TBF, 0.95, na.rm =TRUE )
  odba_95<- quantile(envdata$odba1Hz, 0.95, na.rm =TRUE )
  envdata$burst<-  ifelse(envdata$odba1Hz >=odba_95 & envdata$TBF >=tbf_95 , 1,0)
  envdata$burst[is.na(envdata$burst)]<- 0
  
  
  ## SAVE STATISTICS ABOVE AND BELOW A THRESHOLD 
  
  profile_overall[[i]]<- data.frame(sharkID, 
                                 
                                 
                               
                                 ## TOP 85 M ##
                                 
                                 #depth
                                 mean_depth_50 = mean(envdata$depth[envdata$depth<=depth_threshold], na.rm = TRUE),
                                 sd_depth_50 = sd(envdata$depth[envdata$depth<=depth_threshold], na.rm = TRUE),
                                 min_depth_50 =  min(envdata$depth[envdata$depth<=depth_threshold], na.rm =TRUE),
                                 max_depth_50 =  max(envdata$depth[envdata$depth<=depth_threshold], na.rm =TRUE),
                                 
                                 #temperature
                                 mean_temp_50 = mean(envdata$temperature[envdata$depth<=depth_threshold], na.rm = TRUE),
                                 sd_temp_50  = sd(envdata$temperature[envdata$depth<=depth_threshold], na.rm = TRUE),
                                 min_temp_50 =  min(envdata$temperature[envdata$depth<=depth_threshold], na.rm =TRUE),
                                 max_temp_50 =  max(envdata$temperature[envdata$depth<=depth_threshold], na.rm =TRUE),
                                 rate_temp_50 = (max(envdata$temperature[envdata$depth<=depth_threshold], na.rm=TRUE) - min(envdata$temperature[envdata$depth<=depth_threshold], na.rm=TRUE))/depth_threshold,

                                 #oxygen
                                 mean_DO_50 = mean(envdata$oxygen[envdata$depth<=depth_threshold], na.rm = TRUE),
                                 sd_DO_50  = sd(envdata$oxygen[envdata$depth<=depth_threshold], na.rm = TRUE),
                                 min_DO_50 =  min(envdata$oxygen[envdata$depth<=depth_threshold], na.rm =TRUE),
                                 max_DO_50 =  max(envdata$oxygen[envdata$depth<=depth_threshold], na.rm =TRUE),
                                 rate_DO_50 = (max(envdata$oxygen[envdata$depth<=depth_threshold], na.rm=TRUE) - min(envdata$oxygen[envdata$depth<=depth_threshold], na.rm=TRUE))/depth_threshold,

                                 #tailbeat
                                 mean_tbf_50 = mean(envdata$TBF[envdata$depth<=depth_threshold], na.rm = TRUE),
                                 sd_tbf_50  = sd(envdata$TBF[envdata$depth<=depth_threshold], na.rm = TRUE),
                                 min_tbf_50 =  min(envdata$TBF[envdata$depth<=depth_threshold], na.rm =TRUE),
                                 max_tbf_50 =  max(envdata$TBF[envdata$depth<=depth_threshold], na.rm =TRUE),
    
                                 #pitch
                                 mean_p_50 = mean(envdata$pitch1Hz[envdata$depth<=depth_threshold], na.rm = TRUE),
                                 sd_p_50  = sd(envdata$pitch1Hz[envdata$depth<=depth_threshold], na.rm = TRUE),
                                 min_p_50 =  min(envdata$pitch1Hz[envdata$depth<=depth_threshold], na.rm =TRUE),
                                 max_p_50 =  max(envdata$pitch1Hz[envdata$depth<=depth_threshold], na.rm =TRUE),

                                 #roll
                                 mean_r_50 = mean(envdata$roll1Hz[envdata$depth<=depth_threshold], na.rm = TRUE),
                                 sd_r_50  = sd(envdata$roll1Hz[envdata$depth<=depth_threshold], na.rm = TRUE),
                                 min_r_50 =  min(envdata$roll1Hz[envdata$depth<=depth_threshold], na.rm =TRUE),
                                 max_r_50 =  max(envdata$roll1Hz[envdata$depth<=depth_threshold], na.rm =TRUE),

                                 #odba
                                 mean_odba_50 = mean(envdata$odba1Hz[envdata$depth<=depth_threshold], na.rm = TRUE),
                                 sd_odba_50  = sd(envdata$odba1Hz[envdata$depth<=depth_threshold], na.rm = TRUE),
                                 min_odba_50 =  min(envdata$odba1Hz[envdata$depth<=depth_threshold], na.rm =TRUE),
                                 max_odba_50 =  max(envdata$odba1Hz[envdata$depth<=depth_threshold], na.rm =TRUE),
                                 
                                 #glides & bursts
                                 sum_glides_50 = sum(envdata$glides[envdata$depth<=depth_threshold],na.rm=TRUE),
                                 time_50 = nrow(envdata[envdata$depth<=depth_threshold,]),
                                 burst_50 = sum(envdata$burst[envdata$depth<=depth_threshold],na.rm=TRUE),
                                            
                                 
                                 ## BELOW 85 M ##
                                 
                                 #depth
                                 mean_depth_100 = mean(envdata$depth[envdata$depth>depth_threshold], na.rm = TRUE),
                                 sd_depth_100 = sd(envdata$depth[envdata$depth>depth_threshold], na.rm = TRUE),
                                 min_depth_100 =  min(envdata$depth[envdata$depth>depth_threshold], na.rm =TRUE),
                                 max_depth_100 =  max(envdata$depth[envdata$depth>depth_threshold], na.rm =TRUE),
                                 
                                 #temperature
                                 mean_temp_100 = mean(envdata$temperature[envdata$depth>depth_threshold], na.rm = TRUE),
                                 sd_temp_100  = sd(envdata$temperature[envdata$depth>depth_threshold], na.rm = TRUE),
                                 min_temp_100 =  min(envdata$temperature[envdata$depth>depth_threshold], na.rm =TRUE),
                                 max_temp_100 =  max(envdata$temperature[envdata$depth>depth_threshold], na.rm =TRUE),
                                 rate_temp_100 = (min(envdata$temperature[envdata$depth>depth_threshold], na.rm=TRUE) - max(envdata$temperature[envdata$depth>depth_threshold], na.rm=TRUE))/(max(envdata$depth)-depth_threshold),
                                 
                                 #oxygen
                                 mean_DO_100 = mean(envdata$oxygen[envdata$depth>depth_threshold], na.rm = TRUE),
                                 sd_DO_100  = sd(envdata$oxygen[envdata$depth>depth_threshold], na.rm = TRUE),
                                 min_DO_100 =  min(envdata$oxygen[envdata$depth>depth_threshold], na.rm =TRUE),
                                 max_DO_100 =  max(envdata$oxygen[envdata$depth>depth_threshold], na.rm =TRUE),
                                 rate_DO_100 = (min(envdata$oxygen[envdata$depth>depth_threshold], na.rm=TRUE) - max(envdata$oxygen[envdata$depth>depth_threshold], na.rm=TRUE))/(max(envdata$depth)-depth_threshold),
                                 
                                 #tailbeat
                                 mean_tbf_100 = mean(envdata$TBF[envdata$depth>depth_threshold], na.rm = TRUE),
                                 sd_tbf_100  = sd(envdata$TBF[envdata$depth>depth_threshold], na.rm = TRUE),
                                 min_tbf_100 =  min(envdata$TBF[envdata$depth>depth_threshold], na.rm =TRUE),
                                 max_tbf_100 =  max(envdata$TBF[envdata$depth>depth_threshold], na.rm =TRUE),
                                 
                                 #pitch
                                 mean_p_100 = mean(envdata$pitch1Hz[envdata$depth>depth_threshold], na.rm = TRUE),
                                 sd_p_100  = sd(envdata$pitch1Hz[envdata$depth>depth_threshold], na.rm = TRUE),
                                 min_p_100 =  min(envdata$pitch1Hz[envdata$depth>depth_threshold], na.rm =TRUE),
                                 max_p_100 =  max(envdata$pitch1Hz[envdata$depth>depth_threshold], na.rm =TRUE),
                                 
                                 #roll
                                 mean_r_100 = mean(envdata$roll1Hz[envdata$depth>depth_threshold], na.rm = TRUE),
                                 sd_r_100  = sd(envdata$roll1Hz[envdata$depth>depth_threshold], na.rm = TRUE),
                                 min_r_100 =  min(envdata$roll1Hz[envdata$depth>depth_threshold], na.rm =TRUE),
                                 max_r_100 =  max(envdata$roll1Hz[envdata$depth>depth_threshold], na.rm =TRUE),

                                 #odba
                                 mean_odba_100 = mean(envdata$odba1Hz[envdata$depth>depth_threshold], na.rm = TRUE),
                                 sd_odba_100  = sd(envdata$odba1Hz[envdata$depth>depth_threshold], na.rm = TRUE),
                                 min_odba_100 =  min(envdata$odba1Hz[envdata$depth>depth_threshold], na.rm =TRUE),
                                 max_odba_100 =  max(envdata$odba1Hz[envdata$depth>depth_threshold], na.rm =TRUE),
                                 

                                 #glides & bursts
                                 sum_glides_100 = sum(envdata$glides[envdata$depth>depth_threshold],na.rm=TRUE),
                                 time_100 = nrow(envdata[envdata$depth>depth_threshold,]),
                                 burst_100 = sum(envdata$burst[envdata$depth>depth_threshold],na.rm=TRUE)
    
                                 )
                              
                                 
  # METRICS AVERAGE FOR EACH DIVING PHASE ALONG THE PROFILE
  
  # subset only descent phase
  envdata_descent <- envdata[envdata$phase == 'D',]
 
  # subset only ascent phase
  envdata_ascent <- envdata[envdata$phase == 'A',]

  #descents
  profile_descent[[i]]<- data.frame(sharkID, 
                                    
                                    ## TOP 85 M ##
                                    
                                    #depth
                                    mean_depth_50 = mean(envdata_descent$depth[envdata_descent$depth<=depth_threshold], na.rm = TRUE),
                                    sd_depth_50 = sd(envdata_descent$depth[envdata_descent$depth<=depth_threshold], na.rm = TRUE),
                                    min_depth_50 =  min(envdata_descent$depth[envdata_descent$depth<=depth_threshold], na.rm =TRUE),
                                    max_depth_50 =  max(envdata_descent$depth[envdata_descent$depth<=depth_threshold], na.rm =TRUE),
                                    
                                    #temperature
                                    mean_temp_50 = mean(envdata_descent$temperature[envdata_descent$depth<=depth_threshold], na.rm = TRUE),
                                    sd_temp_50  = sd(envdata_descent$temperature[envdata_descent$depth<=depth_threshold], na.rm = TRUE),
                                    min_temp_50 =  min(envdata_descent$temperature[envdata_descent$depth<=depth_threshold], na.rm =TRUE),
                                    max_temp_50 =  max(envdata_descent$temperature[envdata_descent$depth<=depth_threshold], na.rm =TRUE),
                                    
                                    #oxygen
                                    mean_DO_50 = mean(envdata_descent$oxygen[envdata_descent$depth<=depth_threshold], na.rm = TRUE),
                                    sd_DO_50  = sd(envdata_descent$oxygen[envdata_descent$depth<=depth_threshold], na.rm = TRUE),
                                    min_DO_50 =  min(envdata_descent$oxygen[envdata_descent$depth<=depth_threshold], na.rm =TRUE),
                                    max_DO_50 =  max(envdata_descent$oxygen[envdata_descent$depth<=depth_threshold], na.rm =TRUE),
                                    
                                    #tailbeat
                                    mean_tbf_50 = mean(envdata_descent$TBF[envdata_descent$depth<=depth_threshold], na.rm = TRUE),
                                    sd_tbf_50  = sd(envdata_descent$TBF[envdata_descent$depth<=depth_threshold], na.rm = TRUE),
                                    min_tbf_50 =  min(envdata_descent$TBF[envdata_descent$depth<=depth_threshold], na.rm =TRUE),
                                    max_tbf_50 =  max(envdata_descent$TBF[envdata_descent$depth<=depth_threshold], na.rm =TRUE),
                                    
                                    #pitch
                                    mean_p_50 = mean(envdata_descent$pitch1Hz[envdata_descent$depth<=depth_threshold], na.rm = TRUE),
                                    sd_p_50  = sd(envdata_descent$pitch1Hz[envdata_descent$depth<=depth_threshold], na.rm = TRUE),
                                    min_p_50 =  min(envdata_descent$pitch1Hz[envdata_descent$depth<=depth_threshold], na.rm =TRUE),
                                    max_p_50 =  max(envdata_descent$pitch1Hz[envdata_descent$depth<=depth_threshold], na.rm =TRUE),
                                    
                                    #roll
                                    mean_r_50 = mean(envdata_descent$roll1Hz[envdata_descent$depth<=depth_threshold], na.rm = TRUE),
                                    sd_r_50  = sd(envdata_descent$roll1Hz[envdata_descent$depth<=depth_threshold], na.rm = TRUE),
                                    min_r_50 =  min(envdata_descent$roll1Hz[envdata_descent$depth<=depth_threshold], na.rm =TRUE),
                                    max_r_50 =  max(envdata_descent$roll1Hz[envdata_descent$depth<=depth_threshold], na.rm =TRUE),
                                    
                                    #odba
                                    mean_odba_50 = mean(envdata_descent$odba1Hz[envdata_descent$depth<=depth_threshold], na.rm = TRUE),
                                    sd_odba_50  = sd(envdata_descent$odba1Hz[envdata_descent$depth<=depth_threshold], na.rm = TRUE),
                                    min_odba_50 =  min(envdata_descent$odba1Hz[envdata_descent$depth<=depth_threshold], na.rm =TRUE),
                                    max_odba_50 =  max(envdata_descent$odba1Hz[envdata_descent$depth<=depth_threshold], na.rm =TRUE),
                                    
                                    #glides & bursts
                                    sum_glides_50 = sum(envdata_descent$glides[envdata_descent$depth<=depth_threshold],na.rm=TRUE),
                                    time_50 = nrow(envdata_descent[envdata_descent$depth<=depth_threshold,]),
                                    burst_50 = sum(envdata_descent$burst[envdata_descent$depth<=depth_threshold],na.rm=TRUE),
                                    
                                    ## BELOW 85 M ##
                                    
                                    #depth
                                    mean_depth_100 = mean(envdata_descent$depth[envdata_descent$depth>depth_threshold], na.rm = TRUE),
                                    sd_depth_100 = sd(envdata_descent$depth[envdata_descent$depth>depth_threshold], na.rm = TRUE),
                                    min_depth_100 =  min(envdata_descent$depth[envdata_descent$depth>depth_threshold], na.rm =TRUE),
                                    max_depth_100 =  max(envdata_descent$depth[envdata_descent$depth>depth_threshold], na.rm =TRUE),
                                    
                                    #temperature
                                    mean_temp_100 = mean(envdata_descent$temperature[envdata_descent$depth>depth_threshold], na.rm = TRUE),
                                    sd_temp_100  = sd(envdata_descent$temperature[envdata_descent$depth>depth_threshold], na.rm = TRUE),
                                    min_temp_100 =  min(envdata_descent$temperature[envdata_descent$depth>depth_threshold], na.rm =TRUE),
                                    max_temp_100 =  max(envdata_descent$temperature[envdata_descent$depth>depth_threshold], na.rm =TRUE),
                                    
                                    #oxygen
                                    mean_DO_100 = mean(envdata_descent$oxygen[envdata_descent$depth>depth_threshold], na.rm = TRUE),
                                    sd_DO_100  = sd(envdata_descent$oxygen[envdata_descent$depth>depth_threshold], na.rm = TRUE),
                                    min_DO_100 =  min(envdata_descent$oxygen[envdata_descent$depth>depth_threshold], na.rm =TRUE),
                                    max_DO_100 =  max(envdata_descent$oxygen[envdata_descent$depth>depth_threshold], na.rm =TRUE),
                                    
                                    #tailbeat
                                    mean_tbf_100 = mean(envdata_descent$TBF[envdata_descent$depth>depth_threshold], na.rm = TRUE),
                                    sd_tbf_100  = sd(envdata_descent$TBF[envdata_descent$depth>depth_threshold], na.rm = TRUE),
                                    min_tbf_100 =  min(envdata_descent$TBF[envdata_descent$depth>depth_threshold], na.rm =TRUE),
                                    max_tbf_100 =  max(envdata_descent$TBF[envdata_descent$depth>depth_threshold], na.rm =TRUE),
                                    
                                    #pitch
                                    mean_p_100 = mean(envdata_descent$pitch1Hz[envdata_descent$depth>depth_threshold], na.rm = TRUE),
                                    sd_p_100  = sd(envdata_descent$pitch1Hz[envdata_descent$depth>depth_threshold], na.rm = TRUE),
                                    min_p_100 =  min(envdata_descent$pitch1Hz[envdata_descent$depth>depth_threshold], na.rm =TRUE),
                                    max_p_100 =  max(envdata_descent$pitch1Hz[envdata_descent$depth>depth_threshold], na.rm =TRUE),
                                    
                                    #roll
                                    mean_r_100 = mean(envdata_descent$roll1Hz[envdata_descent$depth>depth_threshold], na.rm = TRUE),
                                    sd_r_100  = sd(envdata_descent$roll1Hz[envdata_descent$depth>depth_threshold], na.rm = TRUE),
                                    min_r_100 =  min(envdata_descent$roll1Hz[envdata_descent$depth>depth_threshold], na.rm =TRUE),
                                    max_r_100 =  max(envdata_descent$roll1Hz[envdata_descent$depth>depth_threshold], na.rm =TRUE),
                                    
                                    #odba
                                    mean_odba_100 = mean(envdata_descent$odba1Hz[envdata_descent$depth>depth_threshold], na.rm = TRUE),
                                    sd_odba_100  = sd(envdata_descent$odba1Hz[envdata_descent$depth>depth_threshold], na.rm = TRUE),
                                    min_odba_100 =  min(envdata_descent$odba1Hz[envdata_descent$depth>depth_threshold], na.rm =TRUE),
                                    max_odba_100 =  max(envdata_descent$odba1Hz[envdata_descent$depth>depth_threshold], na.rm =TRUE),
                                    
                                    #glides & bursts
                                    sum_glides_100 = sum(envdata_descent$glides[envdata_descent$depth>depth_threshold],na.rm=TRUE),
                                    time_100 = nrow(envdata_descent[envdata_descent$depth>depth_threshold,]),
                                    burst_100 = sum(envdata_descent$burst[envdata_descent$depth>depth_threshold],na.rm=TRUE)
  )
  
  
  # ascents
  profile_ascent[[i]]<- data.frame(sharkID, 
                                    
                                    
                                    
                                    ## TOP 85 M ##
                                    
                                    #depth
                                    mean_depth_50 = mean(envdata_ascent$depth[envdata_ascent$depth<=depth_threshold], na.rm = TRUE),
                                    sd_depth_50 = sd(envdata_ascent$depth[envdata_ascent$depth<=depth_threshold], na.rm = TRUE),
                                    min_depth_50 =  min(envdata_ascent$depth[envdata_ascent$depth<=depth_threshold], na.rm =TRUE),
                                    max_depth_50 =  max(envdata_ascent$depth[envdata_ascent$depth<=depth_threshold], na.rm =TRUE),
                                    
                                    #temperature
                                    mean_temp_50 = mean(envdata_ascent$temperature[envdata_ascent$depth<=depth_threshold], na.rm = TRUE),
                                    sd_temp_50  = sd(envdata_ascent$temperature[envdata_ascent$depth<=depth_threshold], na.rm = TRUE),
                                    min_temp_50 =  min(envdata_ascent$temperature[envdata_ascent$depth<=depth_threshold], na.rm =TRUE),
                                    max_temp_50 =  max(envdata_ascent$temperature[envdata_ascent$depth<=depth_threshold], na.rm =TRUE),
                                    
                                    #oxygen
                                    mean_DO_50 = mean(envdata_ascent$oxygen[envdata_ascent$depth<=depth_threshold], na.rm = TRUE),
                                    sd_DO_50  = sd(envdata_ascent$oxygen[envdata_ascent$depth<=depth_threshold], na.rm = TRUE),
                                    min_DO_50 =  min(envdata_ascent$oxygen[envdata_ascent$depth<=depth_threshold], na.rm =TRUE),
                                    max_DO_50 =  max(envdata_ascent$oxygen[envdata_ascent$depth<=depth_threshold], na.rm =TRUE),
                                    
                                    #tailbeat
                                    mean_tbf_50 = mean(envdata_ascent$TBF[envdata_ascent$depth<=depth_threshold], na.rm = TRUE),
                                    sd_tbf_50  = sd(envdata_ascent$TBF[envdata_ascent$depth<=depth_threshold], na.rm = TRUE),
                                    min_tbf_50 =  min(envdata_ascent$TBF[envdata_ascent$depth<=depth_threshold], na.rm =TRUE),
                                    max_tbf_50 =  max(envdata_ascent$TBF[envdata_ascent$depth<=depth_threshold], na.rm =TRUE),
                                    
                                    #pitch
                                    mean_p_50 = mean(envdata_ascent$pitch1Hz[envdata_ascent$depth<=depth_threshold], na.rm = TRUE),
                                    sd_p_50  = sd(envdata_ascent$pitch1Hz[envdata_ascent$depth<=depth_threshold], na.rm = TRUE),
                                    min_p_50 =  min(envdata_ascent$pitch1Hz[envdata_ascent$depth<=depth_threshold], na.rm =TRUE),
                                    max_p_50 =  max(envdata_ascent$pitch1Hz[envdata_ascent$depth<=depth_threshold], na.rm =TRUE),
                                    
                                    #roll
                                    mean_r_50 = mean(envdata_ascent$roll1Hz[envdata_ascent$depth<=depth_threshold], na.rm = TRUE),
                                    sd_r_50  = sd(envdata_ascent$roll1Hz[envdata_ascent$depth<=depth_threshold], na.rm = TRUE),
                                    min_r_50 =  min(envdata_ascent$roll1Hz[envdata_ascent$depth<=depth_threshold], na.rm =TRUE),
                                    max_r_50 =  max(envdata_ascent$roll1Hz[envdata_ascent$depth<=depth_threshold], na.rm =TRUE),
                                    
                                    #odba
                                    mean_odba_50 = mean(envdata_ascent$odba1Hz[envdata_ascent$depth<=depth_threshold], na.rm = TRUE),
                                    sd_odba_50  = sd(envdata_ascent$odba1Hz[envdata_ascent$depth<=depth_threshold], na.rm = TRUE),
                                    min_odba_50 =  min(envdata_ascent$odba1Hz[envdata_ascent$depth<=depth_threshold], na.rm =TRUE),
                                    max_odba_50 =  max(envdata_ascent$odba1Hz[envdata_ascent$depth<=depth_threshold], na.rm =TRUE),
                                    
                                    #glides & bursts
                                    sum_glides_50 = sum(envdata_ascent$glides[envdata_ascent$depth<=depth_threshold],na.rm=TRUE),
                                    time_50 = nrow(envdata_ascent[envdata_ascent$depth<=depth_threshold,]), 
                                    burst_50 = sum(envdata_ascent$burst[envdata_ascent$depth<=depth_threshold],na.rm=TRUE),
                                   
                                    ## BELOW 85 M ##
                                    
                                    #depth
                                    mean_depth_100 = mean(envdata_ascent$depth[envdata_ascent$depth>depth_threshold], na.rm = TRUE),
                                    sd_depth_100 = sd(envdata_ascent$depth[envdata_ascent$depth>depth_threshold], na.rm = TRUE),
                                    min_depth_100 =  min(envdata_ascent$depth[envdata_ascent$depth>depth_threshold], na.rm =TRUE),
                                    max_depth_100 =  max(envdata_ascent$depth[envdata_ascent$depth>depth_threshold], na.rm =TRUE),
                                    
                                    #temperature
                                    mean_temp_100 = mean(envdata_ascent$temperature[envdata_ascent$depth>depth_threshold], na.rm = TRUE),
                                    sd_temp_100  = sd(envdata_ascent$temperature[envdata_ascent$depth>depth_threshold], na.rm = TRUE),
                                    min_temp_100 =  min(envdata_ascent$temperature[envdata_ascent$depth>depth_threshold], na.rm =TRUE),
                                    max_temp_100 =  max(envdata_ascent$temperature[envdata_ascent$depth>depth_threshold], na.rm =TRUE),
                                    
                                    #oxygen
                                    mean_DO_100 = mean(envdata_ascent$oxygen[envdata_ascent$depth>depth_threshold], na.rm = TRUE),
                                    sd_DO_100  = sd(envdata_ascent$oxygen[envdata_ascent$depth>depth_threshold], na.rm = TRUE),
                                    min_DO_100 =  min(envdata_ascent$oxygen[envdata_ascent$depth>depth_threshold], na.rm =TRUE),
                                    max_DO_100 =  max(envdata_ascent$oxygen[envdata_ascent$depth>depth_threshold], na.rm =TRUE),
                                  
                                
                                    #tailbeat
                                    mean_tbf_100 = mean(envdata_ascent$TBF[envdata_ascent$depth>depth_threshold], na.rm = TRUE),
                                    sd_tbf_100  = sd(envdata_ascent$TBF[envdata_ascent$depth>depth_threshold], na.rm = TRUE),
                                    min_tbf_100 =  min(envdata_ascent$TBF[envdata_ascent$depth>depth_threshold], na.rm =TRUE),
                                    max_tbf_100 =  max(envdata_ascent$TBF[envdata_ascent$depth>depth_threshold], na.rm =TRUE),
                                    
                                    #pitch
                                    mean_p_100 = mean(envdata_ascent$pitch1Hz[envdata_ascent$depth>depth_threshold], na.rm = TRUE),
                                    sd_p_100  = sd(envdata_ascent$pitch1Hz[envdata_ascent$depth>depth_threshold], na.rm = TRUE),
                                    min_p_100 =  min(envdata_ascent$pitch1Hz[envdata_ascent$depth>depth_threshold], na.rm =TRUE),
                                    max_p_100 =  max(envdata_ascent$pitch1Hz[envdata_ascent$depth>depth_threshold], na.rm =TRUE),
                                    
                                    #roll
                                    mean_r_100 = mean(envdata_ascent$roll1Hz[envdata_ascent$depth>depth_threshold], na.rm = TRUE),
                                    sd_r_100  = sd(envdata_ascent$roll1Hz[envdata_ascent$depth>depth_threshold], na.rm = TRUE),
                                    min_r_100 =  min(envdata_ascent$roll1Hz[envdata_ascent$depth>depth_threshold], na.rm =TRUE),
                                    max_r_100 =  max(envdata_ascent$roll1Hz[envdata_ascent$depth>depth_threshold], na.rm =TRUE),
                                    
                                    #odba
                                    mean_odba_100 = mean(envdata_ascent$odba1Hz[envdata_ascent$depth>depth_threshold], na.rm = TRUE),
                                    sd_odba_100  = sd(envdata_ascent$odba1Hz[envdata_ascent$depth>depth_threshold], na.rm = TRUE),
                                    min_odba_100 =  min(envdata_ascent$odba1Hz[envdata_ascent$depth>depth_threshold], na.rm =TRUE),
                                    max_odba_100 =  max(envdata_ascent$odba1Hz[envdata_ascent$depth>depth_threshold], na.rm =TRUE),
                                    
                                    #glides & bursts
                                    sum_glides_100 = sum(envdata_ascent$glides[envdata_ascent$depth>depth_threshold],na.rm=TRUE),
                                    time_100 = nrow(envdata_ascent[envdata_ascent$depth>depth_threshold,]),
                                    burst_100 = sum(envdata_ascent$burst[envdata_ascent$depth>depth_threshold],na.rm=TRUE)
  )
  

} # end ID


# merge all descriptive stat information - overall
mean_overall<- do.call(rbind,profile_overall)
mean_overall[mean_overall == "NaN"] <- NA

# mean and sd
mean_all<- c('mean', colMeans(mean_overall[,2:ncol(mean_overall)]))
sd_all<- c('sd', apply(mean_overall[,2:ncol(mean_overall)], 2,sd))
mean_overall[4,]<- mean_all
mean_overall[5,]<- sd_all
mean_overall$type <- 'overall'

# merge all descriptive stat information - descents
mean_descent<- do.call(rbind,profile_descent)
mean_descent[mean_descent == "NaN"] <- NA

# mean and sd
mean_all<- c('mean', colMeans(mean_descent[,2:ncol(mean_descent)]))
sd_all<- c('sd', apply(mean_descent[,2:ncol(mean_descent)], 2,sd))
mean_descent[4,]<- mean_all
mean_descent[5,]<- sd_all
mean_descent$type <- 'descent'

# merge all descriptive stat information - ascents
mean_ascent<- do.call(rbind,profile_ascent)
mean_ascent[mean_ascent == "NaN"] <- NA

# mean and sd
mean_all<- c('mean', colMeans(mean_ascent[,2:ncol(mean_ascent)]))
sd_all<- c('sd', apply(mean_ascent[,2:ncol(mean_ascent)], 2,sd))
mean_ascent[4,]<- mean_all
mean_ascent[5,]<- sd_all
mean_ascent$type <- 'ascent'



# overall
write.csv(mean_overall, paste0(tableDir, 'Descriptive_Stats-Profile-Overall.csv'), row.names = FALSE, quote = FALSE)

# save descent stats
write.csv(mean_descent, paste0(tableDir, 'Descriptive_Stats-Profile-Descents.csv'), row.names = FALSE, quote = FALSE)

# save ascent stats
write.csv(mean_ascent, paste0(tableDir,'Descriptive_Stats-Profile-Ascents.csv'), row.names = FALSE, quote = FALSE)
