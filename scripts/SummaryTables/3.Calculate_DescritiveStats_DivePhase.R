## CODE to calculate descriptive statistics of swimming metrics for each dive phase (descents vs ascents)

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

# files generated from IGOR analysis with acceleration-derived metrics- 3 sharks: blue2 (S1), blue4 (S3), blue5 (S4)
files<-list.files(folder, pattern= '-accdata2.csv', recursive = TRUE)	
plotFolder<- 'C:/Users/ivoda/OneDrive - Universidade do Porto/Thesis/RP2-InSitu_O2Tag/figures/Fig.SU/'

# list to save metrics
sharkList<- list()

for(i in 1:length(files)){
  
  #i=3

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
  
  # FINESCALE METRICS AVERAGE FOR EACH DIVING PHASE
  sharkList[[i]] <- data.frame(
    
    ID = sharkID,
    
    # DEPTH
    mean_depth_descend =  mean(envdata$depth[envdata$phase == 'D'],na.rm=TRUE),
    sd_depth_descend =  sd(envdata$depth[envdata$phase == 'D'],na.rm=TRUE),
    min_depth_descend =  min(envdata$depth[envdata$phase == 'D'], na.rm =TRUE),
    max_depth_descend =  max(envdata$depth[envdata$phase == 'D'], na.rm =TRUE),
    
    mean_depth_ascend  =  mean(envdata$depth[envdata$phase == 'A'],na.rm=TRUE),
    sd_depth_ascend  =  sd(envdata$depth[envdata$phase == 'A'],na.rm=TRUE),
    min_depth_ascend=  min(envdata$depth[envdata$phase == 'A'], na.rm =TRUE),
    max_depth_ascend =  max(envdata$depth[envdata$phase == 'A'], na.rm =TRUE),
  
    # TEMP
    mean_temp_descend =  mean(envdata$temperature[envdata$phase == 'D'],na.rm=TRUE),
    sd_temp_descend =  sd(envdata$temperature[envdata$phase == 'D'],na.rm=TRUE),
    min_temp_descend =  min(envdata$temperature[envdata$phase == 'D'], na.rm =TRUE),
    max_temp_descend =  max(envdata$temperature[envdata$phase == 'D'], na.rm =TRUE),
  
    mean_temp_ascend  =  mean(envdata$temperature[envdata$phase == 'A'],na.rm=TRUE),
    sd_temp_ascend  =  sd(envdata$temperature[envdata$phase == 'A'],na.rm=TRUE),
    min_temp_ascend=  min(envdata$temperature[envdata$phase == 'A'], na.rm =TRUE),
    max_temp_ascend =  max(envdata$temperature[envdata$phase == 'A'], na.rm =TRUE),
    
    # DO
    mean_do_descend =  mean(envdata$oxygen[envdata$phase == 'D'],na.rm=TRUE),
    sd_do_descend =  sd(envdata$oxygen[envdata$phase == 'D'],na.rm=TRUE),
    min_do_descend =  min(envdata$oxygen[envdata$phase == 'D'], na.rm =TRUE),
    max_do_descend =  max(envdata$oxygen[envdata$phase == 'D'], na.rm =TRUE),

    mean_do_ascend  =  mean(envdata$oxygen[envdata$phase == 'A'],na.rm=TRUE),
    sd_do_ascend  =  sd(envdata$oxygen[envdata$phase == 'A'],na.rm=TRUE),
    min_do_ascend=  min(envdata$oxygen[envdata$phase == 'A'], na.rm =TRUE),
    max_do_ascend =  max(envdata$oxygen[envdata$phase == 'A'], na.rm =TRUE),
    
    # TBF
    mean_tbf_descend =  mean(envdata$TBF[envdata$phase == 'D'],na.rm=TRUE),
    sd_tbf_descend =  sd(envdata$TBF[envdata$phase == 'D'],na.rm=TRUE),
    min_tbf_descend =  min(envdata$TBF[envdata$phase == 'D'], na.rm =TRUE),
    max_tbf_descend =  max(envdata$TBF[envdata$phase == 'D'], na.rm =TRUE),
    median_tbf_descend =  median(envdata$TBF[envdata$phase == 'D'], na.rm =TRUE),
    
    
    mean_tbf_ascend  =  mean(envdata$TBF[envdata$phase == 'A'],na.rm=TRUE),
    sd_tbf_ascend  =  sd(envdata$TBF[envdata$phase == 'A'],na.rm=TRUE),
    min_tbf_ascend=  min(envdata$TBF[envdata$phase == 'A'], na.rm =TRUE),
    max_tbf_ascend =  max(envdata$TBF[envdata$phase == 'A'], na.rm =TRUE),

    median_tbf_ascend  =  median(envdata$TBF[envdata$phase == 'A'],na.rm=TRUE),
    
    
    # PITCH
    mean_p_descend =  mean(envdata$pitch1Hz[envdata$phase == 'D'],na.rm=TRUE),
    sd_p_descend =  sd(envdata$pitch1Hz[envdata$phase == 'D'],na.rm=TRUE),
    min_p_descend=  min(envdata$pitch1Hz[envdata$phase == 'D'], na.rm =TRUE),
    max_p_descend =  max(envdata$pitch1Hz[envdata$phase == 'D'], na.rm =TRUE),
     
    mean_p_ascend  =  mean(envdata$pitch1Hz[envdata$phase == 'A'],na.rm=TRUE),
    sd_p_ascend  =  sd(envdata$pitch1Hz[envdata$phase == 'A'],na.rm=TRUE),
    min_p_ascend=  min(envdata$pitch1Hz[envdata$phase == 'A'], na.rm =TRUE),
    max_p_ascend =  max(envdata$pitch1Hz[envdata$phase == 'A'], na.rm =TRUE),

    # ROLL
    mean_r_descend =  mean(envdata$roll1Hz[envdata$phase == 'D'],na.rm=TRUE),
    sd_r_descend =  sd(envdata$roll1Hz[envdata$phase == 'D'],na.rm=TRUE),
    min_r_descend=  min(envdata$roll1Hz[envdata$phase == 'D'], na.rm =TRUE),
    max_r_descend =  max(envdata$roll1Hz[envdata$phase == 'D'], na.rm =TRUE),
    
    mean_r_ascend  =  mean(envdata$roll1Hz[envdata$phase == 'A'],na.rm=TRUE),
    sd_r_ascend  =  sd(envdata$roll1Hz[envdata$phase == 'A'],na.rm=TRUE),
    min_r_ascend=  min(envdata$roll1Hz[envdata$phase == 'A'], na.rm =TRUE),
    max_r_ascend =  max(envdata$roll1Hz[envdata$phase == 'A'], na.rm =TRUE),
    
    # ODBA
    mean_odba_descend = mean(envdata$odba1Hz[envdata$phase == 'D'],na.rm=TRUE),
    sd_odba_descend =  sd(envdata$odba1Hz[envdata$phase == 'D'],na.rm=TRUE),
    min_odba_descend =  min(envdata$odba1Hz[envdata$phase == 'D'], na.rm =TRUE),
    max_odba_descend =  max(envdata$odba1Hz[envdata$phase == 'D'], na.rm =TRUE),
    
    mean_odba_ascend = mean(envdata$odba1Hz[envdata$phase == 'A'],na.rm=TRUE),
    sd_odba_ascend  =  sd(envdata$odba1Hz[envdata$phase == 'A'],na.rm=TRUE),
    min_odba_ascend=  min(envdata$odba1Hz[envdata$phase == 'A'], na.rm =TRUE),
    max_odba_ascend =  max(envdata$odba1Hz[envdata$phase == 'A'], na.rm =TRUE),
    
    # GLIDES
    sum_glides_descend = sum(envdata$glides[envdata$phase == 'D'],na.rm=TRUE),
    sum_glides_ascend = sum(envdata$glides[envdata$phase == 'A'],na.rm=TRUE),
   
    descend_time = nrow(envdata[envdata$phase == 'D',]),
    ascend_time = nrow(envdata[envdata$phase == 'A',]),
   
    # BURSTS
    burst_descent = sum(envdata$burst[envdata$phase == 'D'],na.rm=TRUE),
    burst_ascent = sum(envdata$burst[envdata$phase == 'A'],na.rm=TRUE)
   
   
  )
  
}
  
# merge all
mean_shark_metrics<- do.call(rbind,sharkList )

# mean and sd
mean_all<- c('mean', colMeans(mean_shark_metrics[,2:ncol(mean_shark_metrics)]))
sd_all<- c('sd', apply(mean_shark_metrics[,2:ncol(mean_shark_metrics)], 2,sd))
mean_shark_metrics[4,]<- mean_all
mean_shark_metrics[5,]<- sd_all

# save csv
write.csv(mean_shark_metrics, paste0(tableDir, 'Descriptive_Stats-DivePhase.csv'), row.names = FALSE, quote = FALSE)
