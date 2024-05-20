### CODE to calculate dive phase (descents vs ascent) descriptive statistics (mean, sd, range) of swimming metrics
### of tagged sharks for pre vs post-transition (normal) period.
### Information was used to build informative tables

# Set timezone to GMT
Sys.setenv(TZ='GMT')

## PACKAGES 
pack <- c('data.table','pastecs', 'dplyr','mgcv', 'ggplot2')
# lapply('pals', install.packages)
lapply(pack, require, character.only = TRUE)

## OPTIONS 
# path folder
folder<-'D:/_Papers/O2RecordingTag/'
fileDir<- paste0(folder,'/files/')
tableDir<- paste0(folder,'/tables/')

# files generated from IGOR analysis (with all swimming metrics)  - 2 sharks that showed decrease trend on TBF: blue2 (S1), blue5 (S4)
files<-list.files(folder, pattern= '-accdata2.csv', recursive = TRUE)	
files<- files[grep('blue4',files, invert = TRUE)]

# create list to save descriptive statistics generated on the loop (for pre and normal - post transition period)
# for each shark id
sharkList_recovery<- list()
sharkList_normal<- list()

for(i in 1:length(files)){
  
  
  i=1
  
  # get shark id
  sharkID<- gsub('-accdata2.csv','', basename(files[i]))
  
  # read file
  envdata<- read.csv(paste0(folder,files[i]), header = TRUE)
  
  # correct column names on dataframe
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
  
  # save dive information on the dataframe
  envdata$phase<- dat$phase
  
  # quick check dive phases on time series by plotting
  #ggplot(envdata,aes(x=time,y=-depth,col=phase)) + geom_point()+  scale_colour_manual(name = 'Dive Phase',values = c("#D7A31A", "#002458","#5E81C2")) 
  
  # identify 95 percentiles of TBF and ODBA (which will define periods of swimming bursts)
  tbf_95<- quantile(envdata$TBF, 0.95, na.rm =TRUE )
  odba_95<- quantile(envdata$odba1Hz, 0.95, na.rm =TRUE )
  
  # define burst swimming
  envdata$burst<-  ifelse(envdata$odba1Hz >=odba_95 & envdata$TBF >=tbf_95 , 1,0)
  envdata$burst[is.na(envdata$burst)]<- 0
  
  # define transition time
  # for blue 2 (S1) is 2.16 while blue 5 is 5.08 hours
  if(sharkID == 'blue2'){ recovery_hour<-envdata$time[1] + (2.16*60*60) } else{ recovery_hour<-envdata$time[1] + (5.08*60*60)}
  
  # split main dataframe into pre (recovery) and post-transition (normal)
  recovery_tbl<- envdata[envdata$time<recovery_hour,]
  normal_tbl<- envdata[envdata$time>=recovery_hour,]
  
  ## SAVE STATISTICS
  
  # save descriptive statistics for pre transition period
  sharkList_recovery[[i]] <- data.frame(
    
    ID = sharkID,
    
    #DEPTH
    mean_depth_descend =  mean(recovery_tbl$depth[recovery_tbl$phase == 'D'],na.rm=TRUE),
    sd_depth_descend =  sd(recovery_tbl$depth[recovery_tbl$phase == 'D'],na.rm=TRUE),
    min_depth_descend =  min(recovery_tbl$depth[recovery_tbl$phase == 'D'], na.rm =TRUE),
    max_depth_descend =  max(recovery_tbl$depth[recovery_tbl$phase == 'D'], na.rm =TRUE),
    mean_depth_ascend  =  mean(recovery_tbl$depth[recovery_tbl$phase == 'A'],na.rm=TRUE),
    sd_depth_ascend  =  sd(recovery_tbl$depth[recovery_tbl$phase == 'A'],na.rm=TRUE),
    min_depth_ascend=  min(recovery_tbl$depth[recovery_tbl$phase == 'A'], na.rm =TRUE),
    max_depth_ascend =  max(recovery_tbl$depth[recovery_tbl$phase == 'A'], na.rm =TRUE),
    
    #TEMP
    mean_temp_descend =  mean(recovery_tbl$temperature[recovery_tbl$phase == 'D'],na.rm=TRUE),
    sd_temp_descend =  sd(recovery_tbl$temperature[recovery_tbl$phase == 'D'],na.rm=TRUE),
    min_temp_descend =  min(recovery_tbl$temperature[recovery_tbl$phase == 'D'], na.rm =TRUE),
    max_temp_descend =  max(recovery_tbl$temperature[recovery_tbl$phase == 'D'], na.rm =TRUE),
    mean_temp_ascend  =  mean(recovery_tbl$temperature[recovery_tbl$phase == 'A'],na.rm=TRUE),
    sd_temp_ascend  =  sd(recovery_tbl$temperature[recovery_tbl$phase == 'A'],na.rm=TRUE),
    min_temp_ascend=  min(recovery_tbl$temperature[recovery_tbl$phase == 'A'], na.rm =TRUE),
    max_temp_ascend =  max(recovery_tbl$temperature[recovery_tbl$phase == 'A'], na.rm =TRUE),
  
    #DO
    mean_do_descend =  mean(recovery_tbl$oxygen[recovery_tbl$phase == 'D'],na.rm=TRUE),
    sd_do_descend =  sd(recovery_tbl$oxygen[recovery_tbl$phase == 'D'],na.rm=TRUE),
    min_do_descend =  min(recovery_tbl$oxygen[recovery_tbl$phase == 'D'], na.rm =TRUE),
    max_do_descend =  max(recovery_tbl$oxygen[recovery_tbl$phase == 'D'], na.rm =TRUE),
    mean_do_ascend  =  mean(recovery_tbl$oxygen[recovery_tbl$phase == 'A'],na.rm=TRUE),
    sd_do_ascend  =  sd(recovery_tbl$oxygen[recovery_tbl$phase == 'A'],na.rm=TRUE),
    min_do_ascend=  min(recovery_tbl$oxygen[recovery_tbl$phase == 'A'], na.rm =TRUE),
    max_do_ascend =  max(recovery_tbl$oxygen[recovery_tbl$phase == 'A'], na.rm =TRUE),
    
    #TBF
    mean_tbf_descend =  mean(recovery_tbl$TBF[recovery_tbl$phase == 'D'],na.rm=TRUE),
    sd_tbf_descend =  sd(recovery_tbl$TBF[recovery_tbl$phase == 'D'],na.rm=TRUE),
    min_tbf_descend =  min(recovery_tbl$TBF[recovery_tbl$phase == 'D'], na.rm =TRUE),
    max_tbf_descend =  max(recovery_tbl$TBF[recovery_tbl$phase == 'D'], na.rm =TRUE),
    mean_tbf_ascend  =  mean(recovery_tbl$TBF[recovery_tbl$phase == 'A'],na.rm=TRUE),
    sd_tbf_ascend  =  sd(recovery_tbl$TBF[recovery_tbl$phase == 'A'],na.rm=TRUE),
    min_tbf_ascend=  min(recovery_tbl$TBF[recovery_tbl$phase == 'A'], na.rm =TRUE),
    max_tbf_ascend =  max(recovery_tbl$TBF[recovery_tbl$phase == 'A'], na.rm =TRUE),
    
    #PITCH
    mean_p_descend =  mean(recovery_tbl$pitch1Hz[recovery_tbl$phase == 'D'],na.rm=TRUE),
    sd_p_descend =  sd(recovery_tbl$pitch1Hz[recovery_tbl$phase == 'D'],na.rm=TRUE),
    min_p_descend=  min(recovery_tbl$pitch1Hz[recovery_tbl$phase == 'D'], na.rm =TRUE),
    max_p_descend =  max(recovery_tbl$pitch1Hz[recovery_tbl$phase == 'D'], na.rm =TRUE),
    mean_p_ascend  =  mean(recovery_tbl$pitch1Hz[recovery_tbl$phase == 'A'],na.rm=TRUE),
    sd_p_ascend  =  sd(recovery_tbl$pitch1Hz[recovery_tbl$phase == 'A'],na.rm=TRUE),
    min_p_ascend=  min(recovery_tbl$pitch1Hz[recovery_tbl$phase == 'A'], na.rm =TRUE),
    max_p_ascend =  max(recovery_tbl$pitch1Hz[recovery_tbl$phase == 'A'], na.rm =TRUE),
    
    #ROLL
    mean_r_descend =  mean(recovery_tbl$roll1Hz[recovery_tbl$phase == 'D'],na.rm=TRUE),
    sd_r_descend =  sd(recovery_tbl$roll1Hz[recovery_tbl$phase == 'D'],na.rm=TRUE),
    min_r_descend=  min(recovery_tbl$roll1Hz[recovery_tbl$phase == 'D'], na.rm =TRUE),
    max_r_descend =  max(recovery_tbl$roll1Hz[recovery_tbl$phase == 'D'], na.rm =TRUE),
    mean_r_ascend  =  mean(recovery_tbl$roll1Hz[recovery_tbl$phase == 'A'],na.rm=TRUE),
    sd_r_ascend  =  sd(recovery_tbl$roll1Hz[recovery_tbl$phase == 'A'],na.rm=TRUE),
    min_r_ascend=  min(recovery_tbl$roll1Hz[recovery_tbl$phase == 'A'], na.rm =TRUE),
    max_r_ascend =  max(recovery_tbl$roll1Hz[recovery_tbl$phase == 'A'], na.rm =TRUE),
    
    #ODBA
    mean_odba_descend = mean(recovery_tbl$odba1Hz[recovery_tbl$phase == 'D'],na.rm=TRUE),
    sd_odba_descend =  sd(recovery_tbl$odba1Hz[recovery_tbl$phase == 'D'],na.rm=TRUE),
    min_odba_descend =  min(recovery_tbl$odba1Hz[recovery_tbl$phase == 'D'], na.rm =TRUE),
    max_odba_descend =  max(recovery_tbl$odba1Hz[recovery_tbl$phase == 'D'], na.rm =TRUE),
    mean_odba_ascend = mean(recovery_tbl$odba1Hz[recovery_tbl$phase == 'A'],na.rm=TRUE),
    sd_odba_ascend  =  sd(recovery_tbl$odba1Hz[recovery_tbl$phase == 'A'],na.rm=TRUE),
    min_odba_ascend=  min(recovery_tbl$odba1Hz[recovery_tbl$phase == 'A'], na.rm =TRUE),
    max_odba_ascend =  max(recovery_tbl$odba1Hz[recovery_tbl$phase == 'A'], na.rm =TRUE),
    
    #GLIDES
    sum_glides_descend = sum(recovery_tbl$glides[recovery_tbl$phase == 'D'],na.rm=TRUE),
    sum_glides_ascend = sum(recovery_tbl$glides[recovery_tbl$phase == 'A'],na.rm=TRUE),
    descend_time = nrow(recovery_tbl[recovery_tbl$phase == 'D',]),
    ascend_time = nrow(recovery_tbl[recovery_tbl$phase == 'A',]),
    
    #BURSTS
    burst_descent = sum(recovery_tbl$burst[recovery_tbl$phase == 'D'],na.rm=TRUE),
    burst_ascent = sum(recovery_tbl$burst[recovery_tbl$phase == 'A'],na.rm=TRUE)
    
  )
  
  
  # save descriptive statistics for post-transition period
  sharkList_normal[[i]] <- data.frame(
    
    ID = sharkID,
    
    #DEPTH
    mean_depth_descend =  mean(normal_tbl$depth[normal_tbl$phase == 'D'],na.rm=TRUE),
    sd_depth_descend =  sd(normal_tbl$depth[normal_tbl$phase == 'D'],na.rm=TRUE),
    min_depth_descend =  min(normal_tbl$depth[normal_tbl$phase == 'D'], na.rm =TRUE),
    max_depth_descend =  max(normal_tbl$depth[normal_tbl$phase == 'D'], na.rm =TRUE),
    mean_depth_ascend  =  mean(normal_tbl$depth[normal_tbl$phase == 'A'],na.rm=TRUE),
    sd_depth_ascend  =  sd(normal_tbl$depth[normal_tbl$phase == 'A'],na.rm=TRUE),
    min_depth_ascend=  min(normal_tbl$depth[normal_tbl$phase == 'A'], na.rm =TRUE),
    max_depth_ascend =  max(normal_tbl$depth[normal_tbl$phase == 'A'], na.rm =TRUE),
    
    #TEMP
    mean_temp_descend =  mean(normal_tbl$temperature[normal_tbl$phase == 'D'],na.rm=TRUE),
    sd_temp_descend =  sd(normal_tbl$temperature[normal_tbl$phase == 'D'],na.rm=TRUE),
    min_temp_descend =  min(normal_tbl$temperature[normal_tbl$phase == 'D'], na.rm =TRUE),
    max_temp_descend =  max(normal_tbl$temperature[normal_tbl$phase == 'D'], na.rm =TRUE),
    mean_temp_ascend  =  mean(normal_tbl$temperature[normal_tbl$phase == 'A'],na.rm=TRUE),
    sd_temp_ascend  =  sd(normal_tbl$temperature[normal_tbl$phase == 'A'],na.rm=TRUE),
    min_temp_ascend=  min(normal_tbl$temperature[normal_tbl$phase == 'A'], na.rm =TRUE),
    max_temp_ascend =  max(normal_tbl$temperature[normal_tbl$phase == 'A'], na.rm =TRUE),
    
    #DO
    mean_do_descend =  mean(normal_tbl$oxygen[normal_tbl$phase == 'D'],na.rm=TRUE),
    sd_do_descend =  sd(normal_tbl$oxygen[normal_tbl$phase == 'D'],na.rm=TRUE),
    min_do_descend =  min(normal_tbl$oxygen[normal_tbl$phase == 'D'], na.rm =TRUE),
    max_do_descend =  max(normal_tbl$oxygen[normal_tbl$phase == 'D'], na.rm =TRUE),
    mean_do_ascend  =  mean(normal_tbl$oxygen[normal_tbl$phase == 'A'],na.rm=TRUE),
    sd_do_ascend  =  sd(normal_tbl$oxygen[normal_tbl$phase == 'A'],na.rm=TRUE),
    min_do_ascend=  min(normal_tbl$oxygen[normal_tbl$phase == 'A'], na.rm =TRUE),
    max_do_ascend =  max(normal_tbl$oxygen[normal_tbl$phase == 'A'], na.rm =TRUE),
    
    #TBF
    mean_tbf_descend =  mean(normal_tbl$TBF[normal_tbl$phase == 'D'],na.rm=TRUE),
    sd_tbf_descend =  sd(normal_tbl$TBF[normal_tbl$phase == 'D'],na.rm=TRUE),
    min_tbf_descend =  min(normal_tbl$TBF[normal_tbl$phase == 'D'], na.rm =TRUE),
    max_tbf_descend =  max(normal_tbl$TBF[normal_tbl$phase == 'D'], na.rm =TRUE),
    mean_tbf_ascend  =  mean(normal_tbl$TBF[normal_tbl$phase == 'A'],na.rm=TRUE),
    sd_tbf_ascend  =  sd(normal_tbl$TBF[normal_tbl$phase == 'A'],na.rm=TRUE),
    min_tbf_ascend=  min(normal_tbl$TBF[normal_tbl$phase == 'A'], na.rm =TRUE),
    max_tbf_ascend =  max(normal_tbl$TBF[normal_tbl$phase == 'A'], na.rm =TRUE),
    
    #PITCH
    mean_p_descend =  mean(normal_tbl$pitch1Hz[normal_tbl$phase == 'D'],na.rm=TRUE),
    sd_p_descend =  sd(normal_tbl$pitch1Hz[normal_tbl$phase == 'D'],na.rm=TRUE),
    min_p_descend=  min(normal_tbl$pitch1Hz[normal_tbl$phase == 'D'], na.rm =TRUE),
    max_p_descend =  max(normal_tbl$pitch1Hz[normal_tbl$phase == 'D'], na.rm =TRUE),
    mean_p_ascend  =  mean(normal_tbl$pitch1Hz[normal_tbl$phase == 'A'],na.rm=TRUE),
    sd_p_ascend  =  sd(normal_tbl$pitch1Hz[normal_tbl$phase == 'A'],na.rm=TRUE),
    min_p_ascend=  min(normal_tbl$pitch1Hz[normal_tbl$phase == 'A'], na.rm =TRUE),
    max_p_ascend =  max(normal_tbl$pitch1Hz[normal_tbl$phase == 'A'], na.rm =TRUE),
    
    #ROLL
    mean_r_descend =  mean(normal_tbl$roll1Hz[normal_tbl$phase == 'D'],na.rm=TRUE),
    sd_r_descend =  sd(normal_tbl$roll1Hz[normal_tbl$phase == 'D'],na.rm=TRUE),
    min_r_descend=  min(normal_tbl$roll1Hz[normal_tbl$phase == 'D'], na.rm =TRUE),
    max_r_descend =  max(normal_tbl$roll1Hz[normal_tbl$phase == 'D'], na.rm =TRUE),
    mean_r_ascend  =  mean(normal_tbl$roll1Hz[normal_tbl$phase == 'A'],na.rm=TRUE),
    sd_r_ascend  =  sd(normal_tbl$roll1Hz[normal_tbl$phase == 'A'],na.rm=TRUE),
    min_r_ascend=  min(normal_tbl$roll1Hz[normal_tbl$phase == 'A'], na.rm =TRUE),
    max_r_ascend =  max(normal_tbl$roll1Hz[normal_tbl$phase == 'A'], na.rm =TRUE),
    
    #ODBA
    mean_odba_descend = mean(normal_tbl$odba1Hz[normal_tbl$phase == 'D'],na.rm=TRUE),
    sd_odba_descend =  sd(normal_tbl$odba1Hz[normal_tbl$phase == 'D'],na.rm=TRUE),
    min_odba_descend =  min(normal_tbl$odba1Hz[normal_tbl$phase == 'D'], na.rm =TRUE),
    max_odba_descend =  max(normal_tbl$odba1Hz[normal_tbl$phase == 'D'], na.rm =TRUE),
    mean_odba_ascend = mean(normal_tbl$odba1Hz[normal_tbl$phase == 'A'],na.rm=TRUE),
    sd_odba_ascend  =  sd(normal_tbl$odba1Hz[normal_tbl$phase == 'A'],na.rm=TRUE),
    min_odba_ascend=  min(normal_tbl$odba1Hz[normal_tbl$phase == 'A'], na.rm =TRUE),
    max_odba_ascend =  max(normal_tbl$odba1Hz[normal_tbl$phase == 'A'], na.rm =TRUE),
    
    #GLIDES
    sum_glides_descend = sum(normal_tbl$glides[normal_tbl$phase == 'D'],na.rm=TRUE),
    sum_glides_ascend = sum(normal_tbl$glides[normal_tbl$phase == 'A'],na.rm=TRUE),
    descend_time = nrow(normal_tbl[normal_tbl$phase == 'D',]),
    ascend_time = nrow(normal_tbl[normal_tbl$phase == 'A',]),
    
    # BURSTS
    burst_descent = sum(normal_tbl$burst[normal_tbl$phase == 'D'],na.rm=TRUE),
    burst_ascent = sum(normal_tbl$burst[normal_tbl$phase == 'A'],na.rm=TRUE)
    
  )
  
print(sharkID)  
  
}
  
  
  # merge pre-transition stats
  mean_shark_recovery<- do.call(rbind,sharkList_recovery)
  mean_shark_recovery<- data.frame(period = 'recovery',mean_shark_recovery)
  
  # merge post-transition stats
  mean_shark_normal<- do.call(rbind,sharkList_normal )
  mean_shark_normal<- data.frame(period = 'normal',mean_shark_normal)
  
  # merge both information and save as a csv file on folder /tables
  tbl<- rbind(mean_shark_recovery,mean_shark_normal)
  write.csv(tbl, paste0(tableDir, 'Descriptive_Stats-DivePhase_Recovery_vs_PostRecovery.csv'), row.names = FALSE, quote = FALSE)
