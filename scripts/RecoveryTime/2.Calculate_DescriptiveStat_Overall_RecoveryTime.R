### CODE to calculate overall descriptive statistics (mean, sd, range) of swimming metrics
### of tagged sharks for recovery vs post-recovery (normal) period.
### Information was used to build informative tables
### code to define descriptive statistics can be refined (e.g., using dplyr package) to faster create tables with that information

# Set timezone to GMT
Sys.setenv(TZ='GMT')

## PACKAGES 
pack <- c('data.table','pastecs', 'dplyr','mgcv')
# lapply('pals', install.packages)
lapply(pack, require, character.only = TRUE)

## OPTIONS 
# path folder
folder<-'D:/_Papers/O2RecordingTag/'
fileDir<- paste0(folder,'/files/')
tableDir<- paste0(folder,'/tables/')

# files generated from IGOR analysis (with all swimming metrics) - 2 sharks that showed decrease trend on TBF: blue2 (S1), blue5 (S4)
files<-list.files(folder, pattern= '-accdata2.csv', recursive = TRUE)	
files<- files[grep('blue4',files, invert = TRUE)]

# create list to save descriptive statistics generated on the loop (for recovery and normal - post recovery period)
# for each shark id
sharkList_recovery<- list()
sharkList_normal<- list()

# loop across each shark ID - blue2 or blue 5
for(i in 1:length(files)){
  
  #i=1

  #get shark id
  sharkID<- gsub('-accdata2.csv','', basename(files[i]))
  
  #read file
  envdata<- read.csv(paste0(folder,files[i]), header = TRUE)
  
  # correct column names on dataframe
  colnames(envdata)<- c('time', 'depth', 'temperature', 'oxygen', 'TBF', 'pitch1Hz', 'roll1Hz', 'odba1Hz', 'glides')
  
  # remove negative depths for blue 4
  if(sharkID == 'blue4'){ envdata$depth[envdata$depth <0]<- 0}
  
  # time in correct format
  envdata$time<- as.POSIXct(envdata$time)
  
  # identify 95 percentiles of TBF and ODBA (which will define periods of swimming bursts)
  tbf_95<- quantile(envdata$TBF, 0.95, na.rm =TRUE )
  odba_95<- quantile(envdata$odba1Hz, 0.95, na.rm =TRUE )
  
  # define bursts
  envdata$burst<-  ifelse(envdata$odba1Hz >=odba_95 & envdata$TBF >=tbf_95 , 1,0)
  envdata$burst[is.na(envdata$burst)]<- 0

  # define recovery time
  # for blue 2 (S1) is 2.16 while blue 5 is 5.08 hours
  if(sharkID == 'blue2'){ recovery_hour<-envdata$time[1] + (2.16*60*60) } else{ recovery_hour<-envdata$time[1] + (5.08*60*60)}
  
  # split main dataframe into recovery and post-recovery (normal)
  recovery_tbl<- envdata[envdata$time<recovery_hour,]
  normal_tbl<- envdata[envdata$time>=recovery_hour,]
  

  # save descriptive statistics for recovery period
  sharkList_recovery[[i]] <- data.frame(sharkID, 
                                  
                                  #depth
                                  mean_depth = mean(recovery_tbl$depth, na.rm = TRUE),
                                  sd_depth = sd(recovery_tbl$depth, na.rm = TRUE),
                                  min_depth =  min(recovery_tbl$depth, na.rm =TRUE),
                                  max_depth =  max(recovery_tbl$depth, na.rm =TRUE),
                                  
                                  #temperature
                                  mean_temp = mean(recovery_tbl$temperature, na.rm = TRUE),
                                  sd_temp  = sd(recovery_tbl$temperature, na.rm = TRUE),
                                  min_temp =  min(recovery_tbl$temperature, na.rm =TRUE),
                                  max_temp =  max(recovery_tbl$temperature, na.rm =TRUE),
                                 
                                  
                                  #oxygen
                                  mean_DO = mean(recovery_tbl$oxygen, na.rm = TRUE),
                                  sd_DO  = sd(recovery_tbl$oxygen, na.rm = TRUE),
                                  min_DO =  min(recovery_tbl$oxygen, na.rm =TRUE),
                                  max_DO =  max(recovery_tbl$oxygen, na.rm =TRUE),
                                 
                                  #tailbeat
                                  mean_tbf = mean(recovery_tbl$TBF, na.rm = TRUE),
                                  sd_tbf  = sd(recovery_tbl$TBF, na.rm = TRUE),
                                  min_tbf =  min(recovery_tbl$TBF, na.rm =TRUE),
                                  max_tbf =  max(recovery_tbl$TBF, na.rm =TRUE),
                                  
                                  #pitch
                                  mean_p = mean(recovery_tbl$pitch1Hz, na.rm = TRUE),
                                  sd_p  = sd(recovery_tbl$pitch1Hz, na.rm = TRUE),
                                  min_p =  min(recovery_tbl$pitch1Hz, na.rm =TRUE),
                                  max_p =  max(recovery_tbl$pitch1Hz, na.rm =TRUE),
                                  
                                  #roll
                                  mean_r = mean(recovery_tbl$roll1Hz, na.rm = TRUE),
                                  sd_r  = sd(recovery_tbl$roll1Hz, na.rm = TRUE),
                                  min_r =  min(recovery_tbl$roll1Hz, na.rm =TRUE),
                                  max_r =  max(recovery_tbl$roll1Hz, na.rm =TRUE),
                                  
                                  #odba
                                  mean_odba = mean(recovery_tbl$odba1Hz, na.rm = TRUE),
                                  sd_odba  = sd(recovery_tbl$odba1Hz, na.rm = TRUE),
                                  min_odba =  min(recovery_tbl$odba1Hz, na.rm =TRUE),
                                  max_odba =  max(recovery_tbl$odba1Hz, na.rm =TRUE),
                                  
                                  #glides
                                  sum_glides = sum(recovery_tbl$glides,na.rm=TRUE),
                                  time = nrow(recovery_tbl),
                                  
                                  #bursts
                                  burst = sum(recovery_tbl$burst,na.rm=TRUE)
                                  
  )

  # save descriptive statistics for post-recovery period
  sharkList_normal[[i]] <- data.frame(sharkID, 
                                        
                                        
                                        #depth
                                        mean_depth = mean(normal_tbl$depth, na.rm = TRUE),
                                        sd_depth = sd(normal_tbl$depth, na.rm = TRUE),
                                        min_depth =  min(normal_tbl$depth, na.rm =TRUE),
                                        max_depth =  max(normal_tbl$depth, na.rm =TRUE),
                                        
                                        #temperature
                                        mean_temp = mean(normal_tbl$temperature, na.rm = TRUE),
                                        sd_temp  = sd(normal_tbl$temperature, na.rm = TRUE),
                                        min_temp =  min(normal_tbl$temperature, na.rm =TRUE),
                                        max_temp =  max(normal_tbl$temperature, na.rm =TRUE),

                                        #oxygen
                                        mean_DO = mean(normal_tbl$oxygen, na.rm = TRUE),
                                        sd_DO  = sd(normal_tbl$oxygen, na.rm = TRUE),
                                        min_DO =  min(normal_tbl$oxygen, na.rm =TRUE),
                                        max_DO =  max(normal_tbl$oxygen, na.rm =TRUE),
                                        
                                        #tailbeat
                                        mean_tbf = mean(normal_tbl$TBF, na.rm = TRUE),
                                        sd_tbf  = sd(normal_tbl$TBF, na.rm = TRUE),
                                        min_tbf =  min(normal_tbl$TBF, na.rm =TRUE),
                                        max_tbf =  max(normal_tbl$TBF, na.rm =TRUE),
                                        
                                        #pitch
                                        mean_p = mean(normal_tbl$pitch1Hz, na.rm = TRUE),
                                        sd_p  = sd(normal_tbl$pitch1Hz, na.rm = TRUE),
                                        min_p =  min(normal_tbl$pitch1Hz, na.rm =TRUE),
                                        max_p =  max(normal_tbl$pitch1Hz, na.rm =TRUE),
                                        
                                        #roll
                                        mean_r = mean(normal_tbl$roll1Hz, na.rm = TRUE),
                                        sd_r  = sd(normal_tbl$roll1Hz, na.rm = TRUE),
                                        min_r =  min(normal_tbl$roll1Hz, na.rm =TRUE),
                                        max_r =  max(normal_tbl$roll1Hz, na.rm =TRUE),
                                        
                                        #odba
                                        mean_odba = mean(normal_tbl$odba1Hz, na.rm = TRUE),
                                        sd_odba  = sd(normal_tbl$odba1Hz, na.rm = TRUE),
                                        min_odba =  min(normal_tbl$odba1Hz, na.rm =TRUE),
                                        max_odba =  max(normal_tbl$odba1Hz, na.rm =TRUE),
                                        
                                        #glides
                                        sum_glides = sum(normal_tbl$glides,na.rm=TRUE),
                                        time = nrow(normal_tbl),
                                      
                                        #bursts
                                        burst = sum(normal_tbl$burst,na.rm=TRUE)
                                        
  )

  print(sharkID)  
  
}

# merge data and save information for recovery period
mean_shark_recovery<- do.call(rbind,sharkList_recovery)
mean_shark_recovery<- data.frame(period = 'recovery',mean_shark_recovery)

# merge data and save information for post-recovery period
mean_shark_normal<- do.call(rbind,sharkList_normal )
mean_shark_normal<- data.frame(period = 'normal',mean_shark_normal)

# merge both information and save as a csv file on folder /tables
tbl<- rbind(mean_shark_recovery,mean_shark_normal)
write.csv(tbl, paste0(tableDir, 'Descriptive_Stats-Overall_Recovery_vs_PostRecovery.csv'), row.names = FALSE, quote = FALSE)
