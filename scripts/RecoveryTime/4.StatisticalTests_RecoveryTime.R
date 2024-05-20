### CODE to apply statistical tests comparisons (Mann-Whitney U test ) between pre and post-transition (normal) period 
### for TBF (both blue2 and blue5) and environmental variables (blue 5)

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

# loop across each shark ID - blue2 or blue 5
for(i in 1:length(files)){
  
  #i=1
  
  # get shark id
  sharkID<- gsub('-accdata2.csv','', basename(files[i]))
  
  # read file
  envdata<- read.csv(paste0(folder,files[i]), header = TRUE)
  
  # colnames
  colnames(envdata)<- c('time', 'depth', 'temperature', 'oxygen', 'TBF', 'pitch1Hz', 'roll1Hz', 'odba1Hz', 'glides')
  
  # remove negative depths for blue 4
  if(sharkID == 'blue4'){ envdata$depth[envdata$depth <0]<- 0}
  
  # time in correct format
  envdata$time<- as.POSIXct(envdata$time)
  
  # shark ID
  envdata$id<- sharkID
  
  # split into 5 minute 
  envdata$split<- cut(envdata$time, '5 mins')

  # get calculateed recovery hour for S1 (blue2) and S4 (blue5)
  if(sharkID == 'blue2'){ recovery_hour<-envdata$time[1] + (2.16*60*60) } else{ recovery_hour<-envdata$time[1] + (5.08*60*60)}
  
  # split main dataframe into recovery and post recovery
  recovery_tbl<- envdata[envdata$time<recovery_hour,]
  normal_tbl<- envdata[envdata$time>=recovery_hour,]
  
  # aggregate
  recovery_tbl<- aggregate(recovery_tbl[2:8], by = list(recovery_tbl$split), mean, na.rm =TRUE)
  normal_tbl<- aggregate(normal_tbl[2:8], by = list(normal_tbl$split), mean, na.rm =TRUE)
  
  # remove NA
  recovery_tbl<-  na.omit(recovery_tbl) 
  normal_tbl<-  na.omit(normal_tbl) 

  # TBF (tail-beat frequency)
  # boxplot and Wilcox - Mann-Whitney U test comparisons between recovery and post-recovery phase
  # on paper, reported individual comparison for shark blue2 (S1) and blue 5(S4)
  boxplot(recovery_tbl$TBF, normal_tbl$TBF, names=c("Recovery","Crusing"), ylim=c(0,1))
  wilcox.test(recovery_tbl$TBF, normal_tbl$TBF)
  length(recovery_tbl$TBF) + length(normal_tbl$TBF)
  
  # if shark is S4 (blue 5) we also reported statistical comparisons for oxygen and temperature levels
  if( sharkID == 'blue5'){
    
    # temperature
    boxplot(recovery_tbl$temperature, normal_tbl$temperature, names=c("Recovery","Crusing"))
    wilcox.test(recovery_tbl$temperature, normal_tbl$temperature)
    length(recovery_tbl$temperature) + length(normal_tbl$temperature)
    
    # dissolved oxygen
    boxplot(recovery_tbl$oxygen, normal_tbl$oxygen, names=c("Recovery","Crusing"))
    wilcox.test(recovery_tbl$oxygen,  normal_tbl$oxygen, alternative = 'two.sided')
    length(recovery_tbl$oxygen) + length(normal_tbl$oxygen)
    
  }
  
#print(sharkID)  
   
}
  
