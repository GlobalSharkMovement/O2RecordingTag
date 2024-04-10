### CODE to calculate depth of 75th percentile and time at each percentile of temperature and dissolved oxygen

#Set timezone to GMT
Sys.setenv(TZ='GMT')

## PACKAGES 
pack <- c('data.table','pastecs', 'ggplot2')
# lapply('pals', install.packages)
lapply(pack, require, character.only = TRUE)

## OPTIONS 
# path folders
folder<-'D:/_Papers/O2RecordingTag/'
fileDir<- paste0(folder,'/files/')
tableDir<- paste0(folder,'/tables/')

# files generated from IGOR analysis with acceleration-derived metrics - 3 sharks: blue2 (S1), blue4 (S3), blue5 (S4)
files<-list.files(folder, pattern= '-accdata2.csv', recursive = TRUE)	

# create list to save information generated on the loop (by shark)
envList<- list()

for(i in 1:length(files)){
  
  #i=1
  
  # get shark id
  sharkID<- gsub('-accdata2.csv','', basename(files[i]))

  # read file
  envdata<- read.csv(paste0(folder,files[i]), header = TRUE)
  
  # correct colnames
  colnames(envdata)<- c('time', 'depth', 'temperature', 'oxygen', 'TBF', 'pitch1Hz', 'roll1Hz', 'odba1Hz', 'glides')
  
  # remove negative depths for blue 4
  if(sharkID == 'blue4'){ envdata$depth[envdata$depth <0]<- 0}
  
  # time in correct format
  envdata$time<- as.POSIXct(envdata$time, tz ='GMT')

  # get only column to evaluate percentiles of temperature and DO
  envdata<- envdata[,c(1,2,3,4)]
  
  # include shark name and save information in a list
  envdata$id<- sharkID
  envList[[i]]<- envdata
  
  # check time spent on the percentiles (25/75th) calculated below (all shark data) 
  envdata_tempPerc<- envdata[envdata$temperature >=17 & envdata$temperature <= 21.9,] 
  print(nrow(envdata_tempPerc)/nrow(envdata)*100)
  
  envdata_doPerc<- envdata[envdata$oxygen >= 235 & envdata$oxygen <= 250 ,]
  print(nrow(envdata_doPerc)/nrow(envdata)*100)
  
  envdata_doPerc75<- envdata[envdata$oxygen > 250 ,]
  print(nrow(envdata_doPerc75)/nrow(envdata)*100)
  
  print('---')
  
}
  
# merge all envdata
envAll<- do.call(rbind,envList)
  
## PERCENTILES OF DO AND TEMPERATURE

# calculate 25 and 75th percentile (will be used inside the loop)
quantile(envAll$oxygen, 0.75, na.rm = TRUE)
quantile(envAll$oxygen, 0.25, na.rm = TRUE)
quantile(envAll$temperature, 0.75, na.rm = TRUE)
quantile(envAll$temperature, 0.25, na.rm = TRUE)
  
# mean tad values 
# 25/75th percentile of temperature
mean(c(97.47,89.13, 49.96))
sd(c(97.47,89.13, 49.962))

# 25/75th percentile of DO
mean(c(93.74, 67.85, 34.20))
sd(c(93.74, 67.85, 34.20))

# higher than DO 75th percentile (only two sharks)
mean(c(31.88,  34.31))
sd(c(31.88,  34.31))
  

## CALCULATE DEPTH OF 75TH PERCENTILES OF DO 

# create an average DO profile for each shark
envAll$depth_mean <- round(envAll$depth,0)
profileDO<- aggregate(oxygen ~ depth_mean + id, data = envAll, FUN = mean, na.rm = TRUE)

# check the plot
plot1<- ggplot(profileDO,aes(x=oxygen,y=-depth_mean, group = id, col = as.factor(id)), shape = 16) + geom_point(size =2)+ 
  facet_wrap(~id,scale="free_x") + scale_colour_manual(name = 'Dive ID',values = c("#5E81C2", "#002458", "#D7A31A")) +
  #scale_shape_manual(values=c(21, 24))+
  labs(x= 'DO (umol/L)', y ='Depth (m)', shape = 'Dive Phase') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 

# check aproximately depth of 75th percentile of DO
profile_per75<- profileDO[round(profileDO$oxygen,0) == 245,]
mean(profile_per75$depth_mean[profile_per75$depth_mean>50],na.rm =TRUE)
# round to 85 meter (similar to end of thermocline)

