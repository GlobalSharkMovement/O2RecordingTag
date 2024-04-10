### CODE to evaluate differences between in situ and modelled water temperature

# Set timezone to GMT
Sys.setenv(TZ='GMT')

## PACKAGES 
pack <- c('data.table','pastecs', 'dplyr','ggplot2','viridis')
# lapply('pals', install.packages)
lapply(pack, require, character.only = TRUE)

## OPTIONS 
# path folderS
folder<-'D:/_Papers/O2RecordingTag/'
fileDir<- paste0(folder,'/files/')
tableDir<- paste0(folder,'/tables/')
cmemsDir<- paste0(folder,'/cmems/')
plotDir<- paste0(folder,'/figures/')

# files generated from IGOR analysis - 3 sharks: blue2 (S1), blue4 (S3), blue5 (S4)
files<-list.files(folder, pattern= '-accdata2.csv', recursive = TRUE)	

# model data folder
modelData<- read.csv(paste0(cmemsDir, 'thetao-SPATIAL-AVG-INTERP.csv'), header = TRUE)  
# remove X from colum names
colnames(modelData)[3:ncol(modelData)]<- gsub('X', '',colnames(modelData)[3:ncol(modelData)])

# create list to save information generated on the loop (by shark)
dataTbl<- list()

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
  
  # define midnight (used for splitting in situ DO data into tagging and pop off, to match modelled data )
  midnight <- format(envdata$time[nrow(envdata)], '%Y-%m-%d 00:00:00')
  
  # split dataframe in tagging day/ pop-off day
  envdata$day<- ifelse(envdata$time < midnight, 1,2)
  
  # create list to save information generated on the loop (by day)
  difDataList<- list()
  
  for(d in 1:length(unique(envdata$day))){
  
 # d=1  
    
  # day data
  envdata_day<- envdata[envdata$day == d,]
    
  # depth bin from 1 to max depth of shark
  depthBin<- seq(1,round(max(envdata_day$depth),0),1)
    
  # find depth bin in the data
  envdata_day$dpBin <- findInterval(envdata_day$depth, depthBin, left.open = TRUE)
    
  # remove lines at surface (depth bin  = 0; DO is affected by light)
  envdata_day<- envdata_day[complete.cases(envdata_day$oxygen),]
    
  # aggregate by depth bin (to create a mean profile)
  shark_temp<- aggregate(envdata_day$temperature, by=list(envdata_day$dpBin), mean, na.rm =TRUE)
  colnames(shark_temp)<- c('depth', 'temp')
  shark_temp<- cbind(sharkID, shark_temp)
  shark_temp$day<- as.POSIXct(as.Date(envdata_day$time[1]))
  shark_temp<- shark_temp[,c(1,4,2,3)]
    
  # get model data for that specific day
  modelData_day<- modelData[as.POSIXct(modelData$date, '%d/%m/%Y', tz ='GMT') ==   unique(shark_temp$day),]
  modelData_day<-modelData_day[,-c(2)]
  
  # melt ( give a warning but is ok)
  modelData_day<- melt(modelData_day, id='id')
  colnames(modelData_day)<- c('id', 'depth', 'temp')
  
  # remove surface also (shark data doesnt have surface)
  modelData_day<- modelData_day[!modelData_day$depth == 0,]
  modelData_day$depth<- as.numeric(as.character(modelData_day$depth))
  
  # merge mean in situ with modelled profile
  difTbl<- merge(shark_temp, modelData_day[,2:3], 'depth')
  colnames(difTbl)<- c('depth','sharkID', 'day', 'shark_temp', 'model_temp')
  
  # save data on list
  difDataList[[d]]<- difTbl
  
  }
  
  # bind information for all days of that shark
  dataTbl[[i]]<- do.call(rbind,difDataList)
  print(sharkID)
  
}


# get all daily profiles for all sharks together 
dataTbl_sharks<- do.call(rbind,dataTbl)
dataTbl_sharks$depth<- as.numeric(dataTbl_sharks$depth)

# n = 6 days
unique(dataTbl_sharks$day)

lm_plot<- ggplot(dataTbl_sharks, aes(x=shark_temp, y=model_temp, fill=depth)) +
  geom_point(na.rm =TRUE, shape =21, size =3, color ='black')+
  geom_abline (slope=1, linetype = "dashed", color="Red")+
  scale_fill_viridis(option = 'D') +
  theme(legend.position = 'bottom')+
  coord_cartesian(expand = FALSE) +
  theme_classic()+
  scale_y_continuous(limits = c(12,24), breaks = seq(12,24,2),labels = seq(12,24,2), name = 'Modelled DO (umol l)')+
  scale_x_continuous(limits = c(12,24), breaks = seq(12,24,2),labels = seq(12,24,2),  name = 'in situ DO (umol l)')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text=element_text(size=16,  family="sans", color = 'black'),
        axis.text=element_text(colour="black"),legend.position="bottom")

pdf(paste0(plotDir,'lm_insitu_vs_model_tc.pdf'),width=8,height=8, useDingbats=FALSE)
print(lm_plot)
dev.off()

## CREATE A MEAN TEMPERATURE PROFILE 

# aggregate model data into a mean profile
aggregate_modelData<- dataTbl_sharks[,c('depth', 'model_temp')] %>%
  group_by(depth) %>%
  summarise_all(list(~mean(., na.rm = TRUE), ~sd(., na.rm = TRUE)))
aggregate_modelData$type<- 'model'

# aggregate in situ data into a mean profile
aggregate_insitu_tc<- dataTbl_sharks[,c('depth', 'shark_temp')] %>%
  group_by(depth) %>%
  summarise_all(list(~mean(., na.rm = TRUE), ~sd(., na.rm = TRUE)))
aggregate_insitu_tc$type<- 'shark'

# prepare plot data
plotData<- rbind(aggregate_insitu_tc, aggregate_modelData)
plotData$psd<- plotData$mean + plotData$sd
plotData$nsd<- plotData$mean - plotData$sd

# plot mean profiles
profile1<- ggplot(data=plotData, aes(x=-depth, y=mean, ymin=nsd, ymax=psd)) +
  geom_point(size =1, na.rm =TRUE) + 
  geom_ribbon(fill ='#2166AC',alpha=0.3, na.rm = TRUE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(x= 'Depth', y ='DO (umol l-1)') +
  facet_wrap(~type) +
  scale_x_continuous(limits = c(-403, 0), breaks = seq(-400,0, 100)) +
  scale_y_continuous(limits = c(10,25), breaks = seq(10, 25, 5))  + 
  coord_flip(expand = FALSE) 

pdf(paste0(plotDir,'Meanprofiles_insitu_vs_model_tc.pdf'),width=10,height=8, useDingbats=FALSE)
print(profile1)
dev.off()

## CALCULATE MEAN DIFFERENCE AND RELATIVE ERRORS ACROSS 10 METERS DEPTH BINS

# aggregate by depth (only one profile for model and in situ)
data_agg<- aggregate(dataTbl_sharks[,c(4,5)], list(dataTbl_sharks$depth), mean, na.rm =TRUE)
colnames(data_agg)<- c('depth', 'shark_temp', 'model_temp')
data_agg$dif<- data_agg$model_temp - data_agg$shark_temp 

# DO range of  both profiles
max(data_agg$shark_temp)
min(data_agg$shark_temp)

max(data_agg$model_temp)
min(data_agg$model_temp)


# Get only data from 10 meters depth bins
depth_bin<- seq(10,400,10)
data_agg_sharks<- data_agg[data_agg$depth %in% depth_bin, ]

# correlation test
corr1_sp<- cor.test(data_agg_sharks$shark_temp, data_agg_sharks$model_temp, method = 'spearman')
length(data_agg_sharks$shark_temp) + length(data_agg_sharks$model_temp)

# calculate difference and relative errors
data_agg_sharks$dif<- data_agg_sharks$model_temp - data_agg_sharks$shark_temp 
data_agg_sharks$RE<-  abs(data_agg_sharks$shark_temp - data_agg_sharks$model_temp)/data_agg_sharks$shark_temp *100

# get mean differences for complete profile
mean(data_agg_sharks$dif, na.rm =TRUE)
sd(data_agg_sharks$dif, na.rm =TRUE)

# get mean relative errors for complete profile
mean(data_agg_sharks$RE, na.rm =TRUE)
sd(data_agg_sharks$RE, na.rm =TRUE)

# mean difference for top (<=85 m) vs below
mean(data_agg_sharks$dif[data_agg_sharks$depth <= 85], na.rm =TRUE)
sd(data_agg_sharks$dif[data_agg_sharks$depth <= 85], na.rm =TRUE)
mean(data_agg_sharks$dif[data_agg_sharks$depth > 85], na.rm =TRUE)
sd(data_agg_sharks$dif[data_agg_sharks$depth > 85], na.rm =TRUE)

# relative error for top (<=85 m) vs below
mean(data_agg_sharks$RE[data_agg_sharks$depth <= 85], na.rm =TRUE)
sd(data_agg_sharks$RE[data_agg_sharks$depth <= 85], na.rm =TRUE)
mean(data_agg_sharks$RE[data_agg_sharks$depth > 85], na.rm =TRUE)
sd(data_agg_sharks$RE[data_agg_sharks$depth > 85], na.rm =TRUE)


## CREATE A DIFFERENCE PLOT

# calculate difference between model vs in situ ( no depth bins used here)
data_agg$dif<- data_agg$model_temp - data_agg$shark_temp 

profile2<- ggplot(data=data_agg, aes(x=-depth, y=dif)) +
  geom_point(size =2, na.rm =TRUE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(x= 'Depth', y ='Temperature (ºC)') +
  scale_x_continuous(limits = c(-400, 0), breaks = seq(-400,0, 100)) +
  scale_y_continuous(limits = c(-2,2), breaks = seq(-2, 2, 0.5))  + 
  coord_flip(expand = FALSE) +
  geom_hline(yintercept = 0, linetype = 'dashed',
             color = "black")+
  geom_hline(yintercept = mean(data_agg_sharks$dif, na.rm =TRUE), 
             color = "blue")

# create figure 
pdf(paste0(plotDir,'MeanDifference_insitu_vs_model_tc.pdf'),width=10,height=8, useDingbats=FALSE)
print(profile2)
dev.off()
