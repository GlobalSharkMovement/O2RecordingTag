### CODE to evaluate differences between in situ and modelled dissolved oxygen

# Set timezone to GMT
Sys.setenv(TZ='GMT')

## PACKAGES 
pack <- c('data.table','pastecs', 'dplyr','ggplot2', 'viridis')
# lapply('pals', install.packages)
lapply(pack, require, character.only = TRUE)

## OPTIONS 
# path folder
folder<-'D:/_Papers/O2RecordingTag/'
fileDir<- paste0(folder,'/files/')
tableDir<- paste0(folder,'/tables/')
cmemsDir<- paste0(folder,'/cmems/')
plotDir<- paste0(folder,'/figures/')

# files generated from IGOR analysis with acceleration-derived metrics - 3 sharks: blue2 (S1), blue4 (S3), blue5 (S4)
files<-list.files(folder, pattern= '-accdata2.csv', recursive = TRUE)	

# model data extracted from CMEMS, for tagging and pop-off date/locations for each shark (n =6)
modelData<- read.csv(paste0(cmemsDir, 'DO-SPATIAL-AVG-INTERP.csv'), header = TRUE)  
# remove X from colum names
colnames(modelData)[3:ncol(modelData)]<- gsub('X', '',colnames(modelData)[3:ncol(modelData)])
# transform to umol/L (multiply by 44.661)
modelData[,3:ncol(modelData)]<- modelData[,3:ncol(modelData)]*44.661

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
  
  # loop for each day
  for(d in 1:length(unique(envdata$day))){
  
  #d = 1
  
  # day data
  envdata_day<- envdata[envdata$day == d,]
  
  #depth bin from 1 to max depth of shark
  depthBin<- seq(1,round(max(envdata_day$depth),0),1)
  
  #find depth bin in the data
  envdata_day$dpBin <- findInterval(envdata_day$depth, depthBin, left.open = TRUE)
  
  #remove lines at surface (depth bin  = 0; DO is affected by light)
  envdata_day<- envdata_day[complete.cases(envdata_day$oxygen),]
  
  #aggregate oxygen data by 1m depth bin (to create a mean profile)
  shark_DO<- aggregate(envdata_day$oxygen, by=list(envdata_day$dpBin), mean, na.rm =TRUE)
  colnames(shark_DO)<- c('depth', 'do')
  shark_DO<- cbind(sharkID, shark_DO)
  shark_DO$day<- as.POSIXct(as.Date(envdata_day$time[1]))
  shark_DO<- shark_DO[,c(1,4,2,3)]
    
  # get model data for that specific day
  modelData_day<- modelData[as.POSIXct(modelData$date, '%d/%m/%Y', tz ='GMT') ==   unique(shark_DO$day),]
  modelData_day<-modelData_day[,-c(2)]
  
  # melt ( it gives a warning but is ok)
  modelData_day<- melt(modelData_day, id='id')
  colnames(modelData_day)<- c('id', 'depth', 'do')
  
  # remove surface also (shark data doesnt have surface data)
  modelData_day<- modelData_day[!modelData_day$depth == 0,]
  modelData_day$depth<- as.numeric(as.character(modelData_day$depth))
  
  # merge data
  difTbl<- merge(shark_DO, modelData_day[,2:3], 'depth')
  colnames(difTbl)<- c('depth','sharkID', 'day', 'shark_do', 'model_do')
  
  # save dif table
  difDataList[[d]]<- difTbl
  
  }
  
  # bind information for all days of that shark
  dataTbl[[i]]<- do.call(rbind,difDataList)
  print(sharkID)
  
}


# merge data for all day for all sharks 
dataTbl_sharks<- do.call(rbind,dataTbl)
dataTbl_sharks$depth<- as.numeric(dataTbl_sharks$depth)

# n = 6 days
unique(dataTbl_sharks$day)

## CREATE A YX RELATIONSHIP FIGURE

# create a plot for the relationship between modelled vs in situ DO, with 1:1 line
lm_plot<- ggplot(dataTbl_sharks, aes(x=shark_do, y=model_do, fill=depth)) +
  geom_point(na.rm =TRUE, shape =21, size =3, color ='black')+
  geom_abline (slope=1, linetype = "dashed", color="Red")+
  scale_fill_viridis(option = 'D') +
  theme(legend.position = 'bottom')+
  coord_cartesian(expand = FALSE) +
  theme_classic()+
  scale_y_continuous(limits = c(210,260), breaks = seq(210,260,10),labels = seq(210,260,10), name = 'Modelled DO (umol l)')+
  scale_x_continuous(limits = c(210,260), breaks = seq(210,260,10),labels = seq(210,260,10), name = 'in situ DO (umol l)')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text=element_text(size=16,  family="sans", color = 'black'),
        axis.text=element_text(colour="black"),legend.position="bottom")

# save the plot
pdf(paste0(plotDir,'lm_insitu_vs_model_do.pdf'),width=8,height=8, useDingbats=FALSE)
print(lm_plot)
dev.off()

## CREATE A MEAN OXYGEN PROFILE 

# aggregate model data into a mean profile
aggregate_modelData<- dataTbl_sharks[,c('depth', 'model_do')] %>%
  group_by(depth) %>%
  summarise_all(list(~mean(., na.rm = TRUE), ~sd(., na.rm = TRUE)))
aggregate_modelData$type<- 'model'

# aggregate in situ data into a mean profile
aggregate_insitu_tc<- dataTbl_sharks[,c('depth', 'shark_do')] %>%
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
  scale_y_continuous(limits = c(160,280), breaks = seq(160, 280, 20))  + 
  coord_flip(expand = FALSE) 

pdf(paste0(plotDir,'Meanprofiles_insitu_vs_model_do.pdf'),width=10,height=8, useDingbats=FALSE)
print(profile1)
dev.off()


## CALCULATE MEAN DIFFERENCE AND RELATIVE ERRORS ACROSS 10 METERS DEPTH BINS

# aggregate by depth (only one profile for model and in situ)
data_agg<- aggregate(dataTbl_sharks[,c(4,5)], list(dataTbl_sharks$depth), mean, na.rm =TRUE)
colnames(data_agg)<- c('depth', 'shark_do', 'model_do')

# check DO values
# DO range (0 - 200 m)
max(data_agg$shark_do[data_agg$depth <=200])
min(data_agg$shark_do[data_agg$depth <=200])

# DO range of  both profiles
max(data_agg$shark_do)
min(data_agg$shark_do)

max(data_agg$model_do)
min(data_agg$model_do)

# Get only data from 10 meters depth bins (for comparison)
depth_bin<- seq(10,400,10)
data_agg_sharks<- data_agg[data_agg$depth %in% depth_bin, ]

# apply a correlation test
corr1_sp<- cor.test(data_agg_sharks$shark_do, data_agg_sharks$model_do, method = 'spearman')
length(data_agg_sharks$shark_do) + length(data_agg_sharks$model_do)

# calculate difference and RE
data_agg_sharks$dif<- data_agg_sharks$model_do - data_agg_sharks$shark_do 
data_agg_sharks$RE<-  abs(data_agg_sharks$shark_do - data_agg_sharks$model_do)/data_agg_sharks$shark_do *100

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
data_agg$dif<- data_agg$model_do - data_agg$shark_do 

# build a difference plot 
profile2<- ggplot(data=data_agg, aes(x=-depth, y=dif)) +
  geom_point(size =2, na.rm =TRUE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(x= 'Depth', y ='DO difference (umol l-1)') +
  scale_x_continuous(limits = c(-400, 0), breaks = seq(-400,0, 100)) +
  scale_y_continuous(limits = c(-14,14), breaks = seq(-14, 14, 2))  + 
  coord_flip(expand = FALSE) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = "black")+
  geom_hline(yintercept = mean(data_agg_sharks$dif, na.rm =TRUE), 
             color = "blue")

# create figure 
pdf(paste0(plotDir,'MeanDifference_insitu_vs_model_do.pdf'),width=10,height=8, useDingbats=FALSE)
print(profile2)
dev.off()
