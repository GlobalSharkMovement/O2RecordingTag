### CODE to calculate recovery time of tagged sharks
### as the point at which the mean TBF dropped below the post-6 h mean value for 20 consecutive minutes

# Set timezone to GMT
Sys.setenv(TZ='GMT')

## PACKAGES 
pack <- c('data.table','pastecs', 'dplyr','mgcv', 'ggplot2')
# lapply('pals', install.packages)
lapply(pack, require, character.only = TRUE)

## OPTIONS 
# path folders
folder<-'D:/_Papers/O2RecordingTag/'
fileDir<- paste0(folder,'/files/')
tableDir<- paste0(folder,'/tables/')

# files with acceleration-derived data (e.g., TBF) generated from IGOR analysis - 2 sharks that showed decrease trend on TBF: blue2 (S1), blue5 (S4)
files<-list.files(folder, pattern= '-accdata2.csv', recursive = TRUE)	
#files<- files[grep('blue4',files, invert = TRUE)]

# select time interval - for this example we calculated 5 minutes means
time.int<- 5
# 6 hour in minutes - 6h was selected as the general recovery response from IOSILEVSKII et al., 2022
hour_6<- (6*60)/time.int
# 20 minutes
min_20<- 20/time.int

# create list to save information generated on the loop
sharkList<- list()
recovery_timesList<- list()


# loop across each shark ID - blue2 or blue 5
for(i in 1:length(files))
  
{
  #i=1
  
  # get shark id
  sharkID<- gsub('-accdata2.csv','',basename(files[i]))
  
  # read file
  envdata<- read.csv(paste0(folder, files[i]))
  colnames(envdata)<- c('time', 'depth', 'temperature', 'oxygen', 'TBF', 'pitch1Hz', 'roll1Hz', 'odba1Hz', 'glides')
  
  # date in correct format
  envdata$time<- as.POSIXct(envdata$time)
  
  # correct negative depths on blue4
  if(sharkID == 'blue4'){ envdata$depth[envdata$depth < 0] <- 0 }

  # create pos release colum ( 5 minute window)
  envdata$pr_time <- as.numeric(cut(envdata$time, breaks= paste(time.int,"mins")))
  
  # trim to unique dates
  envdata<-envdata[envdata$pr_time < length(unique(envdata$pr_time)),]
  
  #### AGGREGATE DATA ####
  ## aggregate acceleration derived TBF in 5 minute means
  
  # remove time column
  aggregate_all<- select(envdata, -c('time'))
  
  # aggregate data by post release time window (5 min)
  aggregate_all<- aggregate_all %>%
    group_by(pr_time) %>%
    summarise_all(list(~mean(., na.rm = TRUE)))
  
  # clean data
  aggregate_all[sapply(aggregate_all, is.infinite)] <- NA
  aggregate_all[sapply(aggregate_all, is.nan)] <- NA
  
  # save shark id on a column
  aggregate_all$shark_id<- sharkID
  
  # save 5 minute TBF mean (will be used to build the final plot) 
  sharkList[[i]] <- data.frame(id = sharkID, pr_time = aggregate_all$pr_time, tbf_mean = aggregate_all$TBF)
  
  # if shark is blue 4 (no clear pattern of recovery) just save 5 minute means
  if(sharkID == 'blue4'){ next }
  
  #### RECOVERY TIME ####
  # following Iosilevskii et al., 2022
  
  # mean TBF after 6h of release
  crus_mean<-mean(aggregate_all$TBF[aggregate_all$pr_time>hour_6], na.rm =TRUE)
  aggregate_all$recovery<- ifelse(aggregate_all$TBF < crus_mean, T,F)
  aggregate_all$recovery[is.na(aggregate_all$recovery)]<- F
  
  # count time below mean post 6h release 
  aggregate_all$count<-  ave(aggregate_all$recovery, cumsum(!aggregate_all$recovery), FUN = cumsum)
  recovery_time<- aggregate_all$pr_time[aggregate_all$count == min_20][1]

  # quick plot to check recovery time (blue vertical line)
  plot(aggregate_all$TBF, type = 'b', ylim = c(0.2,0.7))
  abline(v = hour_6, col ='black')
  abline(h = crus_mean, col ='red')
  abline(v = recovery_time, col ='blue')
  
  # save recovery_time hour
  recovery_timesList[[i]]<- data.frame( id = sharkID, recovery_prt = recovery_time, recovey_hour = (recovery_time * time.int)/60, crus_mean)
  print(sharkID)

}

# dataframe with all recovery times
recovery_tbl<- do.call(rbind,recovery_timesList)

# TBF 5 minute means for both sharks merged 
shark_tbl<- do.call(rbind,sharkList)

# average mean (S.D.) recovery time 
reco_mean<- mean(recovery_tbl$recovey_hour)
reco_sd<- sd(recovery_tbl$recovey_hour)

# create final plot (Figure 8) with gam smooth
plot1<- ggplot(shark_tbl, aes(x=pr_time, y = tbf_mean,  group=id, color =id)) + labs(x = 'PRT (hours) ', y = 'Mean TBF')+
        geom_point(na.rm =TRUE) + theme_bw()+
        geom_smooth(aes( col = id, fill = id), method="gam", formula=y~s(x),na.rm = TRUE)+
        theme( panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
        coord_cartesian(ylim = c(0.2, 0.70), expand = FALSE) +
        scale_x_continuous(breaks = seq(0,192,12), labels = seq(0,16,1), limits =c(0,192))+
        geom_vline(xintercept  = recovery_tbl$recovey_hour[recovery_tbl$id == 'blue2']*12, col = 'black') +
        geom_vline(xintercept  = recovery_tbl$recovey_hour[recovery_tbl$id == 'blue5']*12, col = 'black') 
  