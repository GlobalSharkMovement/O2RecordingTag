### CODE to calculate summary statistics of environmental and shark swimming metrics
## on paper, note that stat metrics for pitch, roll and odba (overall) was calculated using 30 Hz file

# Set timezone to GMT
Sys.setenv(TZ='GMT')

## PACKAGES 
pack <- c('data.table','pastecs')
# lapply('pals', install.packages)
lapply(pack, require, character.only = TRUE)

## OPTIONS 
# path folder
folder<-'D:/_Papers/O2RecordingTag/'
fileDir<- paste0(folder,'/files/')
tableDir<- paste0(folder,'/tables/')

# files generated from IGOR analysis with acceleration-derived metrics - 3 sharks: blue2 (S1), blue4 (S3), blue5 (S4)
files<-list.files(folder, pattern= '-accdata2.csv', recursive = TRUE)	

# create list to save metrics
accList<- list()
glidesList<-list()

for(i in 1:length(files))
  
{
  
  #i=1
  # read data
  accData<- fread(paste0(folder,files[i]), header = TRUE)
  accData<- as.data.frame(accData)
  #head(accData)
  
  # format dataframe
  colnames(accData)<-c('time', 'depth', 'temperature', 'oxygen', 'TBF', 'pitch1Hz', 'roll1Hz', 'odba1Hz', 'glides')
  accData$envtime<- as.POSIXct(accData$time,format = "%Y-%m-%d  %H:%M:%S")
  
  # shark name
  sharkID<- gsub('-accdata2.csv','', basename(files[i]))
  
  # calculate TBF and ODBA 95th percentiles (burst swimming definition)
  tbf_95<- quantile(accData$TBF, 0.95, na.rm =TRUE )
  odba_95<- quantile(accData$odba1Hz, 0.95, na.rm =TRUE)
  
  # new column for burst swimming
  accData$burst<-  ifelse(accData$odba >=odba_95 & accData$TBF >=tbf_95 , 1,0)
  accData$burst[is.na(accData$burst)]<- 0

  # count time during burst swimming 
  burst_time <-  sum(accData$burst[accData$burst == 1],na.rm=TRUE)
  burst_time<- burst_time/(nrow(accData))*100
  
  # remove last line with NA on gliding
  glides_tbl<- accData
  glides_tbl$glides[is.na(glides_tbl$glides)]<- 0
  
  # calculate burst time
  Ngl<- length(glides_tbl$glides[glides_tbl$glides==1])
  Zgl<- length(glides_tbl$glides[glides_tbl$glides==0])
  Tgl<- length(glides_tbl$glides)
  
  # save glides stat
  glidesList[[i]]<- data.frame(id=sharkID, Nglides=  Ngl, Total=Tgl,
                               Pglides = Ngl/Tgl*100)

  # calculate summary statistics for all metrics 
  sumTbl<- stat.desc(accData[,c('depth','temperature', 'oxygen','TBF')])
  sumTbl<- sumTbl[grep('min|max|range|median|mean|std.dev', rownames(sumTbl)),]
  sumTbl<- sumTbl[grep('SE|CI', rownames(sumTbl), invert = TRUE),]
  
  # format table
  sumTbl$id<- sharkID
  sumTbl$stat<- rownames(sumTbl)
  sumTbl<- sumTbl[,c('id','stat', 'depth','temperature', 'oxygen','TBF')]
  rownames(sumTbl) <- NULL
  
  # save information
  accList[[i]]<- sumTbl
  print(burst_time)
  
}

# merge all individual shark information and save
accTbl<- do.call(rbind,accList)
write.csv(accTbl, paste0(tableDir,'Overall-Summary_Statistics.csv'), row.names = FALSE, quote = FALSE)  

# merge all glide information and save
glidesTbl<- do.call(rbind,glidesList)
write.csv(glidesTbl, paste0(tableDir,'Glides-Summary_Statistics.csv'), row.names = FALSE, quote = FALSE)  

# calculate overall mean burst
# burst_all<- c(2.38, 3.20, 2.74)
# mean(burst_all)
# sd(burst_all)