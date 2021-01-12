# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# source('C:/Users/mls241/surfdrive/Common_scripts/R_startup_script.R') #VU
# source("C:/DATA/SURFdrive/Common_scripts/R_startup_script.R") #Thuis
# source("E:/SURFdrive/Common_scripts/R_startup_script.R") #Yoga
# 
# setwd(paste(wd.base, '[SURFDRIVE SUBFOLDER]', sep=''))

# Load data and packages
#NA?

db.last.records<-function(registry, #the previously saved registry
                          level='field', #subject, field or value
                          # filename='debugging', #the base filename
                          quiet=F
){
  if(!quiet){
    print('Running db.last.record.R ...')
    print('This function expects a registry as produced by db.registry()')
  }
  
  
  # Checks before anything can be done
  if(db.is.registry(registry = registry, quiet=T)){
    df<-registry
  } else {
    db.is.registry(registry = registry, quiet=F)
    stop()
  }
  
  if(level %in% c('subject','field','value')){
    if(!quiet){
      print(paste('Filtering registry to include only the last record per',level))
    }
  } else {
    print('The level needs to be either subject, field or value')
    stop()
  }
  
  # Limit registry to only the most recent records per field
  if(level == 'subject'){
    df$combo<-df[,3]
  }
  if(level == 'field'){
    df$combo<-paste(df[,3],df[,4],sep='_')
  }
  if(level == 'value'){
    df$combo<-paste(df[,3],df[,4], df[,5],sep='_')
  }
  
  table<-as.data.frame(table(df$combo))
  keep.df<-subset(df,df$combo %in% subset(table, table$Freq==1)[,1])
  process.df<-subset(df,df$combo %in% subset(table, table$Freq>1)[,1])
  process.df<-process.df[order(process.df$combo),]
  omit<-data.frame()
  for (i in unique(process.df$combo)){
    todo<-subset(process.df,process.df$combo==i)
    todo<-todo[order(as.Date(todo[,2],format='%Y-%m-%d'),decreasing=T),] #sort all selected records by date
    todo.s<-todo[todo[,2]==todo[1,2],] #subset all records that have the latest date
    if(nrow(todo.s)>1){#if there is more than one record remaining
      keep<-todo.s[todo.s$ID==max(todo.s$ID),]#keep only the record with the highest ID
    } else {
      keep<-todo[1,]
    }
    keep.df<-rbind(keep.df,keep)
    omit<-rbind(omit,todo[todo$ID!=keep$ID,])
  }
  
  if(!quiet){
    print(paste(nrow(omit),'records were omitted because newer records replace those'))
  }
  
  # Write backups to disk
  write.table(keep.df,file=paste(filename,'.registry.newonly.csv',sep=''), sep=';',row.names=F)
  if(!quiet){
    print(paste('Filtered registry has been saved as',paste(filename,'.registry.newonly.csv',sep='')))
  }
  write.table(omit,file=paste(filename,'.registry.omitted.csv',sep=''), sep=';',row.names=F)
  if(!quiet){
    print(paste('Omitted records have been saved as',paste(filename,'.registry.omitted.csv',sep='')))
  }
  
  # Prepare output
  keep.df<-keep.df[,-9]
  keep.df<-keep.df[order(keep.df$ID),]
  
  # Return filtered registry
  if(!quiet){
    print(paste('Registry with only the last records per ',level,' is returned',sep=''))
  }
  
  return(keep.df)
}
