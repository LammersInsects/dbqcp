# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# source('C:/Users/mls241/surfdrive/Common_scripts/R_startup_script.R') #VU
# source("C:/DATA/SURFdrive/Common_scripts/R_startup_script.R") #Thuis
# source("E:/SURFdrive/Common_scripts/R_startup_script.R") #Yoga
# 
# setwd(paste(wd.base, '[SURFDRIVE SUBFOLDER]', sep=''))

# Load data and packages
#NA?

db.build<-function(registry, #the previously saved registry
                   include.date=T, #whether the date of each record should be displayed in the database
                   filename='debugging', #the base filename
                   quiet=F
){
  if(!quiet){
    print('Running db.build.R ...')
    print('This function expects a registry as produced by db.registry()')
  }
  
  # Checks before anything can be done
  if(db.is.registry(registry = registry, quiet = T)){
    df<-registry
  } else {
    db.is.registry(registry = registry, quiet=F)
    stop()
  }
  
  # Limit registry to only the most recent records per field
  keep.df<-db.last.records(registry = df, level = 'field', quiet = quiet)
  # df$combo<-paste(df[,3],df[,4],sep='_')
  # table<-as.data.frame(table(df$combo))
  # keep.df<-subset(df,df$combo %in% subset(table, table$Freq==1)[,1])
  # process.df<-subset(df,df$combo %in% subset(table, table$Freq>1)[,1])
  # process.df<-process.df[order(process.df$combo),]
  # omit<-data.frame()
  # for (i in unique(process.df$combo)){
  #   todo<-subset(df,df$combo==i)
  #   todo<-todo[order(as.Date(todo[,2],format='%Y-%m-%d'),decreasing=T),]
  #   keep.df<-rbind(keep.df,todo[1,])
  #   omit<-rbind(omit,todo[2:nrow(todo),])
  # }
  # 
  # if(!quiet){
  #   print(paste(nrow(omit),'records were omitted because newer records replace those'))
  # }
  
  # Construct database
  # reshape(data=keep.df[,2:4], idvar='Soortnr', timevar=c('Kolom'), direction='wide')
  if(include.date){
    db<-reshape(data=keep.df[,2:5], idvar=colnames(keep.df)[3], timevar=colnames(keep.df)[4], direction='wide')
  } else {
    db<-reshape(data=keep.df[,3:5], idvar=colnames(keep.df)[3], timevar=colnames(keep.df)[4], direction='wide')
    colnames(db)<-gsub(paste(colnames(df)[5],'.',sep=''),'',colnames(db))
  }
  
  #Include a possibility that columns are numeric (including subjects)
  test.dt<-sapply(db, data.type, quiet = T) #Test what kind of data types are used
  test.dt.interpr<-test.dt['numeric',]>0 & (test.dt['empty',]+test.dt['numeric',])==1
  if(any(test.dt.interpr)){ #When any columns is always numeric or empty
    db[,test.dt.interpr]<-sapply(db[,test.dt.interpr], as.numeric) #automatically convert it as such
  }
  
  #Sort database by subject
  db<-db[order(db[,1]),]
  
  # write.table(keep.df,file=paste(filename,'.registry.newonly.csv',sep=''), sep=';',row.names=F)
  # if(!quiet){
  #   print(paste('Filtered registry has been saved as',paste(filename,'.registry.newonly.csv',sep='')))
  # }
  # write.table(omit,file=paste(filename,'.registry.omitted.csv',sep=''), sep=';',row.names=F)
  # if(!quiet){
  #   print(paste('Omitted records have been saved as',paste(filename,'.registry.omitted.csv',sep='')))
  # }
  
  
  # Return database
  write.table(db,file=paste(filename,'db.csv',sep='.'),sep=';',row.names=F)
  if(!quiet){print(paste('Constructed database is returned and saved as ',filename,'.db.csv',sep=''))
  }
  return(db)
}