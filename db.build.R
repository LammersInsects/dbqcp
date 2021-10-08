# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# Load data and packages
#NA?

db.build<-function(registry, #the previously saved registry
                   include.date=T, #whether the date of each record should be displayed in the database
                   filename='debugging', #the base filename
                   quiet=F, #absolutely no information is printed
                   print.help=F #no help message is printed, overridden by quiet flag
){
  if(!quiet){
    cat(note('Running db.build.R ...\n'))
    if(print.help){
      cat(note('This function expects a registry as produced by db.registry()\n'))
    }
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
  test.dt.interpr<-test.dt['numeric',]>0 & (test.dt['empty',]+test.dt['numeric',])>0.999
  if(any(test.dt.interpr)){ #When any columns is always numeric or empty
    db[,test.dt.interpr]<-sapply(db[,test.dt.interpr], as.numeric) #automatically convert it as such
  }
  
  #Sort database by subject
  db<-db[order(db[,1]),]
  
  # Return database
  write.table(db,file=paste(filename,'db.csv',sep='.'),sep=';',row.names=F)
  if(!quiet){
    cat(note('Constructed database is returned and saved as ',filename,'.db.csv\n',sep=''))
  }
  return(db)
}