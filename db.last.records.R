# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# Load data and packages
#NA?

db.last.records<-function(registry, #the previously saved registry
                          level='field', #subject, field or value
                          # filename='debugging', #the base filename
                          quiet=F, #absolutely no information is printed
                          print.help=F, #no help message is printed, overridden by quiet flag
                          write.output=F #flag whether output should be written to working directory
){
  if(!quiet){
    cat(note('Running db.last.record.R ...\n'))
    if(print.help){
      cat(note('This function expects a registry as produced by db.registry()\n'))
    }
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
      cat(note('Filtering registry to include only the last record per',level,'\n'))
    }
  } else {
    stop('The level needs to be either subject, field or value')
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
    cat(warn(nrow(omit),'records were omitted because newer records replace those\n'))
  }
  
  # Write backups to disk
  if(write.output){
    write.table(keep.df,file=paste(filename,'.registry.newonly.csv',sep=''), sep=';',row.names=F)
    write.table(omit,file=paste(filename,'.registry.omitted.csv',sep=''), sep=';',row.names=F)
    if(!quiet){
      cat(note('Filtered registry has been saved as',paste(filename,'.registry.newonly.csv',sep=''),'\n'))
      cat(note('Omitted records have been saved as',paste(filename,'.registry.omitted.csv',sep=''),'\n'))
    }
  }
  
  # Prepare output
  keep.df<-keep.df[,-9]
  keep.df<-keep.df[order(keep.df$ID),]
  
  # Return filtered registry
  if(!quiet){
    cat(note('Registry with only the last records per',level,'is returned\n'))
  }
  
  return(keep.df)
}
