# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# Load data and packages
#NA?

db.summary<-function(registry, #the previously saved registry
                     columns='all', #all / date / subject / field / value / source / by / verified
                     exclude=F, #values of fiels to be excluded from the output
                     save.STDOUT=F, #should the STOUT text output be saved to file?
                     filename='debugging' #the base filename
){
  print('Running db.summary.R ...')
  print('This function expects a registry as produced by db.registry()')
  
  if(exclude[1]!=F){
    print(paste(length(exclude),'values were excluded:'))
    print(exclude)
  }
  
  # Checks before anything can be done
  if(db.is.registry(registry = registry, quiet=T)){
    df<-registry
  } else {
    db.is.registry(registry = registry, quiet=F)
    stop()
  }
  
  # Optional to save output to file
  if(save.STDOUT){
    con='db.summary.log'
    sink(con)
  }
  
  # Very basic summary statistics
  print(paste('The registry has',nrow(df),'records'))
  print(paste('of which',nrow(unique(df[,3:4])),'are expected to be displayed in the database'))
  
  # Overview of the subjects
  print('These subjects are recorded in the database:')
  subjects<-as.data.frame(table(df[!df[,3] %in% exclude,3]))
  print(subjects)
  
  # All fields
  print('These fields are used in the database:')
  fields<-as.data.frame(table(df[!df[,4] %in% exclude,4]))
  print(fields)
  
  # Summarise and count all values per field
  print('This is a summary of all numeric values for each field as it is in the full registry:')  
  df.s<-df[!df[,4] %in% exclude,]
  t<-as.data.frame(table(df.s[,5:4]),stringsAsFactors = F)
  options(warn = -1)
  t[,1]<-as.numeric(t[,1])
  options(warn = 0)
  t[,1]<-format(t[,1],scientific = F,drop0trailing=T)
  t<-t[order(t[,2],t[,1]),]
  values<-t[t$Freq>0,c(2,1,3)]
  print(values)
  
  print('This is a summary of all possible values for each field as it is in the full registry:')  
  for (i in sort(unique(df[,4]))){
    print(i)
    df.s<-subset(df,df[,4]==i)
    print(as.data.frame(table(df.s[,5])))
  }
  
  # Overview of the sources
  print('These sources were used to provide records for the database:')
  sources<-as.data.frame(table(df[,6]))
  print(as.data.frame(table(df[,6])))
  
  if(save.STDOUT){
    sink()
    print(paste('All printed output is saved to',con))
  }
  
  write.table(subjects,file=paste(today,filename,'summary.subjects.csv',sep='.'),sep=';',row.names=F)
  write.table(fields,file=paste(today,filename,'summary.fields.csv',sep='.'),sep=';',row.names=F)
  write.table(values,file=paste(today,filename,'summary.values.csv',sep='.'),sep=';',row.names=F)
  write.table(sources,file=paste(today,filename,'summary.sources.csv',sep='.'),sep=';',row.names=F)
}