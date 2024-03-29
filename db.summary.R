# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# Load data and packages
#NA?

db.summary<-function(registry, #the previously saved registry
                     columns='all', #all / date / subject / field / value / source / by / verified
                     exclude=F, #values of fiels to be excluded from the output
                     save.STDOUT=F, #should the STOUT text output be saved to file?
                     quiet=F, #absolutely no information is printed
                     print.help=F, #no help message is printed, overridden by quiet flag
                     write.output=F, #flag whether output should be written to working directory
                     file.base.name='debugging'
){
  if(!quiet){
    cat(note('Running db.summary.R ...\n'))
    if(print.help){
      cat(note('This function expects a registry as produced by db.registry()\n'))
    }
  }
  
  if(exclude[1]!=F){
    if(!quiet){
      print(paste(length(exclude),'values were excluded:'))
      print(exclude)
    }
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
  cat(note('The registry has',nrow(df),'records\n'))
  cat(note('of which',nrow(unique(df[,3:4])),'are expected to be displayed in the database\n'))
  
  # Overview of the subjects
  cat(note('These subjects are recorded in the database:\n'))
  subjects<-as.data.frame(table(df[!df[,3] %in% exclude,3]))
  print(subjects)
  
  # All fields
  cat(note('These fields are used in the database:\n'))
  fields<-as.data.frame(table(df[!df[,4] %in% exclude,4]))
  print(fields)
  
  # Summarise and count all values per field
  cat(note('This is a summary of all numeric values for each field as it is in the full registry:\n'))
  df.s<-df[!df[,4] %in% exclude,]
  t<-as.data.frame(table(df.s[,5:4]),stringsAsFactors = F)
  options(warn = -1)
  t[,1]<-as.numeric(t[,1])
  options(warn = 0)
  t[,1]<-format(t[,1],scientific = F,drop0trailing=T)
  t<-t[order(t[,2],t[,1]),]
  values<-t[t$Freq>0,c(2,1,3)]
  print(values)
  
  cat(note('This is a summary of all possible values for each field as it is in the full registry:\n'))
  for (i in sort(unique(df[,4]))){
    print(i)
    df.s<-subset(df,df[,4]==i)
    print(as.data.frame(table(df.s[,5])))
  }
  
  # Overview of the sources
  cat(note('These sources were used to provide records for the database:\n'))
  sources<-as.data.frame(table(df[,6]))
  print(as.data.frame(table(df[,6])))
  
  if(save.STDOUT){
    sink()
    cat(note('All printed output is saved to',con))
  }
  
  if(write.output){
    write.table(subjects,file=paste(format(Sys.Date(),'%Y%m%d'),file.base.name,'summary.subjects.csv',sep='.'),sep=';',row.names=F)
    write.table(fields,file=paste(format(Sys.Date(),'%Y%m%d'),file.base.name,'summary.fields.csv',sep='.'),sep=';',row.names=F)
    write.table(values,file=paste(format(Sys.Date(),'%Y%m%d'),file.base.name,'summary.values.csv',sep='.'),sep=';',row.names=F)
    write.table(sources,file=paste(format(Sys.Date(),'%Y%m%d'),file.base.name,'summary.sources.csv',sep='.'),sep=';',row.names=F)
    if(!quiet){
      cat(note('Four files are written with summaries of the subjects, fields, values and sources'))
    }
  }
}
