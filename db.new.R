# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2020. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data 

# Reformat data

# Define function
# new.records<-pot.new
db.new<-function(new.records, #the records to be used for generating a new database
                 filename='debugging' #the base filename
){
  print('Running db.new.R ...')
  print('This function expects a dataframe with max 5 columns:')
  print('  Date recorded -- Subject -- Field -- Value [-- Source]   (header is compulsory)')
  
  # Checks before anything can be done
  new.records<-new.records[,1:5]
  #TODO use db.input.qc() to do all QC checks >under construction
  
  # Remove NAs
  nosubject<-emptyvalues(new.records[,2])
  nofield<-emptyvalues(new.records[,3])
  novalue<-emptyvalues(new.records[,4])
  remove<-nosubject | nofield | novalue
  if(sum(remove)>0){
    print(paste(sum(remove),'records with missing Subject, Field or Value found. These are removed from the registry.'))
    print(new.records[remove,])
    print('NOTE: If this is unexpected; verify input and re-run db.new()!')
    # print(new.records[remove,])
    new.records<-new.records[!remove,]
  }
  
  # res<-as.data.frame(matrix(nrow = 1, ncol=8))
  # colnames(res)<-c('ID',colnames(new.records),'Recorded.by','Verified')
  
  res<-data.frame('ID'=1,
                   new.records[1,],
                   'Recorded.by'='admin',
                   'Verified'=0)
  # write.table(res, file=paste(filename,'.registry.csv',sep=''), sep=';',row.names=F)
  res[,2]<-read.date.format(res[,2])
  
  registry<-db.registry(existing.data.registry = res, new.records = new.records[2:nrow(new.records),], filename = filename)
  
  print('The new database is stored in the working directory, i.e.')
  print(getwd())
  
  return(registry)
}
