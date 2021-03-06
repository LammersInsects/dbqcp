# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# Load data and packages
#NA?

db.remove<-function(registry, #the previously saved registry
                     remove.IDs, #IDs of the records to remove
                     invert=F, #include everything except the remove.IDs, invert=TRUE gives only the records that conform to remove.IDs
                     filename='debugging' #the base filename
){
  cat(note('Running db.remove.R ...\n'))
  cat(note('This function expects a registry as produced by db.registry() and a set of values to remove\n'))
  
  # Checks before anything can be done
  if(db.is.registry(registry = registry, quiet=T)){
    df<-registry
  } else {
    db.is.registry(registry = registry, quiet=F)
    stop()
  }
  to.remove<-remove.IDs
  
  # Check if today files under the same filename have already been removed If so, load that file and append
  files<-list.files(getwd())
  fullfilename<-paste(today,filename,'removed.csv',sep='.')
  if(fullfilename %in% files){
    cat(warn('Records were already removed earlier today. The newly removed records are appended.\n'))
    removed<-read.table(fullfilename,header=T,sep=';')
    test<-is.na(as.Date(removed[1,2], format='%d-%m-%Y'))
    if(test){
      removed[,2]<-as.Date(removed[,2], format='%d-%m-%Y')
    } else {
      removed[,2]<-as.Date(removed[,2])
    }
  } else {
    removed<-data.frame()
  }
  
  # Get records to remove
  remove<-df[,1] %in% to.remove#[,1]
  if(invert){
    remove<-!remove
    cat(warn('Only keeping records that would have been removed because invert=TRUE\n'))
  }
  cat(note(sum(remove),'records were removed, but a backup of these is saved in',fullfilename,'\n'))
  omit<-df[remove,]
  
  # Produce the new registry
  keep<-df[!remove,]
  cat(note('Registry without the removed records is returned\n'))
  
  # Overwrite the old registry
  # OR MAYBE BETTER NOT?
  
  # Save removed records in a file
  removed<-rbind(removed,omit)
  removed<-unique(removed)
  write.table(removed,file=fullfilename,sep=';',row.names=F)
  
  return(keep)
}