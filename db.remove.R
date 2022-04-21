# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# Load data and packages
#NA?

db.remove<-function(registry, #the previously saved registry
                    remove.IDs, #IDs of the records to remove
                    invert=F, #include everything except the remove.IDs, invert=TRUE gives only the records that conform to remove.IDs
                    quiet=F, #absolutely no information is printed
                    print.help=F, #no help message is printed, overridden by quiet flag
                    write.output=F, #flag whether output should be written to working directory
                    file.base.name='debugging'
){
  if(!quiet){
    cat(note('Running db.remove.R ...\n'))
    if(print.help){
      cat(note('This function expects a registry as produced by db.registry() and a set of values to remove\n'))
      cat(warn('Functionality of this function will be taken over by db.create.action and db.process.actions!\n'))
    }
  }
  
  # Checks before anything can be done
  if(db.is.registry(registry = registry, quiet=T)){
    df<-registry
  } else {
    db.is.registry(registry = registry, quiet=F)
    stop()
  }
  to.remove<-remove.IDs
  
  # Check if today files under the same file.base.name have already been removed If so, load that file and append
  files<-list.files(getwd())
  full.file.name<-paste(format(Sys.Date(),'%Y%m%d'),file.base.name,'removed.csv',sep='.')
  if(full.file.name %in% files){
    cat(warn('Records were already removed earlier today. The newly removed records are appended.\n'))
    removed<-read.table(full.file.name,header=T,sep=';')
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
  if(!quiet){
    cat(note(sum(remove),'records were removed.\n'))
  }
  omit<-df[remove,]
  
  # Produce the new registry
  keep<-df[!remove,]
  if(!quiet){
    cat(note('Registry without the removed records is returned\n'))
  }
  
  # Overwrite the old registry
  # OR MAYBE BETTER NOT?
  
  # Save removed records in a file
  removed<-rbind(removed,omit)
  removed<-unique(removed)
  if(write.output){
    write.table(removed,file=full.file.name,sep=';',row.names=F)
    if(!quiet){
      cat(note(' but a copy of these records is saved in',full.file.name,'\n'))
    }
  }
  
  return(keep)
}
