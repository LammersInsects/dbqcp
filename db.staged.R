# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2021. Released under the terms of the GNU General Public License v3.

# Define function
db.staged<-function(database.folder=getwd(), #The folder holding the database files
                    file.base.name='debugging', #the base name of the database files
                    return.staged.records=F, #whether the staged records should be returned
                    quiet=F, #absolutely no information is printed
                    print.help=F #no help message is printed, overridden by quiet flag
){
  if(!quiet){
    cat(note('Running db.staged.R ...\n'))
    if(print.help){
      cat(note('This function expects a name for the folder that holds the database files\n'))
    }
  }
  
  #checks before anything can be done
  if(!dir.exists(database.folder)){
    cat(error('Provided path to the database folder does not exist!\n'))
    stop()
  }
  
  full.file.path<-paste(database.folder,'/',file.base.name,'.staged.csv',sep='')
  if(!file.exists(full.file.path)){
    if(!quiet){
      cat(warn('No file holding staged records is found\n'))
    }
    return(FALSE)
  } else {
    df<-read.table(full.file.path, sep=';', header=T)
    if(!quiet){
      cat(note('Found a file with',nrow(df),'staged records in the database folder\n'))
    }
    
    if(return.staged.records){
      return(df)
    } else {
      return(TRUE)
    }
  }
}
