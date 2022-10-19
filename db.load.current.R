# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2022. Released under the terms of the GNU General Public License v3.

# Define function
db.load.current<-function(database.folder=getwd(), #The folder holding the database files to analyse
                          file.base.name,
                          quiet=F, #absolutely no information is printed
                          print.help=F #no help message is printed, overridden by quiet flag
){
  if(!quiet){
    cat(note('Running db.load.current.R ...\n'))
    if(print.help){
      cat(note('This function expects a name for the folder that holds the database files\n'))
    }
  }
  
  #Check that the target registry file actually exists
  file.name<-paste(database.folder,'/',file.base.name,'.registry.csv',sep='')
  if(file.exists(file.name)){
    if(!quiet){
      cat(note('Found registry named ',file.name,'. Loading it...\n',sep=''))
    }
  } else {
    stop('ERROR: Cannot find a file named ',file.name)
  }
  
  #Load that registry
  registry.raw<-read.table(file.name,header=T,sep=';')
  registry<-db.registry(existing.data.registry = registry.raw, file.base.name = 'restore', user = 'admin', 
                        write.output = F, save.backup=F, quiet = quiet, print.help = print.help)
  
  #Return it
  if(exists('registry')){
    if(!quiet){
      cat(note('Registry is successfully loaded\n'))
    }
    return(registry)
  } else {
    cat(error('Loading the stored registry failed due to unknown reasons\n'))
  }
}
