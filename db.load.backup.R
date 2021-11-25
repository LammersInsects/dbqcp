# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2021. Released under the terms of the GNU General Public License v3.

database.folder=setwd(paste(wd.base, 'Projects_home/Planten/database_v2', sep=''))
file.base.name='pot'

#try different dates
backup.date=as.Date('20211009',format='%Y%m%d')
backup.date=as.Date('20180101',format='%Y%m%d')
backup.date=as.Date('20211127',format='%Y%m%d')
test<-db.load.backup(database.folder = database.folder, file.base.name = file.base.name, backup.date = backup.date)

# Define function
db.load.backup<-function(database.folder, #The folder holding the database files to analyse
                         file.base.name,
                         backup.date, #date as object of class "Date"
                         quiet=F, #absolutely no information is printed
                         print.help=F #no help message is printed, overridden by quiet flag
){
  if(!quiet){
    cat(note('Running db.load.backup.R ...\n'))
    if(print.help){
      cat(note('This function expects a name for the folder that holds the database files\n'))
    }
  }
  
  #Check date format
  if(class(backup.date)=='Date'){
    if(!quiet){
      cat(note('Looking for the latest backup that was there on',backup.date,'\n'))
    }
  } else {
    stop('ERROR: Target date is not an R object of class Date!\n')
  }
  
  #Use db.dates to get all dates in the registry
  dates<-db.dates(database.folder = database.folder, file.base.name = paste(file.base.name,'registry.backup',sep='.'),
                  return.dates = T, quiet = T)
  #TODO Fix bug that also some NAs are returned. Probably due to bug in db.files()
  
  #Find the date that is closest to the requested date
  comp.date<-sort(dates-backup.date, decreasing = T)
  target.date<-rev(dates)[comp.date<=0][1]
  target.backup<-paste(format(target.date,'%Y%m%d'),file.base.name,'registry.backup',sep='.')
  
  #Check that this file actually exists
  if(file.exists(target.backup)){
    if(!quiet){
      cat(note('Found registry named',target.backup,'as the last backup saved before the given date. Loading it...\n'))
    }
  } else {
    stop('ERROR: No backup is available for the given date, nor for any day before it')
  }
  
  #Load that registry
  backup<-read.table(target.backup,header=T,sep=';')
  registry<-db.registry(existing.data.registry = backup, filename = 'restore', user = 'admin', write.output = F, save.backup=F,
                        quiet = quiet, print.help = print.help)
  
  #Return it
  return(registry)
}
