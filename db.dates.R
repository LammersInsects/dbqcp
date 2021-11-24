# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2021. Released under the terms of the GNU General Public License v3.

# Define function
db.dates<-function(database.folder, #The folder holding the database files to analyse
                   file.base.name=F,
                   exclude=F,
                   return.dates=T,
                   quiet=F, #absolutely no information is printed
                   print.help=F #no help message is printed, overridden by quiet flag
){
  if(!quiet){
    cat(note('Running db.dates.R ...\n'))
    if(print.help){
      cat(note('This function expects a name for the folder that holds the database files\n'))
    }
  }
  
  #Use db.files to get all dates in the registry
  dates<-db.files(database.folder = database.folder, file.base.name = file.base.name, exclude = exclude,
                  return.dates = T, print.output = F, quiet = T)
  
  dates<-as.Date(dates, format = '%Y%m%d')
  
  if(return.dates){
    return(dates)
  }
}
