# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2020. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data 

# Reformat data

# Define function
db.new.input.file<-function(registry=F, #the registry for which an input file needs to be generated
                            filename='debugging', #the base filename
                            colwidths=c(12,8,10,24,8), #preferred for pot_plant_db_new.xlsx
                            extra.header.info=NA #allows parsing this string to write.default.xlsx()
                            
){
  cat(note('Running db.new.input.file.R ...\n'))
  cat(note('This function expects a registry as created by db.registry()\n'))
  
  if(db.is.registry(registry = registry, quiet=T)){
    df<-registry
  } else {
    db.is.registry(registry = registry, quiet=F)
    stop()
  }
  
  # df<-registry[0,2:6]
  df<-data.frame('','',unique(registry[,4]),'','')
  colnames(df)<-colnames(registry[2:6])
  
  filepath<-paste(gd.base, filename, '.input.xlsx' ,sep='') 
  
  cat(note('Calling write.default.xlsx()\n'))
  write.default.xlsx(dataframe = df, file = filepath, filename = filename, colwidths = colwidths, extra.header.info = extra.header.info)
  cat(note('New input file is saved as ',filepath,'\n',sep=''))
}
