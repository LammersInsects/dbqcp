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
                            precomputed.records=F, #a dataframe with 5 columns with pre-filled information
                            extra.header.info=NA, #allows parsing this string to write.default.xlsx()
                            quiet=F
){
  if(!quiet){
    cat(note('Running db.new.input.file.R ...\n'))
    cat(note('This function expects a registry as created by db.registry()\n'))
  }
  
  if(db.is.registry(registry = registry, quiet=T)){
    df<-registry
  } else {
    db.is.registry(registry = registry, quiet=F)
    stop()
  }
  
  # df<-registry[0,2:6]
  df<-data.frame('','',unique(registry[,4]),'','')
  colnames(df)<-colnames(registry[2:6])
  
  if(class(precomputed.records)!='logical'){
    if(is.data.frame(precomputed.records)){
      if(ncol(precomputed.records)==5){
        
      } else {
        cat(warn('The dataframe of precomputed records does not have 5 columns. Only the first five columns are used\n'))
      }
      if(!quiet){
        cat(note('Adding ',nrow(precomputed.records),' records to the new input file\n',sep=''))
      }
      df<-rbind(df,precomputed.records[,1:5])
      for(x in 1:5){
        if(class(precomputed.records[,x])=='integer'){
          df[,x]<-as.integer(df[,x])
        }
      }
    } else {
      cat(warn('The precomputed recods are not a dataframe; the input is ignored!\n'))
    }
  } else {
    if(!quiet){
      cat(note('No precomputed records were provided.\n'))
    }
  }
  
  filepath<-paste(gd.base, filename, '.input.xlsx' ,sep='') 
  
  if(!quiet){
    cat(note('Calling write.default.xlsx()\n'))
  }
  write.default.xlsx(dataframe = df, file = filepath, filename = filename, colwidths = colwidths, 
                     extra.header.info = extra.header.info, quiet = quiet)
  if(!quiet){
    cat(note('New input file is saved as ',filepath,'\n',sep=''))
  }
}
