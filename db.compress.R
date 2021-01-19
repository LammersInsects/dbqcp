# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# source('C:/Users/mls241/surfdrive/Common_scripts/R_startup_script.R') #VU
# source("C:/DATA/SURFdrive/Common_scripts/R_startup_script.R") #Thuis
# source("E:/SURFdrive/Common_scripts/R_startup_script.R") #Yoga
# source("E:/DATA/Common_scripts/R_startup_script.R") #Dell using Maxtor HDD
# source("D:/DATA/Common_scripts/R_startup_script.R") #Dell internal data

# setwd(paste(wd.base, '[DATADRIVE SUBFOLDER]', sep=''))

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data 

# Reformat data

# Define function
db.compress<-function(registry, quiet=F){
  #I thought this would be easy, done as follows:
  # tabled<-as.data.frame(table(registry[,3]))
  # compressed<-sapply(tabled$Var1, db.findrecords, exact = T, registry = registry, level = 'subject', quiet=T)
  #but this does not properly take on the data structure
  
  #played around with the output though:
  # compressed[5,] #grabs all values
  # compressed[,1] #grabs the first pot
  # sapply(compressed['Waarde',],`[`)[1] #grabs the values for the first pot
  # compressed[,1]$Waarde #grabs the values of the first pot
  # sapply(compressed,`[`)[5] #grabs the values of the first pot
  # sapply(compressed,`[`,4)[5] #specific record
  # compressed[,1]$Waarde[4] #the same record: 4th value of the first pot
  # compressed[5,1]$Waarde[4] #the same record because 5=Waarde
  # sapply(compressed,`[`,3) #nope, grabs part of the register but unclear on what logic
  # sapply(sapply(compressed,`[`),`[`,3) #nope
  # sapply(compressed,`[`)[5][2] #nope
  # compressed[,1]$Plant
  # compressed[,1]$Plant=='Aantal'
  # mapply(`==`,compressed,compressed[,1]$Pot==173)
  # sapply(compressed['Plant',],`[`) #now we are getting somewhere
  # sapply(compressed['Plant',],`[`,3) #doesn't seem right 
  # sapply(compressed['Pot',2],`[`,1) #specific record
  
  #Second attempt was more concrete:
  # test<-sapply(db$Pot,function(x){
  #   df<-register[register$Pot==x,]
  #   return(df)
  # })
  #but does not incorporate the field-substructure
  
  #but the following multistep method works! may not be perfect as field names get stored many times
  if(db.is.registry(registry = registry, quiet=T)){
    df<-registry
  } else {
    db.is.registry(registry = registry, quiet=F)
    stop()
  }
  
  if(!quiet){
    print('Running db.compress.R with a registry as input. Calling db.build.R ...')
  }
  db.tmp<-db.build(registry = registry, include.date = F, filename = 'tmp', quiet = quiet) #first build the database so that this is independent
  
  if(!quiet){
    print('Compressing database ...')
  }
  compressed<-sapply(1:nrow(db.tmp), function(i){
    df<-registry[registry[,3]==db.tmp[i,1],] #grab all records for each subject
    partim<-sapply(2:ncol(db.tmp), function(j){ #for each field within the subject, grab all values, dates and sources
      column<-colnames(db.tmp)[j] #get field name
      interm<-df[df[,4]==column,c(-3,-4)] #for the selected field, grab the records
      if(nrow(interm)==0){interm[1,]<-NA} #if no records exist, put NA. Without it the next line doesn't work
      tmp<-data.frame(column,interm)
      colnames(tmp)<-c(colnames(register)[4],colnames(tmp)[2:ncol(tmp)])
      tmp<-tmp[order(tmp$Datum),]
      return(tmp) #return data frame for this field
    }) #produces a 3-dimensional matrix with all values, dates and sources per field
    # res<-rbind(db.tmp[i,1],partim) 
    colnames(partim)<-colnames(db.tmp)[2:ncol(db.tmp)] #seems like we loose these labels later
    return(partim) #produces a 4-dimensional matrix if I'm not mistaken
  })
  colnames(compressed)<-db.tmp[,1]
  
  if(!quiet){
    print('Compressed database is returned')
  }
  return(compressed)
}


# test<-db.compress(registry = register)
# sapply(test['Waarde',], `[`)
# see Analyse_colonies_in_dishes.R for great uses of a compressed database
# sum(table(register$Kolom))
