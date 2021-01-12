# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# source('C:/Users/mls241/surfdrive/Common_scripts/R_startup_script.R') #VU
# source("C:/DATA/SURFdrive/Common_scripts/R_startup_script.R") #Thuis
# source("E:/SURFdrive/Common_scripts/R_startup_script.R") #Yoga
# 
# setwd(paste(wd.base, '[SURFDRIVE SUBFOLDER]', sep=''))

# Load data and packages
#NA?

db.missing<-function(registry, #the previously saved registry
                     database, #the previously saved database
                     columns='all', #all / date / subject / field / value / source / by / verified  #NOT USED YET
                     exclude=F, #values of fiels to be excluded from the output  #NOT USED YET
                     filename='debugging' #the base filename
){
  print('Running db.missing.R ...')
  print('This function expects a registry as produced by db.registry() and the corresponding database as produced by db.build()')
  
#   if(exclude[1]!=F){
#     print(paste(length(exclude),'values were excluded:'))
#     print(exclude)
#   }
  
  # Checks before anything can be done
  if(db.is.registry(registry = registry, quiet=T)){
    df<-registry
  } else {
    db.is.registry(registry = registry, quiet=F)
    stop()
  }
  db<-database
  
  # Find missing values
  missing<-is.na(db)
  
  # Store names of columns that are missing
  columns<-colSums(missing)
  columns<-columns[columns!=0]
  
  # Construct empty output
  result<-data.frame(matrix(ncol = 5))
  colnames(result)<-colnames(df)[2:6]
  
  # Fill table with all missing values
  l=0
  for(i in names(columns)){
    v<-db[missing[,i],1]
    
    result[l+1:length(v),2]<-v
    result[l+1:length(v),3]<-i
    
    l=l+length(v)
  }

# Remove all the NAs
result[,c(1,4,5)]<-''

# Sort by subject
result<-result[order(result[,2]),]

# Export table with missing fields
write.table(result, file=paste(today,filename,'missing.csv',sep='.'), sep=';', row.names=F)

print(paste('A table with missing values is returned and has been exported as ',today,'.',filename,'.missing.csv',sep=''))
return(result)
}