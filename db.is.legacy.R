# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2020. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data

# Reformat data

# Define function
db.is.legacy<-function(registry,
                       quiet=F, #absolutely no information is printed
                       print.help=F #no help message is printed, overridden by quiet flag
){
  if(!quiet){
    cat(note('Running db.is.legacy.R ...\n'))
    if(print.help){
      cat(note('This function expects a registry that is presumed to be produced by db.registry()\n'))
    }
  }
  
  df<-registry
  
  # Test the preconditions
  stopifnot(is.data.frame(df))
  
  # Test for a Verified column
  res<-colnames(df)[8]=='Verified'
  if(is.na(res)){
    res<-F
  }
  
  # Report output
  if(res){
    if(!quiet){
      cat(warn('Input dataframe may be a LEGACY registry from before v0.11 of dbqcp !\n'))
      cat(note('The registry will be converted automatically with db.convert.legacy() in the next commit\n'))
    }
  } else {
    
  }
  
  # Returned is only the conclusion
  return(res)
}
