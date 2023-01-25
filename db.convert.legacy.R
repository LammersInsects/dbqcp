# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2020. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data

# Reformat data

# Define function
db.convert.legacy<-function(registry,
                            quiet=F, #absolutely no information is printed
                            print.help=F #no help message is printed, overridden by quiet flag
){
  if(!quiet){
    cat(note('Running db.convert.legacy.R ...\n'))
    if(print.help){
      cat(note('This function expects a registry that is presumed to be produced by db.registry()\n'))
    }
  }
  
  df<-registry
  
  # Test the preconditions
  stopifnot(is.data.frame(df))
  
  # Remove the Verified column
  if(ncol(df)>=8){
    if(colnames(df)[8]=='Verified'){
      res<-T
      df<-df[,-8]
    } else {
      res<-F
    }
  } else {
    res<-F
  }
  
  # Report output
  if(res){
    if(!quiet){
      cat(note('Successfully converted a LEGACY registry to the current data format of dbqcp\n'))
    }
  } else {
    if(!quiet){
      cat(warn('Attempt to convert the registry failed! Maybe it was not a LEGACY registry?\n'))
    }
  }
  
  # Returned is only the conclusion
  return(df)
}
