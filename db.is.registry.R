# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2020. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data
# df<-register
# head(df)
# db.is.registry(registry = register, quiet = F)
# db.is.registry(db.compress(register), quiet = F)
# db.is.registry(registry = df, quiet = F)
# db.is.registry(registry = wd.base, quiet = F)
# db.is.registry(registry = pot.new, quiet = F)
# db.is.registry(registry = data.frame(matrix(1:40,nrow = 5, ncol = 8)), quiet = F)

# Reformat data

# Define function
db.is.registry<-function(registry,
                         quiet=F, #absolutely no information is printed
                         print.help=F #no help message is printed, overridden by quiet flag
){
  if(!quiet){
    cat(note('Running db.is.registry.R ...\n'))
    if(print.help){
      cat(note('This function expects a registry that is presumed to be produced by db.registry()\n'))
    }
  }
  
  df<-registry
  
  #ALL THE THINGS TO TEST FOR, set them to TRUE for starters
  nc <-T #number of columns
  cn <-T #column names
  dt <-T #data types
  nna<-T #no NAs in all columns except source
  nd <-T #number of unique data points is less or equal to number of rows
  
  # 8 columns in total
  #if this fails it should skip the  next two tests, as they depend on the df having 8 columns
  if(is.null(ncol(df))){
    nc<-F
  } else {
    nc<-dim(df)[2]==8
  }
  
  if(nc){
    # First column is called ID, column 7 is called Recorded.by, column 8 is called Verified
    cn<-all(colnames(df)[c(1,7,8)]==c('ID','Recorded.by','Verified'))
    
    # Column data types are (all) integer, date, character, character, character, character, character, integer
    interchangeable<-c("integer","character","numeric")
    dt<-all(sapply(df[,1:2],class)==c("integer","Date") & sapply(df[,3:8],class) %in% interchangeable)
    #MAYBE INCLUDE AN ATTEMPT TO DATE CONVERSION?
    # sapply(df,function(x){interpret.data.type(data.type(x))}) #first work on that function before using it here
  }
  
  # Most columns cannot have NAs
  nna<-sum(sapply(df,anyNA))<=1
  
  # For some columns there is a limit to the number of unique variables it can have, especially not allowing array data structures
  nd<-all(sapply(df,function(x){length(unique(unlist(x)))})<=nrow(df))
  
  # Summarize output and clarify what the tests means
  tests<-rbind(nc,cn,dt,nna,nd)
  res<-data.frame(abbreviation=row.names(tests),
                  outcome=tests,
                  explanation=c('number of colums != 8',
                                'column 1, 2 or 8 have invalid names',
                                'data types do not match expectations',
                                'NAs in cells that should not have any',
                                'more unique values per column than there are rows'))
  
  # Report output
  if(all(res$outcome)){
    if(!quiet){
      cat(note("Input dataframe is a registry from MarkLammers' database package\n"))
    }
  } else {
    bad<-res[which(res$outcome!=TRUE),]
    if(!quiet){
      cat(error("ERROR: Some criteria are not met, thus input is not a registry as produced by MarkLammers' database package\n"))
      cat(warn('Criteria not met are:\n'))
      print(bad[,2:3])
    }
    if('dt' %in% bad$abbreviation){
      cat(error('CHECKPOINT triggered, unusual data in registry?\n'))
      cat(warn('If the only missed criterion is <dt>, but the data frame should be a registry, then it may be that some data should better be changed to integer, numeric or character\n'))
      cat(warn('Please let MarkLammers look at your registry to investigate the source of this problem\n'))
    } else {
      
    }
  }
  
  # Returned is only the conclusion of doing all tests
  return(all(tests))
  
}
