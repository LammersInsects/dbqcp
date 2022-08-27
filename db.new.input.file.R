# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2020. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data

# Reformat data

# Define function
db.new.input.file<-function(registry=F, #the registry for which an input file needs to be generated
                            file.base.name='debugging',
                            full.file.name=F,
                            colwidths=c(12,8,10,24,8), #preferred for pot_plant_db_new.xlsx
                            precomputed.records=F, #a dataframe with 5 columns with pre-filled information
                            extra.header.info=NA, #allows parsing this string to write.default.xlsx()
                            quiet=F, #absolutely no information is printed
                            print.help=F, #no help message is printed, overridden by quiet flag
                            write.output=T, #flag whether output should be written to working directory
                            return.df=F #flag whether the produced dataframe should be returned
){
  if(!quiet){
    cat(note('Running db.new.input.file.R ...\n'))
    if(print.help){
      cat(note('This function expects a registry as created by db.registry()\n'))
    }
  }
  
  #Check input registry
  if(db.is.registry(registry = registry, quiet=T)){
    df<-registry
  } else {
    db.is.registry(registry = registry, quiet=F)
    stop()
  }
  
  #Create dataframe for the new input file with all field names that are used in the registry
  df<-data.frame('','',unique(registry[,4]),'','')
  colnames(df)<-colnames(registry[2:6])
  
  #Append with precomputed records if those are provided
  if(class(precomputed.records)!='logical'){ #if precomputed records are provided
    if(is.data.frame(precomputed.records)){ #and it is a data frame
      if(ncol(precomputed.records)>5){ #with 5 columns
        cat(warn('The dataframe of precomputed records does not have 5 columns. Only the first five columns are used\n'))
      } else if(ncol(precomputed.records)<5){
        cat(warn('The dataframe of precomputed records does not have 5 columns. Extra empty columns are added\n'))
        precomputed.records[,(ncol(precomputed.records)+1):5]<-''
      }
      if(!quiet){
        cat(note('Adding ',nrow(precomputed.records),' records to the new input file\n',sep=''))
      }
      df<-rbind(df,precomputed.records[,1:5]) #then add them to the new input file
      for(x in 1:5){
        if(class(precomputed.records[,x])=='integer'){ #if we have integers in the precomputed records
          df[,x]<-as.integer(df[,x]) #then pass that on to the new input file
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
  
  #Store the full path where to store the new input file if it is not provided
  if(full.file.name==F){
    full.file.name<-paste(file.base.name, '.input.xlsx' ,sep='')
  }
  filepath<-paste(gd.base, full.file.name,sep='')
  
  #Write to that path with a function from MLmisc
  if(write.output){
    if(!quiet){
      cat(note('Calling write.default.xlsx()\n'))
    }
    write.default.xlsx(dataframe = df, filepath = filepath, full.file.name = full.file.name, colwidths = colwidths,
                       extra.header.info = extra.header.info, quiet = quiet)
    if(!quiet){
      cat(note('New input file is saved as ',filepath,'\n',sep=''))
    }
  }
  
  if(return.df){
    return(df)
  }
}
