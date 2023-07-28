# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2020. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data

# Reformat data

# Define function
db.new.input.file<-function(registry=F, #the registry for which an input file needs to be generated
                            new.table=F, #OR: use a predefined table for the new input file
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
      cat(note('This function expects a registry as created by db.registry(), or a predefined new table\n'))
    }
  }
  
  #A new.table and/or a registry must be provided
  if(!(new.table[[3]][1]!=F | registry[[1]][1]!=F)){
    cat(error('ERROR: A new.table and/or a registry must be provided !\n'))
    stop()
  }
  
  #Check the predefined new.table, if it is provided
  if(new.table[[3]][1]!=F){
    #Check new table
    if(ncol(new.table)==5){
      tmp.df<-new.table
    } else {
      cat(error('ERROR: The predefined new table does not have 5 columns'))
      stop()
    }
  }
  
  #Check input registry, if it is provided
  if(registry[[1]][1]!=F){
    if(db.is.registry(registry = registry, quiet=T)){
      df.new<-registry
      
      #Create dataframe for the new input file with all field names that are used in the registry
      df.new<-data.frame('','',unique(registry[,4]),'','')
      colnames(df.new)<-colnames(registry[2:6])
      
      #If a df was already generated from a predefined table, than prioritise that one
      #but append extra fields from registry, if any
      if(exists('tmp.df')){
        df.new<-rbind(tmp.df, df.new[!df.new[,3] %in% tmp.df[,3],])
      }
    } else {
      db.is.registry(registry = registry, quiet=F)
      stop()
    }
  } else {
    df.new<-tmp.df
  }
  
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
      df.new<-rbind(df.new,precomputed.records[,1:5]) #then add them to the new input file
      for(x in 1:5){
        if(class(precomputed.records[,x])=='integer'){ #if we have integers in the precomputed records
          df.new[,x]<-as.integer(df.new[,x]) #then pass that on to the new input file
        }
      }
    } else {
      cat(warn('The precomputed records are not a dataframe; the input is ignored!\n'))
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
    if(!exists('write.default.xlsx')){
      source('https://raw.githubusercontent.com/LammersInsects/MLmisc/main/write.default.xlsx.R')
    }
    write.default.xlsx(dataframe = df.new, filepath = filepath, full.file.name = full.file.name, colwidths = colwidths,
                       extra.header.info = extra.header.info, quiet = quiet)
    if(!quiet){
      cat(note('New input file is saved as ',filepath,'\n',sep=''))
    }
  }
  
  if(return.df){
    return(df.new)
  }
}
