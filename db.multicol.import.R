# Written by Mark Lammers; Animal Ecology, Vrije Universiteit Amsterdam; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# source('C:/Users/mls241/surfdrive/Common_scripts/R_startup_script.R') #VU
# source("C:/DATA/SURFdrive/Common_scripts/R_startup_script.R") #Thuis
# source("E:/SURFdrive/Common_scripts/R_startup_script.R") #Yoga
# 
# setwd(paste(wd.base, '[SURFDRIVE SUBFOLDER]', sep=''))

# Load data and packages
#NA?

db.multicol.import<-function(dataframe, #the records to be added
                             date.col=F, #the column with the date the records were recorded, or a single date in format='%d.%m.%Y'
                             subject.col=F, #the column specifying the subject of the records
                             value.columns=F, #the column specifying the values associated with the records
                             source.col=F, #the source of the records
                             filename='debugging' #the base filename
){
  print('Running db.multicol.import.R ...')
  print('This function expects a dataframe with any number of columns:')
  print('  Date recorded -- Subject -- Value-columns -- Source   (header is compulsory)')
  
  ## Checks before the start
  df<-dataframe
  
  # Remove any trailing whitespaces in each column
  for(i in 1:ncol(df)){
    df[,i]<-trailingspace(df[,i])
  }
  
  # Date recorded
  if(date.col!=F){ #something is given
    if(length(date.col)==1){ #only one value
      if(date.col %in% colnames(df)){ #which is a column name
        nodate<-emptyvalues(df[,date.col])
        if(sum(nodate)>0){
          print(paste(sum(nodate),"records have no date. Today's date is imputed in these records:"))
          print(df[nodate,])
          df[nodate,date.col]<-as.Date(today, format='%Y%m%d')
        }
        df$Date<-as.Date(df[,date.col], format='%d-%m-%Y') #assumes Excel-formatted short date
        date.col.name<-date.col
      } else { #it's not a column name
        print('A single value is provided for the date, but not found as a column name, trying to use it as a date for all records')
        df$Date<-as.Date(date.col, format='%d.%m.%Y')
        date.col.name<-'Date'
      }
    } else {
      stop('ERROR: Only one column can be the date recorded column')
    }
  } else {
    print("No column specifying the date recorded is provided. Today's date is imputed in all records")
    df$Date<-as.Date(today, format='%Y%m%d')
    date.col.name='Date'
  }
  
  # Subject
  if(subject.col!=F){
    if(length(subject.col)==1){
      if(subject.col %in% colnames(df)){
        nsubj<-length(unique(df[,subject.col]))
        print(paste(nsubj,'unique subjects found in column',subject.col))
        if(nsubj<nrow(df)){
          print('WARNING: There is less unique subjects than rows in the dataframe')
        }
        df$Subject<-df[,subject.col]
        subject.col.name=subject.col
      } else {
        stop('ERROR: Given subject column is not found in the column names of the data frame')
      }
    } else {
      stop('ERROR: Only one column can be the subject column')
    }
  } else {
    stop('ERROR: A subject column is compulsory')
  }
  
  # Values
  if(value.columns[1]!=F){
    if(all(value.columns %in% colnames(df))){
      nvalcol<-length(value.columns)
      print(paste(nvalcol,'columns with values are provided:'))
      print(paste(value.columns,collapse = ', '))
      
      #test whether there is columns in date format
      test<-sapply(df[,value.columns], class)=='Date'
      if(sum(test)>0){
        print('Some value columns contain dates, coercing these to character in format %d.%m.%Y:')
        print(head(df[,value.columns])[,test])
        df[,value.columns][,test]<-sapply(df[,value.columns][,test], function(x){
          as.character(format(x, '%d.%m.%Y'))
        })
      }
      
    } else {
      stop('ERROR: One or more columns with values is not found in the column names of the data frame')
    }
  } else {
    stop('ERROR:At least one column with values should be provided')
  }
  
  # Source
  if(source.col!=F){ #something is provided
    if(length(source.col)==1){ #it's only one value
      if(source.col %in% colnames(df)){ #it matches a column name
        nsources<-length(unique(df[,source.col]))
        print(paste(nsources,'unique sources are found in column',source.col))
        df$Source<-df[,source.col]
        source.col.name<-source.col
      } else { #the value provided is not a column name
        print('A single value is provided for the source, but not found as a column name, trying to use it as a source for all records')
        df$Source<-source.col
        source.col.name<-'Source'
      }
    } else {
      stop('ERROR: Only one column can be the source column')
    }
  } else {
    print('No source column is provided')
    df$Source<-NA
    source.col.name<-'Source'
  }
  
  # Remove NAs
  df<-NA.harmonizer(df)
  nosubject<-emptyvalues(df[,subject.col])
  #   novalue<-emptyvalues(df[,value.columns])
  remove<-nosubject #| novalue
  if(sum(remove)>0){
    print(paste(sum(remove),'records with missing Subject found. These are removed from the registry:'))
    print(df[remove,])
    df<-df[!remove,]
  }
  
  
  ## Build the standardized registry
  output<-data.frame()
  for(col in value.columns){
    add<-data.frame(Date=df$Date,
                    Subject=df$Subject,
                    Field=col,
                    Value=df[,col],
                    Source=df$Source)
    output<-rbind(output,add)
  }
  
  # Rename columns to input names
  colnames(output)<-c(date.col.name,subject.col.name,'Field','Value',source.col.name)
  
  # Sort output
  output<-output[order(output[,date.col.name],output[,subject.col.name]),]
  row.names(output)<-1:nrow(output)
  
  # Return the standardized registry 
  return(output)
  
}

# if(debugging){
#   dataframe<-read.table(paste(gd.base,'Aanschaf_ingredienten_records_telefoon.csv',sep='/'),header=T,sep=';')
#   date.col='Aanschafdatum'
#   subject.col='Ingredient'
#   value.columns=c('Naam','Verpakkingsgrootte','Eenheid','Aantal','Prijs')
#   source.col='Verkoper'
#   
#   load.db.package()
#   db.multicol.import(dataframe,
#                      date.col='Aanschafdatum',
#                      subject.col='Ingredient',
#                      value.columns=c('Naam','Verpakkingsgrootte','Eenheid','Aantal','Prijs'),
#                      source.col='Verkoper')
# }