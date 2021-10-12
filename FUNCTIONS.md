# Functions that are part of the R database package

## Creating a registry

### `db.registry.R`
- *Description*: Load an existing registry, commit new records to a registry, or start a new registry
- *Usage*: db.registry(existing.data.registry=F, new.records=F, too.old=300, expected.missing=0, filename='debugging', user=NA, quiet=F, print.help=F, write.output=T)
- *Arguments*:
Argument | Specification
-------- | -------------
existing.data.registry | The previously saved registry, set to FALSE if no registry exists yet (default)
new.records | The records to be added to the registry, set to FALSE if an existing registry is to be loaded
too.old | A number of days against which new records' dates are checked for age
expected.missing | Use this if the default new input always has a fixed number of incomplete records that will get omitted in the commit process
filename | The base filename of the registry
user | The user who recorded the data. Optional argument: if not entered here, the script will prompt you for a manual input
quiet | Absolutely no information is printed if set to TRUE
print.help | A help message is printed is set to TRUE, overridden by quiet flag
write.output | Should the output be written to working directory? Defaults to TRUE
- *Details*: This function is the workhorse of any database maintained using this package. It is the sole function to write a new version of the registry to be stored permanently.
- *Value*: A dataframe of 8 variables
- *Note*: NA
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*: db.build
- *Examples*:
filename<-'ToDo'
todo.existing<-read.table(paste(filename,'.registry.csv',sep=''),header=T,sep=';')
todo.new<-read.xlsx(paste(filename,'.xlsx',sep=''), sheetIndex = 1, startRow = 3)
registry<-db.registry(existing.data.registry = todo.existing, new.records = todo.new, filename = filename, user = 'MarkLammers')

## Converting a registry to a "wide" format database

### `db.build.R`
- *Description*:
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*:
- *Examples*:

## Preparing input to add to an existing registry

### `db.compare.db.R`
- *Description*:
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*:
- *Examples*:

### `db.multicol.import.R`
- *Description*:
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*:
- *Examples*:

### `db.new.input.file.R`
- *Description*:
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*:
- *Examples*:

## Filtering records

### `db.findrecords.R`
- *Description*:
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*:
- *Examples*:

### `db.last.records.R`
- *Description*:
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*:
- *Examples*:

### `db.mask.R`
- *Description*:
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*:
- *Examples*:

## Analysis of the registry

### `db.missing.R`
- *Description*:
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*:
- *Examples*:

### `db.history.R`
- *Description*:
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*:
- *Examples*:

### `db.summary.R`
- *Description*:
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*:
- *Examples*:

## Plumbing

### `db.compress.R`
- *Description*:
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*:
- *Examples*:

### `db.files.R`
- *Description*:
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*:
- *Examples*:

### `db.is.registry.R`
- *Description*:
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*:
- *Examples*:

### `db.remove.R`
- *Description*:
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*:
- *Examples*:

### `db.translate.R`
- *Description*:
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*:
- *Examples*:

### `users.R`
- *Description*:
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*:
- *Examples*:
