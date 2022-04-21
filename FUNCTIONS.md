Functions that are part of the R database package
=================================================

Table of Contents
=================

* [Functions that are part of the R database package](#functions-that-are-part-of-the-r-database-package)
* [Table of Contents](#table-of-contents)
* [Creating a registry](#creating-a-registry)
   * [db.registry.R](#dbregistryr)
* [Working with an existing registry](#working-with-an-existing-registry)
   * [db.load.backup.R](#dbloadbackupr)
* [Converting a registry to a "wide" format database](#converting-a-registry-to-a-wide-format-database)
   * [db.build.R](#dbbuildr)
* [Preparing input to add to an existing registry](#preparing-input-to-add-to-an-existing-registry)
   * [db.compare.db.R](#dbcomparedbr)
   * [db.multicol.import.R](#dbmulticolimportr)
   * [db.new.input.file.R](#dbnewinputfiler)
* [Filtering records](#filtering-records)
   * [db.findrecords.R](#dbfindrecordsr)
   * [db.last.records.R](#dblastrecordsr)
   * [db.mask.R](#dbmaskr)
* [Analysis of the registry](#analysis-of-the-registry)
   * [db.missing.R](#dbmissingr)
   * [db.history.R](#dbhistoryr)
   * [db.summary.R](#dbsummaryr)
* [Working with actions](#working-with-actions)
   * [`db.create.action.R'](#dbcreateactionr)
   * [`db.process.actions.R'](#dbprocessactionsr)
* [Plumbing](#plumbing)
   * [db.compress.R](#dbcompressr)
   * [db.files.R](#dbfilesr)
   * [db.dates.R](#dbdatesr)
   * [db.is.registry.R](#dbisregistryr)
   * [db.remove.R](#dbremover)
   * [db.staged.R](#dbstagedr)
   * [db.translate.R](#dbtranslater)
   * [users.R](#usersr)


Created by [gh-md-toc](https://github.com/ekalinin/github-markdown-toc)

# Creating a registry

## `db.registry.R`
- *Description*: Load an existing registry, commit new records to a registry, or start a new registry.
- *Usage*: db.registry(existing.data.registry=F, new.records=F, too.old=300, expected.missing=0, file.base.name='debugging', user=NA, quiet=F, print.help=F, write.output=T, save.backup=T)
- *Arguments*:

Argument | Specification
-------- | -------------
existing.data.registry | The previously saved registry, set to FALSE if no registry exists yet (default)
new.records | The records to be added to the registry, set to FALSE if an existing registry is to be loaded
too.old | A number of days against which new records' dates are checked for age
expected.missing | Use this if the default new input always has a fixed number of incomplete records that will get omitted in the commit process
file.base.name | The base file.base.name of the registry
user | The user who recorded the data. Optional argument: if not entered here, the script will prompt you for a manual input
quiet | Absolutely no information is printed if set to TRUE
print.help | A help message is printed is set to TRUE, overridden by quiet flag
write.output | Should the output be written to working directory? Defaults to TRUE
save.backup | Should a backup of the existing registry be saved? Defaults to TRUE

- *Details*: This function is the workhorse of any database maintained using this package. It is the sole function to write a new version of the registry to be stored permanently.
- *Value*: A dataframe of 8 variables
- *Note*: NA
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*: db.build
- *Examples*:
```R
file.base.name<-'ToDo'
todo.existing<-read.table(paste(file.base.name,'.registry.csv',sep=''),header=T,sep=';')
#here the existing database could be edited manually, if one would wish to do so. Only recommended for experienced users, as the file format could become corrupted. Moreover, changes would be propagated throughout all future versions of the database. Usually it is better to use db.translate() or db.remove() to make permanent changes.
todo.new<-read.xlsx(paste(file.base.name,'.xlsx',sep=''), sheetIndex = 1, startRow = 3)
registry<-db.registry(existing.data.registry = todo.existing, new.records = todo.new, file.base.name = file.base.name, user = 'MarkLammers')
```

# Working with an existing registry

## `db.load.backup.R`
- *Description*: Load a backup of an existing registry as it was stored at the beginning of a given date, or the closest earlier date if no backup was saved on the given date.
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*: db.registry
- *Examples*:

# Converting a registry to a "wide" format database

## `db.build.R`
- *Description*: Convert a loaded registry into a "wide" format table with all subjects as rows and the fields as columns.
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*: db.registry, stats::reshape for definitions of long and wide format.
- *Examples*:

# Preparing input to add to an existing registry

## `db.compare.db.R`
- *Description*: Compare the contents of all cells of a "wide" database against all cells of a different "wide" database, and generate records from the differences that can directly be committed to a registry.
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*: db.registry, db.build, stats::reshape for definitions of long and wide format.
- *Examples*:

## `db.multicol.import.R`
- *Description*: Take a table in "wide" format and generate records from it, so that the same table would be reproduced if db.build() would be run with those records stored in a registry.
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*: db.registry, db.build
- *Examples*:

## `db.new.input.file.R`
- *Description*: Generate a new input file to facilitate recording new records for an existing registry.
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*: db.registry
- *Examples*:

# Filtering records

## `db.findrecords.R`
- *Description*: Search a registry for records that (not) match a search term in the subject, field, and/or value.
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*: db.registry, db.mask
- *Examples*:

## `db.last.records.R`
- *Description*: Filter a registry so that only the last record of each of the subjects/fields/values, is returned.
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*: db.registry
- *Examples*:

## `db.mask.R`
- *Description*: Search a registry for records that (not) match a search term in the subject or field, but then also include all other records from those subjects or fields.
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*: db.registry, db.findrecords
- *Examples*:

# Analysis of the registry

## `db.missing.R`
- *Description*: Evaluate a registry and generate partial new records for missing values of each subject-field combination.
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*: db.registry
- *Examples*:

## `db.history.R`
- *Description*: Evaluate the recording history of a registry and produce some plots to visualize that.
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*: db.registry
- *Examples*:

## `db.summary.R`
- *Description*: Summarize each of the data columns in a registry and print/save that information.
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*: db.registry
- *Examples*:

# Working with actions

## `db.create.action.R'
- *Description*: Create a removal action or a translation action, and store the created records in the staging file.
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*: db.registry, db.staged
- *Examples*:

## `db.process.actions.R'
- *Description*: Process actions that are stored in the registry in antechronological order.
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*: db.registry
- *Examples*:

# Plumbing

## `db.compress.R`
- *Description*: Convert a loaded registry into a ragged array.
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*: db.registry
- *Examples*:

## `db.files.R`
- *Description*: Evaluate the files in the working directory and report which files were created, when and whether this was done by the package or not.
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*:
- *Examples*:

## `db.dates.R`
- *Description*: A wrapper for db.files.R to only extract the dates on which records were committed to the registry
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*: A vector of all dates of class "Date" for the files matching the query of the registry history
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*: db.files
- *Examples*:

## `db.is.registry.R`
- *Description*: Test whether a data frame matches the format of a registry.
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*: db.registry
- *Examples*:

## `db.remove.R`
- *Description*: Remove records from a registry based on their record IDs.
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*: db.registry
- *Examples*:

## `db.staged.R`
- *Description*: Read the staged records from the database folder.
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*: db.registry
- *Examples*:

## `db.translate.R`
- *Description*: Translate a term to any other term in an entire registry.
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*: db.registry
- *Examples*:

## `users.R`
- *Description*: Parse the users.csv file to store known user names in a hidden environment.
- *Usage*:
- *Arguments*:
- *Details*:
- *Value*:
- *Note*:
- *Author(s)*: Concept and implementation by Mark Lammers, @LammersInsects
- *See also*:
- *Examples*:
