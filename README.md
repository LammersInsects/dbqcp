R package "dbpq"
================

By Mark Lammers, @LammersInsects


Table of Contents
=================

* [R package "dbpq"](#r-package-dbpq)
* [Table of Contents](#table-of-contents)
* [Introduction](#introduction)
* [Getting started](#getting-started)
   * [Language](#language)
   * [Operating System](#operating-system)
   * [Downloading the set of functions](#downloading-the-set-of-functions)
   * [Loading the package](#loading-the-package)
   * [Dependencies](#dependencies)
   * [Issues?](#issues)
* [File format](#file-format)
   * [New records](#new-records)
   * [Stored registry](#stored-registry)
   * [A compressed registry](#a-compressed-registry)
   * [Output "wide format" database](#output-wide-format-database)
* [Functions](#functions)
* [Contributing](#contributing)
* [Example work flows](#example-work-flows)
   * [Initiating a database](#initiating-a-database)
   * [Maintaining a database](#maintaining-a-database)
   * [Maintaining a todo-list](#maintaining-a-todo-list)

Created by [gh-md-toc](https://github.com/ekalinin/github-markdown-toc)

# Introduction

TODO
Here write about:
- philosophy of data management (automation, no deletion)
- automation of basic tasks and repetitive work
- if it doesn't exist, write it

The registry **never** removes any data. To remove a previous record, a new record specifying the removal should be added (this is not implemented yet!)

As a consequence, this package is not suitable for recording any privacy-sensitive data that must be removed as specified in any applicable regulations!

# Getting started

## Language

The entire set of functions is written in `R`, my native programming language.
Your system requires:

- [R](https://www.r-project.org/) v4.0.3 or higher
- [RStudio](https://www.rstudio.com) installed (preferably, although not essential)

I have not tested in which range of R versions the scripts work. Since most use only `base` R, it should work out-of-the-box on yours. As a rule, strive to always work in the latest full release. Please open an issue if your combination of OS and R doesn't work while you think it should.

Perhaps you prefer python? I think it is possible to make everything work in python using [Rpy2](https://rpy2.github.io/doc/latest/html/)
If anyone feels like writing code to use an API for python or any other language: you're most welcome to contribute!

## Operating System

All functions were written on Windows OS with Git taking care of line break conversion. Therefore the code should work on any OS as long as you retrieved it from GitHub.

Please open an [issue](https://github.com/LammersInsects/dbpq/issues) if you find a piece of code that is Windows-specific.

## Downloading the set of functions

Ideal situation (requires a GitHub account and basic understanding of git): Make a GitHub `fork` and use `git clone` to get a copy of the repository.

Manual download method: Go to @LammersInsects [package page](https://github.com/LammersInsects/dbpq) and download the files.

Alternatively: use `github2R()` from my repo [MLmisc](https://github.com/LammersInsects/MLmisc/blob/main/github2R.R). This way you will always be using the latest stable version.

Further suggestions on https://stackoverflow.com/questions/48612676/how-to-download-entire-repository-from-github-using-r/48613007

## Loading the package

CHECK my load function

## Dependencies

In my programming philosophy, a script should have as little dependencies as possible. However, sometimes you simply cannot do without one. Make sure to have the latest version of these R packages installed:

- R base packages: `stats graphics grDevices utils datasets methods base`
- package `crayon` version 1.3.4 or higher
- package `xlsx` version 0.6.4.2 or higher (only used in the convenience function `db.new.input.file.R`, everything else is independent of package `xlsx`)

Use `if(!require('<package name>')){install.packages('<package name>')}` to install a missing package, and `library('<package name>')` to attach it.

All code was written in Rstudio with the Editor Theme set to `Cobalt` (under Global Options > Appearance), in other words, all output looks best against a dark background. If something is unreadable in your system, please submit an issue with a specification of the problem.

## Issues?

Open an issue in GitHub's [issue tracker for this repo](https://github.com/LammersInsects/dbpq/issues) if you encounter a bug or any other issue. Please add the output of `sessionInfo()` to your issue report. Using GitHub's issue labels is encouraged.

# File format

All data is recorded in a continuous registry, with a new line for each new value recorded. The metadata associated to the record are stored on the same line as the value. See below for specifics at different stages of recording and storing data.

Prior to recording, it is wise to consider the subject identifiers. The subject should be practical, and be the lowest level of the unit of analysis. Each subject should be assigned a unique identifier, so that records belonging to the same subject share an identifier in this column.

## New records

A table with five columns. The columns should have names, but these names are not actually verified upon committing the records. The columns must be in the order as given in this table:

Column name | Explanation
----------- | -----------
Date | The date on which the record was recorded in format `%d.%m.%Y` or `%d/%m/%Y` or `%d/%m/%Y`
Subject | Specifies the subject of which the value of an attribute is recorded
Field | The attribute for which a value is recorded.
Value | The value of the given attribute.
Source | The source of the record.

Take care that field names do not contain typos, it would be interpreted as a new field name! Some provided functions can remedy such errors at later stages (`db.remove`, `db.translate`), but it is cumbersome to fix situations.

## Stored registry

The data is stored in eight columns in semicolon-separated CSV format as `<filename>.registry.csv` in the working directory. Many columns can be renamed to basically any other string when a registry is initiated, see the table below which columns this (not) applies to.

Primarily, each record specifies the value of an attribute of a subject. For each subject, many atrributes can be recorded, and for each attribute, multiple values can be given.

Column name | Explanation | Can be renamed
----------- | ----------- | --------------
ID | The unique identifier automatically assigned to the record by `db.registry`, always an integer ascending by 1 for each new record.| no
Date | The date on which a record was recorded in format `%Y-%m-%d` (R default format in most locales). Defaults to today if left empty prior to committing the record. | yes
Subject | Specifies the subject for which the value of an attribute is recorded. | yes
Field | The attribute for which a value is recorded. | yes
Value | The value of the given attribute. | yes
Source | The source of the record. | yes
Recorded.by | The person who entered the data. | no
Verified | This is a legacy column, due to be removed in future versions. Currently, no parts of of the code use this column. | no

The first three lines of such a file could look like this:
```R
"ID";"Date";"Subject";"Field";"Value";"Source";"Recorded.by";"Verified"
1;2018-12-22;"Agave americana";"Spines";"many at leaf margin";"a book";"MarkLammers";0
2;2018-12-22;"Agave americana";"Type";"succulent";"a book";"MarkLammers";0
3;2019-02-21;"Trichodiadema spp.";"Type";"Succulent";"another book";"MarkLammers";0
4;2019-02-21;"Trichodiadema spp.";"Spines";"many at leaf tip";"another book";"MarkLammers";0
```

## A compressed registry

Can be created in R using `db.compress`. It contains all the same data as a stored registry, but reorganizes it into a ragged array. This format may be more convenient for certain analyses.

Function db.compress seems to have a bug while I am writing this... TODO!

## Output "wide format" database

The function `db.build` outputs a table in "wide format" where all rows are the subjects, with the subject name in the first column. The remaining columns are all the fields of the registry. The cells are filled with the values of the attributes of the subjects.

There can be duplicity, i.e. multiple values for the same attribute (=field) of the same subject. How this duplicity is handled, is controlled by the user. By default, the latest record for each subject-field-value combination is used.

The database output is written in semicolon-separated CSV format to the disk as `<filename>.db.csv` in the working directory.

Using the same example as above, the file would look like this:
```R
"Subject";"Type;"Spines"
"Agave americana";"succulent";"many at leaf margin"
"Trichdiadema sp";"succulent";"many at leaf tip"
```

# Functions

See [FUNCTIONS.md](https://github.com/LammersInsects/dbpq/blob/main/FUNCTIONS.md) for the help files of all functions in the package.

# Contributing

YES PLEASE!

Please use GitHub's [Issue](https://github.com/LammersInsects/dbpq/issues) and [Pull Request](https://github.com/LammersInsects/dbpq/pulls) functions for suggestions and patches. I get automatic notifications on issues and pull requests. Please refrain from contacting me through email.

See [CONTRIBUTING.md](https://github.com/LammersInsects/dbpq/blob/main/FUNCTIONS.md) for details on ways to contribute.

# Example work flows

TODO

## Initiating a database

## Maintaining a database

## Maintaining a todo-list
