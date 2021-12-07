# R package "dbpq"

By Mark Lammers, @LammersInsects

## Introduction

## Getting started

### Language

The entire set of functions is written in R, my native programming language.
Your system requires:

- [R](https://www.r-project.org/) v4.0.3 or higher
- [RStudio](https://www.rstudio.com) installed (preferably, although not essential)

I have not tested in which range of R versions the scripts work. Since most use only base R, it should work out-of-the-box on yours. As a rule, strive to always work in the latest full release. Please open an issue if your combination of OS and R doesn't work while you think it should.

Perhaps you prefer python? I think it is possible to make everything work in python using [Rpy2](https://rpy2.github.io/doc/latest/html/)
If anyone feels like writing code to use an API for python or any other language: you're most welcome to contribute!

### Operating System

All functions were written on Windows OS with Git taking care of line break conversion. Therefore the code should work on any OS as long as you retrieved it from GitHub.
Please open an issue if you find a piece of code that is Windows-specific.

### Downloading the set of functions

Ideal situation (requires a GitHub account and basic understanding of git): Make a GitHub `fork` and use `git clone` to get a copy of the repository.

Manual download method: Go to @LammersInsects (link) and download the files.

Alternatively: use `github2R()` from my repo [MLmisc](https://github.com/LammersInsects/MLmisc/blob/main/github2R.R). This way you will always be using the latest stable version.

Further suggestions on https://stackoverflow.com/questions/48612676/how-to-download-entire-repository-from-github-using-r/48613007

### Loading the package

CHECK my load function

### Dependencies

In my programming philosophy, a script should have as little dependencies as possible. However, sometimes you simply cannot do without one. Make sure to have the latest version of these R packages installed:

- R base packages: `stats graphics grDevices utils datasets methods base`
- package `crayon` version 1.3.4 or higher
- package `xlsx` version 0.6.4.2 or higher

Use `if(!require('<package name>')){install.packages('<package name>')}` to install a missing package, and `library('<package name>')` to attach it.

## Issues?

Open an issue in GitHub's [issue tracker for this repo](https://github.com/LammersInsects/dbpq/issues) if you encounter a bug or any other issue. Please add the output of `sessionInfo()` to your issue report. Using GitHub's issue labels is encouraged.

## Functions

See [FUNCTIONS.md](https://github.com/LammersInsects/dbpq/blob/main/FUNCTIONS.md) for the help files of all functions in the package.

## Example work flows

### Initiating a database

### Maintaining a database

### Maintaining a todo-list

## Contributing

YES PLEASE!
Please use GitHub's [Issue](https://github.com/LammersInsects/dbpq/issues) and [Pull Request](https://github.com/LammersInsects/dbpq/pulls) functions for suggestions and patches. I get automatic notifications on issues and pull requests. Please refrain from contacting me through email.
See [CONTRIBUTING.md](https://github.com/LammersInsects/dbpq/blob/main/FUNCTIONS.md) for details on ways to contribute.
