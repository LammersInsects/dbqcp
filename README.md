# R package "DB"

By Mark Lammers, @LammersInsects

## Introduction

## Getting started

### Language 

The entire set of functions is written in R, my native programming language. 
Your system requires:

- [R](https://www.r-project.org/) installed
- [RStudio](https://www.rstudio.com) installed (preferably, although not essential)

I have not tested in which range of R versions the scripts work. Since most use only base R, it should work out-of-the-box on yours. As a rule, strive to always work in the latest full release. Please open an issue if your combination of OS and R doesn't work while you think it should. 

Perhaps you prefer python? I think it is possible to make everything work in python using [Rpy2](https://rpy2.github.io/doc/latest/html/)
If anyone feels like writing code to use an API for python or any other language: you're most welcome to contribute!

### Operating System 

All functions were written on Windows OS with Git taking care of line break conversion. Therefore the code should work on any OS as long as you retrieved it from GitHub.
Please open an issue if you find a piece of code that is Windows-specific. 

### Downloading the set of functions 

Ideal situation (requires a GitHub account and basic understanding of git): Make a GitHub `fork` and use `git clone` to get a copy of the repository.

Alternative download method: Go to @LammersInsects (link) and download the files.

All other usage cases: use `github2r()`. This way you will always be using the latest stable version.

### Loading the package

CHECK my load function 

### Dependencies

In my programming philosophy, a script should have as little dependencies as possible. However, sometimes you simply cannot do without one. Make sure to have the latest version of these R packages installed:

- CHECK if any are used

Use `if(!require('<package name>')){install.packages('<package name>')}` to install a missing package.

## Functions

See FUNCTIONS.md for the help files of all functions in the package (Add link)

## Example work flows

### Initiating a database

### Maintaining a database

### Maintaining a todo-list

## Contributing

Please use GitHub's Issue and Pull Request functions for suggestions and patches. I get automatic notifications on issues and pull requests. Please refrain from contacting me through email.
See CONTRIBUTING.md for details on ways to contribute.

## To Do

- [ ] Add LICENSE 
- [ ] Write CONTRIBUTING.md
- [ ] Write description of the functions