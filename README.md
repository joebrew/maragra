
<!-- README.md is generated from README.Rmd. Please edit that file -->
Malaria and sugar
=================

*An in-depth examination of the effect of malaria control activities on the health and productivity of Maragra sugarcane factory workers*

Purpose
=======

This code repository serves as the "research compendium" for the analysis of Maragra malaria incidence, malaria control, and worker absenteeism and productivity data. The purpose of this repository - which takes the form of a fully documented R package - is "to integrate the computations and code used in data analyses, methodological descriptions, simulations, etc. with the documents that describe and rely on them" ([Gentleman and Lang, 2004](http://biostats.bepress.com/bioconductor/paper2/)).

In research, reproducibility is key to transparency and reliability. By using this package, any authorized collaborator will be able to reproduce every step of the analysis and mansucript writing by following the below instructions.

Instructions
============

#### 1. Clone this repository (using the following code in bash).

    $ git clone https://github.com/joebrew/maragra

#### 2. Prepare data

First, youĺl need to populate your `data-raw` folder. This is where you should place those files which are not publicly available. These are:

    ├── HRS - Leave Applications.csv
    ├── HRS - Leave Applications - Sheet 2.csv
    ├── HRS - Leave Applications.xls
    └── maragra_monthly_data.csv

If you do not have these files but are an authorized collaborator, email <joebrew@gmail.com>.

#### 3. Populate your `credentials.yaml` folder

You'll need to send some credentials to the CISM database when accessing it. You should create a `credentials.yaml` file in the `credentials` folder with the following values:

    dbname: openhds
    host: sap.manhica.net
    port: 4706 # (use 3306 instead if based in the CISM)
    user: xxx
    password: xxx

Note that the `user` and `password` fields should be changed to database credentials provided by the CISM. If you do not have these credentials but are an authorized collaborator, email <joebrew@gmail.com>.

#### 4. Process the data

The raw data is not in suitable form for analysis. By running the below script, you'll create [tidy datasets](http://vita.had.co.nz/papers/tidy-data.html) from the raw data, which can then be called by simply naming them after attaching the `maragra` package.

To process data, run the following in an R session from within the `maragra` directory:

    setwd('data-raw')
    source('create_data_files.R')

Alternatively, you can run this directly from bash:

    $ cd data-raw
    $ Rscript create_data_files.R

Having run this, you'll now have the following datasets for analysis:

-   ab
-   ab\_panel
-   bairros
-   bairros\_maragra\_bairro
-   bairros\_maragra\_fabrica
-   bes
-   census
-   clinic
-   clinic\_agg
-   costs
-   costs\_itemized
-   costs\_raw
-   hiv\_prevalence
-   irs
-   mc
-   weather
-   workers

#### 5. Install the code in the repository and build the R package.

    # !/usr/bin/R
    devtools::install('maragra')
    devtools::document('maragra')

If the above fails, it will most likely be due to R package dependencies which you do not have. Examine the error messages, and install R packages as necessary. Note that the `cism` and `brew` packages are available from <https://github.com/joebrew>, whereas the `databrew` package is available from <https://github.com/databrew>.

As an alternative to the above, or to update documentation (including this README), run the following in an R session from within the `maragra` directory:

    source('build_package.R')

Alternatively, you can run the script from directly within bash:

    $ Rscript build_package.R

#### 6. Run the tests

Unit tests have been written to ensure that the processed data and packages are behaving correctly. Run these tests by running the following in an R session from within the `maragra` directory:

    setwd('tests')
    source('testthat.R')

#### 7. Generate outputs

All analysis is written in the `.Rmd` format. Analysis files are in the `inst/rmd` folder. These can be knitted directly using `rmarkdown::render`, or called via specific functions. For example, to compile the "Maragra clinical malaria incidence" report, simply run the following from within an R session:

    maragra::generate_maragra_clinical_malaria_incidence()

All functions which begin with the term `generate` will produce an output (either pdf or html). These functions also all share arguments for `date`, `output_dir`, and `output_file`. So, one could specify that the output an analysis should go to the `outputs` folder and be named "incidence\_report.hml" as follows:

    maragra::generate_maragra_clinical_malaria_incidence(date = Sys.Date(),
                                                         output_dir = 'outputs',
                                                         output_file = 'incidence_report.html')

To run *all* analyses, and produce all reports and papers to the `outputs` folder, once can simply run the following from an R session within the `maragra` folder:

    maragra::run_all_analyses(output_dir = 'outputs')

Details
-------

### Funding

This research is for Joe Brew's PhD. Full details at [www.economicsofmalaria.com](http://economicsofmalaria.com) He is generously funded by the Erasmus Mundus Joint Doctorate Fellowship, Specific Grant Agreement 2016-1346. His program of study is the [Transdisciplinary Global Health programme](http://www.transglobalhealth.org/).

### Collaboration

A debt of gratitude is owed to the following people and institutions, without whom this research would not be possible:

#### People

-   Elisa Sicuri, for her guidance and expertise
-   Laia Cirera, for her on-the-ground knowledge and willingness to work hard
-   Jacqueline Broerse, for her global persepctive and critical contributions
-   Menno Pradhan, for his technical knowledge and experience
-   Kizito Gondo, for helping to make this collaboration go from idea to reality
-   Elton Dorkin, for his subject matter insight and administrative familiarity
-   The entire team at the Centro de Investigação em Saude de Manhiça

#### Institutions

-   Maragra Açucar and Illovo Sugar (Mozambique)
-   Institut de Salut Global de Barcelona (Spain)
-   Centro de Investigação de Saude de Manhiça (Mozambique)
-   Vrije Universiteit, Amsterdam (Netherlands)
-   The European Commission, Erasmus Mundus fellowship, and Transdisciplinary Global Health programme (Europe)
