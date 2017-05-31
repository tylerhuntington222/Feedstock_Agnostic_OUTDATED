# Feedstock Agnostic Project

This project aims to assess the feasibility of a hypothetical "feedstock agnostic" lignocellulosic biorefinery in the US. Such a biorefinery would be capable of accepting different types of lignocellulosic biomass, thus enabling it to operate at high capacity throughout the year. 

### Guide to the Directory

The project files have been organized into a directory structure that is designed to maximize portability and reproducibility. The directory structure is also well suited for easily bundling
project files into an R Package down the line. Here, I will provide a brief orientation to the directory and general underlying workflow.

You are currently in the Feedstock_Agnostic main directory which is hosted by GitHub as a 
"repository" (or repo) of files. Downloading the repo as a ZIP file will give you an up-to-date
version of the main project directory. I would recommend doing this every time you are interested 
in checking out models, code, etc. in the directory because I am constantly making updates and frequently push these changes to GitHub.  The main project directory is also designed to be 
an entirely self-contained ecosystem. That is, no matter where you store the directory on you local machine, all* scripts within the subdirectories should run and retrieve the data they need 
from other subdirectories using relative filepaths. The one exception to this is the `load.R` script within the R_code subdirectory. This script was developed to initially load the raw data files required for the project (i.e. `.csv`, `.shp`, `.xls`, etc.) and convert them an R-specific
binary data file (.RData) which could be loaded and handled more easily in subsequent processing
steps. Since the raw data files loaded by `load.R` are too large to store on GitHub, they are
housed separately in a `raw_data_files` subdirectory of the Google Drive's  `Feedstock_Agnostic/Analysis/feedstock_agnostic_Tyler` directory. 

The following provides an overview of each subdirectory, and the files contained within

#### R_code

This subdirectory contains all the R source code and scripts for the project. 

`load.R` - An R script that takes the raw data files required for the project as input and outputs a binary workspace image (`.RData`) to the`\Feedstock_Agnostic\raw_binary_data` directory. This workspace image is loaded into the `clean.R` script for data cleaning and organization. 

`clean.R` - An R script that takes the workspace image outputted by `load.R` as input, cleans and organizes datasets as needed for subsequent analyses and outputs individual binary data files (`.rds`) to the `\Feedstock_Agnostic\clean_binary_data` directory.

`<name>_func.R` - A script containing a function (or functions) that are sourced and called in 
analysis scripts. The file name should generally reflect the aspect of the analysis to which the function(s) pertain. The header comment at the top of the script itself will provide a more detailed description of the function's purpose, inputs and outputs. 

`<name>_analsysis.R` - An analysis script that loads cleaned binary data from `\Feedstock_Agnostic\clean_binary_data` and sources `<name>.func.R` files to call functions 
designed for the particular analysis being performed. The file name generally reflects the nature of the analysis. The header comment at the top of the script itself will provide a more detailed description of what it does and it's outputs (i.e. figures, files, reformatted data etc.)

#### python_code

This subdirectory follows parallel structure of the the `R_code` directory with the only difference being that scripts are written in Python and thus have the extension `.py` instead of `.R`.  Additionally, 

#### figures

This subdirectoy is where all figures and graphs will be outputted from analysis scripts. A `README.txt` file also located here for including written documentation of figures if necessary. 

#### documentation

This subdirectory contains supporting written documents for analysis. The `methods.txt` file which details the analytical methodology of the project and the `Tyler_log.gdoc` is a running log where I am keeping notes of my daily progress. 

#### raw_binary_data

This subdirectory contains the raw_binary_data files outputted by the `load.R` script described above. 

#### clean_binary_data

This subdirectory contains the clean_binary_data files outputted by the `clean.R` script described above. These files are treated as read-only in analysis scripts.

#### Other Files

For all intents and purposes you can ignore the `.gitattributes`, `.gitignore` and `README.md`
files in the main project directory. Thes first two exist to administer git functionality for version controlling of the project and syncing local git repos with GitHub.com. The `README.md` file is what you are looking at right now! I hope it has been helfpul. 







