# What Benefits Drive Membership in Medicare Advantage Plans?

This repository contains the supplementary material to the chapter 'What Benefits Drive Membership in Medicare Advantage Plans?' 

## Software Requirements

To run the code in this you need an updated version of RStudio and the libraries ```readxl```, ```tidyverse```, ```FactoMineR``` and ```factoextra``` installed. 

- To install RStudio, make sure you have a recent verison of R installed. In case you do not, please go to the [R Project installation website](https://www.r-project.org) and follow the instructions therein. Then, install RStudio following the instructions [here](https://posit.co/downloads/).

- Open RStudio and run the following command in the console ```install.packages(c('readxl', 'tidyverse', 'FactoMineR', 'factoextra'))```.

## Downloading the Repository

To download this repository, open your terminal and type `cd [YOUR SELECTED LOCATION]`, then type ```git clone https://github.com/judmejiabe/ma_chapter.git```. The files in this repository will apear in your selected location. Alternatively, you can click Code > Download ZIP and the repository files will be downloaded in a compressed folder named ```ma_chapter-main```.

## Running the Repository Code

To run the code in this repository, please open the file ```ma_chapter.RProj```; RStudio will open automatically and you will see the project ```ma_chapter``` in the right corner of the user interface. Then select the tab ```Files``` and click on ```accompanying_code.R```. The script will open and run it by clicking 'Source' on the top right corner of the script. This repository also contains the following files:

- ```Benefit_Total.xlsx``` contains the benefits and features file.
- ```Benefit_Dictionary.xlsx``` is the dictionary of the benefits and features file.
- ```Enrollment_Total.xlsx``` contains the plan enrollment and eligibility data.
- ```macros.txt```, graceously provided by [Professor Luis Hernando Vanegas Penagos](http://www.hermes.unal.edu.co/pages/Docentes/Docente.jsf?u=lhvanegasp) at National University of Colombia, contains a handful of useful functions for generalized linear modelling and regression analysis.
- ```profile_plot.R``` contains the code to do the profile in figure 3 of the document.
