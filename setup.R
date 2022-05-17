# Super quick load (no install)
library(devtools) ; document() ; load_all()


# Full load and check
library(devtools) ; document() ; load_all() ; check() ; install() ; library(breastfeeding)

# Create package environment

#library(devtools)
#create_package('/Users/erickeen/repos/breastfeeding')

use_pipe(export=TRUE)

# Import packages
if(FALSE){
  use_package('magrittr')
  use_package('plyr')
  use_package('dplyr')
  use_package('ggplot2')
  use_package('readr')
  use_package('stringr')
  use_package('lubridate')
  use_package('usethis')
  use_package('devtools')
  use_package('shiny')
  use_package('shinyjs')
  use_package('shinydashboard')
  use_package('shinythemes')
  use_package('shinyWidgets')
  use_package('rintrojs')
  use_package('DT')
}

#use_mit_license()


#### Try it out

# Install `suRvey`breasfeeding`
library(remotes)
remotes::install_github('ericmkeen/breastfeeding',
                         force=TRUE)
library(breastfeeding)
breastfeeding()



