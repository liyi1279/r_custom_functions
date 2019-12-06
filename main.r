## copy from website
## usage example
#packages <- c('ggplot2', 'afex')
#CheckPackages(packages)
CheckPackages <- function(pkg){
  # check if biocManager install
  pkg.manager <- ('BiocManager' %in% installed.packages()[,'Package'])
  if(!pkg.manager) install.packages('BioManager')
    
  # check if input package installed
  new.pkg <- pkg[!(pkg %in% installed.packages()[, 'Package'])]
  if(length(new.pkg)) BiocManager::install(new.pkg)
  
  # load all packages
  sapply(pkg, require, character.only=TRUE)
}
