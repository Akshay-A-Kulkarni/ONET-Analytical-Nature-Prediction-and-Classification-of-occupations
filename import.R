packages <- c("tidyverse","readxl","scales","mice","randomForest","MASS","caret","klaR","writexl","modelr","kernlab") # add any packages that need to be installed to this vec.
checkPackage <- function(package_vec){                 # defining a custom function for checking packages.
                            for (p in package_vec){
                                if(p %in% rownames(installed.packages()) == FALSE){
                                    cat(paste(p,"Package is not found/installed on this machine,                  installing the required package... \n"))
                                    install.packages(p,dependencies = TRUE) # Installing with dependancies
                                } else {
                                cat(paste("[",p,"]","is present. \n"))
                              }
                            }
                          }
                          
checkPackage(packages) # running check 
