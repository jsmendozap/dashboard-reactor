@echo off

rem Setting the path to the Rscript executable
PATH C:\Program Files\R\R-4.3.3\bin\x64

rem Executing Shiny applicaion from GitHub
Rscript -e "if(!rlang::is_installed('shiny')) { install.packages('shiny') }"
Rscript -e "shiny::runGitHub('dashboard-reactor', 'jsmendozap', ref = 'main', launch.browser = T)"
pause
