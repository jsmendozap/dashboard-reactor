@echo off

rem Setting the path to the Rscript executable
PATH C:\Program Files\R\R-4.2.3\bin\x64

rem Executing Shiny applicaion from GitHub
Rscript -e "path <- paste0('~/R/win-library/', getRversion()); if(!dir.exists(path)) {dir.create(path, recursive = T); Sys.setenv(R_LIBS_USER = path)}"
Rscript -e "if(is.na(match('shiny', installed.packages()))) { .libPaths(Sys.getenv('R_LIBS_USER')); install.packages('shiny', repos = 'http://cran.us.r-project.org') }"
Rscript -e "shiny::runGitHub('dashboard-reactor', 'jsmendozap', ref = 'main', launch.browser = T)"
pause
