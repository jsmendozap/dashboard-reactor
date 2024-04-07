@echo off

rem Setting the path to the Rscript executable
PATH C:\Program Files\R\R-4.3.3\bin\x64

rem Setting libraries location 
Rscript -e "path <- paste0('~/R/win-library/', getRversion()); if(!dir.exists(path)) { dir.create(path, recursive = T) }"
Rscript -e "path <- paste0('~/R/win-library/', getRversion()); cat(paste0('.libPaths(\"', path, '\")'), sep = '\n', file = '~/.Rprofile', append = TRUE)"

rem Installing shiny if required
Rscript -e "if(is.na(match('shiny', installed.packages()))) { install.packages('shiny', repos = 'http://cran.us.r-project.org') }"

echo Executing Shiny application from GitHub
Rscript -e "shiny::runGitHub('dashboard-reactor', 'jsmendozap', ref = 'main', launch.browser = T)"

pause
