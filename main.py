from subprocess import check_call, run, Popen
from os import path, environ, makedirs
from sys import executable

try:
    from winapps import list_installed
except ImportError:
    check_call([executable, '-m', 'pip', 'install', 'winapps'])
    from winapps import list_installed

for item in list_installed():
    if "R for Windows" in item.name:
        R = f"{item.install_location}\\bin\\x64\\Rscript.exe"

# Setting libraries location
Rversion = run(f"{R} -e \"getRversion() |> as.character() |> cat()\"", capture_output = True, text = True).stdout.strip()
Rlib = path.join(environ["USERPROFILE"], f"Documents\\R\\win-library\\{Rversion}")

if not path.exists(Rlib):
    makedirs(Rlib)
    path = path.join(environ["USERPROFILE"], "Documents\\.Rprofile")
    with open(path, 'a') as Rprofile:
        Rprofile.write(f".libPaths(\"~/R/win-library/{Rversion}\")\n")

shiny = "if(is.na(match('shiny', installed.packages()))) { install.packages('shiny', repos = 'http://cran.us.r-project.org') }"
run(f"{R} -e \"{shiny}\"")

# Verifying quarto installation
quarto_path = run("where quarto.exe", capture_output = True, text = True).stdout.strip()

app = "shiny::runGitHub('dashboard-reactor', 'jsmendozap', ref = 'main', launch.browser = T)"
command = f'{R} -e "{app}" "{quarto_path}"'

# Executing the application 
Popen(command, shell = True)