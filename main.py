import subprocess
import os 
import sys

try:
    import winapps
except ImportError:
    subprocess.check_call([sys.executable, '-m', 'pip', 'install', 'winapps'])
    import winapps

for item in winapps.list_installed():
    if "R for Windows" in item.name:
        R = f"{item.install_location}\\bin\\x64\\Rscript.exe"

app = "shiny::runGitHub('dashboard-reactor', 'jsmendozap', ref = 'main', launch.browser = T)"

# Setting libraries location
Rversion = subprocess.run(f"{R} -e \"getRversion() |> as.character() |> cat()\"", capture_output = True, text = True).stdout.strip()
Rlib = os.path.join(os.environ["USERPROFILE"], f"Documents\\R\\win-library\\{Rversion}")

if not os.path.exists(Rlib):
    os.makedirs(Rlib)
    path = os.path.join(os.environ["USERPROFILE"], "Documents\\.Rprofile")
    with open(path, 'a') as Rprofile:
        Rprofile.write(f".libPaths(\"~/R/win-library/{Rversion}\")\n")

shiny = "if(is.na(match('shiny', installed.packages()))) { install.packages('shiny', repos = 'http://cran.us.r-project.org') }"
subprocess.run(f"{R} -e \"{shiny}\"")

# Verifying quarto installation
quarto_path = subprocess.run("where quarto.exe", capture_output = True, text = True).stdout.strip()
command = f'{R} -e "{app}" "{quarto_path}"'

# Executing the application 
subprocess.Popen(command, shell = True)
