import subprocess
import os 

R = r"C:\Program Files\R\R-4.2.1\bin\x64\Rscript.exe"  
app = r"C:\Users\juan\Downloads\dashboard-reactor-development\app.R" 
#"shiny::runGitHub('dashboard-reactor', 'jsmendozap', ref = 'main', launch.browser = T)"

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

# Verificar la ubicación de quarto.exe
quarto_path = subprocess.run("where quarto.exe", capture_output = True, text = True).stdout.strip()

# Construir el comando para ejecutar el script R
command = f'"{R}" "{app}" "{quarto_path}"'

# Ejecutar el comando y capturar la salida
subprocess.Popen(command, shell = True)