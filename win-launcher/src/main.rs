#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use eframe::egui;
use egui::RichText;
use std::process::{Child, Command, Stdio};
use std::os::windows::process::CommandExt;
use std::sync::{Arc, Mutex, mpsc::{self, Sender, Receiver}};
use std::thread;
use std::io::{BufRead, BufReader};
use std::path::Path;
use std::env;
use winreg::enums::*;
use winreg::RegKey;
use rfd::FileDialog;
use chrono::Local;

struct MyApp {
    log: String,                        
    r_path: Arc<Mutex<Option<String>>>, 
    quarto_path: Arc<Mutex<Option<String>>>, 
    log_receiver: Receiver<String>,     
    log_sender: Sender<String>,
    app_launched: bool,
    r_process: Arc<Mutex<Option<Child>>>         
}

impl MyApp {
    fn new(
        r_path: Arc<Mutex<Option<String>>>,
        quarto_path: Arc<Mutex<Option<String>>>,
        log_receiver: Receiver<String>,
        log_sender: Sender<String>,
    ) -> Self {
        Self {
            log: String::new(),
            r_path,
            quarto_path,
            log_receiver,
            log_sender,
            app_launched: false,
            r_process: Arc::new(Mutex::new(None))
        }
    }

    fn stop_r_process(&mut self) {
        let mut process_guard = self.r_process.lock().unwrap();
        if let Some(mut child) = process_guard.take() {
            let _ = child.kill();
            let _ = child.wait(); 
        }
    }
}

impl eframe::App for MyApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        
        // Execute Shiny app if R and Quarto are detected
        if !self.app_launched 
            && self.r_path.lock().unwrap().is_some() 
            && self.quarto_path.lock().unwrap().is_some() 
        {
            let r_path = self.r_path.lock().unwrap().clone().unwrap();
            let sender = self.log_sender.clone();
            let r_process_clone = self.r_process.clone(); // Clonar el Arc<Mutex>
            
            thread::spawn(move || {
                if let Some(child) = run_shiny(r_path, sender) {
                    let mut guard = r_process_clone.lock().unwrap();
                    *guard = Some(child);
                }
            });
            
            self.app_launched = true;
        }

        while let Ok(msg) = self.log_receiver.try_recv() {
            let timestamp = Local::now().format("[%H:%M:%S]").to_string();
            self.log.push_str(&format!("{} {}\n", timestamp, msg));
        }
        
        ctx.request_repaint();

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.vertical(|ui| {

                // Header
                ui.add_space(10.0);
                ui.heading("🛠 Dashboard Reactor");
                ui.add_space(5.0);
                ui.separator();
                ui.add_space(15.0);

                // Binaries
                ui.group(|ui| {
                    ui.label(RichText::new("🔧 Configuration").size(16.0));
                    ui.add_space(10.0);

                    // R
                    ui.horizontal(|ui| {
                        let r_path = self.r_path.lock().unwrap().clone();
                       
                        ui.label("R:");
                        ui.label(
                            RichText::new(if r_path.is_some() { "✅ Detected" } else { "⚠️ Not detected" })
                                .color(if r_path.is_some() { egui::Color32::GREEN } else { egui::Color32::YELLOW })
                        );

                        if let Some(path) = r_path {
                            ui.with_layout(
                                egui::Layout::left_to_right(egui::Align::Center),
                                |ui| {
                                    ui.label(
                                        RichText::new(path)
                                            .size(12.0)
                                            .color(egui::Color32::GRAY)
                                            .weak()
                                    );
                                }
                            );
                        } else {
                            if ui.button("📂 Select Rscript location").clicked() {
                                if let Some(path) = FileDialog::new().pick_file() {
                                    *self.r_path.lock().unwrap() = Some(path.display().to_string());
                                }
                            }
                        }
                    });

                    ui.add_space(5.0);

                    // Quarto
                    ui.horizontal(|ui| {
                        let quarto_path = self.quarto_path.lock().unwrap().clone();
                        
                        ui.label("Quarto:");
                        ui.label(
                            RichText::new(if quarto_path.is_some() { "✅ Detected" } else { "⚠️ Not detected" })
                                .color(if quarto_path.is_some() { egui::Color32::GREEN } else { egui::Color32::YELLOW })
                        );

                        if let Some(path) = quarto_path {
                            ui.with_layout(
                                egui::Layout::left_to_right(egui::Align::Center),
                                |ui| {
                                    ui.label(
                                        RichText::new(path)
                                            .size(12.0)
                                            .color(egui::Color32::GRAY)
                                            .weak()
                                    );
                                }
                            );
                        } else {
                            if ui.button("🔄 Relocated Quarto").clicked() {
                                *self.quarto_path.lock().unwrap() = detect_quarto().ok();
                            }
                        }
                    });
                });

                // Logs section
                ui.group(|ui| {
                    ui.label(RichText::new("📝 Log").heading());
                    ui.add_space(10.0);
                    
                    egui::ScrollArea::vertical()
                        .max_height(200.0)
                        .stick_to_bottom(true)
                        .show(ui, |ui| {
                            ui.add(
                                egui::TextEdit::multiline(&mut self.log)
                                    .desired_rows(12)
                                    .font(egui::TextStyle::Monospace)
                                    .frame(true)
                                    .desired_width(f32::INFINITY)
                            );
                        });
                    
                    ui.add_space(5.0);
                    ui.horizontal(|ui| {
                        if ui.button("🧹 Clean log").clicked() {
                            self.log.clear();
                        }
                    });
                });
            });

            // Footer
            egui::TopBottomPanel::bottom("footer").show(ctx, |ui| {
                ui.horizontal(|ui| {
                    ui.spacing_mut().item_spacing.x = 0.0;
                    ui.label("v1.0.0 • ");
                    ui.hyperlink_to("GitHub", "https://github.com/jsmendozap/dashboard-reactor");
                });
            });
        });
    }

    fn on_exit(&mut self, _gl: Option<&eframe::glow::Context>) {
        self.stop_r_process();
    }
}

fn main() -> eframe::Result<()> {
    let (log_sender, log_receiver) = mpsc::channel();

    let app = MyApp::new(
        Arc::new(Mutex::new(find_r_path())),
        Arc::new(Mutex::new(detect_quarto().ok())),
        log_receiver,
        log_sender
    );

    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_inner_size([540.0, 450.0])
            .with_icon(load_icon()), 
        ..Default::default()
    };

    eframe::run_native(
        "Dashboard Reactor",
        options,
        Box::new(|_| Ok(Box::new(app))),
    )
}

// Searching R in Windows Registry
fn find_r_path() -> Option<String> {
    const REGISTRY_KEY: &str = "SOFTWARE\\R-Core\\R";
    let hklm = RegKey::predef(HKEY_LOCAL_MACHINE);
    hklm.open_subkey(REGISTRY_KEY)
        .ok()?
        .get_value::<String, &str>("InstallPath") 
        .map(|path| format!("{}\\bin\\x64\\Rscript.exe", path))
        .ok()
}

fn run_shiny(r_path: String, sender: Sender<String>) -> Option<std::process::Child> {
    // Detecting Quarto
    let quarto_path = match detect_quarto() {
        Ok(path) => path,
        Err(e) => {
            sender.send(format!("[ERROR] {}", e)).unwrap();
            return None;
        }
    };

    let r_script = format!(
        r##"
        tryCatch({{

            user_lib <- normalizePath(file.path(Sys.getenv("USERPROFILE"), "Documents", "R", "lib", as.character(getRversion())))
            if (!dir.exists(user_lib)) {{
                cat("Creating user library\n")
                dir.create(user_lib, recursive = TRUE)
                Rprofile <- normalizePath(file.path(Sys.getenv("USERPROFILE"), "Documents", ".Rprofile"))
                cat(sprintf("%s", .libPaths(user_lib)), file = Rprofile, sep = "\n")
                if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
            }} else {{
                .libPaths(c(user_lib, .libPaths()))
            }}

            shiny::runGitHub(
                repo = "dashboard-reactor",
                username = "jsmendozap",
                ref = "main",
                launch.browser = TRUE
            )

        }}, error = function(e) {{
            message("ERROR: ", e$message)
            quit(status = 1)
        }})
        "##,
    );

    // 3. Executing R 
    let mut cmd = match Command::new(&r_path)
        .args(["--vanilla", "-e", &r_script, &quarto_path])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .creation_flags(0x08000000)
        .spawn()
    {
        Ok(c) => c,
        Err(e) => {
            sender.send(format!("[ERROR] It was not possible to execute R: {}", e)).unwrap();
            return None;
        }
    };

    // 4. Catching output and errors
    if let Some(stdout) = cmd.stdout.take() {
        let sender_stdout = sender.clone();
        thread::spawn(move || {
            let reader = BufReader::new(stdout);
            for line in reader.lines() {
                let line = line.unwrap_or_else(|e| format!("[ERROR] Lectura stdout: {}", e));
                let cleaned_line = if line.contains("Listening on") {
                    format!("[URL] {}", extract_shiny_url(&line))
                } else {
                    line
                };
                let _ = sender_stdout.send(cleaned_line);
            }
        });
    }

    if let Some(stderr) = cmd.stderr.take() {
        let sender_stderr = sender.clone();
        thread::spawn(move || {
            let reader = BufReader::new(stderr);
            for line in reader.lines() {
                let line = line.unwrap_or_else(|e| format!("[ERROR] Lectura stderr: {}", e));
                let _ = sender_stderr.send(format!("{}", line));
            }
        });
    }

    Some(cmd)
}

// Helper functions
fn detect_quarto() -> Result<String, String> {
    // Looking for quarto installation in Windows Registry
    let reg_key = RegKey::predef(HKEY_LOCAL_MACHINE)
        .open_subkey(r"SOFTWARE\Quarto")
        .and_then(|k| k.get_value::<String, _>("InstallPath"));
    
    if let Ok(path) = reg_key {
        let full_path = format!(r"{}\bin\quarto.exe", path);
        if Path::new(&full_path).exists() {
            return Ok(full_path);
        }
    }

    // Other common locations
    let paths: Vec<String> = vec![
    r"C:\Program Files\Quarto\bin\quarto.exe".to_string(),
    r"C:\Program Files (x86)\Quarto\bin\quarto.exe".to_string(),
    format!(r"{}\AppData\Local\Programs\Quarto\bin\quarto.exe", env::var("USERPROFILE").unwrap())
];


    for path in paths {
        if Path::new(&path).exists() {
            return Ok(path.to_string());
        }
    }

    Err("Quarto not found. Please download it from: https://quarto.org".into())
}

fn extract_shiny_url(line: &str) -> String {
    line.replace("http://", "")
        .replace("Listening on ", "")
        .trim()
        .to_string()
}

fn load_icon() -> egui::IconData {
    let icon_bytes = include_bytes!("../assets/icon.png");
    let image = image::load_from_memory(icon_bytes).unwrap();
    let rgba = image.into_rgba8();
    
    egui::IconData {
        rgba: rgba.to_vec(),
        width: rgba.width(),
        height: rgba.height(),
    }
}