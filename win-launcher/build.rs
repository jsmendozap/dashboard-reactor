use winres::WindowsResource;

fn main() {
    if cfg!(target_os = "windows") {
        WindowsResource::new()
            .set_icon("assets/icon.ico")
            .set("ProductName", "Dashboard Reactor")
            .set("FileDescription", "Analysis and visualization of reactor, MS and GC data")
            .set("LegalCopyright", "Copyright © 2024")
            .compile()
            .unwrap();
    }
}