#![no_main]

#[link(wasm_import_module = "wayland")]
unsafe extern "C" {
    #[link_name = "wl_registry_global"]
    fn wl_registry_global(name: u64, iface: &str, version: u64);
}

#[unsafe(export_name = "wl_registry_global")]
pub extern "C" fn _bar(name: u64, iface: &str, version: u64) {
    unsafe {
        wl_registry_global(
            name,
            &format!("name={name},iface={iface},version={version}"),
            version,
        )
    };
}
