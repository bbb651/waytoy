//! `rustc --target=wasm32-unknown-unknown --edition=2024 examples/interface_filter.rs`

#![no_std]
#![no_main]
#![allow(wasm_c_abi)]
#![allow(improper_ctypes)]
#![allow(improper_ctypes_definitions)]

const INTERFACES: &[&'static str] = &[
    "wl_compositor",
    "wl_seat",
    "wl_shm",
    "xdg_wm_base",
    "wl_data_device_manager",
];
const BLACKLIST: bool = false;

#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}

#[link(wasm_import_module = "wayland")]
unsafe extern "C" {
    #[link_name = "wl_registry_global"]
    fn wl_registry_global(wl_registry: u32, name: u32, interface: &str, version: u32);
}

#[unsafe(export_name = "wl_registry_global")]
pub extern "C" fn _wl_registry_global(wl_registry: u32, name: u32, interface: &str, version: u32) {
    // TODO: Also error on `wl_registry_bind` with disallowed name?
    if INTERFACES.contains(&interface) != BLACKLIST {
        unsafe {
            wl_registry_global(wl_registry, name, interface, version);
        }
    }
}
