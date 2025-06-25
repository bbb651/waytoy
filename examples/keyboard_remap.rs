//! `rustc --target=wasm32-unknown-unknown --edition=2024 examples/keyboard_remap.rs`

#![no_std]
#![no_main]

#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}

#[link(wasm_import_module = "wayland")]
unsafe extern "C" {
    #[link_name = "wl_keyboard_modifiers"]
    fn wl_keyboard_modifiers(
        wl_keyboard: u32,
        serial: u32,
        mods_depressed: u32,
        mods_latched: u32,
        mods_locked: u32,
        group: u32,
    );

    #[link_name = "wl_keyboard_key"]
    fn wl_keyboard_key(wl_keyboard: u32, serial: u32, time: u32, key: u32, state: u32);
}

static mut CTRL: bool = false;

#[unsafe(export_name = "wl_keyboard_modifiers")]
pub extern "C" fn _wl_keyboard_modifiers(
    wl_keyboard: u32,
    serial: u32,
    mods_depressed: u32,
    mods_latched: u32,
    mods_locked: u32,
    group: u32,
) {
    unsafe {
        CTRL = (mods_depressed >> 2) & 1 != 0;
        wl_keyboard_modifiers(
            wl_keyboard,
            serial,
            mods_depressed,
            mods_latched,
            mods_locked,
            group,
        );
    }
}

#[unsafe(export_name = "wl_keyboard_key")]
pub extern "C" fn _wl_keyboard_key(wl_keyboard: u32, serial: u32, time: u32, key: u32, state: u32) {
    unsafe {
        wl_keyboard_key(
            wl_keyboard,
            serial,
            time,
            match key {
                25 if CTRL => 103,
                49 if CTRL => 108,
                code => code,
            },
            state,
        );
    }
}
