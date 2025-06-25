use std::{
    collections::{
        HashMap,
        hash_map::Entry::{Occupied, Vacant},
    },
    env,
    fs::File,
    io::BufReader,
    path::{Path, PathBuf},
    sync::LazyLock,
};

use serde_derive::Deserialize;

#[derive(Debug, Deserialize, Clone)]
pub struct Protocol {
    #[serde(rename = "@name")]
    pub name: String,
    #[serde(default, rename(deserialize = "interface"))]
    pub interfaces: Vec<Interface>,
}

#[derive(Debug, Deserialize, Clone)]
pub struct Interface {
    #[serde(rename = "@name")]
    pub name: String,
    #[serde(rename = "@version")]
    pub version: u32,
    #[serde(default, rename = "request")]
    pub requests: Vec<Message>,
    #[serde(default, rename = "event")]
    pub events: Vec<Message>,
}

#[derive(Debug, Deserialize, Clone)]
pub struct Message {
    #[serde(rename = "@name")]
    pub name: String,
    #[serde(rename = "@type")]
    pub ty: Option<MessageType>,
    #[serde(default, rename = "arg")]
    pub args: Vec<Arg>,
}

#[derive(Debug, Deserialize, Clone)]
pub struct Arg {
    #[serde(rename = "@name")]
    pub name: String,
    #[serde(rename = "@type")]
    pub ty: Type,
    #[serde(rename = "@interface")]
    pub interface: Option<String>,
    #[serde(default, rename = "@allow-null")]
    pub allow_null: bool,
    #[serde(rename = "@enum")]
    pub enu: Option<String>,
}

#[derive(Debug, Deserialize, PartialEq, Eq, Copy, Clone)]
#[serde(rename_all = "snake_case")]
pub enum Type {
    Int,
    Uint,
    Fixed,
    String,
    Object,
    NewId,
    Array,
    Fd,
}

#[derive(Debug, Deserialize, PartialEq, Eq, Copy, Clone)]
#[serde(rename_all = "snake_case")]
pub enum MessageType {
    Destructor,
}

pub static PROTOCOLS: LazyLock<HashMap<String, Protocol>> = LazyLock::new(|| {
    let base_directories = env::var("XDG_DATA_DIRS").map_or(
        vec![
            PathBuf::from("/usr/local/share"),
            PathBuf::from("/usr/share"),
        ],
        |dirs| dirs.split(':').map(PathBuf::from).collect(),
    );
    let mut protocols = HashMap::new();
    let mut read_protocol = |path: &Path| -> Result<(), String> {
        match protocols.entry(
            path.file_stem()
                .ok_or("cannot get protocol name")?
                .to_str()
                .ok_or("protocol name should be valid utf-8")?
                .to_string(),
        ) {
            Occupied(_) => {}
            Vacant(e) => {
                e.insert(
                    quick_xml::de::from_reader(BufReader::new(
                        File::open(&path).map_err(|e| format!("failed to read protocol: {e}"))?,
                    ))
                    .map_err(|e| format!("failed to parse protocol: {e}"))?,
                );
            }
        }
        Ok(())
    };
    for mut dir in base_directories {
        dir.push("wayland");
        dir.push("wayland.xml");
        if dir.exists() {
            let _ = read_protocol(&dir);
        }
        dir.pop();
        dir.pop();
        dir.push("wayland-protocols");
        let mut namespace_dir = dir.clone();
        for namespace in dir.read_dir().into_iter().flatten() {
            let Ok(namespace) = namespace else {
                continue;
            };
            let namespace = namespace.file_name();
            namespace_dir.push(&namespace);
            let mut protocol_dir = namespace_dir.clone();
            for protocol in namespace_dir.read_dir().into_iter().flatten() {
                let Ok(protocol) = protocol else {
                    continue;
                };
                let protocol = protocol.file_name();
                // HACK: Ignore old xdg-shell protocol that overwrite interfaces
                if namespace.to_string_lossy() == "unstable"
                    && protocol.to_string_lossy().starts_with("xdg-shell")
                {
                    continue;
                }
                protocol_dir.push(protocol);
                let mut version_dir = protocol_dir.clone();
                for version in protocol_dir.read_dir().into_iter().flatten() {
                    let Ok(version) = version else {
                        continue;
                    };
                    let version = version.file_name();
                    version_dir.push(version);
                    let _ = read_protocol(&version_dir);
                    version_dir.pop();
                }
                protocol_dir.pop();
            }
            namespace_dir.pop();
        }
        dir.pop();
        dir.push("wayland-wlroots");
        let mut protocol_dir = dir.clone();
        for protocol in dir.read_dir().into_iter().flatten() {
            let Ok(protocol) = protocol else {
                continue;
            };
            let protocol = protocol.file_name();
            protocol_dir.push(protocol);
            let _ = read_protocol(&protocol_dir);
            protocol_dir.pop();
        }
        dir.pop();
    }
    protocols
});

pub static INTERFACE_TO_PROTOCOL: LazyLock<HashMap<String, String>> = LazyLock::new(|| {
    PROTOCOLS
        .iter()
        .flat_map(|(name, Protocol { interfaces, .. })| {
            interfaces
                .iter()
                .map(|interface| (interface.name.clone(), name.clone()))
        })
        .collect()
});
