use std::sync::LazyLock;

#[derive(Debug)]
pub struct Protocol {
    pub name: String,
    pub interfaces: Vec<Interface>,
}

#[derive(Debug)]
pub struct Interface {
    pub name: String,
    pub requests: Vec<Message>,
    pub events: Vec<Message>,
}

#[derive(Debug)]
pub struct Message {
    pub name: String,
    pub typ: Option<Type>,
    pub args: Vec<Arg>,
}

#[derive(Debug)]
pub struct Arg {
    pub name: String,
    pub typ: Type,
    pub interface: Option<String>,
    pub allow_null: bool,
    pub enu: Option<String>,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Type {
    Int,
    Uint,
    Fixed,
    String,
    Object,
    NewId,
    Array,
    Fd,
    Destructor,
}

pub static WAYLAND: LazyLock<Protocol> = LazyLock::new(|| Protocol {
    name: "wayland".to_string(),
    interfaces: vec![
        Interface {
            name: "wl_display".to_string(),
            requests: vec![
                Message {
                    name: "sync".to_string(),
                    typ: None,
                    args: vec![Arg {
                        name: "callback".to_string(),
                        typ: Type::NewId,
                        interface: Some("wl_callback".to_string()),
                        allow_null: false,
                        enu: None,
                    }],
                },
                Message {
                    name: "get_registry".to_string(),
                    typ: None,
                    args: vec![Arg {
                        name: "registry".to_string(),
                        typ: Type::NewId,
                        interface: Some("wl_registry".to_string()),
                        allow_null: false,
                        enu: None,
                    }],
                },
            ],
            events: vec![
                Message {
                    name: "error".to_string(),
                    typ: None,
                    args: vec![
                        Arg {
                            name: "object_id".to_string(),
                            typ: Type::Object,
                            interface: None,
                            allow_null: false,
                            enu: None,
                        },
                        Arg {
                            name: "code".to_string(),
                            typ: Type::Uint,
                            interface: None,
                            allow_null: false,
                            enu: None,
                        },
                        Arg {
                            name: "message".to_string(),
                            typ: Type::String,
                            interface: None,
                            allow_null: false,
                            enu: None,
                        },
                    ],
                },
                Message {
                    name: "delete_id".to_string(),
                    typ: None,
                    args: vec![Arg {
                        name: "id".to_string(),
                        typ: Type::Uint,
                        interface: None,
                        allow_null: false,
                        enu: None,
                    }],
                },
            ],
        },
        Interface {
            name: "wl_registry".to_string(),
            requests: vec![],
            events: vec![Message {
                name: "global".to_string(),
                typ: None,
                args: vec![
                    Arg {
                        name: "name".to_string(),
                        typ: Type::Uint,
                        interface: None,
                        allow_null: false,
                        enu: None,
                    },
                    Arg {
                        name: "interface".to_string(),
                        typ: Type::String,
                        interface: None,
                        allow_null: false,
                        enu: None,
                    },
                    Arg {
                        name: "version".to_string(),
                        typ: Type::Uint,
                        interface: None,
                        allow_null: false,
                        enu: None,
                    },
                ],
            }],
        },
    ],
});
