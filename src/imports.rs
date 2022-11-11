use std::collections::HashMap;
use std::path::PathBuf;

use crate::ast::Item;
use crate::error::Error;
use crate::lex::{leak, Span, Tokens};
use crate::parse::{parse, parse_file};

const STRING: &str = include_str!("../std/string.hop");
const PRELUDE: &str = include_str!("../std/prelude.hop");
const VECTOR: &str = include_str!("../std/vector.hop");

fn std_unit(path: &[&'static str], err_span: Span) -> Result<Vec<Item>, Error> {
    match path {
        ["std", "string"] => Ok(parse(Tokens::from_str(STRING, "std/string.hop")?)?),
        ["std", "prelude"] => Ok(parse(Tokens::from_str(PRELUDE, "std/prelude.hop")?)?),
        ["std", "vector"] => Ok(parse(Tokens::from_str(VECTOR, "std/vector.hop")?)?),
        _ => Err(Error::Import(
            err_span,
            format!("cannot find {} in the standard library", path.join(":")),
        )),
    }
}

enum PathKind {
    File(PathBuf),
    Std(Vec<&'static str>),
}

pub fn resolve(unit: Vec<Item>, path: PathBuf) -> Result<Vec<Item>, Error> {
    let mut path_map = HashMap::new();
    let mut std_map = HashMap::new();
    walk_files(unit, PathKind::File(path), &mut path_map, &mut std_map)?;
    Ok(std_map
        .into_values()
        .flatten()
        .chain(path_map.into_values().flatten())
        .collect())
}

fn walk_files(
    unit: Vec<Item>,
    path: PathKind,
    path_map: &mut HashMap<PathBuf, Vec<Item>>,
    std_map: &mut HashMap<Vec<&'static str>, Vec<Item>>,
) -> Result<(), Error> {
    match path {
        PathKind::File(file_path) => {
            if !path_map.contains_key(&file_path) {
                path_map.insert(file_path.clone(), Vec::new());
                for item in unit {
                    if let Item::Import { path, .. } = item {
                        let err_span = path.last().unwrap().span;
                        if path[0].name == "std" {
                            let path: Vec<_> = path.iter().map(|name| name.name).collect();
                            #[allow(clippy::map_entry)]
                            if !std_map.contains_key(&path) {
                                let unit = std_unit(&path, err_span)?;
                                walk_files(unit, PathKind::Std(path), path_map, std_map)?;
                            }
                        } else {
                            let mut new_path = file_path.parent().unwrap().to_path_buf();
                            for elem in path {
                                new_path.push(elem.name);
                            }
                            new_path.set_extension("hop");
                            let leaked = leak(new_path.to_str().unwrap().to_string());
                            match parse_file(leaked) {
                                Ok(unit) => {
                                    walk_files(unit, PathKind::File(new_path), path_map, std_map)?;
                                }
                                Err(err) => match err {
                                    Error::Io(msg) => return Err(Error::Import(err_span, msg)),
                                    other => return Err(other),
                                },
                            }
                        }
                    } else {
                        path_map.get_mut(&file_path).unwrap().push(item);
                    }
                }
            }
        }
        PathKind::Std(std_path) => {
            if !std_map.contains_key(&std_path) {
                std_map.insert(std_path.clone(), Vec::new());
                for item in unit {
                    if let Item::Import { path, .. } = item {
                        let err_span = path.last().unwrap().span;
                        if path[0].name != "std" {
                            panic!("std imports must be from std");
                        }
                        let path: Vec<_> = path.iter().map(|name| name.name).collect();
                        #[allow(clippy::map_entry)]
                        if !std_map.contains_key(&path) {
                            let unit = std_unit(&path, err_span)?;
                            walk_files(unit, PathKind::Std(path), path_map, std_map)?;
                        }
                    } else {
                        std_map.get_mut(&std_path).unwrap().push(item);
                    }
                }
            }
        }
    }
    Ok(())
}
