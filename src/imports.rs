use std::collections::HashMap;
use std::path::PathBuf;

use crate::ast::{Item, Name};
use crate::error::{Error, Note};
use crate::lex::{leak, Span, Tokens};
use crate::parse::{parse, parse_file};

const STRING: &str = include_str!("../std/string.hop");
const PRELUDE: &str = include_str!("../std/prelude.hop");
const VECTOR: &str = include_str!("../std/vector.hop");

fn get_std_unit(path: &[&'static str], err_span: Span) -> Result<Vec<Item>, Error> {
    match path {
        ["std", "string"] => parse(Tokens::from_str(STRING, "std/string.hop")?),
        ["std", "prelude"] => parse(Tokens::from_str(PRELUDE, "std/prelude.hop")?),
        ["std", "vector"] => parse(Tokens::from_str(VECTOR, "std/vector.hop")?),
        _ => Err(Error::Import(
            Some(err_span),
            format!("cannot find {} in the standard library", path.join("::")),
            vec![],
        )),
    }
}

#[allow(clippy::type_complexity)]
pub fn collect(
    main_unit: Vec<Item>,
    file_prefix: PathBuf,
) -> Result<(HashMap<Vec<&'static str>, Vec<Item>>, Vec<&'static str>), Error> {
    let mut map = HashMap::new();
    let file_name = leak(
        file_prefix
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string(),
    );
    let name = if let Some(name) = file_name.strip_suffix(".hop") {
        if name == "std" {
            return Err(Error::Import(
                None,
                "file name 'std.hop' is reserved".to_string(),
                vec![],
            ));
        } else {
            name
        }
    } else {
        return Err(Error::Import(
            None,
            "main file should end with '.hop'".to_string(),
            vec![Note::new(None, format!("main file is '{file_name}'"))],
        ));
    };
    handle_file(main_unit, vec![name], file_prefix, &mut map)?;
    Ok((map, vec![name]))
}

fn handle_file(
    mut unit: Vec<Item>,
    path: Vec<&'static str>,
    file_path: PathBuf,
    map: &mut HashMap<Vec<&'static str>, Vec<Item>>,
) -> Result<(), Error> {
    map.insert(path.clone(), Vec::new());
    for names in unit.iter().filter_map(get_path) {
        let path_suffix: Vec<_> = names.iter().map(|name| name.name).collect();
        let err_span = names.last().unwrap().span;
        if path_suffix[0] == "std" {
            if !map.contains_key(&path_suffix) {
                handle_std(path_suffix, map, err_span)?;
            }
        } else {
            let mut new_path = path[..path.len() - 1].to_vec();
            let mut new_file_path = file_path.parent().unwrap().to_path_buf();
            for elem in &path_suffix {
                new_file_path.push(elem);
                new_path.push(elem);
            }
            if new_path == path {
                return Err(Error::Import(
                    Some(err_span),
                    format!("file '{}.hop' cannot import itself", path.last().unwrap()),
                    vec![],
                ));
            }
            if map.contains_key(&new_path) {
                continue;
            }
            new_file_path.set_extension("hop");
            let leaked = leak(new_file_path.to_str().unwrap().to_string());
            match parse_file(leaked) {
                Ok(unit) => handle_file(unit, new_path, new_file_path, map)?,
                Err(err) => match err {
                    Error::Io(msg) => return Err(Error::Import(Some(err_span), msg, vec![])),
                    other => return Err(other),
                },
            }
        }
    }
    map.get_mut(&path).unwrap().append(&mut unit);
    Ok(())
}

fn handle_std(
    path: Vec<&'static str>,
    map: &mut HashMap<Vec<&'static str>, Vec<Item>>,
    err_span: Span,
) -> Result<(), Error> {
    map.insert(path.clone(), Vec::new());
    let mut unit = get_std_unit(&path, err_span)?;
    for names in unit.iter().filter_map(get_path) {
        let path: Vec<_> = names.iter().map(|name| name.name).collect();
        if !map.contains_key(&path) {
            handle_std(path, map, names.last().unwrap().span)?;
        }
    }
    map.get_mut(&path).unwrap().append(&mut unit);
    Ok(())
}

fn get_path(item: &Item) -> Option<&Vec<Name>> {
    match item {
        Item::Import { path, .. }
        | Item::ImportStruct { path, .. }
        | Item::ImportModule { path, .. } => Some(path),
        _ => None,
    }
}
