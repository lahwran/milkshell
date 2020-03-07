use std::fs;
use std::io;
use std::path::Path;

// one possible implementation of walking a directory only visiting files
pub(crate) fn visit_dirs(dir: &Path, cb: &dyn Fn(&Path)) -> io::Result<()> {
    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                visit_dirs(&path, cb)?;
            } else {
                cb(&path)
            }
        }
    }
    Ok(())
}

pub(crate) fn visit_filtered(dir: &Path, ext: &str, cb: &dyn Fn(&Path, &str)) -> io::Result<()> {
    visit_dirs(dir, &|path: &Path| {
        let v = path.file_name().unwrap().to_str().unwrap();
        if v.ends_with(ext) {
            match fs::read_to_string(path) {
                Ok(contents) => cb(path, &contents),
                Err(e) => println!("Error reading file {:?}: {:?}", path, e),
            }
        }
    })
}
