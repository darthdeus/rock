use anyhow::Result;
use log::info;
use std::{ops::Range, path::Path};

/// A span represents a range of locations in the source code that may span
/// multiple lines. Used for error reporting. Each AST node has a span.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Span {
    pub file: ustr::Ustr,
    pub line_range: (usize, usize),
    pub col_range: (usize, usize),
    pub offset_range: (usize, usize),
}

// impl std::fmt::Debug for Span {
//     fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         Ok(())
//         // f.debug_struct("Span").field("file", &self.file).field("line_range", &self.line_range).field("col_range", &self.col_range).field("offset_range", &self.offset_range).finish()
//     }
// }

impl Span {
    pub fn unknown() -> Self {
        Self {
            file: "<unknown>".into(),
            line_range: (0, 0),
            col_range: (0, 0),
            offset_range: (0, 0),
        }
    }

    pub fn start(&self) -> LineCol {
        LineCol {
            file: self.file,
            line: self.line_range.0,
            col: self.col_range.0,
            offset: self.offset_range.0,
        }
    }

    pub fn end(&self) -> LineCol {
        LineCol {
            file: self.file,
            line: self.line_range.1,
            col: self.col_range.1,
            offset: self.offset_range.1,
        }
    }

    /// Returns whether this span contains the given location.
    pub fn contains_loc(&self, query_loc: &LineCol) -> bool {
        let file_match = query_loc.file == self.file;
        let loc_match =
            query_loc.offset >= self.offset_range.0 && query_loc.offset <= self.offset_range.1;

        if !file_match && loc_match {
            info!("File mismatch: {:?} != {:?}", query_loc.file, self.file);
        }

        file_match && loc_match
    }

    pub fn to_label(&self, file: &str, message: String) -> ariadne::Label<(String, Range<usize>)> {
        ariadne::Label::new((file.to_string(), self.start().offset..self.end().offset))
            .with_message(message)
    }
}

/// A LineCol represents a specific point in the source code. Used for error
/// reporting.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineCol {
    pub file: ustr::Ustr,
    pub line: usize,
    pub col: usize,
    pub offset: usize,
}

impl LineCol {
    pub fn unknown() -> Self {
        Self {
            file: "<unknown>".into(),
            line: 0,
            col: 0,
            offset: 0,
        }
    }

    /// Construct a LineCol from a file, line, and column number (i.e. computes
    /// the offset automatically).
    pub fn from_file_line_col(
        sources: &SourceFiles,
        file_name: &str,
        line: usize,
        col: usize,
    ) -> Result<LineCol, String> {
        let file = sources
            .files
            .iter()
            .find(|f| f.path == file_name)
            .ok_or_else(|| format!("Could not locate file '{}' in sources", file_name))?;

        let mut global_offset = 0;
        for (i, line_contents) in file.contents.lines().enumerate() {
            if i == line {
                if col == 0 || col > line_contents.chars().count() + 1 {
                    return Err("Column number out of range".to_owned());
                }
                global_offset += line_contents
                    .chars()
                    .take(col - 1)
                    .map(|c| c.len_utf8())
                    .sum::<usize>();
                return Ok(LineCol {
                    file: ustr::ustr(&file.path),
                    line,
                    col,
                    offset: global_offset,
                });
            } else {
                global_offset += line_contents.len() + 1; // +1 for the newline character
            }
        }

        Err("Line number out of range".to_owned())
    }
}

/// A source file is a file on disk that contains Rebel source code (which has
/// been loaded into memory).
#[derive(Debug, Clone)]
pub struct SourceFile {
    /// The fully qualified path to the file
    path: String,
    /// A shortened version of the path
    filename: String,
    /// The contents of the file
    contents: String,
}

impl SourceFile {
    pub fn from_path(path: impl AsRef<Path>) -> Result<Self> {
        let contents = std::fs::read_to_string(&path)?;
        Self::from_path_and_contents(path, contents)
    }

    pub fn from_path_and_contents(path: impl AsRef<Path>, contents: String) -> Result<Self> {
        let path = std::fs::canonicalize(path.as_ref())?;
        Ok(SourceFile {
            path: path.to_str().unwrap().to_string(),
            filename: path.file_name().unwrap().to_string_lossy().to_string(),
            contents,
        })
    }

    #[cfg(test)]
    pub fn test(path: String, filename: String, contents: String) -> Self {
        Self {
            path,
            filename,
            contents,
        }
    }

    pub fn path(&self) -> &str {
        self.path.as_ref()
    }

    pub fn filename(&self) -> &str {
        self.filename.as_ref()
    }

    pub fn contents(&self) -> &str {
        self.contents.as_ref()
    }
}

#[derive(Debug, Clone)]
pub struct SourceFiles {
    pub files: Vec<SourceFile>,
}

impl SourceFiles {
    pub fn new(files: Vec<SourceFile>) -> Self {
        Self { files }
    }

    pub fn new_with_stdlib(files: Vec<SourceFile>) -> Self {
        // // Who needs includes?
        // let stdlib_path = concat!(env!("CARGO_MANIFEST_DIR"), "/../rebel_std");
        // for entry in std::fs::read_dir(stdlib_path).unwrap() {
        //     let entry = entry.unwrap();
        //     let path = entry.path();
        //     let path = std::fs::canonicalize(path).unwrap();
        //     let path_str = path.to_str().unwrap().to_owned();
        //     if path_str.ends_with(".rbl") {
        //         files.push(SourceFile {
        //             path: path.to_string_lossy().to_string(),
        //             filename: path.file_name().unwrap().to_string_lossy().to_string(),
        //             contents: std::fs::read_to_string(path).unwrap(),
        //         });
        //     }
        // }

        // // Also add the string codegen file
        // files.push(SourceFile {
        //     path: "<autogenerated>".to_owned(),
        //     filename: "<autogenerated>".to_owned(),
        //     contents: crate::bindings_api::get_string_codegen_registry_file(),
        // });

        Self { files }
    }

    pub fn iter(&self) -> impl Iterator<Item = &SourceFile> {
        self.files.iter()
    }

    pub fn add_file(&mut self, file: SourceFile) {
        self.files.push(file);
    }

    pub fn add_or_update_file(&mut self, file: SourceFile) {
        if let Some(existing) = self.files.iter_mut().find(|f| f.path == file.path) {
            *existing = file;
        } else {
            self.files.push(file);
        }
    }

    pub fn get_line(&self, line_col: &LineCol) -> Option<&str> {
        let file = self.files.iter().find(|f| *f.path == *line_col.file)?;
        file.contents.lines().nth(line_col.line)
    }
}
