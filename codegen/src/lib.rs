mod registers;
mod x86_64;

use std::process::Stdio;

use analysis::{ir::Ir, LoweringCx};
use object::{
    elf,
    write::{Object, Symbol},
};
use parser::Error;

type Result<T, E = Error> = std::result::Result<T, E>;

pub fn generate<'cx>(lcx: &'cx LoweringCx<'cx>, ir: &Ir<'cx>) -> Result<()> {
    let mut obj = Object::new(
        object::BinaryFormat::Elf,
        object::Architecture::X86_64,
        object::Endianness::Little,
    );

    // GNU linkers have this awesome thing where they'll mark your stack as executable unless you tell them not to.
    obj.add_section(
        Vec::new(),
        b".note.GNU-stack".to_vec(),
        object::SectionKind::Note,
    );

    let text = obj.add_section(Vec::new(), b".text".to_vec(), object::SectionKind::Text);

    for func in ir.funcs.values() {
        let code = x86_64::generate_func(lcx, func)?;

        let offset = obj.append_section_data(text, &code, 8);

        let sym = Symbol {
            name: func.name.as_str(|s| s.as_bytes().to_vec()),
            value: offset,
            size: code.len().try_into().unwrap(),
            kind: object::SymbolKind::Text,
            scope: object::SymbolScope::Linkage,
            weak: false,
            section: object::write::SymbolSection::Section(text),
            flags: object::SymbolFlags::Elf {
                st_info: ((elf::STB_GLOBAL) << 4) | (elf::STT_FUNC),
                st_other: elf::STV_DEFAULT,
            },
        };

        obj.add_symbol(sym);
    }

    let object_file = obj
        .write()
        .map_err(|err| Error::new_without_span(format!("failed to create object file: {err}")))?;

    std::fs::write("main.o", object_file).map_err(|err| {
        Error::new_without_span(format!("failed to write object file main.o: {err}"))
    })?;

    let output = std::process::Command::new("cc")
        .arg("main.o")
        .arg("-g")
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .output()
        .map_err(|err| Error::new_without_span(format!("failed to spawn `cc`: {err}")))?;

    if !output.status.success() {
        return Err(Error::new_without_span("linking with `cc` failed"));
    } else {
        // std::fs::remove_file("main.o").map_err(|err| {
        //     analysis::Error::new_without_span(format!(
        //         "failed to remove temporary file main.o: {err}"
        //     ))
        // })?;
    }

    Ok(())
}
