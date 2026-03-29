mod parser;
mod errors;
mod code_lowerer;
mod function_lowerer;
mod expr_lowerer;
mod type_lowerer;
mod impl_lowerer;
mod trait_lowerer;
mod infix_lowerer;
mod postfix_lowerer;
mod prefix_lowerer;
mod core_lowerer;
mod conditional_lowerer;
mod array_lowerer;

use indexmap::IndexMap;
use inkwell::context::Context;

use colored::*;
use inkwell::module::Module;
use inkwell::passes::{PassBuilderOptions, PassManager};
use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine};
use std::collections::HashMap;
use std::hash::Hash;
use std::process::{Command, Stdio};
use std::{env, path::Path};
use crate::code_lowerer::{CodeLowerer, IRTemplateKey, IRTemplateValue};
use crate::code_lowerer::IRScope;
use crate::code_lowerer::IRScopePath;
use crate::parser::*;
use inkwell::{OptimizationLevel, passes};

fn main() {
    let str_path: &str = "src/test";
    let path: &Path = Path::new(str_path);
    if let Err(e) = env::set_current_dir(&path) {
        eprintln!("Failed to open working directory '{}', {}", str_path, e);
        return;
    }

    let mut file_context = FileContext::new();
    let mut parser: Parser = Parser::new(&mut file_context);
    let mut main_scope: Scope = Scope::new();

    if let Err(err) = parser.parse_file("main.cf".to_string(), &mut main_scope) {
        eprintln!("{}", err);
        return;
    }

    // println!("{}", main_scope.to_string());
    
    let llvm_context: Context = Context::create();
    let mut code_lowerer = CodeLowerer::new(&main_scope, file_context, &llvm_context);

    let global_scope = code_lowerer.get_global_scope();
    let main_fun_result = code_lowerer.lower_fun(global_scope, "main", &Vec::new(), None);

    if let Err(err) = main_fun_result {
        eprintln!("{}", err);
        return;
    }

    if let Err(err) = run_pipeline(&code_lowerer.module) {
        eprintln!("{}", err);
        return;
    }

    code_lowerer.export_ir_to_file(Path::new("output.ll"));

    if let Err(err) = Command::new("clang").arg("output.ll").arg("-o").arg("output_exec").output() {
        eprintln!("{}", err);
        return;
    }

    println!("{}", "Done! [Running Script]:".green());

    let mut child = Command::new("./output_exec")
    .stdin(Stdio::inherit())  
    .stdout(Stdio::inherit())
    .stderr(Stdio::inherit())
    .spawn()                  
    .expect("Failed to execute script");

    let status = child.wait().expect("Failed to wait on child");

    println!("\nScript exited with status: {}", status);

}

fn run_pipeline(module: &Module) -> Result<(), String> {
    Target::initialize_native(&InitializationConfig::default())
        .map_err(|e| format!("Target init failed: {}", e))?;

    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple)
        .map_err(|e| e.to_string())?;
    
    let target_machine = target
        .create_target_machine(
            &triple,
            &TargetMachine::get_host_cpu_name().to_string(),
            &TargetMachine::get_host_cpu_features().to_string(),
            OptimizationLevel::Default,
            RelocMode::Default,
            CodeModel::Default,
        )
        .ok_or_else(|| "Failed to create target machine".to_string())?;

    let options = PassBuilderOptions::create();
    options.set_verify_each(true);

    let passes = "always-inline,instcombine,simplifycfg,adce,globaldce";
    
    module.run_passes(passes, &target_machine, options)
        .map_err(|e| format!("Pass execution failed: {:?}", e))?;

    Ok(())
}