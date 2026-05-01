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
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::{env, path::Path};
use crate::code_lowerer::{CodeLowerer, IRTemplateKey, IRTemplateValue};
use crate::code_lowerer::IRScope;
use crate::code_lowerer::IRScopePath;
use crate::errors::CompilerError;
use crate::parser::*;
use inkwell::{OptimizationLevel, passes};
use serde::Deserialize;

fn main() {
    if let Err(err) = run() {
        eprintln!("{}", err);
    }
}

fn run() -> Result<(), CompilerError> {
    let std_folder = PathBuf::from("/home/yiftach/dev/c_flat/src/std".to_string());
    //let code_folder = PathBuf::from("/home/yiftach/dev/c_flat/src/test".to_string());
    let code_folder = PathBuf::from("/home/yiftach/dev/c_flat/src/networking test/server".to_string());

    let mut file_context = FileContext::new();
    let mut parser: Parser = Parser::new(&mut file_context);

    parser.add_workspace(std_folder.clone());
    parser.add_workspace(code_folder.clone());

    let mut main_scope: Scope = Scope::new();

    parser.parse_file("std/core.cf", &mut main_scope)?;
    parser.parse_file("main.cf", &mut main_scope)?;

    let c_helpers = parser.c_helpers;
    
    let llvm_context: Context = Context::create();
    let mut code_lowerer = CodeLowerer::new(&main_scope, file_context, &llvm_context);

    let global_scope = code_lowerer.get_global_scope()?;
    
    code_lowerer.lower_fun(global_scope, "main", &Vec::new(), None)?;

    if let Err(err) = run_pipeline(&code_lowerer.module) {
        eprintln!("{}", err);
        return Ok(());
    }

    std::env::set_current_dir(code_folder).unwrap();

    println!("{}", std::env::current_dir().unwrap().display());

    code_lowerer.export_ir_to_file(Path::new("output.ll"));

    let mut output = Command::new("clang");
    output.arg("-I/usr/local/include");
    output.arg("output.ll");

    for c_helper in c_helpers.iter() {
        output.arg(c_helper);
    }

    let result = output
        .arg("-L/usr/local/lib") 
        .arg("-lraylib")
        .arg("-lGL")             
        .arg("-lm")
        .arg("-lpthread")
        .arg("-ldl")
        .arg("-lrt")
        .arg("-lX11")          
        .arg("-o")
        .arg("output_exec")
        .arg("-lm").output().expect("Failed to launch clang");
    if !result.status.success() {
        eprintln!("{}", String::from_utf8_lossy(&result.stderr));
        return Ok(());
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

    Ok(())
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

    let passes = "always-inline,instcombine<no-verify-fixpoint>,simplifycfg,adce,globaldce";
    
    module.run_passes(passes, &target_machine, options)
        .map_err(|e| format!("Pass execution failed: {:?}", e))?;

    Ok(())
}