use std::{collections::HashMap, io::{Write, Read}, fs::{self, File}, env};
use rand::Rng;

type Data = i32;

#[derive(Clone)]
struct Program {
    lines: Vec<Command>,
    labels: HashMap<String, usize>,
}

#[derive(Clone, Debug, PartialEq)]
enum PushArgument {
    Const(Data),
    Label(String),
}

#[derive(Clone, Debug, PartialEq)]
enum Command {
    Left,
    Right,
    PushT,
    Push(PushArgument),
    Pop,
    Dup,
    Del,
    Eq,
    Not,
    Gt,
    Lt,
    Add,
    Sub,
    Mult,
    Div,
    Mod,
    Read,
    Print,
    Jmp,
    JmpC,

    Label(String),
    Null, //used for empty lines
}

impl ToString for Command {
    fn to_string(&self) -> String {
        match self {
            Command::Left => "left".to_string(),
            Command::Right => "right".to_string(),
            Command::PushT => "pusht".to_string(),
            Command::Push(PushArgument::Const(c)) => format!("push {}", c),
            Command::Push(PushArgument::Label(l)) => format!("push {}", l),
            Command::Pop => "pop".to_string(),
            Command::Dup => "dup".to_string(),
            Command::Del => "del".to_string(),
            Command::Eq => "eq".to_string(),
            Command::Not => "not".to_string(),
            Command::Gt => "gt".to_string(),
            Command::Lt => "lt".to_string(),
            Command::Add => "add".to_string(),
            Command::Sub => "sub".to_string(),
            Command::Mult => "mult".to_string(),
            Command::Div => "div".to_string(),
            Command::Mod => "mod".to_string(),
            Command::Read => "read".to_string(),
            Command::Print => "print".to_string(),
            Command::Jmp => "jmp".to_string(),
            Command::JmpC => "jmpc".to_string(),
            Command::Label(l) => format!("{}:", l),
            Command::Null => "".to_string(),
        }.to_owned()
    }
}

#[derive(Default, Debug, Clone)]
struct MachineState {
    program_counter: usize,
    tape_head: usize,
    tape: Vec<Data>,
    stack: Vec<Data>,
    terminated: bool,
}

fn is_comment(line: &str) -> bool{
    line.starts_with('#') || line.starts_with("//") || line.starts_with(';')
}

fn parse_program(prog_string: &str) -> Program {
    let mut program = Program {
        lines: Vec::new(),
        labels: HashMap::new(),
    };
    for line in prog_string.lines() {
        let line = line.trim().to_lowercase();
        if is_comment(&line) || line.is_empty() {
            program.lines.push(Command::Null);
            continue;
        }
        if let Some(command_text) = line.split_whitespace().next() {
            let arg = line.split_whitespace().nth(1);
            let command = match (command_text, arg) {
                ("left", _) => Some(Command::Left),
                ("right", _) => Some(Command::Right),
                ("pusht", _) => Some(Command::PushT),
                ("push", Some(arg)) => Some(Command::Push(
                    arg.parse::<Data>()
                        .map(PushArgument::Const)
                        .unwrap_or(PushArgument::Label(arg.into())),
                )),
                ("pop", _) => Some(Command::Pop),
                ("dup", _) => Some(Command::Dup),
                ("del", _) => Some(Command::Del),
                ("eq", _) => Some(Command::Eq),
                ("not", _) => Some(Command::Not),
                ("gt", _) => Some(Command::Gt),
                ("lt", _) => Some(Command::Lt),
                ("add", _) => Some(Command::Add),
                ("sub", _) => Some(Command::Sub),
                ("mult", _) => Some(Command::Mult),
                ("div", _) => Some(Command::Div),
                ("mod", _) => Some(Command::Mod),
                ("read", _) => Some(Command::Read),
                ("print", _) => Some(Command::Print),
                ("jmp", _) => Some(Command::Jmp),
                ("jmpc", _) => Some(Command::JmpC),
                (_, _) => None,
            };
            let Some(command) = command else {
                assert!(command_text.ends_with(':'), "Label must end with colon. Offending line: {}", line);
                let label: String = command_text.split(':').next().unwrap().into();
                program.labels.insert(
                    label.clone(),
                    program.lines.len(),
                );
                program.lines.push(Command::Label(label.clone()));
                continue;
            };
            program.lines.push(command);
        }
        else {
            program.lines.push(Command::Null);
        }
    }
    program
}

#[derive(Debug)]
enum ExecutionError {
    Terminated,
    InvalidLine(usize),
    InvalidLabel(String),
    StackUnderflow,
    TapeHeadUnderflow,
}
use ExecutionError::*;

fn execute_command(
    program: &Program,
    mut machine: MachineState,
) -> Result<MachineState, ExecutionError> {
    if machine.terminated {
        if machine.program_counter < program.lines.len() {
            machine.terminated = false;
        } else {
            return Err(Terminated);
        }
    }
    let command = program
        .lines
        .get(machine.program_counter)
        .ok_or(InvalidLine(machine.program_counter))?
        .clone();
    machine.program_counter += 1;
    match command {
        Command::Left => {
            machine.tape_head = machine.tape_head.checked_sub(1).ok_or(TapeHeadUnderflow)?
        }
        Command::Right => machine.tape_head += 1,
        Command::PushT => machine.stack.push(machine.tape[machine.tape_head]),
        Command::Push(PushArgument::Const(c)) => machine.stack.push(c),
        Command::Push(PushArgument::Label(label)) => machine
            .stack
            .push(*program.labels.get(&label).ok_or(InvalidLabel(label))? as Data),
        Command::Pop => {
            if machine.tape_head >= machine.tape.len() {
                machine
                    .tape
                    .resize_with(machine.tape_head + 1, Data::default)
            };
            machine.tape[machine.tape_head] = machine.stack.pop().ok_or(StackUnderflow)?
        }
        Command::Dup => machine
            .stack
            .push(*machine.stack.last().ok_or(StackUnderflow)?),
        Command::Del => _ = machine.stack.pop().ok_or(StackUnderflow)?,
        Command::Eq => {
            let result = machine.stack.pop().ok_or(StackUnderflow)?
                == machine.stack.pop().ok_or(StackUnderflow)?;
            machine.stack.push(result as Data)
        }
        Command::Not => {
            let result = machine.stack.pop().ok_or(StackUnderflow)? == 0 as Data;
            machine.stack.push(result as Data)
        }
        Command::Gt => {
            let result = machine.stack.pop().ok_or(StackUnderflow)?
                > machine.stack.pop().ok_or(StackUnderflow)?;
            machine.stack.push(result as Data)
        }
        Command::Lt => {
            let result = machine.stack.pop().ok_or(StackUnderflow)?
                < machine.stack.pop().ok_or(StackUnderflow)?;
            machine.stack.push(result as Data)
        }
        Command::Add => {
            let result = machine.stack.pop().ok_or(StackUnderflow)?
                + machine.stack.pop().ok_or(StackUnderflow)?;
            machine.stack.push(result as Data)
        }
        Command::Sub => {
            let result = machine.stack.pop().ok_or(StackUnderflow)?
                - machine.stack.pop().ok_or(StackUnderflow)?;
            machine.stack.push(result as Data)
        }
        Command::Mult => {
            let result = machine.stack.pop().ok_or(StackUnderflow)?
                * machine.stack.pop().ok_or(StackUnderflow)?;
            machine.stack.push(result as Data)
        }
        Command::Div => {
            let result = machine.stack.pop().ok_or(StackUnderflow)?
                / machine.stack.pop().ok_or(StackUnderflow)?;
            machine.stack.push(result as Data)
        }
        Command::Mod => {
            let result = machine.stack.pop().ok_or(StackUnderflow)?
                % machine.stack.pop().ok_or(StackUnderflow)?;
            machine.stack.push(result as Data)
        }
        Command::Read => machine.stack.push(std::io::stdin().lock().bytes().next().unwrap().unwrap() as Data),
        Command::Print => {
            print!(
                "{}",
                machine.stack.pop().ok_or(StackUnderflow)? as u8 as char
            );
            std::io::stdout().flush().unwrap();
        }
        Command::Jmp => {
            machine.program_counter = machine.stack.pop().ok_or(StackUnderflow)? as usize
        }
        Command::JmpC => {
            let addr = machine.stack.pop().ok_or(StackUnderflow)?;
            if machine.stack.pop().ok_or(StackUnderflow)? != 0 {
                machine.program_counter = addr as usize
            }
        }
        Command::Null | Command::Label(_) => {} //no-op for empty lines
    }
    if machine.program_counter >= program.lines.len() {
        machine.terminated = true;
    }
    Ok(machine)
}

// Anything passed to this function needs to be effectively a noop. 
fn insert_dead_code<T: FnMut() -> Vec<Command>>(mut program: Program, mut dead_code_gen: T, quantity: usize) -> Program {
    let mut rng = rand::thread_rng();
    for _ in 0..quantity {
        let mut index = rng.gen_range(0..program.lines.len());
        let dead_code = dead_code_gen();
        for command in dead_code.iter() {
            program.lines.insert(index, command.clone());
            index += 1;
        }
    }
    program
}

// This code is ugly. Do not read. It just generates an obfuscated script called ./fuxxor.mxc
// Not even sure if it still does the same thing after my refactor ...
// The original code also tried to add nonsense labels that it then removed a few lines later ...
// ... in my defense, I was tired ...
fn generate_fuxxor(mut program: Program){
    program.lines.retain(|command| *command != Command::Null);

    for _ in 0..100 {
        let mut rng = rand::thread_rng();
        for _ in 0..rng.gen_range(1..4){
            program.lines.insert(rng.gen_range(0..program.lines.len()), Command::Null);
        }
    }


    let mut rng = rand::thread_rng();

    program = insert_dead_code(program, || vec![Command::Push(PushArgument::Const(rng.gen_range(-3..10))), Command::Del], 200);
    program = insert_dead_code(program, || vec![Command::Push(PushArgument::Const(rng.gen_range(-3..10))), Command::Push(PushArgument::Const(rng.gen_range(-3..10))), Command::Add, Command::Del], 200);
    program = insert_dead_code(program, || vec![Command::Push(PushArgument::Const(rng.gen_range(-3..10))), Command::Push(PushArgument::Const(rng.gen_range(-3..10))), Command::Sub, Command::Del], 200);
    program = insert_dead_code(program, || vec![Command::Push(PushArgument::Const(rng.gen_range(-3..10))), Command::Dup, Command::Add, Command::Del], 200);
    program = insert_dead_code(program, || vec![Command::Push(PushArgument::Const(rng.gen_range(3..10))), Command::Push(PushArgument::Const(rng.gen_range(3..10))), Command::Div, Command::Push(PushArgument::Const(rng.gen_range(3..10))), Command::Mult,  Command::Del], 20);
    program = insert_dead_code(program, || vec![Command::Push(PushArgument::Const(rng.gen_range(-3..10))), Command::Dup, Command::Mult, Command::Push(PushArgument::Const(rng.gen_range(3..10))), Command::Mult,  Command::Del], 20);
    program = insert_dead_code(program, || vec![Command::Null], 100);

    // update label locations
    program.labels = HashMap::new();
    for (line, command) in program.lines.iter().enumerate() {
        if let Command::Label(label) = command{
            program.labels.insert(label.clone(), line);
        }
    }

    // substitute labels for consts
    for  command in program.lines.iter_mut() {
        if let Command::Push(PushArgument::Label(label)) = command{
            *command = Command::Push(PushArgument::Const(*program.labels.get(label).unwrap() as i32));
        }
    }
    program.lines = program.lines.iter().map(|command| match command.clone() {Command::Label(_) => Command::Null, x => x}).collect();

    let mut rng = rand::thread_rng();
    let mut indent: String = "".into();
    let prog_string: String = program.lines.iter().map(|command| {
        let mut line = format!("{}{}", indent, command.to_string());
        // add trailing space
        if rng.gen_ratio(1, 50){
            line.push(' ');
        }
        // generate random indentation
        if rng.gen_ratio(1, 20){
            indent.push(' ');
        }
        if rng.gen_ratio(1, 20){
            indent.push('\t');
        }
        if rng.gen_ratio(1, 8){
            indent.pop();
        }
        line.push('\n');
        line
    }).collect();
    let mut file = File::create("fuxxor.mxc").unwrap();
    let bytes: Vec<u8> = prog_string.bytes().collect();
    file.write_all(&bytes).unwrap();
}

#[cfg(debug_assertions)]
fn execute_or_crash(program: &Program, machine: MachineState) -> MachineState {
    match execute_command(&program, machine.clone()) { // this clone slows everything down
        Ok(new_state) => new_state,
        Err(e) => {
            println!("\n\nProgram crashed, error: {:?}\n\nState before failed execution: \nProgram counter: {}\nTape head: {}\nTape length: {}\nStack size: {} Command: {:?}\n", 
                e, machine.program_counter, machine.tape_head, machine.tape.len(), &machine.stack.len(), &program.lines[machine.program_counter]);
            println!("\nProgram crashed with tape state:\n\n{:?}", machine.tape);
            dbg!(machine.stack);
            panic!()
        }
    }
}
#[cfg(not(debug_assertions))]
fn execute_or_crash(program: &Program, machine: MachineState) -> MachineState {
    match execute_command(&program, machine) { 
        Ok(new_state) => new_state,
        Err(e) => {
            panic!("Program crashed. Error code: {:?} \nUse a debug build for a more detailed crash report. ", e);
        }
    }
}

fn main() {
    let prog_string = fs::read_to_string(env::args().nth(1).expect("file argument needed")).expect("invalid file");
    let program = parse_program(&prog_string);
    generate_fuxxor(program.clone());
    let mut machine = MachineState::default();
    while !machine.terminated {
        machine = execute_or_crash(&program, machine);
    }
    println!("\nProgram terminated with tape state:\n\n{:?}", machine.tape)
}
