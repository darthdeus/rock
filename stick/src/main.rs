use std::{
    fs,
    io::{self, BufRead, BufReader, Write},
    os::unix::net::{UnixListener, UnixStream},
    path::Path,
    process::{Command, Stdio},
    thread,
};

use anyhow::Result;
use clap::{Parser, ValueEnum};

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Mode {
    Client,
    Server,
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(value_enum)]
    mode: Mode,

    // Command to run
    #[arg()]
    command: String,

    // Path to the Unix socket
    #[arg(short, long)]
    socket_path: String,
}

fn main() -> Result<()> {
    let args = Args::parse();

    match args.mode {
        Mode::Client => {
            start_client(&args.command, &args.socket_path)?;
        }
        Mode::Server => {
            start_server(&args.socket_path)?;
        }
    }

    Ok(())
}

fn start_client(command: &str, socket_path: &str) -> Result<()> {
    // Spawn a subprocess
    let mut child = Command::new(command)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn child process");

    let mut child_stdin = child.stdin.take().expect("Failed to open stdin");
    let child_stdout = child.stdout.take().expect("Failed to open stdout");
    let child_stderr = child.stderr.take().expect("Failed to open stderr");

    let s1 = socket_path.to_string();

    // Line-buffered stdin forwarding
    let stdin_handle = thread::spawn(move || {
        let mut socket = UnixStream::connect(s1).unwrap();
        let stdin = io::stdin();
        let stdin_lock = stdin.lock();
        let mut lines = stdin_lock.lines();

        while let Some(Ok(line)) = lines.next() {
            writeln!(child_stdin, "{}", line).expect("Failed to write to child stdin");
            writeln!(socket, "IN:{}", line).expect("Failed to write to socket");
        }
    });

    let s2 = socket_path.to_string();

    // Capture and forward stdout line-by-line
    let stdout_handle = thread::spawn(move || {
        let mut socket = UnixStream::connect(s2).unwrap();
        let stdout_reader = BufReader::new(child_stdout);

        for line in stdout_reader.lines() {
            let line = line.expect("Failed to read line from stdout");
            println!("{}", line);
            writeln!(socket, "OUT:{}", line).expect("Failed to write to socket");
        }
    });

    let s3 = socket_path.to_string();

    // Capture and forward stderr line-by-line
    let stderr_handle = thread::spawn(move || {
        let mut socket = UnixStream::connect(s3).unwrap();
        let stderr_reader = BufReader::new(child_stderr);

        for line in stderr_reader.lines() {
            let line = line.expect("Failed to read line from stderr");
            eprintln!("{}", line);
            writeln!(socket, "ERR:{}", line).expect("Failed to write to socket");
        }
    });

    // Wait for all threads to finish
    let _ = stdin_handle.join();
    let _ = stdout_handle.join();
    let _ = stderr_handle.join();

    // Wait for child process to exit
    let status = child.wait().expect("Child process wasn't running");
    println!("Process exited with: {}", status);

    Ok(())
}

fn start_server(socket_path: &str) -> Result<()> {
    // Ensure the socket file doesn't already exist
    if Path::new(socket_path).exists() {
        fs::remove_file(socket_path)?;
    }

    // Create a Unix domain socket listener
    let listener = UnixListener::bind(socket_path)?;
    println!("Server listening on {}", socket_path);

    // Handle incoming connections
    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                handle_client(stream)?;
            }
            Err(e) => {
                eprintln!("Connection failed: {}", e);
            }
        }
    }

    Ok(())
}

fn handle_client(stream: UnixStream) -> Result<()> {
    let reader = BufReader::new(&stream);

    for line in reader.lines() {
        let line = line?;
        if let Some((prefix, message)) = line.split_once(':') {
            match prefix {
                "IN" => println!("Received IN: {}", message),
                "OUT" => println!("Received OUT: {}", message),
                "ERR" => eprintln!("Received ERR: {}", message),
                _ => eprintln!("Unknown prefix: {}", line),
            }
        }
    }

    Ok(())
}
