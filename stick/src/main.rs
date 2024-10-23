use std::{
    fs,
    io::{self, BufRead, BufReader, Write},
    os::unix::net::{UnixListener, UnixStream},
    path::Path,
    process::{Command, Stdio},
    sync::mpsc,
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

    #[arg()]
    command: Option<String>,

    #[arg(short, long, default_value = "/tmp/rock-lsp-stick.sock")]
    socket_path: String,
}

fn main() -> Result<()> {
    let args = Args::parse();

    // Ensure the `command` is provided if the mode is `Client`
    if args.mode == Mode::Client && args.command.is_none() {
        return Err(anyhow::anyhow!("`command` is required when mode is Client"));
    }

    match args.mode {
        Mode::Client => {
            start_client(&args.command.unwrap(), &args.socket_path).unwrap();
        }
        Mode::Server => {
            start_server(&args.socket_path).unwrap();
        }
    }

    Ok(())
}

fn start_client(command: &str, socket_path: &str) -> Result<()> {
    // Create a channel for sending messages between threads
    let (tx, rx) = mpsc::channel();

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

    // Thread for handling stdin, prefix with "IN"
    let stdin_tx = tx.clone();
    let stdin_handle = thread::spawn(move || {
        let stdin = io::stdin();
        let stdin_lock = stdin.lock();
        let mut lines = stdin_lock.lines();

        while let Some(Ok(line)) = lines.next() {
            writeln!(child_stdin, "{}", line).expect("Failed to write to child stdin");
            stdin_tx
                .send(format!("IN:{}", line))
                .expect("Failed to send stdin to channel");
        }
    });

    // Thread for handling stdout, prefix with "OUT"
    let stdout_tx = tx.clone();
    let stdout_handle = thread::spawn(move || {
        let stdout_reader = BufReader::new(child_stdout);

        for line in stdout_reader.lines() {
            let line = line.expect("Failed to read line from stdout");
            println!("{}", line);
            stdout_tx
                .send(format!("OUT:{}", line))
                .expect("Failed to send stdout to channel");
        }
    });

    // Thread for handling stderr, prefix with "ERR"
    let stderr_tx = tx.clone();
    let stderr_handle = thread::spawn(move || {
        let stderr_reader = BufReader::new(child_stderr);

        for line in stderr_reader.lines() {
            let line = line.expect("Failed to read line from stderr");
            eprintln!("{}", line);
            stderr_tx
                .send(format!("ERR:{}", line))
                .expect("Failed to send stderr to channel");
        }
    });

    let s = socket_path.to_string();

    // Thread for writing to the Unix domain socket
    let writer_handle = thread::spawn(move || {
        let mut socket = UnixStream::connect(s).expect("Failed to connect to socket");

        for message in rx {
            writeln!(socket, "{}", message).expect("Failed to write to socket");
        }
    });

    // Wait for all threads to finish
    let _ = stdin_handle.join();
    let _ = stdout_handle.join();
    let _ = stderr_handle.join();

    // Close the channel by dropping the sender (so that the writer thread can finish)
    drop(tx);

    // Wait for the writer thread to finish
    let _ = writer_handle.join();

    // Wait for child process to exit
    let status = child.wait().expect("Child process wasn't running");
    eprintln!("Process exited with: {}", status);

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
