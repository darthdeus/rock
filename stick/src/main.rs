use std::{
    fs,
    io::{self, BufRead, BufReader, Read, Write},
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
    let (tx, rx) = mpsc::channel();

    let mut child = Command::new(command)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn child process");

    let mut child_stdin = child.stdin.take().expect("Failed to open stdin");
    let mut child_stdout = child.stdout.take().expect("Failed to open stdout");
    let mut child_stderr = child.stderr.take().expect("Failed to open stderr");

    // STDIN
    let stdin_tx = tx.clone();
    let stdin_handle = thread::spawn(move || {
        let stdin = io::stdin();
        let mut stdin_lock = stdin.lock();
        let mut buffer = [0u8; 1]; // Buffer for single character

        while stdin_lock.read_exact(&mut buffer).is_ok() {
            // Send to child stdin
            child_stdin
                .write_all(&buffer)
                .expect("Failed to write to child stdin");
            child_stdin.flush().expect("Failed to flush stdin");

            // Send the character to the channel prefixed with "IN"
            stdin_tx
                .send(format!("IN:{}", buffer[0] as char))
                .expect("Failed to send stdin to channel");
        }
    });

    // STDOUT
    let stdout_tx = tx.clone();
    let stdout_handle = thread::spawn(move || {
        let mut buffer = [0u8; 1]; // Buffer for single character

        while child_stdout.read_exact(&mut buffer).is_ok() {
            // Print stdout character to screen
            print!("{}", buffer[0] as char);
            io::stdout().flush().expect("Failed to flush stdout");

            // Send the character to the channel prefixed with "OUT"
            stdout_tx
                .send(format!("OUT:{}", buffer[0] as char))
                .expect("Failed to send stdout to channel");
        }
    });

    // STDERR
    let stderr_tx = tx.clone();
    let stderr_handle = thread::spawn(move || {
        let mut buffer = [0u8; 1]; // Buffer for single character

        while child_stderr.read_exact(&mut buffer).is_ok() {
            // Print stderr character to screen
            eprint!("{}", buffer[0] as char);
            io::stderr().flush().expect("Failed to flush stderr");

            // Send the character to the channel prefixed with "ERR"
            stderr_tx
                .send(format!("ERR:{}", buffer[0] as char))
                .expect("Failed to send stderr to channel");
        }
    });

    // Path to the Unix socket
    let s = socket_path.to_string();

    // Thread for writing to the Unix domain socket
    let writer_handle = thread::spawn(move || {
        let mut socket = UnixStream::connect(s).expect("Failed to connect to socket");

        for message in rx {
            socket
                .write_all(message.as_bytes())
                .expect("Failed to write to socket");
            socket.flush().expect("Failed to flush socket");
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
                // _ => eprintln!("Unknown prefix: {}", line),
                _ => print!("{}...{}", prefix, message),
            }
        }
    }

    Ok(())
}
