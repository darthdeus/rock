use std::{
    fs,
    io::{self, BufReader, Read, Write},
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

#[derive(Copy, Clone, Debug)]
enum BufByte {
    Stdin(u8),
    Stdout(u8),
    Stderr(u8),
}

fn start_client(command: &str, socket_path: &str) -> Result<()> {
    // Create a channel for sending messages between threads
    let (tx, rx) = mpsc::channel::<BufByte>();

    // Spawn a subprocess
    let mut child = Command::new(command)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn child process");

    let mut child_stdin = child.stdin.take().expect("Failed to open stdin");
    let mut child_stdout = child.stdout.take().expect("Failed to open stdout");
    let mut child_stderr = child.stderr.take().expect("Failed to open stderr");

    let stdin_tx = tx.clone();
    let stdin_handle = thread::spawn(move || {
        let mut buffer = [0u8; 1];

        let stdin = io::stdin();
        let mut stdin_lock = stdin.lock();

        while stdin_lock.read_exact(&mut buffer).is_ok() {
            child_stdin
                .write_all(&buffer)
                .expect("Failed to write to child stdin");

            stdin_tx
                .send(BufByte::Stdin(buffer[0]))
                .expect("Failed to send stdin to channel");
        }
    });

    let stdout_tx = tx.clone();
    let stdout_handle = thread::spawn(move || {
        let mut buffer = [0u8; 1];
        let mut stdout = io::stdout();

        while child_stdout.read_exact(&mut buffer).is_ok() {
            stdout.write_all(&buffer).unwrap();
            stdout.flush().unwrap();

            stdout_tx
                .send(BufByte::Stdout(buffer[0]))
                .expect("Failed to send stdout to channel");
        }
    });

    let stderr_tx = tx.clone();
    let stderr_handle = thread::spawn(move || {
        let mut buffer = [0u8; 1];
        let mut stderr = io::stderr();

        while child_stderr.read_exact(&mut buffer).is_ok() {
            stderr.write_all(&buffer).unwrap();
            stderr.flush().unwrap();

            stderr_tx
                .send(BufByte::Stderr(buffer[0]))
                .expect("Failed to send stderr to channel");
        }
    });

    let s = socket_path.to_string();

    // Thread for writing to the Unix domain socket
    let writer_handle = thread::spawn(move || {
        let mut socket = UnixStream::connect(s).expect("Failed to connect to socket");

        for message in rx {
            let buf: [u8; 2] = match message {
                BufByte::Stdin(c) => [0, c],
                BufByte::Stdout(c) => [1, c],
                BufByte::Stderr(c) => [2, c],
            };

            socket.write_all(&buf).expect("Failed to write to socket");
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
                handle_stream(stream)?;
            }
            Err(e) => {
                eprintln!("Connection failed: {}", e);
            }
        }
    }

    Ok(())
}

fn handle_stream(stream: UnixStream) -> Result<()> {
    let reader = BufReader::new(stream);

    let mut ty: u8 = 0;
    let mut c: u8;
    let mut tiktok = false;

    let mut in_buf = Vec::new();
    let mut out_buf = Vec::new();
    let mut err_buf = Vec::new();

    for byte in reader.bytes() {
        let byte = byte?;

        if !tiktok {
            tiktok = true;
            ty = byte;
        } else {
            tiktok = false;
            c = byte;

            match ty {
                0 => in_buf.push(c),
                1 => out_buf.push(c),
                2 => err_buf.push(c),
                _ => println!("INVALID TAG BYTE"),
            }

            if c == b'\n' {
                match ty {
                    0 => print_buf("IN", &mut in_buf),
                    1 => print_buf("OUT", &mut out_buf),
                    2 => print_buf("ERR", &mut err_buf),
                    _ => println!("INVALID TAG BYTE"),
                }
            }
        }
    }

    Ok(())
}

fn print_buf(prefix: &str, buf: &mut Vec<u8>) {
    print!("{}: {}", prefix, String::from_utf8_lossy(buf));
    buf.clear();
}
