use std::io::process::Command;
use std::string::String;


pub enum PlayStatus {
    Playing,
    Paused,
    Stopped
}

pub fn env_kded4_pid() -> Result<String,  &'static str> {
    let output = match Command::new("pidof").arg("kdeinit4").output() {
        Ok(output) => output,
        Err(e) => return Err("exec error"),
    };
    let op = match String::from_utf8(output.output) {
        Ok(output) => output,
        Err(e) => return Err("conv error"),
    };
    
    match op.as_slice().splitn(1, ' ').nth(0) {
        Some(output) => Ok(output.to_string()),
        None => Err("spliterror"),
    }
}
