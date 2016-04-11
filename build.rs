use std::process::Command;

fn main() {
        let output = Command::new("R").arg("CMD").arg("config").arg("--ldflags").output().unwrap_or_else(|e| {
                        panic!("During building, we could not get the output of R CMD config --ldflags: {}", e)
                });
        if !output.status.success() {
                println!("There was an error running R CMD config --ldflags: {}", String::from_utf8_lossy(&output.stderr));
        } else {
                let result = String::from_utf8_lossy(&output.stdout);
                println!("R CMD --ldflags:  {}",result );
                let comp = result. split_whitespace();
                let mut xpectfrmwk  = false;
                for c in comp {
                        let cas_string = c.to_string();
                        let mut fchar = &cas_string[0..1];
                        if fchar == "-" {
                                fchar =  &cas_string[0..2];
                        }
                        match fchar {
                                "-F" => {
                                        println!("cargo:rustc-link-search=framework={}", &cas_string[2..]);
                                },
                                "-f" => {
                                        xpectfrmwk = true;
                                },
                                "-L" => {
                                        println!("cargo:rustc-link-search=native={}",&cas_string[2..]);
                                },
                                "-l" => {
                                        println!("cargo:rustc-link-lib={}",&cas_string[2..]);
                                },
                                _ => {
                                        if xpectfrmwk {
                                                println!("cargo:rustc-link-lib=framework={}",&cas_string);
                                                xpectfrmwk = false;
                                        } else {
                                               panic!("Unexpected flag passed: {}",cas_string);
                                        }
                                }               
                        }
                }
        }
}

