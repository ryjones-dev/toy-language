#! /bin/bash

# Run the compiler with peg tracing enabled, outputting to a file
RUST_BACKTRACE=1 cargo run --features peg/trace > parse_log.txt

# Remove a bunch of noise related to matching comments 
grep -v '\[PEG_TRACE\] Failed to match rule `comment`*' parse_log.txt > temp  && mv temp parse_log.txt
grep -v '\[PEG_TRACE\] Matched rule `_`*' parse_log.txt > temp && mv temp parse_log.txt
grep -v '\[PEG_TRACE\] Attempting to match rule `comment`*' parse_log.txt > temp && mv temp parse_log.txt
grep -v '\[PEG_TRACE\] Attempting to match rule `_`*' parse_log.txt > temp && mv temp parse_log.txt
