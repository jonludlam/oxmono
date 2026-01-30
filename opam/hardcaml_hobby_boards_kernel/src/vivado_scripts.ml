open! Base

let build_tcl ~name ~part =
  [%rope
    {|# vivado -mode batch -source %{name#String}.tcl

set PROJECT_NAME "%{name#String}"
set FPGA_PART "%{part#String}"
set DEBUG false

read_verilog ${PROJECT_NAME}.v
read_xdc ${PROJECT_NAME}.xdc
synth_design -top ${PROJECT_NAME}_top -part $FPGA_PART
opt_design
if {$DEBUG} { write_checkpoint -force ${PROJECT_NAME}.synth.dcp }
place_design
if {$DEBUG} { write_checkpoint -force ${PROJECT_NAME}.place.dcp }
route_design
if {$DEBUG} { write_checkpoint -force ${PROJECT_NAME}.route.dcp }
if {$DEBUG} { report_utilization -hierarchical -file ${PROJECT_NAME}.utilization.rpt }
report_timing_summary -file ${PROJECT_NAME}.timing.rpt
write_bitstream -force ${PROJECT_NAME}.bit
set WNS [get_property SLACK [get_timing_paths -max_paths 1 -nworst 1 -setup]]
puts "WNS=$WNS"
|}]
;;

let flash_tcl ~name =
  [%rope
    {|# vivado -mode batch -source flash.tcl

set PROJECT_NAME "%{name#String}"

open_hw
connect_hw_server
open_hw_target
current_hw_device [lindex [get_hw_devices] 0]
set_property PROGRAM.FILE ${PROJECT_NAME}.bit [current_hw_device]
program_hw_devices [current_hw_device]
exit
    |}]
;;

let run_vivado_remotely_sh ~name =
  [%rope
    {|#!/bin/sh

PROJECT_NAME="%{name#String}"

PROJECT_FILES="$PROJECT_NAME.v $PROJECT_NAME.tcl $PROJECT_NAME.xdc"
VIVADO_COMMAND="vivado -mode batch -source $PROJECT_NAME.tcl"
COPY_FILES_ON_COMPLETION="$PROJECT_NAME.bit $PROJECT_NAME.timing.rpt vivado.log"

if [ $# -ne 1 ]; then
  echo "Usage: $0 <user@server_ip>"
  exit 1
fi

server_arg="$1"
socket_file="/tmp/ssh_socket_$$"

# Cleanup function
cleanup() {
  ssh -S "$socket_file" -O exit "$server_arg" 2> /dev/null
}

echo "Connecting to server, you may be prompted to authenticate..."

# Create master SSH connection in background after auth
ssh -fNM -S "$socket_file" "$server_arg"

# Set cleanup to run on exit
trap cleanup EXIT

echo "Checking for vivado on server..."

# Check if vivado is available on server
if ! vivado_path=$(ssh -S "$socket_file" "$server_arg" "which vivado"); then
  echo "Error: vivado not found on server"
  exit 1
fi

echo "Vivado path: $vivado_path"

# Create temp directory on server
temp_dir=$(ssh -S "$socket_file" "$server_arg" "mktemp -d")
echo "Server temp directory: $temp_dir"

echo "Copying project files to server..."

# Copy project files
scp -o "ControlPath=$socket_file" $PROJECT_FILES "$server_arg:$temp_dir/"

echo "Running vivado build..."

# Run vivado command
if ! ssh -S "$socket_file" "$server_arg" "cd $temp_dir && $VIVADO_COMMAND"; then
  echo "Vivado build failed, but continuing to copy files..."
fi

echo "Copying build output files..."

# Copy files that exist
for file in $COPY_FILES_ON_COMPLETION; do
  if ssh -S "$socket_file" "$server_arg" "test -f $temp_dir/$file"; then
    echo "Copying $file"
    scp -o "ControlPath=$socket_file" "$server_arg:$temp_dir/$file" .
  fi
done

echo "Build completed, files copied back, SSH connection closed"
|}]
;;
