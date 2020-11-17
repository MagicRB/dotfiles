{ config, pkgs, ... }:

with (import <nixpkgs> {});
with lib;

let
  hostname = (import <nixpkgs/nixos> {}).config.networking.hostName;
in
{
  config = ''
theme = "modern"
${
if (hostname == "fractal") then
  ''icons = "awesome"''
else
  ''icons = "awesome5"''
}

${
if (hostname == "omen") then
  ''
[[block]]
block = "battery"
interval = 30
format = "{percentage}% {time} {power}W"

[[block]]
block = "networkmanager"
on_click = "nm-connection-editor"
interface_name_include = [ "wlo1", "eno1", "wg0" ]
  ''
else
  ""
}

[[block]]
block = "memory"
display_type = "memory"
format_mem = "{Mup}%"
format_swap = "{SUp}%"

${
if (hostname == "omen") then
  ''
[[block]]
block = "nvidia_gpu"
label = "GTX 1050"
show_memory = true
show_clocks = true
interval = 1
  ''
else if (hostname == "heater") then
  ''
[[block]]
block = "nvidia_gpu"
label = "GTX 1060"
show_memory = true
show_clocks = true
interval = 1
  ''
else
  ""
}

[[block]]
block = "cpu"
interval = 1

[[block]]
block = "sound"

[[block]]
block = "time"
interval = 60
format = "%a %d/%m %R"
  '';
}