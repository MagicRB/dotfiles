final: prev:
with final; {
  magic_rb.emacsclient-remote =
    prev.writeShellScriptBin
      "emacsclient-remote" (builtins.readFile ./emacsclient-remote);
}
