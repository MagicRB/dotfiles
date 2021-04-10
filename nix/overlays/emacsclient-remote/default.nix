final: prev:
with final; {
  magic_rb = prev.magic_rb or {} // {
    emacsclient-remote =
      prev.writeShellScriptBin
        "emacsclient-remote" (builtins.readFile ./emacsclient-remote);
  };
}
