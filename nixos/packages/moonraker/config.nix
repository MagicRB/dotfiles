value: lib: with lib;
''
  [server]
  host: ${value.config.server.host}
  port: ${builtins.toString value.config.server.port}
  klippy_uds_address: ${value.config.server.klippyUdsAddress}
  max_upload_size: ${builtins.toString value.config.server.maxUploadSize}
  enable_debug_logging: ${if value.config.server.enableDebugLogging then "True" else "False"}
  ${if value.config.server.configPath != "" then "config_path: " + value.config.authorization.configPath else ""}

  [authorization]
  enabled: ${if value.config.authorization.enabled then "True" else "False"}
  ${if value.config.authorization.apiKeyFile != "" then "api_key_file: " + value.config.authorization.apiKeyFile else ""}
  trusted_clients:
  ${foldl (str: acc: acc + str) "" (map (x: "  " + x + "\n") value.config.authorization.trustedClients)}
  cors_domains:
  ${foldl (str: acc: acc + str) "" (map (x: "  " + x + "\n") value.config.authorization.corsDomains)} 
''
