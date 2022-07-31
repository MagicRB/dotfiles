terraform {
  required_providers {
    vault = {
      source = "hashicorp/vault"
      version = "~> 3.7.0"
    }
    consul = {
      source = "hashicorp/consul"
      version = "~> 2.15.0"
    }
    external = {
      source = "hashicorp/external"
      version = "~> 2.2.0"
    }
  }
}
