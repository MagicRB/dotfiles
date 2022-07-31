terraform {
  backend "consul" {
    address = "10.64.1.201:8500"
    scheme  = "http"
    path    = "terraform/dotfiles"
  }
}
