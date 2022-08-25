terraform {
  backend "consul" {
    address = "10.64.2.1:8500"
    scheme  = "http"
    path    = "terraform/dotfiles"
  }
}
