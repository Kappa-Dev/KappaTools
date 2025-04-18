#!/bin/bash

set -e  # Exit on any error

# Function to check if a command exists
command_exists () {
    command -v "$1" >/dev/null 2>&1
}

echo "📦 Setting up OCaml project build..."

# Check for opam
if ! command_exists opam; then
    echo "❌ 'opam' not found. Please install it from https://opam.ocaml.org"
    exit 1
fi

# Initialize opam (if not already done)
if [ ! -d "$HOME/.opam" ]; then
    echo "🛠️ Initializing opam..."
    opam init -y --bare
    eval $(opam env)
fi

# Check if dune is installed
if ! command_exists dune; then
    echo "🔧 Installing dune..."
    opam install -y dune
fi

# Install dependencies
echo "📥 Installing dependencies..."
opam install . --deps-only -y --update-invariant

# Load opam environment
eval $(opam env)

# Build the project using dune
echo "🏗️ Building the project..."
dune build

echo "✅ Build complete!"
