#!/bin/bash

# This script installs Nula binaries on Linux.
# Assumptions: Binaries are downloaded or built locally; here we simulate downloading from a GitHub release.
# Change the URL as needed.

set -e

# Define variables
VERSION="0.1.0"
BASE_URL="https://github.com/yourusername/nula/releases/download/v${VERSION}"
NULA_BIN="nula"
FRONTEND_BIN="nula-frontend"
COMPILER_BIN="nula-compiler"
INSTALL_DIR="/usr/lib/nula-lang/bin"
BIN_DIR="/usr/bin"

# Create directories if not exist
sudo mkdir -p "${INSTALL_DIR}"
sudo mkdir -p "${BIN_DIR}"

# Download and install nula-frontend
echo "Downloading and installing ${FRONTEND_BIN}..."
curl -L "${BASE_URL}/${FRONTEND_BIN}-linux" -o "${FRONTEND_BIN}"
chmod +x "${FRONTEND_BIN}"
sudo mv "${FRONTEND_BIN}" "${INSTALL_DIR}/"

# Download and install nula-compiler
echo "Downloading and installing ${COMPILER_BIN}..."
curl -L "${BASE_URL}/${COMPILER_BIN}-linux" -o "${COMPILER_BIN}"
chmod +x "${COMPILER_BIN}"
sudo mv "${COMPILER_BIN}" "${INSTALL_DIR}/"

# Download and install nula
echo "Downloading and installing ${NULA_BIN}..."
curl -L "${BASE_URL}/${NULA_BIN}-linux" -o "${NULA_BIN}"
chmod +x "${NULA_BIN}"
sudo mv "${NULA_BIN}" "${BIN_DIR}/"

# Clean up (if any temp files)

echo "Installation complete! You can now use 'nula' command."
