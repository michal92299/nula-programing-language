# This script installs Nula binaries on Windows.
# Assumptions: Binaries are downloaded or built locally; here we simulate downloading from a GitHub release.
# Change the URL as needed.
# Run this script as Administrator.

# Define variables
$VERSION = "0.1.0"
$BASE_URL = "https://github.com/yourusername/nula/releases/download/v$VERSION"
$NULA_BIN = "nula.exe"
$FRONTEND_BIN = "nula-frontend.exe"
$COMPILER_BIN = "nula-compiler.exe"
$INSTALL_DIR = Join-Path $env:PROGRAMFILES "Nula\bin"
$BIN_DIR = $INSTALL_DIR  # For simplicity, put nula.exe in the same dir and add to PATH

# Create directories if not exist
if (-not (Test-Path $INSTALL_DIR)) {
    New-Item -ItemType Directory -Path $INSTALL_DIR -Force | Out-Null
}

# Download and install nula-frontend
Write-Host "Downloading and installing $FRONTEND_BIN..."
Invoke-WebRequest -Uri "$BASE_URL/$FRONTEND_BIN-windows" -OutFile $FRONTEND_BIN
Move-Item -Path $FRONTEND_BIN -Destination $INSTALL_DIR

# Download and install nula-compiler
Write-Host "Downloading and installing $COMPILER_BIN..."
Invoke-WebRequest -Uri "$BASE_URL/$COMPILER_BIN-windows" -OutFile $COMPILER_BIN
Move-Item -Path $COMPILER_BIN -Destination $INSTALL_DIR

# Download and install nula
Write-Host "Downloading and installing $NULA_BIN..."
Invoke-WebRequest -Uri "$BASE_URL/$NULA_BIN-windows" -OutFile $NULA_BIN
Move-Item -Path $NULA_BIN -Destination $INSTALL_DIR

# Add to PATH if not already
$path = [Environment]::GetEnvironmentVariable("Path", "Machine")
if ($path -notlike "*$INSTALL_DIR*") {
    [Environment]::SetEnvironmentVariable("Path", "$path;$INSTALL_DIR", "Machine")
    Write-Host "Added $INSTALL_DIR to system PATH. Restart your shell to apply changes."
}

Write-Host "Installation complete! You can now use 'nula' command after restarting your shell."
