package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var (
	frontendBin string
	compilerBin string
	configFile  string
)

func init() {
	// Determine binary paths based on OS
	var baseDir string
	if runtime.GOOS == "linux" {
		baseDir = "/usr/lib/nula-lang/bin"
	} else if runtime.GOOS == "windows" {
		baseDir = filepath.Join(os.Getenv("PROGRAMFILES"), "Nula", "bin")
	} else {
		baseDir = "/usr/local/nula/bin" // Default for others, e.g., macOS
	}
	frontendBin = filepath.Join(baseDir, "nula-frontend")
	if runtime.GOOS == "windows" {
		frontendBin += ".exe"
	}
	compilerBin = filepath.Join(baseDir, "nula-compiler")
	if runtime.GOOS == "windows" {
		compilerBin += ".exe"
	}

	// Viper for config (Nula.hcl)
	viper.SetConfigName("Nula")
	viper.SetConfigType("hcl")
	viper.AddConfigPath(".")
	viper.AddConfigPath("$HOME/.nula")
}

func main() {
	var rootCmd = &cobra.Command{
		Use:   "nula [flags] <input.nula>",
		Short: "Nula Programming Language CLI",
		Long:  `Nula is a magical compiler for the Nula language, handling frontend and backend compilation.`,
		Args:  cobra.MinimumNArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			inputFile := args[0]
			outputFile, _ := cmd.Flags().GetString("output")
			if outputFile == "" {
				outputFile = strings.TrimSuffix(inputFile, filepath.Ext(inputFile))
				if runtime.GOOS == "windows" {
					outputFile += ".exe"
				}
			}

			target, _ := cmd.Flags().GetString("target")
			arenaAllocator, _ := cmd.Flags().GetBool("arena-allocator")
			manualMemory, _ := cmd.Flags().GetBool("manual-memory")
			staticBinary, _ := cmd.Flags().GetBool("static-binary")
			dynamicBinary, _ := cmd.Flags().GetBool("dynamic-binary")
			smallBinary, _ := cmd.Flags().GetBool("small-binary")
			fastCompile, _ := cmd.Flags().GetBool("fast-compile")

			// Read config if exists
			if configFile != "" {
				viper.SetConfigFile(configFile)
			}
			if err := viper.ReadInConfig(); err == nil {
				// Override flags with config if not set
				if !cmd.Flags().Changed("target") {
					target = viper.GetString("target")
				}
				if !cmd.Flags().Changed("arena-allocator") {
					arenaAllocator = viper.GetBool("arena-allocator")
				}
				// Similarly for others
			}

			// Validate options
			if arenaAllocator && manualMemory {
				return fmt.Errorf("cannot enable both arena-allocator and manual-memory")
			}
			if staticBinary && dynamicBinary {
				return fmt.Errorf("cannot enable both static-binary and dynamic-binary")
			}
			if smallBinary && fastCompile {
				return fmt.Errorf("cannot enable both small-binary and fast-compile")
			}

			// Create temp JSON file
			tempJSON, err := ioutil.TempFile("", "nula-*.json")
			if err != nil {
				return err
			}
			defer os.Remove(tempJSON.Name())
			tempJSON.Close()

			// Run frontend
			frontendArgs := []string{
				"--input", inputFile,
				"--output", tempJSON.Name(),
			}
			frontendCmd := exec.Command(frontendBin, frontendArgs...)
			frontendCmd.Stdout = os.Stdout
			frontendCmd.Stderr = os.Stderr
			if err := frontendCmd.Run(); err != nil {
				return fmt.Errorf("frontend failed: %v", err)
			}

			// Run compiler
			compilerArgs := []string{
				"--input", tempJSON.Name(),
				"--output", outputFile,
				"--target", target,
			}
			if arenaAllocator {
				compilerArgs = append(compilerArgs, "--arena-allocator")
			}
			if manualMemory {
				compilerArgs = append(compilerArgs, "--manual-memory")
			}
			if staticBinary {
				compilerArgs = append(compilerArgs, "--static-binary")
			}
			if dynamicBinary {
				compilerArgs = append(compilerArgs, "--dynamic-binary")
			}
			if smallBinary {
				compilerArgs = append(compilerArgs, "--small-binary")
			}
			if fastCompile {
				compilerArgs = append(compilerArgs, "--fast-compile")
			}

			compilerCmd := exec.Command(compilerBin, compilerArgs...)
			compilerCmd.Stdout = os.Stdout
			compilerCmd.Stderr = os.Stderr
			if err := compilerCmd.Run(); err != nil {
				return fmt.Errorf("compiler failed: %v", err)
			}

			fmt.Printf("Compilation successful! Output: %s\n", outputFile)
			return nil
		},
	}

	rootCmd.Flags().StringP("output", "o", "", "Output binary file")
	rootCmd.Flags().String("target", "native", "Target triple for cross-compilation")
	rootCmd.Flags().Bool("arena-allocator", false, "Use arena allocator for automatic memory management")
	rootCmd.Flags().Bool("manual-memory", false, "Use manual memory management")
	rootCmd.Flags().Bool("static-binary", false, "Generate fully static binary")
	rootCmd.Flags().Bool("dynamic-binary", false, "Generate fully dynamic binary")
	rootCmd.Flags().Bool("small-binary", false, "Optimize for small binary size (slower compile)")
	rootCmd.Flags().Bool("fast-compile", false, "Optimize for fast compilation (larger binary)")
	rootCmd.Flags().StringVar(&configFile, "config", "", "Config file (default is Nula.hcl)")

	if err := rootCmd.Execute(); err != nil {
		os.Exit(1)
	}
}
