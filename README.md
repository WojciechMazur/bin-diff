# bin-diff

A binary comparison tool that analyzes differences between two compiled binaries at multiple levels: bit-level hashes, symbols, function bodies, and embedded strings.

## Requirements

- Scala CLI
- System tools: `nm`, `objdump`, `file`, `shasum`

## Usage

```bash
scala-cli run bin-diff.scala -- --old <path> --new <path> [options]
```

### Options

| Option | Description |
|--------|-------------|
| `--old <path>` | Path to the reference binary (required) |
| `--new <path>` | Path to the binary to compare (required) |
| `-v, --verbose` | Enable verbose output |
| `--focus <prefix>` | Only show symbols matching this prefix |
| `--ignore-file <path>` | File with glob patterns to ignore (one per line) |
| `--diff <regex>` | Show instruction diff for functions matching regex |
| `--raw-addresses` | Show raw addresses in diff output |

## Example

```bash
# Basic comparison
scala . -- --old build-a/app --new build-b/app

# Verbose comparison with symbol filter
scala . -- --old old/demo --new new/demo -v --focus "log_"

# Show instruction diff for specific functions
scala . -- --old old/demo --new new/demo --diff "main|log_.*"

# Using an ignore file for linker noise
scala . -- --old a.out --new b.out --ignore-file symbols.ignore
```
