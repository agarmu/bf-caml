# bf-caml

Brainf*ck in Hardcaml

## Usage

```
bf_caml [--memory ram|rom] [--bank tape|program|all|none] PROGRAM.bf
```

- `--memory` chooses whether we use (emulated) RAM or ROM for the program memory
- `--bank` chooses whether we split memory into lanes

Simulation input comes from `stdin` and output goes to `stdout`. After a simulation
ends, statistics are printed to `stderr`, and a waveform is written to `cell.vcd`.