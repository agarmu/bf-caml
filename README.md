# bf-caml

Brainf*ck in Hardcaml

## Usage

```
bf_caml [--memory ram|rom] [--bank tape|program|all|none] [--dump-vcd FILE] PROGRAM.bf
bf_caml [--memory ram|rom] [--bank tape|program|all|none] --dump-rtl FILE [PROGRAM.bf]
```

- `--memory` chooses whether we use (emulated) RAM or ROM for the program memory
- `--bank` chooses whether we split memory into lanes

Simulation input comes from `stdin` and output goes to `stdout`. After a simulation
ends, statistics are printed to `stderr`. Pass `--dump-vcd` to write the waveform and
`--dump-rtl` to generate verilog.

