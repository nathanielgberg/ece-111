# ECE 111 Final Project – Bitcoin Hashing using SHA-256

**Authors:** Sean Fuhrman and Nathaniel Greenberg  
**Course:** ECE 111  

This repository contains all the files for our final project. The project implements Bitcoin hashing using the SHA-256 cryptographic hash function and mimics the core operations of Bitcoin mining by processing a 19-word block header and computing double SHA-256 hashes for 16 different nonce values.

---

## Overview

**SHA-256:**  
SHA-256 is a cryptographic hash function that transforms any input message into a unique 256-bit (32-byte) hash. Its properties include:
- **Determinism:** The same input always produces the same hash.
- **Collision Resistance:** It is computationally infeasible to find two different inputs that produce the same output.
- **Preimage Resistance:** It is nearly impossible to reverse the hash to recover the original input.

In Bitcoin, SHA-256 is central to the proof-of-work algorithm and is used to secure transactions and link blocks in the blockchain.

**Bitcoin Hashing:**  
In this project, our design:
- Reads a 19-word block header.
- Computes a double SHA-256 hash for 16 different nonce values.
- Stores the resulting hash values in memory.
- Asserts a "done" flag upon completion.

This process mirrors the essential operation of Bitcoin mining, where miners search for a nonce that produces a hash meeting specific criteria.

---

## Design Objectives and Constraints

**Objectives:**
- **Delay and Area Minimization:** Optimize for efficient hardware usage.
- **Consistency:** Ensure the same input always generates the same 256-bit output.
- **Speed:** Implement a fast hashing process suitable for Bitcoin mining.
- **Security:** Maintain collision and preimage resistance.

**Constraints:**
- No inferred megafunctions or latches.
- Accurate and correct padding to form complete blocks.
- Process a 20-word Bitcoin header block (620 bits total), which requires two blocks for one input message.
- Cycle through 16 nonces in parallel.

---

## Design Challenges and Solutions

- **Padding Implementation:**  
  Handling correct padding was challenging, especially with varying input lengths. We redesigned the padding logic to detect when to create a new block.

- **Blocking vs. Nonblocking Statements:**  
  Initial syntax issues led to inferred latches. We resolved these by correcting our use of `always_ff` blocks with nonblocking assignments.

- **Parallel Nonce Computation:**  
  Efficiently computing 16 hashes in parallel required a carefully managed state machine. We implemented multiple states to handle header reading, processing, and output, which simplified control and improved efficiency.

- **State Machine Complexity:**  
  Our design uses seven primary states (IDLE, READ, WAIT, COMPUTE_FIRST, COMPUTE_SECOND, PREP_THIRD, COMPUTE_THIRD, WRITE) to manage the hashing process. Extra states allowed us to handle parallel processing effectively.

- **Word Size Optimization:**  
  Choosing an optimal word size (e.g., using a word size of 12 or 16 with shifting during execution) reduced the number of ALUTs and improved both area and delay performance.

---

## Implementation Details

### SHA-256 Core State Machine

- **IDLE:** Waits for the start signal; initializes internal registers (H0–H7, A–H, counters, etc.).
- **BLOCK:** Checks if a full block is ready; if not, transitions to WAIT.
- **READ & WAIT:** Reads words from memory and adds padding when needed.
- **COMPUTE:** Executes 64 cycles of the SHA-256 compression function.
- **INIT_WRITE & WRITE:** Prepares and writes the final hash output (H0–H7) to memory.

### Bitcoin Hashing Algorithm

The Bitcoin hashing engine extends the SHA-256 core with additional states to handle double hashing and nonce processing:
- **IDLE, READ, WAIT:** Manage input and header reading.
- **COMPUTE_FIRST:** Processes the first hash block (64 cycles).
- **COMPUTE_SECOND:** Processes the second hash block in parallel for all nonces.
- **PREP_THIRD & COMPUTE_THIRD:** Prepares and executes a third SHA-256 computation for final hash outputs.
- **WRITE:** Writes the computed hash values into memory.

---

## Hierarchical Synthesis & Optimizations

1. **Single Core Implementation:**  
   A single core performs vector-matrix multiplication and SHA-256 hashing. Results are stored in parameter memory (pmem) and verified using a testbench.

2. **Output Normalization:**  
   Partial results are fetched from pmem, normalized (each element divided by the sum of absolute values), and written back. This is verified through behavioral simulation.

3. **Hierarchical Synthesis of the Core:**  
   SRAM macros are synthesized separately (using top metal layer M4, a 4 µm pin pitch, and specified pin placement) and then integrated into the core design using hierarchical PnR.

4. **Dual Core Design:**  
   Two single cores are combined to process 16 vectors. An asynchronous communication protocol (using FIFOs and synchronizers) merges partial sums across different clock domains.

5. **Final Optimizations & PnR:**  
   Techniques such as multi-cycle paths and pipelining of critical combinational logic (multiplication and addition) were applied to meet a 1 GHz target with zero negative slack. Additional registers were added to pipeline long combinational paths, reducing leakage power and improving overall performance.

---

## Deliverables

- **RTL Source Files:** Verilog files for the SHA-256 engine and Bitcoin hashing implementation.
- **Synthesis/PnR Reports:** Completed .enc files, timing reports (Fmax, WNS), and resource utilization reports.
- **Simulation Artifacts:** Waveform VCD files and Modelsim transcript outputs showing passing test results for various input sizes (20, 30, and 40 words).
- **Documentation:** This README, the final project report, and presentation files.

---

## Results

- **Functional Verification:**  
  Waveform snapshots and testbench outputs demonstrate the correct generation of hash values for 19-word block headers and 16 nonces.

- **Resource Utilization:**  
  Resource reports for different input sizes are similar, indicating that the design scales well without increasing area or delay significantly.

- **Timing Performance:**  
  Timing reports and STA results confirm that the optimized design meets the 1 GHz target with zero negative slack.

---

## Conclusion

Our project successfully implements a hardware-based SHA-256 engine for Bitcoin hashing. By addressing key challenges such as padding, state machine complexity, and parallel nonce computation, we achieved an efficient and robust design. Hierarchical synthesis and targeted optimizations ensured that our design meets strict timing, area, and power constraints, laying a strong foundation for cryptographic hash accelerators in Bitcoin mining applications.

---
