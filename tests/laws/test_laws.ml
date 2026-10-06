let () =
  let open Alcotest in
  run "Vesper Domain Invariants"
    [
      ("Law 1: Total Diagnosability & Host Isolation", Test_law1_diagnosability.tests);
      ("Law 2: Invertible Concrete Syntax", Test_law2_roundtrip.tests);
      ("Law 3: Strict Source Provenance", Test_law3_provenance.tests);
      ("Law 4: Idempotent Normalization & Desugaring", Test_law4_idempotence.tests);
      ("Law 5: Deterministic Evaluation & Sound Termination", Test_law5_determinism.tests);
      ("Phase 8: VM Determinism & Dual-Execution", Test_vm_determinism.tests);
      ("Phase 9: Sandboxed I/O Isolation & Replay", Test_io_isolation.tests);
      ("Phase 10: Tooling (CLI, REPL, LSP)", Test_tooling_laws.tests);
    ]
