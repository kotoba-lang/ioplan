# Changelog

## 0.1.0 — 2026-08-03

Initial implementation: align → merge → order → batch, as a pure function of
requests plus a device descriptor.

- Ordering is decided by the device's `:seek-cost`. C-SCAN on a seeking
  device; submission order preserved, untouched, on a zero-seek one.
- Merging follows the same split: sorted-then-merged where a seek costs
  something, submission-neighbours-only where it does not, so merging cannot
  reorder a device by the back door.
- Block alignment reported as amplification rather than hidden; sub-block
  writes reported as write amplification.
- `explain` states what the planner declined to do, not only what it did.

18 tests, 50 assertions.
