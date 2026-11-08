# vertical 0.3.0

- Add neutral policy-fit conversion, upstream/downstream tariffs and constrained quotas with independent Nash and KKT validation.
- Preserve policy state, originating quota capacities and cumulative costs across sequential and resumed simulations.
- Correct policy-auction concentration reporting using physical winner shares.
- Downstream second-score tariffs and finite second-score quotas remain explicitly unsupported pending an allocation derivation.

# vertical 0.2.0

- Added neutral `VerticalTradeFit` promotion and tier-specific trade
  counterfactuals with physical net seller receipts, portfolio bargaining and
  capacity KKT diagnostics. Policies and cumulative costs persist through
  sequential simulation and resume.
- Added independent net-profit, bargaining and quota economic tests. Full-chain
  Bertrand supports finite quotas; second-score auctions support upstream
  tariffs and reject unimplemented downstream-tax or quota allocation.

- Added inactive-product subset solving for Bertrand, nested Logit, and
  second-score vertical models, with strict logical-mask validation and
  full-length equilibrium outputs.
- Added first-class hybrid merger classification for simultaneous horizontal
  and vertical ownership changes.
- Added `merger_type()` and tier-specific HHI reporting for hybrid mergers.
- Added a residual-checked second-score solver fallback for hybrid ownership
  structures.

* Counterfactual linked-product exits now remove both tiers, persist through
  ordinary and policy sequences/resumes, preserve originating quota anchors,
  and clear exited capacity rents. Promoted pre-state outputs retain the
  preceding active market without recalibration.
