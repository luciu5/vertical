# Pre-extraction vertical fixtures

`generate_pre_extraction_fixtures.R` records reviewed numerical behavior from
the vertical implementation at antitrust commit
`24caac92a6c8ad4dc1d3d754c4c26ad0afd4248f`.

Run it before removing the implementation from antitrust:

```sh
Rscript --vanilla baseline/generate_pre_extraction_fixtures.R
```

The script refuses to run at another antitrust commit and writes only explicit
economic outputs to `tests/testthat/fixtures/pre-extraction-vertical.rds`. It
does not serialize legacy S4 model objects. Post-extraction tests must compare
the new implementation with these recorded values, never with a live old
implementation on the same code path.

Negative inferred downstream marginal-cost warnings and the historical
`chain_level` behavior are recorded as compatibility observations. They are
not assertions that those behaviors are economically desirable.
