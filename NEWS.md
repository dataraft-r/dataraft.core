# dataraft.core 0.1.0.9001

* `dr_update_contract()` is the preferred name for explicit contract revisions and can replace a product or workflow contract. `dr_contract_update()` remains a deprecated compatibility spelling without runtime warnings.
* `dr_extract_contract()` and `dr_remove_contract()` complete contract composition; `dr_add_contract()` also accepts workflows.
* `dr_update_source()`, `dr_extract_source()` and `dr_remove_source()` edit named primary sources without reading them. Ambiguous source selection fails explicitly.
* Quality-rule documentation consistently recommends `action` and `threshold`; `severity` and `max_failure` remain compatibility arguments, with conflicting pairs rejected.

# dataraft.core 0.1.0.9000

* Definition fingerprints now include referenced lexical bindings, defaults and reference data as non-disclosing hashes. Unsupported mutable captures fail with `dataraft_error_fingerprint`; existing definition fingerprints change. Bump registered contract/product versions intentionally when adopting this format; old fingerprints are not silently treated as equivalent.
* `dr_contract()` accepts typed bounds, enums, nullability, timezone and precision constraints. Portable bounds/enums run on lazy tables; timezone and precision fail closed until data is collected.
* `dr_review()` opens bounded affected rows, a quality report or recorded lineage in the IDE data viewer, using the last failed result by default and returning the displayed table invisibly.
* `dr_set_engine()` preserves quarantine actions, dimensions, policies, reference fields and custom metadata.

* Add block/warn/quarantine actions, retained quarantine rows, descriptive governance metadata, conservative direct-column lineage and versioned profile comparisons. Unknown rule evaluations continue to block publication.

* Keep stateless helpers private and prefix shared implementation interfaces with `dr_internal_`. Move component tests into their owning repository; add minimal and downstream CI.

* Readable formula labels and formula shortcuts; actionable optional-dependency errors. Failed collection retains its result and local quality conditions. Added independent contract, recipe and diagnostic regression tests.

* Initial independent DataRaft package.

* Add `dr_last_failure()` and standalone getting-started and extension articles.

* Runs without a declared contract now return `unvalidated` evidence and do not
  call writers. Observed columns and types are no longer presented as guarantees.
* Generic execution collects a lazy delivery once before quality evaluation, so
  its writer receives the same snapshot that was checked.
* Quality rules reject recognizable random and time-dependent calls unless
  `volatile = TRUE` is declared. Volatile checks are diagnostic only and cannot
  authorize publication or approval. Arbitrary callback purity is not guaranteed.

* Diagnostic providers now implement public S3 methods; status, quality and lineage no longer require reverse calls from core into extension packages.
* `dr_run(write = FALSE)` shares the recursive no-write path with `dr_trial()`;
  existing `dr_trial()` calls remain supported.
* `dr_contract_policy()` and `dr_contract_meta()` provide pipeable declaration
  builders, retaining constructor validation and avoiding duplicate constraints.
* `dr_pointblank_checks()` accepts `action` and `threshold`, matching native rules.
  Legacy arguments remain supported but cannot be supplied together with aliases.
