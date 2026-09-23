# dataraft.core 0.1.0.9005

* The modular workflow wrapper is removed; the deprecated builder produces ordinary products.
* `dr_contract()` now has five core arguments. Compose metadata and policy separately; legacy dots are deprecated.
* `dr_quality_rule()` and pointblank specifications store action/threshold; severity/max_failure are deprecated.
* `dr_run()` reuses a bounded session-local determinism verdict; dry runs always repeat checks.
* `dr_set_sources()` replaces source CRUD exports, with NULL removal and explicit recursive graph editing.
* `dr_trial()` is soft-deprecated and now raises on failure by default, like dr_run().
* Lookup validation accepts only dm; unused partition capabilities are removed.

# dataraft.core 0.1.0.9004

* Mark inferred schema evidence unvalidated and freeze lazy candidates before generic writes.
* Dispatch backend operations through provider S3 methods, with no reverse sibling namespace calls.
* Use product execution with write = FALSE for trials, including upstream target/catalog/evidence suppression.
* Add canonical action/threshold quality composition, volatile rule detection and repeat evaluation, contract policy/metadata helpers and dm-only lookup validation.

* Use the umbrella CI manifest as the single immutable family dependency lock.

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
