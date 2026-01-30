## Release v0.17.0

- Removed support for explicit "nonce" and "hash" sources, in place of an "inline-content"
  that implicitly produces a hash. The removed source types were, in practice, unused.
- Removed some deprecated CSP features (`plugin_types`, `require_sri_for_script`, and
  `require_sri_for_style`).
- Added a monoidal interface for constructing policies.
