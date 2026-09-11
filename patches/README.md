# Emacs 31 compatibility patches

`core-package-patches.el` applies these patches through Straight's prepare hook,
which also runs for existing installations. Reapplying a patch is a no-op.
Conflicts produce a warning and leave the checkout intact; review the patch
against the updated source before rebuilding the package.

- `avy-flash` and `ol-emacs-slack`: explicitly retain dynamic binding with
  `lexical-binding: nil`. This resolves the missing-cookie warning without
  changing the packages' binding semantics.
- `consult-mu`: replace obsolete conditional binding macros (including the old
  single-binding shorthand), and quote mode symbols in the contacts history
  matcher so that unrelated modes reach the fallback branch.

These patches intentionally modify the Straight source checkouts; their build
files are symlinks. Keep local changes if Straight asks about modified repos.
Remove the corresponding patch and package entry from the hook once upstream
provides the same fixes. Do not discard unrelated checkout changes.
