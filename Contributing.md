# Contributing

## Patches

* No contribution is too small. Even if it is a typo fix, please submit it!

* Before you start working on something, please open an issue on the issue
  tracker for it, or leave a comment on an existing issue.

  Similarly, if you need help (while trying to write a patch), or have become
  busy with other commitments (and hence will not be able to contribute) while
  working on an issue, please leave a comment. Both of these are perfectly
  understandable situations, and I will try to help if I'm able to do so.

* Try to follow the conventions (e.g. naming, formatting, how imports are
  organized) of the surrounding code.

* Force-pushing to pull request branches is perfectly okay. Not having a clean
  git history is perfectly okay too. Don't sweat it.

# Development notes

* Use the incantation `rg "\\\\[^ -]+" man.1 -N -o | sort | uniq` to obtain
  the list of escape codes under use in `man.1`.
* To see list of directives at line-start across all man files -
  ```
  cd /usr/share/man
  for f in ./*/*.gz; do gunzip --stdout "$f"; done \
     | rg "^\\.[[:alpha:]]{1,2}" -o -U | sort | uniq -c
  ```
