# org-roam-logseq
Convert Logseq's fuzzy file block links to Org-roam's UUID links.

Based on https://gist.github.com/zot/ddf1a89a567fea73bc3c8a209d48f527. Thanks!

Bug:
If org file's title includes ":" "?", org-roam-db could not insert the node normally.
* If so you should delete the org-roam.db and restart Emacs.
