# What

Simple package that allows point to go off screen, and restores it when a character is typed.

Internally this package creates a single marker object per buffer per window, for the lifetime of the buffer.

"Lightly tested rigorously" (tm)

# TODO

come up with better name

# Substitutes

[![scroll-restore-mode](https://github.com/emacsmirror/scroll-restore)](scroll-restore-mode)
GNU Elpa package. duplicate functionality. Looks like it has way more features / tested more thoroughly with more features enabled than this. I used this for a bit but I found some heavy consing going on at least at commit [07af7cd] when scrolling, causing more and more lag when rolling the mouse scroll wheel up and down.
