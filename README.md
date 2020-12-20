# `good-scroll.el`

Attempt at good pixel-based smooth scrolling in Emacs

## About

This package implements smooth scrolling by pixel lines.
It attempts to improve upon `pixel-scroll-mode` by adding variable speed.

![Demo](./demo.gif)

## Setup

Install and load the package.
Then, enable `good-scroll-mode`.
For example, you can add the following snippet to your config.

```lisp
(good-scroll-mode 1)
```

## FAQ

### Is this on [MELPA](https://melpa.org/)?

No.

### Can you add this to [MELPA](https://melpa.org/)?

Yes, if enough people want it.

### How does this work?

Instead of scroll events directly scrolling the screen,
they update a destination variable.
A timer that runs every `good-scroll-render-rate` seconds
calculates the expected position and actually scrolls the window to it.
To make the window scrolled partially through a line,
`good-scroll` updates the window's
[*vscroll* (vertical scroll)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Vertical-Scrolling.html)
position.

### Why is performance sometimes bad?

Scrolling sometimes pauses or stutters.
It's unclear *exactly* why,
but one factor is that Emacs lacks animation support.
Emacs has timers for updating the screen contents,
which is enough for playing simple animated GIF files,
but not enough for video playback or frame-perfect smooth scrolling.

### Why doesn't this feel right compared to smooth scrolling in Firefox?

By default, Firefox uses an algorithm based on a Bézier curve for scrolling,
so that the deceleration feels natural.
Currently, `good-scroll` uses a simpler scrolling algorithm
that linearly interpolates the window position,
which feels less natural.
Support for other scrolling algorithms is a
[planned feature](https://github.com/io12/good-scroll.el/issues/2).

### How does this compare to other scrolling packages?

Other modifications, like
[`smooth-scrolling`](https://github.com/aspiers/smooth-scrolling),
[`smooth-scroll`](https://github.com/k-talo/smooth-scroll.el),
[`sublimity-scroll`](https://github.com/zk-phi/sublimity),
and [`inertial-scroll`](https://github.com/kiwanami/emacs-inertial-scroll)
also aim to improve scrolling in Emacs,
but none of them involve scrolling by pixel lines, only by text lines.
The built-in
[`pixel-scroll`](https://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/pixel-scroll.el)
*does* implement pixel line scrolling,
but, unlike `good-scroll`, does not support dynamic scrolling velocity.

### Why is this file written in Markdown and not Org?

Apparently, GitHub does not yet support rendering
Org links with formatting inside of them.

https://github.com/novoid/github-orgmode-tests/issues/3
