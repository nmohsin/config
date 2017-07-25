Emacs configuration
====================

This repo is an effort to bring all my Emacs configuration tweaks into one place. It took me a while to refactor things into a form I was happy with, and I'm slowly merging in configurations from different machines.

I use a fairly simple system. All customizations live in .emacs.d/config. They're divided up into a few general categories for simplicity. The file init.el sets everything up and fires up the customizations. To hook it up to Emacs initialization, simply symlink .emacs.d/config/init.el to .emacs.d/init.el. Emacs will automatically find this file on startup and run it to initialize everything.

Machine-specific configuration is stored in config/local, using the same categories and naming scheme. Git is configured to ignore this directory, so I can tweak things to work differently at home, at work, on a laptop, etc.
