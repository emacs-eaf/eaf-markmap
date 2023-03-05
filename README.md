# EAF Markmap
<p align="center">
  <img width="800" src="./screenshot.png">
</p>

This repository provides the EAF Markdown/Org Mindmap application for the [Emacs Application Framework](https://github.com/emacs-eaf/emacs-application-framework).

### Load application

[Install EAF](https://github.com/emacs-eaf/emacs-application-framework#install) first, then add below code in your emacs config:

```Elisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
(require 'eaf)
(require 'eaf-markmap)
```

Then execute `eaf-open` command to open .md or .org file, choose `markmap` app.

### The keybinding of EAF Vue demo.

| Key   | Event   |
| :---- | :------ |
| `s` | save_as_screenshot |
| `<f12>` | open_devtools |

