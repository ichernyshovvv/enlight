# `light-dashboard`

A simple dashboard package that may be used as a startup screen in Emacs.

<p align="center">
  <img src="demo.gif" alt="Demo gif"/>
</p>

## Installation and configuration

``` elisp
(use-package light-dashboard
  :init
  (unless (package-installed-p 'light-dashboard)
    (package-vc-install
     '(light-dashboard
       :vc-backend Git
       :url "https://github.com/ichernyshovvv/light-dashboard"
       :branch "master")))
  :custom
  (light-dashboard-alist
   '(("Org Mode"
      ("Org-Agenda (current day)" (org-agenda nil "a") "a")
      ("Recipes" (find-file "/org/recipes.org"))
      ("Inbox" (find-file "/org/inbox.org") "i"))
     ("Downloads" ("Transmission" transmission "t"))
     ("Current Projects"
      ("org-timeblock" (dired "~/.my-emacs-packages.d/active/org-timeblock/") "o")
      ("chloe" (dired "~/.my-emacs-packages.d/active/chloe/") "x")))))
```

## Usage

You can set `initial-buffer-choice` custom variable to #'light-dashboard in
order to show light-dashboard right after Emacs startup:

```
(setopt initial-buffer-choice #'light-dashboard)
```

Also you always can switch to the dashboard by calling

``` 
M-x light-dashboard-open
```

Thanks to @a13 and @progfolio for code review.
