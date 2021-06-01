# mvn-el: [Emacs][0] things for [mvn][1]

By Andrew Gwozdziewycz, licensed under the [GNU GPLv3][2]

This is a few helpers for using compilation mode in Emacs with maven.

### Customizations

If your maven is in a nonstandard location, `(setq mvn-command "/path/to/ant -emacs")`

If your build doesn't use "pom.xml", you'll need to modify `mvn-command` to end with with `-f` or `--file` and also do `(setq mvn-build-file-name "somethingelse.xml")` so that the automated project root discovery works correctly.

### Installation

Put mvn.el in your `load-path` and add `(require 'mvn)` to your .emacs

### Usage

The basic operation is to invoke `M-x mvn`, which will ask you for a goal.

`M-x mvn-last` will re-issue the last command

`M-x mvn-compile` will run the standard `mvn compile`

`M-x mvn-clean` will run the standard `mvn clean`

`M-x mvn-test` will run the standard `mvn test`

`mvn` can be called non-interactively too, in which case it's called as such: `(mvn "sometask")`. This means that you can can define your own functions like `mvn-compile` for your projects:

    (defun mvn-compile-full ()
        (interactive)
        (mvn "dependency:sources"))
        
`M-x mvn-kill-cache` kills the internal cache used to speed up the auto-completion of ant tasks in the mini-buffer.

### Tips

When the compilation buffer looks garbled, it usually from the wrong terminal escape sequences.  You may insert following code into your init script so that compilation buffer can correctly shows colored messages [ansi-color][4]:

    (ignore-errors
      (require 'ansi-color)
      (defun colorize-compilation-buffer ()
        (when (eq major-mode 'compilation-mode)
          (let ((inhibit-read-only t))
            (if (boundp 'compilation-filter-start)
                (ansi-color-apply-on-region compilation-filter-start (point))))))
      (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

### Future

In the future, I'd really like to build a more general mode for working with Java code--something like [malabar-mode][3], but without a lot of the semantic stuff it does. Minimal requirements:

    1. It needs to support navigation really well (jtags does a decent to good job here)
    2. It needs to be able to find code in source jars (if available)
    3. It needs to have a built in way to launch a REPL (clojure, jython, sisc) for experimental programming purposes
    4. Run test at point/file
    5. List possible maven goals

[0]: http://gnu.org/software/emacs
[1]: http://maven.apache.org
[2]: http://www.gnu.org/licenses/gpl.html
[3]: https://github.com/espenhw/malabar-mode
[4]: http://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
