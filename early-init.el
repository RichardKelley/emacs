;;; early-init.el --- Startup compiler settings -*- lexical-binding: t; -*-

;; This Mac's Emacs 31 native compiler passes an unsupported
;; -mmacosx-version-min=18.0 to Clang.  Use bytecode until the bundled
;; compiler/toolchain is repaired.  This must run before package activation.
(when (eq system-type 'darwin)
  (setq native-comp-jit-compilation nil
        native-comp-enable-subr-trampolines nil
        package-native-compile nil))

;;; early-init.el ends here
