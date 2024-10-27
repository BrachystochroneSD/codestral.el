# codestral.el

Completion system inspired by copilot.el

## Usage


You need to create an account at mistral.ai and activate a codestral plan.
And create an api token at https://console.mistral.ai/codestral

You just need to set `codestral-api-key` and `codestral-api-url` custom variables.

Then invoke the `codestral-mode` minor mode via `global-codestral-mode`.

Here is an example of a configuration:

```emacs-lisp
(use-package codestral
  :load-path "~/.emacs.d/my-packages/codestral.el"
  :custom
  (codestral-api-key "your api key")
  (codestral-api-url "https://codestral.mistral.ai")
  :bind (("C-M-:" . global-codestral-mode)
         :map codestral-mode-map
         ("C-:" . codestral-accept-completion)
         ("C-=" . codestral-next-completion)
         ("C-;" . codestral-previous-completion)))
```
