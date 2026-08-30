# gptel-openrouter-models

Pick an OpenRouter model for [gptel](https://github.com/karthink/gptel) using
`completing-read` — so it works out of the box with vertico, ivy, helm, or
plain Emacs completion.

- Fetches the live model list from OpenRouter's public `/api/v1/models`
  endpoint. No API key is needed just to list models.
- Descriptions are passed via `completion-extra-properties`
  (`:annotation-function`), so they show up nicely if you use
  [marginalia](https://github.com/minad/marginalia), without hardcoding any
  formatting into the candidate strings.
- Only one command to remember: `gptel-openrouter-models-pick`.

## Installation

### straight.el + leaf

```elisp
(leaf gptel-openrouter-models
  :straight (gptel-openrouter-models
             :type git :host github
             :repo "mofucat/gptel-openrouter-models")
  :after gptel
  :custom
  (gptel-backend . (gptel-make-openai "OpenRouter"
                     :host "openrouter.ai"
                     :endpoint "/api/v1/chat/completions"
                     :stream t
                     :key (gptel-api-key-from-environment "OPENROUTER_API_KEY")
                     :models '(openrouter/free))))
```

### straight.el + use-package

```elisp
(use-package gptel-openrouter-models
  :straight (gptel-openrouter-models
             :type git :host github
             :repo "mofucat/gptel-openrouter-models")
  :after gptel)
```

### package-vc.el (built into Emacs 29+, no straight.el required)

```elisp
(unless (package-installed-p 'gptel-openrouter-models)
  (package-vc-install "https://github.com/mofucat/gptel-openrouter-models"))

(use-package gptel-openrouter-models
  :after gptel)
```

### Manual (package.el, no MELPA needed)

Clone the repo somewhere on your `load-path` and:

```elisp
(add-to-list 'load-path "/path/to/gptel-openrouter-models")
(require 'gptel-openrouter-models)
```

## Usage

You need a gptel backend for OpenRouter already configured, e.g.:

```elisp
(setq my/openrouter-backend
      (gptel-make-openai "OpenRouter"
        :host "openrouter.ai"
        :endpoint "/api/v1/chat/completions"
        :stream t
        :key (gptel-api-key-from-environment "OPENROUTER_API_KEY")
        :models '(openrouter/free))) ; placeholder, run gptel-openrouter-models-pick to change it

(setq gptel-backend my/openrouter-backend)
```

Then:

- `M-x gptel-openrouter-models-pick` — search all models and set
  `gptel-model` to your selection.
- `(gptel-openrouter-models-pick "anthropic/")` from Lisp (or your own
  interactive wrapper) — restrict the search to a prefix.

The command only changes `gptel-model`; it doesn't switch `gptel-backend`
for you.

## License

MIT. See [LICENSE](LICENSE).
