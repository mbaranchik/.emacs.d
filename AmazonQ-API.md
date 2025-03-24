# Amazon Q Integration for Emacs

This document describes the configurable features in elevate.el.

## API Providers

The package now supports multiple LLM API providers:

- Amazon internal API (default)
- OpenAI API

### Configuration

To set your preferred API provider, customize the `elevate-api-provider` variable:

```elisp
;; Use Amazon internal API
(setq elevate-api-provider 'amazon)

;; Use OpenAI API
(setq elevate-api-provider 'openai)
```

For OpenAI API, you'll need to set your API key:

```elisp
(setq elevate-openai-api-key "your-api-key-here")
```

You can also switch providers interactively with:

```
M-x elevate-switch-api-provider
```

## Available Models

### Amazon Models

- AmazonNovaPro
- Sonnet3.5v2
- Haiku3.5
- Sonnet3.7Think

### OpenAI Models

- GPT-4o
- GPT-4-Turbo
- GPT-3.5-Turbo

## Key Bindings

- `C-c l a` - Start or continue a chat
- `C-c l e` - Explain code with context
- `C-c l c` - Complete code at point
- `C-c l n` - Create a new context
- `C-c l s` - Switch context
- `C-c l l` - List available contexts
- `C-c l r` - Remove a context
- `C-c l m` - Switch chat model
- `C-c l M` - Switch completion model
- `C-c l p` - Switch API provider
- `TAB` - Accept completion suggestion
