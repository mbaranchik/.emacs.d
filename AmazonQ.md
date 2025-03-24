# Amazon Q Integration for Emacs

This document describes the configurable output format feature in elevate.el.

## Overview

The `elevate.el` package now supports configurable output formats for chat buffers. You can choose between:

- Markdown format
- Org-mode format

## Configuration

To set your preferred output format, customize the `elevate-output-format` variable:

```elisp
;; Set to 'markdown for markdown format
(setq elevate-output-format 'markdown)

;; Set to 'org-mode for org-mode format (default)
(setq elevate-output-format 'org-mode)
```

You can also set this through the customization interface:

```
M-x customize-variable RET elevate-output-format RET
```

## Features Affected

The following features respect the output format setting:

1. Chat buffers (created with `elevate-chat`)
2. Context history display
3. Code explanations
4. Context listings

## Example

When set to `'markdown`, chat responses will be formatted with markdown headers:

```markdown
## Question

Your question here

## Answer

The assistant's response here
```

When set to `'org-mode`, chat responses will be formatted with org-mode headers:

```org
* Question

Your question here

* Answer

The assistant's response here
```

## Implementation Details

The implementation checks the value of `elevate-output-format` when:
- Creating new chat buffers
- Displaying context history
- Formatting questions and answers
- Setting the major mode for output buffers
