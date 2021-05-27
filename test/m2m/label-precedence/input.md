---
defaultOption:
  - numberSections
prefixes:
  sec:
    numbering: roman
    captionIndexTemplate: $$s.i%.$$$$ri$$
    sub:
      numbering: alpha A
  fig:
    numbering: alpha α
---

# First Section {label="*"}

text

![A figure](image.png){#fig:fig1}

## Subsection

other text

![A figure with custom label](image.png){#fig:fig2 label="+"}

### Subsubsection

text text text
