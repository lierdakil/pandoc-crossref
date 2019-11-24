---
prefixes:
  sec:
    listItemTemplate: '$$idx$$$$listItemNumberDelim$$$$sectionType[type]%: $$$$t$$'
    sectionType:
      cha: "Chapter"
      sec: "Section"
      par: "Paragraph"
...

# Section 1 {#sec:1}
## Section 1.1 {#sec:11}
# Section 2 {#sec:2 type="cha"}
## Section 2.1 {#sec:21}
### Section 2.1.1 {#sec:211}
### Section 2.1.2 {#sec:212 type="par"}
# Section 3

\listof{sec}
