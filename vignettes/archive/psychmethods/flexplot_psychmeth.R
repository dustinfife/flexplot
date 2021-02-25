---
title             : "Flexplot: Graphically-Based Data Analysis"
shorttitle        : "Flexplot: Graphically-Based Data Analysis"

author: 
  - name      : "Dustin Fife"
affiliation   : "1"
corresponding : yes    # Define only one corresponding author
address       : "201 Mullica Hill Road
                    Glassboro, NJ 08028"
email         : "fife.dustin@gmail.com"

affiliation:
  - id            : "1"
institution   : "Rowan University"

author_note: >
  I wish to thank those who assisted in reviewing this manuscript, including Tom Dinzeo, Polly Tremoulet, Jeffrey Greeson, Yoav Zeevi, Christine Simmons, and Joseph Rodgers, as well as the anonymous reviewers and editor.  

abstract: >
  Data analysis is a risky endeavor, particularly among those unaware of its dangers. In the words of Cook and Campbell [-@cook1976design; see also @Cook2002], "Statistical Conclusions Validity" threatens all research subjected to the dark arts of statistical magic. Although traditional statistics classes may advise against certain practices (e.g., multiple comparisons, small sample sizes, violating normality), they may fail to cover others (e.g., outlier detection and violating linearity). More common, perhaps, is that researchers may fail to remember them. In this paper, rather than rehashing old warnings and diatribes against this practice or that, I instead advocate a general statistical analysis strategy. This graphically-based eight step strategy promises to resolve the majority of statistical traps researchers may fall in without having to remember large lists of problematic statistical practices. These steps will assist in preventing both false positives and negatives and yield critical insights about the data that would have otherwise been missed. I conclude with an applied example that shows how the eight steps reveal interesting insights that would not be detected with standard statistical practices. 


keywords          : "statistical assumptions, NHST, confirmatory data analysis, graphical data analysis, fishing, $p$-hacking"
#wordcount         : "X"

bibliography      : ["../../../masterbib/My Collection.bib"]
figsintext        : no
figurelist        : no
tablelist         : no
footnotelist      : no
lineno            : no

lang              : "english"
class             : "doc"
output            : papaja::apa6_pdf

header-includes:
  - \usepackage{colortbl}
- \usepackage[export]{adjustbox}   ## to vertical align images in textbox
- \usepackage{pdflscape}  ### to do a sideways table
- \usepackage{longtable} #### to let the table span multiple pages
- \usepackage{setspace}
- \AtBeginEnvironment{tabular}{\onehalfspacing}
- \AtBeginEnvironment{lltable}{\onehalfspacing}
- \AtBeginEnvironment{tablenotes}{\doublespacing}
- \captionsetup[table]{font={stretch=1.5}}
- \captionsetup[figure]{font={stretch=1.5}}   
---