A BibTeX bibliography manager based on Ivy and the
bibtex-completion backend.  If you are familiar with helm-bibtex,
this is the ivy version.

News:
- 11/24/2016: Added support for bare relative paths to PDF
  files.  Concatenates the path in the `file' field to all paths
  in `bibtex-completion-library-path'.
- 11/24/2016: Added citation function for APA-style citations in org
  files.  See `bibtex-completion-format-citation-org-apa-link-to-PDF'.
- 11/18/2016: Added support for bibliographies in org-bibtex
  format.  See docstring of `bibtex-completion-bibliography'.
- 11/10/2016: Layout of search results can now be customized.
- 09/29/2016: Performance improvements in ivy-bibtex.  Note: If
  you changed your default action in ivy-bibtex, you have to rename
  the action, e.g. from `bibtex-completion-insert-key` to
  `ivy-bibtex-insert-key`.  For details see
  https://github.com/tmalsburg/helm-bibtex#change-the-default-action
- 09/20/2016: Added fallback options to ivy frontend.
- 04/18/2016: Improved support for Mendely/Jabref/Zotero way of
  referencing PDFs.
- 04/12/2016: Published ivy version of helm-bibtex.

See NEWS.org for old news.

Key features:
- Quick access to your bibliography from within Emacs
- Tightly integrated workflows
- Provides instant search results as you type
- Powerful search expressions
- Open the PDFs, URLs, or DOIs associated with an entry
- Insert LaTeX cite commands, Ebib links, or Pandoc citations,
  BibTeX entries, or plain text references at point, attach PDFs to
  emails
- Attach notes to publications

Install:

  Put this file in a directory included in your load path or
  install ivy-bibtex from MELPA (preferred).  Then add the
  following in your Emacs startup file:

    (require 'ivy-bibtex)

  Alternatively, you can use autoload:

    (autoload 'ivy-bibtex "ivy-bibtex" "" t)

  Requirements are parsebib, swiper, s, dash, and f.  The easiest way
  to install these packages is through MELPA.

  Let ivy-bibtex know where it can find your bibliography by
  setting the variable `bibtex-completion-bibliography'.  See the
  manual for more details:

    https://github.com/tmalsburg/helm-bibtex/blob/master/README.ivy-bibtex.org

Usage:

   Do M-x ivy-bibtex and start typing a search query when prompted.
