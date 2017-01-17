"" setting

" general
if has('autocmd')
  filetype plugin indent on
endif
if has('syntax') && !exists('g:syntax_on')
  syntax enable
endif

if !&scrolloff
  set scrolloff=1
endif
if !&sidescrolloff
  set sidescrolloff=5
endif
set display+=lastline

if &encoding ==# 'latin1' && has('gui_running')
  set encoding=utf-8
endif

if &listchars ==# 'eol:$'
  set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
endif

if v:version > 703 || v:version == 703 && has("patch541")
  set formatoptions+=j " Delete comment character when joining commented lines
endif

if has('path_extra')
  setglobal tags-=./tags tags-=./tags; tags^=./tags;
endif

if &shell =~# 'fish$' && (v:version < 704 || v:version == 704 && !has('patch276'))
  set shell=/bin/zsh
endif

set autoread

if &history < 1000
  set history=1000
endif
if &tabpagemax < 50
  set tabpagemax=50
endif
if !empty(&viminfo)
  set viminfo^=!
endif
set sessionoptions-=options
set sessionoptions-=slash

" Allow color schemes to do bright colors without forcing bold.
if &t_Co == 8 && $TERM !~# '^linux\|^Eterm'
  set t_Co=16
endif

" Load matchit.vim, but only if the user hasn't installed a newer version.
if !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# ''
  runtime! macros/matchit.vim
endif

inoremap <C-U> <C-G>u<C-U>
set ruler
set wildmenu
set incsearch
set smartcase
set textwidth=80    " Text width maximum chars before wrapping
set expandtab     " Don't expand tabs to spaces.
set tabstop=2       " The number of spaces a tab is
set softtabstop=2   " While performing editing operations
set shiftwidth=2    " Number of spaces to use in auto(indent)
set smarttab        " Tab insert blanks according to 'shiftwidth'
set autoindent      " Use same indenting on new lines
set smartindent     " Smart autoindenting on new lines
set shiftround      " Round indent to multiple of 'shiftwidth'
set clipboard=unnamedplus
set modeline                 " automatically setting options from modelines
set report=0                 " Don't report on line changes
set fileformats=unix,mac,dos " Use Unix as the standard file type
set magic                    " For regular expressions turn magic on
set path=.,**                " Directories to search when using gf
set virtualedit=block        " Position cursor anywhere in visual block
set synmaxcol=100000           " Don't syntax highlight long lines
set history=2000
if has('nvim')
  "  ShaDa/viminfo:
  "   ' - Maximum number of previously edited files marks
  "   < - Maximum number of lines saved for each register
  "   @ - Maximum number of items in the input-line history to be
  "   s - Maximum size of an item contents in KiB
  "   h - Disable the effect of 'hlsearch' when loading the shada
  set shada='300,<10,@50,s100,h
else
  set viminfo='300,<10,@50,h,n$HOME/.cache/viminfo
endif

" wildmenu
set wildmode=list:longest,full
set wildoptions=tagfile
set wildignorecase
set wildignore+=.git,.hg,.svn,.stversions,*.pyc,*.spl,*.o,*.out,*~,%*
set wildignore+=*.jpg,*.jpeg,*.png,*.gif,*.zip,**/tmp/**,*.DS_Store
set wildignore+=**/node_modules/**,**/bower_modules/**,*/.sass-cache/*

set undofile swapfile nobackup
set directory=$HOME/.cache/swap//,~/tmp,/var/tmp,/tmp
set undodir=$HOME/.cache/undo//,~/tmp,/var/tmp,/tmp
set backupdir=$HOME/.cache/backup/,~/tmp,/var/tmp,/tmp
set viewdir=$HOME/.cache/view/
set nospell spellfile=$VIMPATH/spell/en.utf-8.add

" Time out on key codes
if has('nvim')
  " https://github.com/neovim/neovim/issues/2017
  set ttimeoutlen=-1
else
  set ttimeoutlen=250
endif

" Searching {{{
" ---------
set ignorecase      " Search ignoring case
set smartcase       " Keep case when searching with *
set infercase       " Adjust case in insert completion mode
set incsearch       " Incremental search
set hlsearch        " Highlight search results
set wrapscan        " Searches wrap around the end of the file
set showmatch       " Jump to matching bracket
set matchpairs+=<:> " Add HTML brackets to pair matching
set matchtime=1     " Tenths of a second to show the matching paren
set cpoptions-=m    " showmatch will wait 0.5s or until a char is typed

" }}}
" Behavior {{{
" --------
set nowrap                      " No wrap by default
set linebreak                   " Break long lines at 'breakat'
set breakat=\ \ ;:,!?           " Long lines break chars
set nostartofline               " Cursor in same column for few commands
set whichwrap+=h,l,<,>,[,],~    " Move to following line on certain keys
set splitbelow splitright       " Splits open bottom right
"set switchbuf=useopen,usetab    " Jump to the first open window in any tab
"set switchbuf+=vsplit           " Switch buffer behavior to vsplit
set backspace=indent,eol,start  " Intuitive backspacing in insert mode
set diffopt=filler,iwhite       " Diff mode: show fillers, ignore white
set showfulltag                 " Show tag and tidy search in completion
set complete-=i
set completeopt=menuone         " Show menu even for one item
set completeopt+=noselect       " Do not select a match in the menu
if has('patch-7.4.775')
  set completeopt+=noinsert
endif

if exists('+inccommand')
  set inccommand=nosplit
endif

" }}}
" Editor UI Appearance {{{
" --------------------
set shortmess=aoOTI     " Shorten messages and don't show intro
set nonumber              " Show line numbers
set list                " Show hidden characters

set tabpagemax=10       " Maximum number of tab pages
set winwidth=80         " Minimum width for current window
set winminwidth=8       " Minimum width for inactive windows
set winheight=13        " Minimum height for active window
set winminheight=3      " Minimum height for inactive windows
set pumheight=20        " Pop-up menu's line height
set helpheight=12       " Minimum help window height
set previewheight=8     " Completion preview height

set laststatus=2        " Always show a status line
set colorcolumn=80      " Highlight the 80th character limit

" Do not display completion messages
" Patch: https://groups.google.com/forum/#!topic/vim_dev/WeBBjkXE8H8
if has('patch-7.4.314')
  set shortmess+=c
endif

" Do not display message when editing files
if has('patch-7.4.1570')
  set shortmess+=F
endif

" For snippet_complete marker
if has('conceal') && v:version >= 703
  set conceallevel=2 concealcursor=niv
endif

" }}}
" Folds {{{
" -----
set foldenable
set foldmethod=syntax
let javaScript_fold=1         " JavaScript
"  set foldmethod=indent
set foldlevelstart=1
set foldtext=FoldText()

" Improved Vim fold-text
" See: http://www.gregsexton.org/2011/03/improving-the-text-displayed-in-a-fold/
function! FoldText()
  " Get first non-blank line
  let fs = v:foldstart
  while getline(fs) =~? '^\s*$' | let fs = nextnonblank(fs + 1)
  endwhile
  if fs > v:foldend
    let line = getline(v:foldstart)
  else
    let line = substitute(getline(fs), '\t', repeat(' ', &tabstop), 'g')
  endif

  let w = winwidth(0) - &foldcolumn - (&number ? 8 : 0)
  let foldSize = 1 + v:foldend - v:foldstart
  let foldSizeStr = ' ' . foldSize . ' lines '
  let foldLevelStr = repeat('+--', v:foldlevel)
  let lineCount = line('$')
  let foldPercentage = printf('[%.1f', (foldSize*1.0)/lineCount*100) . '%] '
  let expansionString = repeat('.', w - strwidth(foldSizeStr.line.foldLevelStr.foldPercentage))
  return line . expansionString . foldSizeStr . foldPercentage . foldLevelStr
endfunction

" }}}

" vim: set ts=2 sw=2 tw=80 noet :
