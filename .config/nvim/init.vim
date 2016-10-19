if &nu == 1
  set rnu
elseif &rnu == 1
  set nornu
else
  set nu
endif
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                        get rid of *.*~ file                         "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set backupdir=~/vimtmp,.
set directory=~/vimtmp,.
set nobackup

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                           Neo terminal emulator                            "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
:tnoremap <Esc>1 <C-\><C-n>
:tnoremap <C-w>h <C-\><C-n><C-w>h
:tnoremap <C-w>j <C-\><C-n><C-w>j
:tnoremap <C-w>k <C-\><C-n><C-w>k
:tnoremap <C-w>l <C-\><C-n><C-w>l
":nnoremap <C-w><C-h> <C-w>h
":nnoremap <C-w><C-j> <C-w>j
":nnoremap <C-w><C-k> <C-w>k
":nnoremap <C-w><C-l> <C-w>l
:nnoremap <silent> <leader>t :terminal<CR>
:nnoremap <leader><leader>t :te


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                         jira configuration                          "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:jira_url = 'http://115.28.138.35:8080'
let g:jira_username = 'zhangyuxiao'
let g:jira_password = 'namy0000'

" Customize
let g:unite_source_issue_jira_priority_table = {
      \ 10000: '◡', 1: '⚡', 2: 'ᛏ', 3: '●', 4: '○', 5: '▽' }

let g:unite_source_issue_jira_status_table = {
      \ 1: 'plan', 3: 'develop', 4: 'reopened', 5: 'resolved', 6: 'closed',
      \ 10000: 'feedback', 10001: 'staged', 10002: 'waiting',
      \ 10003: 'deployed', 10004: 'pending', 10008: 'review' }

let g:unite_source_issue_jira_type_table = {
      \ 1: 'bug', 2: 'feature', 3: 'task', 4: 'change', 5: 'sub-task',
      \ 6: 'epic', 7: 'story', 8: 'system', 9: 'sub-bug' }


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                             quicker ESC                             "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set ttimeoutlen=30
augroup FastEscape
  autocmd!
  au InsertEnter * set timeoutlen=0
  au InsertLeave * set timeoutlen=1000
augroup END

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                 lopen lclose                               "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nmap <script> <silent> <C-l> :call ToggleLocationList()<CR>
nmap <script> <silent> <C-k> :call ToggleQuickfixList()<CR>
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                 neomake                                    "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd! BufWritePost * Neomake
nmap <leader><leader>w :Neomake<cr>:lopen<cr>:w!<cr>
" let g:neomake_javascript_eslint_exe = '/usr/local/bin/eslint'
" let g:neomake_javascript_jscs_exe = '/usr/local/bin/jscs'
let g:neomake_javascript_eslint_maker = {
    \ 'args': ['--no-color', '--format','compact'],
    \ 'errorformat': '%f: line %l\, col %c\, %m'
    \ }
let g:neomake_javascript_jscs_maker = {
    \ 'args': ['--no-colors', '--reporter', 'inline','--preset=aribnb'],
    \ 'errorformat': '%E%f: line %l\, col %c\, %m',
    \ }
let g:neomake_javascript_enabled_makers = ['eslint','jscs']

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                 syntastic                                  "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" nmap <leader><leader>w :w!<cr>:SyntasticCheck<cr>

" set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*
" set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P
" let g:syntastic_auto_loc_list = 0
" let g:syntastic_always_populate_loc_list = 1
" let g:syntastic_auto_loc_list = 1
" let g:syntastic_check_on_open = 1
" let g:syntastic_check_on_wq = 0
" "let g:syntastic_javascript_checkers = ['jshint']
" let g:syntastic_javascript_checkers = ['eslint','jscs']
" let g:syntastic_javascript_eslint_exec = '/usr/local/bin/eslint'
" let g:syntastic_javascript_jscs_exec = '/usr/local/bin/jscs'
" let g:syntastic_javascript_jscs_args = '--preset=airbnb'
" " let g:syntastic_javascript_jscs_args = '--preset=google'
" let g:syntastic_loc_list_height = 5
" let g:syntastic_mode_map = {
      " \ "mode": "passive",
      " \ "active_filetypes": ["json"],
      " \ "passive_filetypes": []}

let $NVIM_TUI_ENABLE_TRUE_COLOR=1
au BufNewFile,BufRead *.handlebars set filetype=html
let g:ycm_server_python_interpreter = '/usr/bin/python'

set ttimeoutlen=10

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                               Plugins                               "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

call plug#begin('~/.vim/plugged')
Plug 'Chiel92/vim-autoformat'
Plug 'milkypostman/vim-togglelist'
Plug 'mattn/webapi-vim'
Plug 'benjie/neomake-local-eslint.vim'
Plug 'mileszs/ack.vim'
Plug 'neomake/neomake'
Plug 'moll/vim-node'
Plug 'mhinz/vim-startify' "welcoming view
Plug 'scrooloose/nerdtree' ,{ 'on': 'NERDTreeToggle' }
Plug 'heavenshell/vim-jsdoc', { 'for': 'javascript' }
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'mattn/ctrlp-register'
Plug 'terryma/vim-multiple-cursors'
Plug 'bling/vim-airline'
Plug 'powerline/fonts'
Plug 'easymotion/vim-easymotion'
Plug 'jistr/vim-nerdtree-tabs',{ 'on': 'NERDTreeToggle' }
Plug 'flazz/vim-colorschemes'
Plug 'mbbill/undotree' , {'on':'UndotreeToggle'}
Plug 'nathanaelkane/vim-indent-guides'
Plug 'vim-scripts/restore_view.vim' "restor cursor position and fold state
Plug 'mhinz/vim-signify' "show lines modified
Plug 'tpope/vim-fugitive'
Plug 'scrooloose/nerdcommenter'
Plug 'godlygeek/tabular'
Plug 'luochen1990/rainbow'
Plug 'majutsushi/tagbar'
Plug 'honza/vim-snippets'
Plug 'elzr/vim-json', { 'for': 'json' }
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
Plug 'hail2u/vim-css3-syntax', {'for': 'css'}
Plug 'gorodinskiy/vim-coloresque', {'for': 'css'}
Plug 'tpope/vim-markdown', { 'for': ['markdown', 'md'] }
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-repeat'
Plug 'maksimr/vim-jsbeautify',
Plug 'othree/html5.vim' , {'for': 'html'}
Plug 'Olical/vim-enmasse'
Plug 'tell-k/vim-browsereload-mac', {'for': ['javascript','css','html']}
Plug 'vim-airline/vim-airline-themes'
Plug 'SirVer/ultisnips'
Plug 't9md/vim-choosewin' "选择窗口
Plug 'vim-utils/vim-man'
Plug 'Valloric/YouCompleteMe'
Plug 'vim-scripts/mru.vim'
Plug 'junegunn/seoul256.vim'
Plug '/usr/local/opt/fzf' | Plug 'junegunn/fzf.vim'
Plug 'morhetz/gruvbox'
Plug 'justinmk/vim-sneak'
Plug 'tomtom/quickfixsigns_vim'
" Plug 'scrooloose/syntastic',{ 'for':'javascript' }
" Plug 'git-time-metric/gtm-vim-plugin'
" Plug 'othree/yajs.vim'
" Plug 'othree/es.next.syntax.vim'
" Plug 'sheerun/vim-polyglot',{'for':'javascript'}
"Plug 'thinca/vim-prettyprint'
"Plug 'thinca/vim-ref'
"for jira
" Plug 'carlitux/deoplete-ternjs'
" Plug 'tyru/open-browser.vim'
" Plug 'digitaltoad/vim-pug' ,{'for': 'jade'}
"Plug 'rafi/vim-unite-issue'
"Plug 'Shougo/unite.vim'
" Plug 'vimoutliner/vimoutliner'
" Plug 'MattesGroeger/vim-bookmarks' "mm mi mn mp ma mc mx :BookMardSave
" Plug 'frankier/neovim-colors-solarized-truecolor-only'
"Plug 'MarcWeber/vim-addon-mw-utils'
"Plug 'tomtom/tlib_vim'
" Plug 'junegunn/gv.vim'
"Plug 'ervandew/supertab'
"Plug 'altercation/vim-colors-solarized'
"Plug 'itchyny/dictionary.vim'
" Plug 'tpope/vim-endwise'
" Plug 'groenewege/vim-less', { 'for': 'less' }
" Plug 'vim-scripts/sessionman.vim'
" Plug 'jceb/vim-orgmode'
" Plug 'xolox/vim-notes'
" Plug 'xolox/vim-misc'
" Plug 'tacahiroy/ctrlp-funky'
"Plug 'matchit.zip'
" Plug 'othree/javascript-libraries-syntax.vim'
" Plug 'bling/vim-bufferline'
"Plug 'gcmt/wildfire.vim' "press enter to select words
"Plug 'reedes/vim-litecorrect'
" Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
"Plug 'tpope/vim-commentary'
" Plug 'briancollins/vim-jst', { 'for': 'javascript' }
"Plug 'mattn/emmet-vim'
" Plug 'KabbAmine/vCoolor.vim'
"Plug 'benmills/vimux'
" Plug 'thinca/vim-quickrun'
"Plug 'Shougo/vimproc.vim'
" Plug 'vim-scripts/a.vim'
"Plug 'rizzatti/dash.vim'
"Plug 'junegunn/goyo.vim'
" Plug 'ternjs/tern_for_vim'
"Plug 'benmills/vimux'
"Plug 'wookiehangover/jshint.vim', {'for':'javascript'}
" Plug '1995eaton/vim-better-javascript-completion',{'for': ['javascript','css','html','json']}
call plug#end()
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                  vim-better-javascript-completion                   "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" let g:vimjs#casesensistive = 1
" Enabled by default. flip the value to make completion matches case insensitive

" let g:vimjs#smartcomplete = 1
" Disabled by default. Enabling this will let vim complete matches at any location
" e.g. typing 'ocument' will suggest 'document' if enabled.

" let g:vimjs#chromeapis = 1
" Disabled by default. Toggling this will enable completion for a number of Chrome's JavaScript extension APIs
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                              deoplete                               "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Use deoplete.
" let g:deoplete#enable_at_startup = 1
" let g:tern_request_timeout = 1
" let g:tern_show_signature_in_pum = '0'  " This do disable full signature type on autocomplete
" Use tern_for_vim.
" let g:tern#command = ["tern"]
" let g:tern#arguments = ["--persistent"]
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                              vim-notes                              "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
:let g:notes_directories = ['/Users/Rashawn/Library/Mobile\ Documents/com~apple~CloudDocs/']
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                              choosewin                              "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:choosewin_overlay_enable = 1
nmap  <C-w><C-f> <Plug>(choosewin)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                              Quickrun                               "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:quickrun_config = {}
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                             Autoformat                              "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"au BufWrite * :Autoformat
let g:autoformat_verbosemode=0 " for debug
map <leader><leader>0 :Autoformat<cr>
let g:formatters_javascript = ['jscs']
let g:formatters_json = ['js-beautify']
let g:formatters_html = ['htmlbeautify']
let g:formatters_css = ['css-beautify']
let g:formatters_markdown = ['remark']
let g:formatdef_jscs = '"jscs -x -c /Users/Rashawn/.jscsrc"'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                            for markdown                             "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
" 代码块高亮
let g:markdown_fenced_languages = ['html', 'python', 'bash=sh','javascript=js','json']
" 不根据语法隐藏
let g:markdown_syntax_conceal = 1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                              for Unite                              "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"if executable('ag')
" Use ag (the silver searcher)
" https://github.com/ggreer/the_silver_searcher
let g:unite_source_grep_command = 'ag'
let g:unite_source_grep_default_opts =
      \ '-i --vimgrep --hidden --ignore ' .
      \ '''.hg'' --ignore ''.svn'' --ignore ''.git'' --ignore ''.bzr'''
let g:unite_source_grep_recursive_opt = ''
"elseif executable('ack-grep')
"" Use ack
"" http://beyondgrep.com/
"let g:unite_source_grep_command = 'ack-grep'
"let g:unite_source_grep_default_opts =
"\ '-i --no-heading --no-color -k -H'
"let g:unite_source_grep_recursive_opt = ''
"endif

"let g:unite_source_rec_async_command =
"\ ['ack', '-f', '--nofilter']

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                             custom map                              "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"noremap <silent> <leader>ct :Unite file/async file_rec/neovim buffer neomru/file file/new -direction=dynamicbottom -start-insert<CR>
nmap <NUL> :Unite line -direction=dynamicbottom -start-insert<CR>
"nmap <leader><leader>t :Unite jump -direction=dynamicbottom -start-insert -quick-match<CR>
nmap <leader><leader>x :Unite register -direction=dynamicbottom -start-insert<CR><CR>
nmap <leader><space> :Unite grep -direction=dynamicbottom<CR><CR>
nmap <leader><leader><space> :Unite grep -direction=dynamicbottom<CR>

"nmap <leader><leader><leader> :source ~/.config/nvim/init.vim<cr>
nmap <silent> <leader>l <Plug>(jsdoc)
nmap <leader>,, :set wrap<CR>
let leader='\'
imap <C-j> <cr>
nnoremap <C-w>= <C-w>+
nnoremap <C-w><C-=> <C-w>+
nnoremap <C-w><C--> <C-w>-
"imap jk <Esc>
"imap kj <Esc>
nnoremap <C-d>  <C-d>zz
nnoremap <C-u>  <C-u>zz
nnoremap j jzz
nnoremap k kzz
nnoremap <Down> jzz
nnoremap <Up> kzz
map <leader><leader>q :bd!<CR>
map <leader><leader><leader>q :tabc<CR>
imap <C-f> <Right>
imap <C-b> <Left>
imap <C-e> <Esc><S-A>
imap <C-a> <Esc><S-I>
" nmap <leader>jst :JSHintToggle<CR>
nnoremap <leader>bn :bn<CR>
nnoremap <leader>bp :bp<CR>
map <leader><leader>- mzgg=G`z
nmap <leader><leader>r :w!<CR>:ChromeReload<CR>
nnoremap <F5> :UndotreeToggle<cr>
"nnoremap <C-e> :NERDTreeToggle<CR>
map <leader><F10> :QuickRun<CR>
nmap j jzz
nmap k kzz

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                     javascript-libraries-syntax                     "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:used_javascript_libs = 'chai,underscore'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                           vim-javascript                            "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:javascript_plugin_jsdoc = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                   jsdoc                                    "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:jsdoc_enable_es6 = 1
let g:jsdoc_allow_input_prompt = 1
let g:jsdoc_input_description = 1
let g:jsdoc_underscore_private = 1
let g:jsdoc_access_descriptions = 1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                nerdcommenter too many unsless issue                 "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let NERDSpaceDelims = 1
nmap <leader>rccccccccc :NERDComComment
nmap <leader>rcccccccccccn :NERDComNestedComment
nmap <leader>rrrrrrrrc<space> :NERDComToggleComment

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                 ack                                 "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

nnoremap <leader>ac :Ack
let g:ackprg = 'ag --vimgrep'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                tmux                                 "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" for tmux to automatically set paste and nopaste mode at the time pasting (as
" happens in VIM UI)
function! WrapForTmux(s)
  if !exists('$TMUX')
    return a:s
  endif

  let tmux_start = "\<Esc>Ptmux;"
  let tmux_end = "\<Esc>\\"

  return tmux_start . substitute(a:s, "\<Esc>", "\<Esc>\<Esc>", 'g') . tmux_end
endfunction

let &t_SI .= WrapForTmux("\<Esc>[?2004h")
let &t_EI .= WrapForTmux("\<Esc>[?2004l")

function! XTermPasteBegin()
  set pastetoggle=<Esc>[201~
  set paste
  return ""
endfunction

inoremap <special> <expr> <Esc>[200~ XTermPasteBegin()


if exists('$ITERM_PROFILE')
  if exists('$TMUX')
    let &t_SI = "\<Esc>[3 q"
    let &t_EI = "\<Esc>[0 q"
  else
    let &t_SI = "\<Esc>]50;CursorShape=1\x7"
    let &t_EI = "\<Esc>]50;CursorShape=0\x7"
  endif
end


"if exists('$TMUX')
" set term=screen-256color
"endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                             vim-airline                             "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"gtm airline
let g:gtm_plugin_status_enabled = 1
function! AirlineInit()
  if exists('*GTMStatusline')
    call airline#parts#define_function('gtmstatus', 'GTMStatusline')
    let g:airline_section_b = airline#section#create([g:airline_section_b, ' ', '[', 'gtmstatus', ']'])
  endif
endfunction
autocmd User AirlineAfterInit call AirlineInit()

let g:airline#extensions#tabline#buffer_nr_show = 0
" 关闭状态显示空白符号计数,这个对我用处不大"
let g:airline#extensions#whitespace#enabled = 0
let g:airline#extensions#whitespace#symbol = '!'
let g:airline#extensions#tabline#buffer_idx_mode = 1
nmap <leader>1 <Plug>AirlineSelectTab1
nmap <leader>2 <Plug>AirlineSelectTab2
nmap <leader>3 <Plug>AirlineSelectTab3
nmap <leader>4 <Plug>AirlineSelectTab4
nmap <leader>5 <Plug>AirlineSelectTab5
nmap <leader>6 <Plug>AirlineSelectTab6
nmap <leader>7 <Plug>AirlineSelectTab7
nmap <leader>8 <Plug>AirlineSelectTab8


:tnoremap <leader><leader> <C-\><C-n>
:tnoremap <leader>1 <C-\><C-n><Plug>AirlineSelectTab1
:tnoremap <leader>2 <C-\><C-n><Plug>AirlineSelectTab2
:tnoremap <leader>3 <C-\><C-n><Plug>AirlineSelectTab3
:tnoremap <leader>4 <C-\><C-n><Plug>AirlineSelectTab4
:tnoremap <leader>5 <C-\><C-n><Plug>AirlineSelectTab5
:tnoremap <leader>6 <C-\><C-n><Plug>AirlineSelectTab6
:tnoremap <leader>7 <C-\><C-n><Plug>AirlineSelectTab7
:tnoremap <leader>8 <C-\><C-n><Plug>AirlineSelectTab8

" enable/disable enhanced tabline. (c)
let g:airline#extensions#tabline#enabled = 1

" enable/disable displaying open splits per tab (only when tabs are opened). >
let g:airline#extensions#tabline#show_splits = 0

" switch position of buffers and tabs on splited tabline (c)
let g:airline#extensions#tabline#switch_buffers_and_tabs = 0

" enable/disable displaying buffers with a single tab. (c)
let g:airline#extensions#tabline#show_buffers = 1

" enable/disable display preview window buffer in the tabline. >
let g:airline#extensions#tabline#exclude_preview = 0

" configure how numbers are displayed in tab mode. >
"let g:airline#extensions#tabline#tab_nr_type = 0 " # of splits (default)
let g:airline#extensions#tabline#tab_nr_type = 1 " tab number
"let g:airline#extensions#tabline#tab_nr_type = 2 " splits and tab number

" enable/disable displaying tab number in tabs mode. >
let g:airline#extensions#tabline#show_tab_nr = 1

" enable/disable displaying tab type (far right) >
let g:airline#extensions#tabline#show_tab_type = 1

" rename label for buffers (default: 'buffers') (c)
let g:airline#extensions#tabline#buffers_label = 'buffers'

" rename label for tabs (default: 'tabs') (c)
let g:airline#extensions#tabline#tabs_label = 'tabs'

" configure separators for the tabline only. >
"let g:airline#extensions#tabline#left_sep = '⚡'
"let g:airline#extensions#tabline#left_alt_sep = '⚡'
"let g:airline#extensions#tabline#right_sep = '<<'
"let g:airline#extensions#tabline#right_alt_sep = '<<'

" configure whether close button should be shown: >
let g:airline#extensions#tabline#show_close_button = 0

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                 set                                 "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"highlight current line and column
set cursorcolumn
"fold depend on syntax
set foldmethod=syntax
set foldlevelstart=20
set foldnestmax=9      "deepest fold is 10 levels
"set foldlevel=1         "this is just what i use
"set cmdheight=2
set wrap
syntax on

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                 ycm                                 "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ycm_allow_changing_updatetime = 1
let g:ycm_min_num_of_chars_for_completion = 1
let g:ycm_use_ultisnips_completer = 1
"let g:ycm_seed_identifiers_with_syntax = 1
"compatible with UltiSnips (using supertab)
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"
autocmd BufLeave,FocusLost * silent! wall
"autocmd VimEnter  * silent! :split | term

let g:ycm_semantic_triggers =  {
      \   'c' : ['->', '.'],
      \   'objc' : ['->', '.', 're!\[[_a-zA-Z]+\w*\s', 're!^\s*[^\W\d]\w*\s',
      \             're!\[.*\]\s'],
      \   'ocaml' : ['.', '#'],
      \   'cpp,objcpp' : ['->', '.', '::'],
      \   'perl' : ['->'],
      \   'php' : ['->', '::'],
      \   'cs,java,javascript,typescript,d,python,perl6,scala,vb,elixir,go' : ['.'],
      \   'ruby' : ['.', '::'],
      \   'lua' : ['.', ':'],
      \   'erlang' : [':'],
      \ }

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Turn persistent undo on
"    means that you can undo even when you close a buffer/VIM
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
try
  set undodir=~/.vim_runtime/temp_dirs/undodir
  set undofile
catch
endtry

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                         command line alias                          "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Smart mappings on the command line
cno jira Unite issue:jira<cr>
cno hhh ~/
cno xihemap ~/workspace/XiheMap
cno cesium ~/workspace/projects/cesium
cno ddd ~/Desktop/
cno kkk ./
"cno $c e <C-\>eCurrentFileDir("e")<cr>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => vim-javascropt
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let javascript_enable_domhtmlcss = 1
let g:javascript_conceal_function       = "ƒ"
let g:javascript_conceal_null           = "ø"
let g:javascript_conceal_this           = "@"
let g:javascript_conceal_return         = "⇚"
let g:javascript_conceal_undefined      = "¿"
let g:javascript_conceal_NaN            = "ℕ"
let g:javascript_conceal_prototype      = "¶"
let g:javascript_conceal_static         = "•"
let g:javascript_conceal_super          = "Ω"
let g:javascript_conceal_arrow_function = "⇒"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Sets how many lines of history VIM has to remember
set history=1000
" Enable filetype plugins
filetype plugin on
filetype indent on

" Set to auto read when a file is changed from the outside
set autoread
"set relativenumber

"Always show current position
set ruler

" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file
let mapleader = '\'
let g:mapleader = '\'

" Fast saving
nmap <leader>w :w!<cr>

" :W sudo saves the file
" (useful for handling the permission-denied error)
" command W w !sudo tee % > /dev/null

"command line autocompletion
set wildmode=longest:full,full

" No annoying sound on errors
set noerrorbells
set novisualbell
set t_vb=
set tm=500

" <Space> to / (search) and Ctrl-<Space> to ? (backwards search)
map <space> /

" Close the current buffer
map <leader>bd :bdelete<cr>:tabclose<cr>gT

" Close all the buffers
map <leader>bac :bufdo bd<cr>

" Useful mappings for managing tabs
"map <leader>bn :tabnew<cr>
"map <leader>bo :tabonly<cr>
"map <leader>bm :tabmove
"map <leader>b<leader> :tabnext


" Let 'tl' toggle between this and the last accessed tab
let g:lasttab = 1
" nmap <Leader>tl :exe "tabn ".g:lasttab<CR>
au TabLeave * let g:lasttab = tabpagenr()

" Opens a new tab with the current buffer's path
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Command mode related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Smart mappings on the command line
cno hhh  ~/
cno ddd  ~/Desktop/
cno ccc ./

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Parenthesis/bracket
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
vnoremap $3 <esc>`>a)<esc>`<i(<esc>
"vnoremap $2 <esc>`>a]<esc>`<i[<esc>
"vnoremap $3 <esc>`>a}<esc>`<i{<esc>
"vnoremap $$ <esc>`>a"<esc>`<i"<esc>
"vnoremap $q <esc>`>a'<esc>`<i'<esc>
"vnoremap $e <esc>`>a"<esc>`<i"<esc>

" Map auto complete of (, ", ', [
inoremap $3 ()<esc>i
"inoremap $2 []<esc>i
"inoremap $3 {}<esc>i
"inoremap $4 {}<esc>i<C-j>
"inoremap $q ''<esc>i
"inoremap $e ""<esc>i
"inoremap $t <><esc>i


"""""""""""""""""""
"easyMotion
"""""""""""""""""""""
map <Leader>L <Plug>(easymotion-bd-jk)
"nmap <Leader>LL <Plug>(easymotion-overwin-line)
let g:EasyMotion_do_mapping = 0 " Disable default mappings
" Jump to anywhere you want with minimal keystrokes, with just one key binding.
" `s{char}{label}`
nmap <C-s> <Plug>(easymotion-overwin-f)
vmap <C-s> <Plug>(easymotion-overwin-f)
" or
" `s{char}{char}{label}`
" Need one more keystroke, but on average, it may be more comfortable.
" nmap e <Plug>(easymotion-overwin-f2)
" Turn on case insensitive feature
let g:EasyMotion_smartcase = 1
" JK motions: Line motions
"map <Leader>j <Plug>(easymotion-j)
"map <Leader>k <Plug>(easymotion-k)

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                              UltiSnips                              "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:UltiSnipsSnippetsDir='~/.config/nvim/UltiSnips'
let g:UltiSnipsUsePythonVersion = 3
"let  g:UltiSnipsListSnippets ='<C-tab>'
let g:UltiSnipsSnippetDirectories=["UltiSnips"]
let g:snips_author = 'yqrashawn <namy.19@gmail.com>'
nmap <leader><leader>u :UltiSnipsEdit<CR>

"change this variables
let g:returnApp = "iTerm"
let g:returnAppFlag = 0
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                              Startify                               "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:startify_bookmarks = ['~/workspace']

"""""""""""""""""""""""""""""""""""""""""""""""spf13
"""""""""""""""""""""""""""""""""""""""""""""""spf13
"""""""""""""""""""""""""""""""""""""""""""""""spf13

" Arrow Key Fix {
" https://github.com/spf13/spf13-vim/issues/780
if &term[:4] == "xterm" || &term[:5] == 'screen' || &term[:3] == 'rxvt'
  inoremap <silent> <C-[>OC <RIGHT>
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                             colorscheme                             "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set background=dark " Assume a dark background
" colorscheme solarized
" colorscheme seoul256
" let g:seoul256_light_background = 252
colorscheme gruvbox
let g:gruvbox_contrast_light = 'hard'
let g:gruvbox_contrast_dark = 'hard'
" let g:gruvbox_contrast_dark = 'soft'
" let g:gruvbox_contrast_dark = 'medium'
" let g:gruvbox_hls_cursor = 'white'

" Allow to trigger background
function! ToggleBG()
  let s:tbg = &background
  " Inversion
  if s:tbg == "dark"
    set background=light
  else
    set background=dark
  endif
endfunction
noremap <leader>bg :call ToggleBG()<CR>

" if !has('gui')
"set term=$TERM          " Make arrow and other keys work
" endif

filetype plugin indent on   " Automatically detect file types.
set mouse=a                 " Automatically enable mouse usage
set mousehide               " Hide the mouse cursor while typing
scriptencoding utf-8

if has('clipboard')
  if has('unnamedplus')  " When possible use + register for copy-paste
    set clipboard=unnamed,unnamedplus
  else         " On mac and Windows, use * register for copy-paste
    set clipboard=unnamed
  endif
endif
"打开新buffer时 目录自动移动到新buffer文件的位置
autocmd BufEnter * if bufname("") !~ "^\[A-Za-z0-9\]*://" | lcd %:p:h | endif
set shortmess+=filmnrxoOtT          " Abbrev. of messages (avoids 'hit enter')
set viewoptions=folds,options,cursor,unix,slash " Better Unix / Windows compatibility
set virtualedit=onemore             " Allow for cursor beyond last character
" set spell                           " Spell checking on
set hidden                          " Allow buffer switching without saving
set iskeyword-=.                    " '.' is an end of word designator
set iskeyword-=#                    " '#' is an end of word designator
set iskeyword-=-                    " '-' is an end of word designator

" Instead of reverting the cursor to the last position in the buffer, we
" set it to the first line when editing a git commit message
au FileType gitcommit au! BufEnter COMMIT_EDITMSG call setpos('.', [0, 1, 1, 0])

" http://vim.wikia.com/wiki/Restore_cursor_to_file_position_in_previous_editing_session
" Restore cursor to file position in previous editing session
function! ResCur()
  if line("'\"") <= line("$")
    silent! normal! g`"
    return 1
  endif
endfunction

augroup resCur
  autocmd!
  autocmd BufWinEnter * call ResCur()
augroup END

" Setting up the directories {
if has('persistent_undo')
  set undofile                " So is persistent undo ...
  set undolevels=1000         " Maximum number of changes that can be undone
  set undoreload=10000        " Maximum number lines to save for undo on a buffer reload
endif

" To disable views add the following to your .vimrc.before.local file:
" Add exclusions to mkview and loadview
" eg: *.*, svn-commit.tmp
let g:skipview_files = [
      \ '\[example pattern\]'
      \ ]

" Vim UI {

"if filereadable(expand("~/.vim/plugged/vim-colors-solarized/colors/solarized.vim"))
"let g:solarized_termcolors=256
"let g:solarized_termtrans=1
"let g:solarized_contrast="normal"
"let g:solarized_visibility="normal"
"color solarized              Load a colorscheme
"endif

set tabpagemax=15               " Only show 15 tabs
set showmode                    " Display the current mode

set cursorline                  " Highlight current line

highlight clear SignColumn      " SignColumn should match background
highlight clear LineNr          " Current line number row will have same background color in relative mode
"highlight clear CursorLineNr    " Remove highlight color from current line number

if has('cmdline_info')
  set ruler                   " Show the ruler
  set rulerformat=%30(%=\:b%n%y%m%r%w\ %l,%c%V\ %P%) " A ruler on steroids
  set showcmd                 " Show partial commands in status line and
  " Selected characters/lines in visual mode
endif

if has('statusline')
  set laststatus=2

  " Broken down into easily includeable segments
  set statusline=%<%f\                     " Filename
  set statusline+=%w%h%m%r                 " Options
  if !exists('g:override_spf13_bundles')
    " set statusline+=%{fugitive#statusline()} " Git Hotness
    set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P
  endif
  set statusline+=\ [%{&ff}/%Y]            " Filetype
  set statusline+=\ [%{getcwd()}]          " Current dir
  set statusline+=%=%-14.(%l,%c%V%)\ %p%%  " Right aligned file nav info
endif

set backspace=indent,eol,start  " Backspace for dummies
set linespace=0                 " No extra spaces between rows
" set number                      " Line numbers on
set showmatch                   " Show matching brackets/parenthesis
set incsearch                   " Find as you type search
set hlsearch                    " Highlight search terms
set winminheight=0              " Windows can be 0 line high
set ignorecase                  " Case insensitive search
set smartcase                   " Case sensitive when uc present
set wildmenu                    " Show list instead of just completing
set wildmode=list:longest,full  " Command <Tab> completion, list matches, then longest common part, then all.
set whichwrap=b,s,h,l,<,>,[,]   " Backspace and cursor keys wrap too
set scrolljump=6                " Lines to scroll when cursor leaves screen
set scrolloff=3                 " Minimum lines to keep above and below cursor
set foldenable                  " Auto fold code
set list
set listchars=tab:›\ ,trail:•,extends:#,nbsp:. " Highlight problematic whitespace

" Formatting {

" set nowrap                      " Do not wrap long lines
set autoindent                  " Indent at the same level of the previous line
set shiftwidth=2                " Use indents of 4 spaces
set expandtab                   " Tabs are spaces, not tabs
set tabstop=4                   " An indentation every four columns
set softtabstop=4               " Let backspace delete indent
set nojoinspaces                " Prevents inserting two spaces after punctuation on a join (J)
set splitright                  " Puts new vsplit windows to the right of the current
set splitbelow                  " Puts new split windows to the bottom of the current
"set matchpairs+=<:>             " Match, to be used with %
" set pastetoggle=<F12>           " pastetoggle (sane indentation on pastes)
set comments=sl:/*,mb:*,elx:*/  " auto format comment blocks
" Remove trailing whitespaces and ^M chars
" To disable the stripping of whitespace, add the following to your
" .vimrc.before.local file:
"   let g:spf13_keep_trailing_whitespace = 1
" autocmd FileType c,cpp,java,go,php,javascript,puppet,python,rust,twig,xml,yml,perl,sql autocmd BufWritePre <buffer> if !exists('g:spf13_keep_trailing_whitespace') | call StripTrailingWhitespace() | endif
"autocmd FileType go autocmd BufWritePre <buffer> Fmt
autocmd BufNewFile,BufRead *.html.twig set filetype=html.twig
autocmd FileType haskell,puppet,ruby,yml setlocal expandtab shiftwidth=2 softtabstop=2
" preceding line best in a plugin but here for now.

autocmd BufNewFile,BufRead *.coffee set filetype=coffee

" Workaround vim-commentary for Haskell
autocmd FileType haskell setlocal commentstring=--\ %s
" Workaround broken colour highlighting in Haskell
autocmd FileType haskell,rust setlocal nospell


"""""""""""""""""""
"for edit vimrc
"""""""""""""""""""""
let s:spf13_edit_config_mapping = '<leader>ec'
let s:spf13_apply_config_mapping = '<leader>sc'
nmap <leader>sc :source ~/.config/nvim/init.vim<CR>
execute "noremap " . s:spf13_edit_config_mapping " :call <SID>EditSpf13Config()<CR>"
"execute "noremap " . s:spf13_apply_config_mapping  " :source ~/.config/nvim/init.vim<CR>"

function! s:ExpandFilenameAndExecute(command, file)
  execute a:command . " " . expand(a:file, ":p")
endfunction

function! s:EditSpf13Config()
  call <SID>ExpandFilenameAndExecute("tabedit", "~/.config/nvim/init.vim")
  "call <SID>ExpandFilenameAndExecute("vsplit", "~/.vimrc.before")
  "call <SID>ExpandFilenameAndExecute("vsplit", "~/.vimrc.bundles")

  "execute bufwinnr(".vimrc") . "wincmd w"
  "call <SID>ExpandFilenameAndExecute("split", "~/.vimrc.local")
  "wincmd l
  "call <SID>ExpandFilenameAndExecute("vsplit", "~/.vimrc.before.local")
  "wincmd l
  "call <SID>ExpandFilenameAndExecute("vsplit", "~/.vimrc.bundles.local")

  "        if <SID>IsSpf14Fork()
  "            execute bufwinnr(".vimrc") . "wincmd w"
  "            call <SID>ExpandFilenameAndExecute("split", "~/.vimrc.fork")
  "            wincmd l
  "            call <SID>ExpandFilenameAndExecute("split", "~/.vimrc.before.fork")
  "            wincmd l
  "            call <SID>ExpandFilenameAndExecute("split", "~/.vimrc.bundles.fork")
  "        endif

  "        execute bufwinnr(".vimrc.local") . "wincmd w"
endfunction


" indent_guides {
"if isdirectory(expand("~/.vim/plugged/vim-indent-guides/"))
let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 1
let g:indent_guides_enable_on_vim_startup = 1
"endif

function! WrapRelativeMotion(key, ...)
  let vis_sel=""
  if a:0
    let vis_sel="gv"
  endif
  if &wrap
    execute "normal!" vis_sel . "g" . a:key
  else
    execute "normal!" vis_sel . a:key
  endif
endfunction

"" Map g* keys in Normal, Operator-pending, and Visual+select
"noremap $ :call WrapRelativeMotion("$")<CR>
"noremap <End> :call WrapRelativeMotion("$")<CR>
"noremap 0 :call WrapRelativeMotion("0")<CR>
"noremap <Home> :call WrapRelativeMotion("0")<CR>
"noremap ^ :call WrapRelativeMotion("^")<CR>
"" Overwrite the operator pending $/<End> mappings from above
"" to force inclusive motion with :execute normal!
"onoremap $ v:call WrapRelativeMotion("$")<CR>
"onoremap <End> v:call WrapRelativeMotion("$")<CR>
"" Overwrite the Visual+select mode mappings from above
"" to ensure the correct vis_sel flag is passed to function
"vnoremap $ :<C-U>call WrapRelativeMotion("$", 1)<CR>
"vnoremap <End> :<C-U>call WrapRelativeMotion("$", 1)<CR>
"vnoremap 0 :<C-U>call WrapRelativeMotion("0", 1)<CR>
"vnoremap <Home> :<C-U>call WrapRelativeMotion("0", 1)<CR>
"vnoremap ^ :<C-U>call WrapRelativeMotion("^", 1)<CR>

if has("user_commands")
  command! -bang -nargs=* -complete=file E e<bang> <args>
  command! -bang -nargs=* -complete=file W w<bang> <args>
  command! -bang -nargs=* -complete=file Wq wq<bang> <args>
  command! -bang -nargs=* -complete=file WQ wq<bang> <args>
  command! -bang Wa wa<bang>
  command! -bang WA wa<bang>
  command! -bang Q q<bang>
  command! -bang QA qa<bang>
  command! -bang Qa qa<bang>
endif

"cmap Tabe tabe

" Yank from the cursor to the end of the line, to be consistent with C and D.
nnoremap Y y$

" Code folding options
nmap <leader>f0 :set foldlevel=0<CR>
nmap <leader>f1 :set foldlevel=1<CR>
nmap <leader>f2 :set foldlevel=2<CR>
nmap <leader>f3 :set foldlevel=3<CR>
nmap <leader>f4 :set foldlevel=4<CR>
nmap <leader>f5 :set foldlevel=5<CR>
nmap <leader>f6 :set foldlevel=6<CR>
nmap <leader>f7 :set foldlevel=7<CR>
nmap <leader>f8 :set foldlevel=8<CR>
nmap <leader>f9 :set foldlevel=9<CR>

nmap <silent> <leader>/ :set invhlsearch<CR>

" Find merge conflict markers
map <leader>fc /\v^[<\|=>]{7}( .*\|$)<CR>

" Shortcuts
" Change Working Directory to that of the current file
cmap cwd lcd %:p:h
cmap cd. lcd %:p:h

" Visual shifting (does not exit Visual mode)
vnoremap < <gv
vnoremap > >gv

" Allow using the repeat operator with a visual selection (!)
" http://stackoverflow.com/a/8064607/127816
" 用.再normal mod 中重复之前对某一行的操作
vnoremap . :normal .<CR>

" For when you forget to sudo.. Really Write the file.
cmap w!! w !sudo tee % >/dev/null

" Some helpers to edit mode
" http://vimcasts.org/e/14
cnoremap %% <C-R>=fnameescape(expand('%:h')).'/'<cr>
map <leader>ew :e ./
map <leader><leader>ew :e ~/
map <leader>es :sp ./
map <leader>ev :vsp ./
map <leader><leader>ev :vsp ~/
map <leader>et :tabe ./
map <leader><leader>et :tabe ~/

" Adjust viewports to the same size
map <Leader>= <C-w>=

" Map <Leader>ff to display all lines with keyword under cursor
" and ask which one to jump to
"nmap <Leader>ff [I:let nr = input("Which one: ")<Bar>exe "normal " . nr ."[\t"<CR>

" Easier horizontal scrolling
map zl zL
map zh zH

" Easier formatting
nnoremap <silent> <leader>q ZZ

" FIXME: Revert this f70be548
" fullscreen mode for GVIM and Terminal, need 'wmctrl' in you PATH
"map <silent> <F11> :call system("wmctrl -ir " . v:windowid . " -b toggle,fullscreen")<CR>

"if isdirectory(expand("~/.vim/plugged/nerdtree"))
"let g:NERDShutUp=1
"endif
"if isdirectory(expand("~/.vim/plugged/matchit.zip"))
"let b:match_ignorecase = 1
"endif

"""""""""""omnicomplete
if has("autocmd") && exists("+omnifunc")
  autocmd Filetype *
        \if &omnifunc == "" |
        \setlocal omnifunc=syntaxcomplete#Complete |
        \endif
endif

hi Pmenu  guifg=#000000 guibg=#F8F8F8 ctermfg=black ctermbg=Lightgray
hi PmenuSbar  guifg=#8A95A7 guibg=#F8F8F8 gui=NONE ctermfg=darkcyan ctermbg=lightgray cterm=NONE
hi PmenuThumb  guifg=#F8F8F8 guibg=#8A95A7 gui=NONE ctermfg=lightgray ctermbg=darkcyan cterm=NONE

"inoremap <expr> <Esc>      pumvisible() ? "\<C-e>" : "\<Esc>"
"if exists('g:spf13_map_cr_omni_complete')
inoremap <expr> <CR>     pumvisible() ? "\<C-y>" : "\<CR>"
"endif
inoremap <expr> <Down>     pumvisible() ? "\<C-n>" : "\<Down>"
inoremap <expr> <Up>       pumvisible() ? "\<C-p>" : "\<Up>"
inoremap <expr> <C-d>      pumvisible() ? "\<PageDown>\<C-p>\<C-n>" : "\<C-d>"
inoremap <expr> <C-u>      pumvisible() ? "\<PageUp>\<C-p>\<C-n>" : "\<C-u>"

" Automatically open and close the popup menu / preview window
"            au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
set completeopt=menu,preview,longest,menuone
"endif
" }

" AutoCloseTag {
" Make it so AutoCloseTag works for xml and xhtml files as well
au FileType xhtml,xml ru ftplugin/html/autoclosetag.vim
"nmap <Leader>ac <Plug>ToggleAutoCloseMappings
" }

" NerdTree {
if isdirectory(expand("~/.vim/plugged/nerdtree"))
  map <C-e> :NERDTreeToggle<CR>
  map <leader>e :NERDTreeFind<CR>
  " nmap <leader>nt :NERDTreeFind<CR>

  let NERDTreeShowBookmarks=1
  let NERDTreeIgnore=['\.py[cd]$', '\~$', '\.swo$', '\.swp$', '^\.git$', '^\.hg$', '^\.svn$', '\.bzr$']
  let NERDTreeChDirMode=0
  let NERDTreeQuitOnOpen=1
  let NERDTreeMouseMode=2
  let NERDTreeShowHidden=1
  let NERDTreeKeepTreeInNewTab=1
  let g:nerdtree_tabs_open_on_gui_startup=0
endif
" }

" Tabularize {
if isdirectory(expand("~/.vim/plugged/tabular"))
  nmap <Leader>a& :Tabularize /&<CR>
  vmap <Leader>a& :Tabularize /&<CR>
  vmap <Leader>a\ :Tabularize /\<CR>
  nmap <Leader>a= :Tabularize /^[^=]*\zs=<CR>
  vmap <Leader>a= :Tabularize /^[^=]*\zs=<CR>
  nmap <Leader>a=> :Tabularize /=><CR>
  vmap <Leader>a=> :Tabularize /=><CR>
  nmap <Leader>a: :Tabularize /:<CR>
  vmap <Leader>a: :Tabularize /:<CR>
  nmap <Leader>a:: :Tabularize /:\zs<CR>
  vmap <Leader>a:: :Tabularize /:\zs<CR>
  nmap <Leader>a, :Tabularize /,<CR>
  vmap <Leader>a, :Tabularize /,<CR>
  nmap <Leader>a,, :Tabularize /,\zs<CR>
  vmap <Leader>a,, :Tabularize /,\zs<CR>
  nmap <Leader>a<Bar> :Tabularize /<Bar><CR>
  vmap <Leader>a<Bar> :Tabularize /<Bar><CR>
endif
" }

" Session List {
" set sessionoptions=blank,buffers,curdir,folds,tabpages,winsize
" if isdirectory(expand("~/.vim/plugged/sessionman.vim/"))
  " nmap <leader>sl :SessionList<CR>
  " nmap <leader>ss :SessionSave<CR>
  " "nmap <leader>sc :SessionClose<CR>
" endif
" }

" JSON {
nmap <leader>jt <Esc>:%!python -m json.tool<CR><Esc>:set filetype=json<CR>
let g:vim_json_syntax_conceal = 0
" }

" PyMode {
" Disable if python support not present
if !has('python') && !has('python3')
  let g:pymode = 0
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                              ctrlp fzf                              "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <silent> <Leader>ct :CtrlPMRU<CR>
nnoremap <silent> <Leader>vt :CtrlPRegister<CR>
nnoremap <silent> <Leader>ff :FZF<CR>
let g:fzf_buffers_jump = 1
nmap <C-f> :Lines<CR>
let g:ctrlp_custom_ignore = {
      \ 'dir':  '\.git$\|\.hg$\|\.svn$',
      \ 'file': '\.exe$\|\.so$\|\.dll$\|\.pyc$' }

if executable('ag')
  let s:ctrlp_fallback = 'ag %s --nocolor -l -g ""'
  let g:ackprg = 'ag --vimgrep'
elseif executable('ack-grep')
  let s:ctrlp_fallback = 'ack-grep %s --nocolor -f'
elseif executable('ack')
  let s:ctrlp_fallback = 'ack %s --nocolor -f'
  if exists("g:ctrlp_user_command")
    unlet g:ctrlp_user_command
  endif
  let g:ctrlp_user_command = {
        \ 'types': {
        \ 1: ['.git', 'cd %s && git ls-files . --cached --exclude-standard --others'],
        \ 2: ['.hg', 'hg --cwd %s locate -I .'],
        \ },
        \ 'fallback': s:ctrlp_fallback
        \ }
endif
"}

" TagBar {
nnoremap <silent> <leader>f :TagbarToggle<CR>
"}

" Rainbow {
if isdirectory(expand("~/.vim/plugged/rainbow/"))
  let g:rainbow_active = 1 "0 if you want to enable it later via :RainbowToggle
endif
"}

" Fugitive {
if isdirectory(expand("~/.vim/plugged/vim-fugitive/"))
  cno gitfindcommit Glog --grep=

  nnoremap <silent> <leader>gca :te git commit --all<CR>
  nnoremap <silent> <leader>gpp :te git pull origin "$(git-branch-current 2> /dev/null)" && git push origin "$(git-branch-current 2> /dev/null)"<CR>
  nnoremap <silent> <leader>gcff :te git commit --amend --reuse-message HEAD --all<CR>
  nnoremap <silent> <leader>gws :te git status --short<CR>
  nnoremap <silent> <leader>gs :Gstatus<CR>
  nnoremap <silent> <leader>gd :Gvdiff<CR>
  nnoremap <silent> <leader>gc :Gcommit<CR>
  nnoremap <silent> <leader>gb :Gblame<CR>
  nnoremap <silent> <leader>gl :Glog<CR>
  nnoremap <silent> <leader>gp :Git push<CR>
  nnoremap <silent> <leader>gr :Gread<CR>
  nnoremap <silent> <leader>gw :Gwrite<CR>
  nnoremap <silent> <leader>ge :Gedit<CR>
  " Mnemonic _i_nteractive
  nnoremap <silent> <leader>gi :Git add -p %<CR>
  nnoremap <silent> <leader>gg :SignifyToggle<CR>
endif
"}

"         " Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete

" Haskell post write lint and check with ghcmod
" $ `cabal install ghcmod` if missing and ensure
" ~/.cabal/bin is in your $PATH.
" if !executable("ghcmod")
" autocmd BufWritePost *.hs GhcModCheckAndLintAsync
" endif

" For snippet_complete marker.
"if !exists("g:spf13_no_conceal")
"if has('conceal')
"set conceallevel=2 concealcursor=i
"endif
"         " Disable the neosnippet preview candidate window
"         " When enabled, there can be too much visual noise
"         " especially when splits are used.
"set completeopt-=preview
" Shell command {
" function! s:RunShellCommand(cmdline)
" botright new

" setlocal buftype=nofile
" setlocal bufhidden=delete
" setlocal nobuflisted
" setlocal noswapfile
" setlocal nowrap
" setlocal filetype=shell
" setlocal syntax=shell

" call setline(1, a:cmdline)
" call setline(2, substitute(a:cmdline, '.', '=', 'g'))
" execute 'silent $read !' . escape(a:cmdline, '%#')
" setlocal nomodifiable
" 1
" endfunction

" command! -complete=file -nargs=+ Shell call s:RunShellCommand(<q-args>)
" e.g. Grep current file for <search_term>: Shell grep -Hn <search_term> %
" }
