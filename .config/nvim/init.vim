let s:darwin = has('mac')
if &nu == 1
  set rnu
elseif &rnu == 1
  set nornu
else
  set nu
endif

" autoinsert in terminal buffer
" :au BufEnter * if &buftype == 'terminal' | :startinsert | endif
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                  neoterm                                   "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:neoterm_autoinsert = 1
set statusline+=%#NeotermTestRunning#%{neoterm#test#status('running')}%*
set statusline+=%#NeotermTestSuccess#%{neoterm#test#status('success')}%*
set statusline+=%#NeotermTestFailed#%{neoterm#test#status('failed')}%*
nmap <NUL> :Ttoggle<CR>
" imap <NUL> <ESC>:Ttoggle<CR>
tnoremap <NUL> <C-\><C-n>:Ttoggle<CR>
" ----------------------------------------------------------------------------
" EX | chmod +x
" ----------------------------------------------------------------------------
command! EX if !empty(expand('%'))
         \|   write
         \|   call system('chmod +x '.expand('%'))
         \|   silent e
         \| else
         \|   echohl WarningMsg
         \|   echo 'Save the file first'
         \|   echohl None
         \| endif
" ----------------------------------------------------------------------------
" ?ii / ?ai | indent-object
" ?io       | strictly-indent-object
" ----------------------------------------------------------------------------
function! s:indent_len(str)
  return type(a:str) == 1 ? len(matchstr(a:str, '^\s*')) : 0
endfunction

function! s:indent_object(op, skip_blank, b, e, bd, ed)
  let i = min([s:indent_len(getline(a:b)), s:indent_len(getline(a:e))])
  let x = line('$')
  let d = [a:b, a:e]

  if i == 0 && empty(getline(a:b)) && empty(getline(a:e))
    let [b, e] = [a:b, a:e]
    while b > 0 && e <= line('$')
      let b -= 1
      let e += 1
      let i = min(filter(map([b, e], 's:indent_len(getline(v:val))'), 'v:val != 0'))
      if i > 0
        break
      endif
    endwhile
  endif

  for triple in [[0, 'd[o] > 1', -1], [1, 'd[o] < x', +1]]
    let [o, ev, df] = triple

    while eval(ev)
      let line = getline(d[o] + df)
      let idt = s:indent_len(line)

      if eval('idt '.a:op.' i') && (a:skip_blank || !empty(line)) || (a:skip_blank && empty(line))
        let d[o] += df
      else | break | end
    endwhile
  endfor
  execute printf('normal! %dGV%dG', max([1, d[0] + a:bd]), min([x, d[1] + a:ed]))
endfunction
xnoremap <silent> ii :<c-u>call <SID>indent_object('>=', 1, line("'<"), line("'>"), 0, 0)<cr>
onoremap <silent> ii :<c-u>call <SID>indent_object('>=', 1, line('.'), line('.'), 0, 0)<cr>
xnoremap <silent> ai :<c-u>call <SID>indent_object('>=', 1, line("'<"), line("'>"), -1, 1)<cr>
onoremap <silent> ai :<c-u>call <SID>indent_object('>=', 1, line('.'), line('.'), -1, 1)<cr>
xnoremap <silent> io :<c-u>call <SID>indent_object('==', 0, line("'<"), line("'>"), 0, 0)<cr>
onoremap <silent> io :<c-u>call <SID>indent_object('==', 0, line('.'), line('.'), 0, 0)<cr>
" ----------------------------------------------------------------------------
" ?ic / ?iC | Blockwise column object
" ----------------------------------------------------------------------------
function! s:inner_blockwise_column(vmode, cmd)
  if a:vmode == "\<C-V>"
    let [pvb, pve] = [getpos("'<"), getpos("'>")]
    normal! `z
  endif

  execute "normal! \<C-V>".a:cmd."o\<C-C>"
  let [line, col] = [line('.'), col('.')]
  let [cb, ce]    = [col("'<"), col("'>")]
  let [mn, mx]    = [line, line]

  for dir in [1, -1]
    let l = line + dir
    while line('.') > 1 && line('.') < line('$')
      execute "normal! ".l."G".col."|"
      execute "normal! v".a:cmd."\<C-C>"
      if cb != col("'<") || ce != col("'>")
        break
      endif
      let [mn, mx] = [min([line('.'), mn]), max([line('.'), mx])]
      let l += dir
    endwhile
  endfor

  execute printf("normal! %dG%d|\<C-V>%s%dG", mn, col, a:cmd, mx)

  if a:vmode == "\<C-V>"
    normal! o
    if pvb[1] < line('.') | execute "normal! ".pvb[1]."G" | endif
    if pvb[2] < col('.')  | execute "normal! ".pvb[2]."|" | endif
    normal! o
    if pve[1] > line('.') | execute "normal! ".pve[1]."G" | endif
    if pve[2] > col('.')  | execute "normal! ".pve[2]."|" | endif
  endif
endfunction

xnoremap <silent> ic mz:<C-U>call <SID>inner_blockwise_column(visualmode(), 'iw')<CR>
" xnoremap <silent> iC mz:<C-U>call <SID>inner_blockwise_column(visualmode(), 'iW')<CR>
" xnoremap <silent> ac mz:<C-U>call <SID>inner_blockwise_column(visualmode(), 'aw')<CR>
" xnoremap <silent> aC mz:<C-U>call <SID>inner_blockwise_column(visualmode(), 'aW')<CR>
onoremap <silent> ic :<C-U>call   <SID>inner_blockwise_column('',           'iw')<CR>
onoremap <silent> iC :<C-U>call   <SID>inner_blockwise_column('',           'iW')<CR>
onoremap <silent> ac :<C-U>call   <SID>inner_blockwise_column('',           'aw')<CR>
onoremap <silent> aC :<C-U>call   <SID>inner_blockwise_column('',           'aW')<CR>
" ----------------------------------------------------------------------------
" MatchParen delay
" ----------------------------------------------------------------------------
let g:matchparen_insert_timeout=5
" ----------------------------------------------------------------------------
" ?il | inner line
" ----------------------------------------------------------------------------
xnoremap <silent> il <Esc>^vg_
onoremap <silent> il :<C-U>normal! ^vg_<CR>
" ----------------------------------------------------------------------------
" ?ie | entire object
" ----------------------------------------------------------------------------
xnoremap <silent> ie gg0oG$
onoremap <silent> ie :<C-U>execute "normal! m`"<Bar>keepjumps normal! ggVG<CR>
" ----------------------------------------------------------------------------
" <Leader>I/A | Prepend/Append to all adjacent lines with same indentation
" ----------------------------------------------------------------------------
nmap <silent> <leader>I ^vio<C-V>I
nmap <silent> <leader>A ^vio<C-V>$A
" ----------------------------------------------------------------------------
" splitjoin
" ----------------------------------------------------------------------------
let g:splitjoin_split_mapping = ''
let g:splitjoin_join_mapping = ''
nnoremap gss :SplitjoinSplit<cr>
nnoremap gsj :SplitjoinJoin<cr>
" ----------------------------------------------------------------------------
" vim-signify
" ----------------------------------------------------------------------------
let g:signify_vcs_list = ['git']
let g:signify_skip_filetype = { 'journal': 1 }
" ----------------------------------------------------------------------------
" AutoSave
" ----------------------------------------------------------------------------
function! s:autosave(enable)
  augroup autosave
    autocmd!
    if a:enable
      autocmd TextChanged,InsertLeave <buffer>
            \  if empty(&buftype) && !empty(bufname(''))
            \|   silent! update
            \| endif
    endif
  augroup END
endfunction

command! -bang AutoSave call s:autosave(<bang>1)
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
nmap <script> <silent> <BS> :call ToggleQuickfixList()<CR>
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                 neomake                                    "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" autocmd! BufWritePost * Neomake
nmap <leader><leader>w :Neomake<cr>:w<cr>
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
let g:neomake_enabled_makers=['eslint','jscs']
" let g:neomake_logfile='~/Downloads/neomake.log'
" let g:neomake_verbose=0

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                 syntastic                                  "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" nmap <leader><leader>w :w!<cr>:SyntasticCheck<cr>

" set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*

set statusline=%<[%n]\ %F\ %m%r%y\ %{exists('g:loaded_fugitive')?fugitive#statusline():''}\ %=%-14.(%l,%c%V%)\ %P
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

" ----------------------------------------------------------------------------
" Todo
" ----------------------------------------------------------------------------
function! s:todo() abort
  let entries = []
  for cmd in ['git grep -n -e TODO -e FIXME -e XXX 2> /dev/null',
            \ 'grep -rn -e TODO -e FIXME -e XXX * 2> /dev/null']
    let lines = split(system(cmd), '\n')
    if v:shell_error != 0 | continue | endif
    for line in lines
      let [fname, lno, text] = matchlist(line, '^\([^:]*\):\([^:]*\):\(.*\)')[1:3]
      call add(entries, { 'filename': fname, 'lnum': lno, 'text': text })
    endfor
    break
  endfor

  if !empty(entries)
    call setqflist(entries)
    copen
  endif
endfunction
command! Todo call s:todo()
" ----------------------------------------------------------------------------
" :Root | Change directory to the root of the Git repository
" ----------------------------------------------------------------------------
function! s:root()
  let root = systemlist('git rev-parse --show-toplevel')[0]
  if v:shell_error
    echo 'Not in git repo'
  else
    execute 'lcd' root
    echo 'Changed directory to: '.root
  endif
endfunction
command! Root call s:root()
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                               Plugins                               "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

call plug#begin('~/.vim/plugged')
Plug 'Chiel92/vim-autoformat'
Plug 'junegunn/vim-peekaboo'
Plug 'kassio/neoterm'
Plug 'milkypostman/vim-togglelist'
Plug 'mattn/webapi-vim'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'benjie/neomake-local-eslint.vim'
Plug 'mileszs/ack.vim'
Plug 'neomake/neomake'
Plug 'moll/vim-node'
Plug 'mhinz/vim-startify' "welcoming view
Plug 'scrooloose/nerdtree' ,{ 'on': 'NERDTreeToggle' }
Plug 'justinmk/vim-gtfo'
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
Plug 'chrisbra/vim-zsh', {'for': 'zsh'}
Plug 'junegunn/seoul256.vim'
Plug '/usr/local/opt/fzf' | Plug 'junegunn/fzf.vim'
Plug 'morhetz/gruvbox'
Plug 'justinmk/vim-sneak'
Plug 'tomtom/quickfixsigns_vim'
" Plug 'majutsushi/tagbar'
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
" Plug 'jlanzarotta/bufexplorer'
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
" Plug 'vim-ctrlspace/vim-ctrlspace'
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
" let g:formatters_javascript = ['jscs']
let g:formatters_javascript = ['jscs']
let g:formatters_json = ['jscs']
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
command! -nargs=1 Count execute printf('%%s/%s//gn', escape(<q-args>, '/')) | normal! ``
xnoremap <silent> <C-k> :move-2<cr>gv
xnoremap <silent> <C-j> :move'>+<cr>gv
xnoremap <silent> <BS> <gv
xnoremap <silent> <C-l> >gv
xnoremap < <gv
xnoremap > >gv
nnoremap <tab>   <c-w>w
nnoremap <S-tab> <c-w>W
nnoremap <silent> <C-k> :move-2<cr>
nnoremap <silent> <C-j> :move+<cr>
nnoremap ]q :cnext<cr>zz
nnoremap [q :cprev<cr>zz
let leader='\'
nmap <silent> <leader>l <Plug>(jsdoc)
nmap <leader>,, :set wrap<CR>
imap <C-j> <cr>
nnoremap <C-w>= <C-w>+
nnoremap <C-w><C-=> <C-w>+
nnoremap <C-w><C--> <C-w>-
nnoremap <C-d>  <C-d>zz
nnoremap <C-u>  <C-u>zz
nnoremap n nzz
nnoremap N Nzz
nnoremap j jzz
nnoremap k kzz
nnoremap # #zz
nnoremap * *zz
nnoremap <Down> jzz
nnoremap <Up> kzz
map <leader><leader>q :bd!<CR>
map <leader><leader><leader>q :tabc<CR>
nmap Q @q
imap <C-f> <Right>
imap <C-b> <Left>
imap <C-e> <Esc><S-A>
imap <C-a> <Esc><S-I>
nnoremap <leader>bn :bn<CR>
nnoremap <leader>bp :bp<CR>
map <leader><leader>- mzgg=G`z
nmap <leader><leader>r :w!<CR>:ChromeReload<CR>
nnoremap <F5> :UndotreeToggle<cr>
map <leader><F10> :QuickRun<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                     javascript-libraries-syntax                     "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" let g:used_javascript_libs = 'chai,underscore'

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
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                             vim-airline                             "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" enable/disable detection of whitespace errors. >
let g:airline#extensions#whitespace#enabled = 0
" enable/disable showing only non-zero hunks. >
let g:airline#extensions#hunks#non_zero_only = 0
" enable/disable word counting. >
let g:airline#extensions#wordcount#enabled = 0
let g:airline_inactive_collapse=0
let g:airline_powerline_fonts=1
let g:airline#extensions#branch#empty_message = 'NoBranch'

let g:airline#extensions#tabline#buffer_nr_show = 0

let g:airline#extensions#tabline#buffer_idx_mode = 1
nmap <leader>1 <Plug>AirlineSelectTab1
nmap <leader>2 <Plug>AirlineSelectTab2
nmap <leader>3 <Plug>AirlineSelectTab3
nmap <leader>4 <Plug>AirlineSelectTab4
nmap <leader>5 <Plug>AirlineSelectTab5
nmap <leader>6 <Plug>AirlineSelectTab6
nmap <leader>7 <Plug>AirlineSelectTab7
nmap <leader>8 <Plug>AirlineSelectTab8
nmap <leader>9 <Plug>AirlineSelectTab9

let airline#extensions#tabline#ignore_bufadd_pat =
          \ '\c\vgundo|undotree|vimfiler|tagbar|nerd_tree|ycm'

:tnoremap <leader><leader> <C-\><C-n>
:tnoremap <leader>1 <C-\><C-n><C-\><C-n><Plug>AirlineSelectTab1
:tnoremap <leader>2 <C-\><C-n><C-\><C-n><Plug>AirlineSelectTab2
:tnoremap <leader>3 <C-\><C-n><C-\><C-n><Plug>AirlineSelectTab3
:tnoremap <leader>4 <C-\><C-n><C-\><C-n><Plug>AirlineSelectTab4
:tnoremap <leader>5 <C-\><C-n><C-\><C-n><Plug>AirlineSelectTab5
:tnoremap <leader>6 <C-\><C-n><C-\><C-n><Plug>AirlineSelectTab6
:tnoremap <leader>7 <C-\><C-n><C-\><C-n><Plug>AirlineSelectTab7
:tnoremap <leader>8 <C-\><C-n><C-\><C-n><Plug>AirlineSelectTab8

" enable/disable enhanced tabline. (c)
let g:airline#extensions#tabline#enabled = 1

" enable/disable displaying open splits per tab (only when tabs are opened). >
let g:airline#extensions#tabline#show_splits = 0

" switch position of buffers and tabs on splited tabline (c)
let g:airline#extensions#tabline#switch_buffers_and_tabs = 0

" enable/disable displaying buffers with a single tab. (c)
let g:airline#extensions#tabline#show_buffers = 1

" enable/disable displaying tabs, regardless of number. (c)
  let g:airline#extensions#tabline#show_tabs = 0

" enable/disable display preview window buffer in the tabline. >
let g:airline#extensions#tabline#exclude_preview = 1

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

" configure the minimum number of buffers needed to show the tabline. >
  let g:airline#extensions#tabline#buffer_min_count = 1

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
set wrap
syntax on

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                 ycm                                 "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ycm_autoclose_preview_window_after_completion = 1
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
"autocmd VimEnter  * silent! :split | term

let g:ycm_server_python_interpreter = '/usr/local/bin/python3.5'
let g:ycm_python_binary_path = '/usr/local/bin/python3.5'
let g:ycm_semantic_triggers =  {
      \   'c' : ['->', '.'],
      \   'objc' : ['->', '.', 're!\[[_a-zA-Z]+\w*\s', 're!^\s*[^\W\d]\w*\s',
      \             're!\[.*\]\s'],
      \   'cpp,objcpp' : ['->', '.', '::'],
      \   'javascript,typescript,python,go' : ['.'],
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
cno hhh ~/
cno ddd ~/Desktop/
cno kkk ./
"cno $c e <C-\>eCurrentFileDir("e")<cr>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => vim-javascript
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" let javascript_enable_domhtmlcss = 1
" let g:javascript_conceal_function       = "ƒ"
" let g:javascript_conceal_null           = "ø"
" let g:javascript_conceal_this           = "@"
" let g:javascript_conceal_return         = "⇚"
" let g:javascript_conceal_undefined      = "¿"
" let g:javascript_conceal_NaN            = "ℕ"
" let g:javascript_conceal_prototype      = "¶"
" let g:javascript_conceal_static         = "•"
" let g:javascript_conceal_super          = "Ω"
" let g:javascript_conceal_arrow_function = "⇒"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Sets how many lines of history VIM has to remember
set history=500

" Set to auto read when a file is changed from the outside
set autoread

"Always show current position
set ruler

" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file
let mapleader = '\'
let g:mapleader = '\'

" Fast saving
nmap <leader>w :w!<cr>

"command line autocompletion
set wildmode=longest:full,full

" No annoying sound on errors
set noerrorbells
set novisualbell
set virtualedit=block
set t_vb=
set tm=500

" <Space> to / (search) and Ctrl-<Space> to ? (backwards search)
map <space> /

" Let 'tl' toggle between this and the last accessed tab
let g:lasttab = 1
nmap <Leader>tl :exe "tabn ".g:lasttab<CR>
au TabLeave * let g:lasttab = tabpagenr()
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Parenthesis/bracket
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" inoremap $3 ()<esc>i
"""""""""""""""""""
"easyMotion
"""""""""""""""""""""
let g:EasyMotion_do_mapping = 0 " Disable default mappings
" Jump to anywhere you want with minimal keystrokes, with just one key binding.
" `s{char}{label}`
nmap <C-s> <Plug>(easymotion-overwin-f)
vmap <C-s> <Plug>(easymotion-overwin-f)
" or
" `s{char}{char}{label}`
" Need one more keystroke, but on average, it may be more comfortable.
" Turn on case insensitive feature
let g:EasyMotion_smartcase = 1
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
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                             colorscheme                             "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set termguicolors
set background=dark " Assume a dark background
" colorscheme solarized
" colorscheme seoul256
" let g:seoul256_light_background = 252
colorscheme gruvbox
let g:gruvbox_italic=1
let g:gruvbox_contrast_light = 'hard'
let g:gruvbox_contrast_dark = 'hard'
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
" Fugitive
  au FileType gitcommit nnoremap <buffer> <silent> cA :<C-U>Gcommit --amend --date="$(date)"<CR>

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

set tabpagemax=15               " Only show 15 tabs
set showmode                    " Display the current mode
set cursorline                  " Highlight current line

highlight clear SignColumn      " SignColumn should match background
highlight clear LineNr          " Current line number row will have same background color in relative mode
"highlight clear CursorLineNr    " Remove highlight color from current line number

set rulerformat=%30(%=\:b%n%y%m%r%w\ %l,%c%V\ %P%) " A ruler on steroids
set showcmd                 " Show partial commands in status line and
" Selected characters/lines in visual mode

set laststatus=2
set backspace=indent,eol,start  " Backspace for dummies
set linespace=0                 " No extra spaces between rows
set showmatch                   " Show matching brackets/parenthesis
set shiftround                  " Round indent to multiple of 'shiftwidth'.
set magic
set sidescrolloff=5
set incsearch                   " Find as you type search
set lazyredraw                  " the screen will not be redrawn while executing macros, registers and other commands that have not been typed" 
set linebreak                   " If on, Vim will wrap long lines at a character in 'breakat' rather
set hlsearch                    " Highlight search terms
set winminheight=0              " Windows can be 0 line high
set ignorecase                  " Case insensitive search
set smartcase                   " Case sensitive when uc present
set wildmenu                    " Show list instead of just completing
" set wildmode=list:longest,full  " Command <Tab> completion, list matches, then longest common part, then all.
set wildmode=longest:full,full  " Command <Tab> completion, list matches, then longest common part, then all.
set whichwrap=b,s,h,l,<,>,[,]   " Backspace and cursor keys wrap too
set scrolljump=6                " Lines to scroll when cursor leaves screen
set scrolloff=3                 " Minimum lines to keep above and below cursor
set foldenable                  " Auto fold code
set list
set listchars=tab:›\ ,trail:•,extends:#,nbsp:. " Highlight problematic whitespace

" Formatting {
set autoindent                  " Indent at the same level of the previous line
set shiftwidth=2                " Use indents of 4 spaces
set expandtab                   " Tabs are spaces, not tabs
set tabstop=4                   " An indentation every four columns
set softtabstop=4               " Let backspace delete indent
set nojoinspaces                " Prevents inserting two spaces after punctuation on a join (J)
set splitright                  " Puts new vsplit windows to the right of the current
set splitbelow                  " Puts new split windows to the bottom of the current
"set matchpairs+=<:>             " Match, to be used with %
"set pastetoggle=<F12>           " pastetoggle (sane indentation on pastes)
set comments=sl:/*,mb:*,elx:*/  " auto format comment blocks

"""""""""""""""""""
"for edit vimrc
"""""""""""""""""""""
let s:spf13_edit_config_mapping = '<leader>ec'
let s:spf13_apply_config_mapping = '<leader>sc'
nmap <leader>sc :source ~/.config/nvim/init.vim<CR>
execute "noremap " . s:spf13_edit_config_mapping " :call <SID>EditSpf13Config()<CR>"

function! s:ExpandFilenameAndExecute(command, file)
  execute a:command . " " . expand(a:file, ":p")
endfunction

function! s:EditSpf13Config()
  call <SID>ExpandFilenameAndExecute("tabedit", "~/.config/nvim/init.vim")
endfunction
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                             vim indent guides                              "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 1
let g:indent_guides_enable_on_vim_startup = 1

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
vnoremap . :normal .<CR>

" For when you forget to sudo.. Really Write the file.
cmap w!! w !sudo tee % >/dev/null

" Some helpers to edit mode
" http://vimcasts.org/e/14
cnoremap %% <C-R>=fnameescape(expand('%:h')).'/'<cr>
map <leader>ew :e ./
map <leader><leader>ew :e ../
map <leader><leader><leader>ew :e ~/
map <leader>es :sp ./
map <leader>ev :vsp ./
map <leader><leader>ev :vsp ~/

" Adjust viewports to the same size
map <Leader>= <C-w>=

" Easier horizontal scrolling
map zl zL
map zh zH
nnoremap <silent> <leader>q ZZ

"""""""""""omnicomplete
autocmd Filetype *
  \if &omnifunc == "" |
  \setlocal omnifunc=syntaxcomplete#Complete |
  \endif

" set completeopt=menu,preview,longest,menuone
set completeopt-=i " Searching includes can be slow
set fileformats=unix,dos,mac

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                  NerdTree                                  "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <C-e> :NERDTreeToggle<CR>
map <leader>e :NERDTreeFind<CR>

let NERDTreeShowBookmarks=1
let NERDTreeIgnore=['\.py[cd]$', '\~$', '\.swo$', '\.swp$', '^\.git$', '^\.hg$', '^\.svn$', '\.bzr$']
let NERDTreeChDirMode=0
let NERDTreeQuitOnOpen=1
let NERDTreeMouseMode=2
let NERDTreeShowHidden=1
let NERDTreeKeepTreeInNewTab=1
let g:nerdtree_tabs_open_on_gui_startup=0

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                 Tabularize                                 "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
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
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                              ctrlp fzf                              "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let $FZF_DEFAULT_OPTS .= ' --inline-info'
let g:fzf_files_options =
  \ '--preview "(highlight -O ansi {} || cat {}) 2> /dev/null | head -'.&lines.'"'
nnoremap <silent> <expr> <Leader>ff (expand('%') =~ 'NERD_tree' ? "\<c-w>\<c-w>" : '').":Files\<cr>"
nnoremap <silent> <Leader>ag :Ag <C-R><C-W><CR>
nnoremap <silent> <Leader>ct :CtrlPMRU<CR>
nnoremap <silent> <Leader>vt :CtrlPRegister<CR>
nnoremap <silent> <leader>fgc :Gcommit<CR>
nnoremap <silent> <leader>fbc :BGcommit<CR>
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

" TagBar
" nnoremap <silent> <leader>f :TagbarToggle<CR>

" Rainbow {
let g:rainbow_active = 1 "0 if you want to enable it later via :RainbowToggle

" Fugitive {
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
" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
