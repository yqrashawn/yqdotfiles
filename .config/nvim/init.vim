map <NUL> <Plug>(incsearch-forward)
map <C-space> <Plug>(incsearch-forward)
map <C-s> <Plug>(expand_region_expand)
nmap sw <Plug>(choosewin)
nnoremap <C-a> ^
nnoremap <C-e> $
nnoremap sv <C-w><C-v>
nnoremap so :on<CR>
nnoremap sc <C-w><C-c>
nnoremap sk :bd<CR>
let leader=' '
let mapleader = ' '
let g:mapleader = ' '

" autoinsert in terminal buffer
" :au BufEnter * if &buftype == 'terminal' | :startinsert | endif
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                  neoterm                                   "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:neoterm_autoinsert = 1
nnoremap <silent> <leader>' :Ttoggle<CR>
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
" onoremap <silent> iC :<C-U>call   <SID>inner_blockwise_column('',           'iW')<CR>
" onoremap <silent> ac :<C-U>call   <SID>inner_blockwise_column('',           'aw')<CR>
" onoremap <silent> aC :<C-U>call   <SID>inner_blockwise_column('',           'aW')<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Tabularize mappings
" For custom Tabularize definitions see after/plugin/tabularize.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap gsa      :call <SID>Tabularize(0)<cr>
xnoremap gsa :<c-u>call <SID>Tabularize(1)<cr>
function! s:Tabularize(visual)
  let saved_cursor = getpos('.')

  echohl ModeMsg | echo "-- ALIGN -- "  | echohl None
  let char = nr2char(getchar())

  if     char == '=' | let alignment = 'equals'
  elseif char == '>' | let alignment = 'ruby_hash'
  elseif char == ',' | let alignment = 'commas'
  elseif char == ':' | let alignment = 'colons'
  elseif char == ' ' | let alignment = 'space'
  else
    " just try aligning by the character
    let char = escape(char, '/.')
    let alignment = '/'.char
  endif

  if a:visual
    exe "'<,'>Tabularize ".alignment
  else
    exe 'Tabularize '.alignment
  endif

  echo
  call setpos('.', saved_cursor)
endfunction

" Tabularize "reset" -- removes all duplicate whitespace
nnoremap gs= :call <SID>TabularizeReset()<cr>
xnoremap gs= :call <SID>TabularizeReset()<cr>
function! s:TabularizeReset()
  let original_cursor = getpos('.')

  s/\S\zs \+/ /ge
  call histdel('search', -1)
  let @/ = histget('search', -1)

  call setpos('.', original_cursor)
endfunction
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
autocmd BufLeave,FocusLost * silent! wall
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
" set nobackup

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                           Neo terminal emulator                            "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
tnoremap <C-w>h <C-\><C-n><C-w>h
tnoremap <C-w>j <C-\><C-n><C-w>j
tnoremap <C-w>k <C-\><C-n><C-w>k
tnoremap <C-w>l <C-\><C-n><C-w>l
nnoremap <silent> <leader>' :terminal<CR>
nnoremap <leader><leader>' :te
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
nmap <script> <silent> gh :call ToggleLocationList()<CR>
nmap <script> <silent> g; :call ToggleQuickfixList()<CR>
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
Plug 'airblade/vim-rooter'
Plug 'elzr/vim-json', { 'for': 'json'}
Plug 't9md/vim-foldtext'
Plug 'w0rp/ale'
Plug 'pangloss/vim-javascript'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'scrooloose/nerdtree' ,{ 'on': 'NERDTreeToggle' }
Plug 'jistr/vim-nerdtree-tabs',{ 'on': 'NERDTreeToggle' }
Plug 'gabesoft/vim-ags'
Plug 'heavenshell/vim-jsdoc', { 'for': 'javascript' }
Plug 'itchyny/lightline.vim'

Plug 'kana/vim-textobj-function'
Plug 'AndrewRadev/dsf.vim'
Plug 'osyo-manga/vim-textobj-multiblock'
Plug 'kana/vim-operator-replace'
Plug 'kana/vim-textobj-user'
Plug 'rhysd/vim-operator-surround'
Plug 'haya14busa/vim-operator-flashy'

Plug 'Shougo/unite.vim'
Plug 'Shougo/denite.nvim'
Plug 'chemzqm/unite-location'
Plug 'Shougo/vimproc.vim'
Plug 'kana/vim-operator-user'

Plug 'junegunn/vim-peekaboo'
Plug 'lambdalisue/vim-findent'
Plug 'Konfekt/FastFold'
Plug 'haya14busa/vim-asterisk'
Plug 'itchyny/vim-gitbranch'
Plug 'lambdalisue/vim-gita'
Plug 'Shougo/vimfiler.vim'
Plug 'tpope/vim-surround'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'haya14busa/incsearch.vim'
Plug 'thinca/vim-ref' " don know what does this do
Plug 'Raimondi/delimitMate'
Plug 'terryma/vim-expand-region'
Plug 'mhinz/vim-startify' "welcoming view
Plug 'terryma/vim-multiple-cursors'
Plug 'jnurmine/Zenburn'
Plug 'godlygeek/tabular', {'on':'Tabularize'}
Plug 'kana/vim-textobj-user'
Plug 't9md/vim-choosewin'
Plug 'rhysd/vim-textobj-anyblock'
Plug 'honza/vim-snippets'
Plug 'kana/vim-textobj-function'
Plug 'SirVer/ultisnips'
Plug 'justinmk/vim-sneak'
Plug 'rhysd/clever-f.vim'
Plug 'carlitux/deoplete-ternjs'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug '/usr/local/opt/fzf' | Plug 'junegunn/fzf.vim'
Plug 'bogado/file-line'
Plug 'jiangmiao/simple-javascript-indenter'
 " performace
Plug 'itchyny/vim-cursorword'
Plug 'jelera/vim-javascript-syntax'
call plug#end()
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                nerdcommenter too many unsless issue                 "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <C-x>; :Commentary<CR>
vnoremap <C-x>; :Commentary<CR>
nnoremap <leader>;; :Commentary<CR>
vnoremap <leader>;; :Commentary<CR>
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                  deplete                                   "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif
" <C-h>, <BS>: close popup and delete backword char.
inoremap <C-j> <cr>
inoremap <expr> <C-j> pumvisible() ? '<C-n>' : '<C-j>'
inoremap <expr> <C-k> pumvisible() ? '<C-p>' : '<C-k>'
inoremap <expr><C-h> deoplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS>  deoplete#smart_close_popup()."\<C-h>"
inoremap <silent> <C-l> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function() abort
  return deoplete#close_popup() . ""
endfunction
let g:deoplete#enable_at_startup = 1
let g:deoplete#auto_refresh_delay=50
let g:deoplete#auto_complete_delay=0
let g:deoplete#file#enable_buffer_path=1
let g:deoplete#sources = {}
let g:deoplete#sources._ = []
let g:deoplete#sources.javascript = ['buffer', 'ternjs']
" let g:deoplete#omni#functions = {}
" let g:deoplete#omni#functions.javascript = [
"   \ 'tern#Complete',
"   \ 'jspc#omni'
"   \]
let g:tern_request_timeout = 1
let g:tern_show_signature_in_pum = '0'  " This do disable full signature type on autocomplete

"Add extra filetypes
let g:tern#filetypes = [
                \ 'jsx',
                \ 'vue',
                \ 'html'
                \ ]
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                 statusline                                 "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ale_statusline_format = ['⨉ %d', '⚠ %d', '⬥ ok']
" let g:lightline = {
"       \ 'colorscheme': 'wombat',
"       \ 'component': {
"       \   'linter': '%{ALEGetStatusLine():""}',
"       \ },
"       \ 'component_visible_condition': {
"       \   'linter': '1',
"       \ },
"       \ }
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                   rooter                                   "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:rooter_silent_chdir = 1
let g:rooter_change_directory_for_non_project_files = 'current'
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                    ale                                     "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ale_linters = {
\   'javascript': ['eslint','jscs'],
\}

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                 vim-sneak                                  "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:sneak#streak = 0
nmap gs <Plug>Sneak_s
nmap gs <Plug>Sneak_S
" visual-mode
xmap z <Plug>Sneak_s
xmap Z <Plug>Sneak_S
" operator-pending-mode
omap z <Plug>Sneak_s
omap Z <Plug>Sneak_S
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
"                             custom map                              "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file
map <silent> <C-g> <esc>
nnoremap <leader><tab> <C-^>
nnoremap <silent> zl :exe 'silent! normal! za'.(foldlevel('.') ? '' : 'l')<CR>
nnoremap <leader>skf :Ags 
nnoremap <leader>fs :w!<cr>
nnoremap <silent> <C-l> zz
inoremap <silent> <C-g> <esc>
nnoremap <silent> <leader>bd :bd<CR>
nnoremap <silent> <leader>bD :bd!<CR>
xnoremap <silent> <C-k> :move-2<cr>gv
xnoremap <silent> <C-j> :move'>+<cr>gv
nnoremap gp %
vnoremap gp %
nnoremap ge $
nnoremap ga ^
nnoremap <tab>   <c-w>w
nnoremap ]q :cnext<cr>zz
nnoremap [q :cprev<cr>zz
nnoremap <silent> <leader>md <Plug>(jsdoc)
nnoremap <leader>tw :set wrap<CR>
nnoremap <C-w>= <C-w>+
nnoremap <C-d>  <C-d>zz
nnoremap <C-u>  <C-u>zz
nnoremap é <C-i>zz
nnoremap <C-o> <C-o>zz
nnoremap n nzz
nnoremap N Nzz
nnoremap # #zz
nnoremap * *zz
nnoremap <Down> jzz
nnoremap <Up> kzz
inoremap <C-f> <Right>
inoremap <C-b> <Left>
inoremap <C-e> <Esc><S-A>
inoremap <C-a> <Esc><S-I>
nnoremap <leader>bn :bn<CR>
nnoremap <leader>bp :bp<CR>
nnoremap <leader><leader>- mzgg=G`z
nmap <silent> [q <Plug>(ale_previous_wrap)
nmap <silent> ]q <Plug>(ale_next_wrap)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                   jsdoc                                    "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:jsdoc_enable_es6 = 1
let g:jsdoc_allow_input_prompt = 1
let g:jsdoc_input_description = 1
let g:jsdoc_underscore_private = 1
let g:jsdoc_access_descriptions = 1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                 set                                 "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"fold depend on syntax
set foldmethod=syntax
set nonumber
set foldlevelstart=20
set foldnestmax=9      "deepest fold is 10 levels
set wrap
syntax on
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
cmap <C-g> <C-c>
cmap <C-a> <home>
cmap <C-e> <end>
cmap <silent> <C-e> <end>
cno hhh ~/
cno kkk ./
cno xx .*
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Sets how many lines of history VIM has to remember
set history=500

" Set to auto read when a file is changed from the outside
set autoread

"Always show current position
set ruler

" No annoying sound on errors
set noerrorbells
set novisualbell
set virtualedit=block
set t_vb=
set tm=500
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
"                             colorscheme                             "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set termguicolors
set background=dark " Assume a dark background
" set background=light " Assume a dark background
colorscheme zenburn

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
noremap <leader>Tn :call ToggleBG()<CR>
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
set shortmess+=filmnrxoOtT          " Abbrev. of messages (avoids 'hit enter')
set viewoptions=folds,options,cursor,unix,slash " Better Unix / Windows compatibility
set virtualedit=onemore             " Allow for cursor beyond last character
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
  set undoreload=1000        " Maximum number lines to save for undo on a buffer reload
endif

set tabpagemax=10               " Only show 15 tabs
set showmode                    " Display the current mode
set cursorline                  " Highlight current line

" highlight clear SignColumn      " SignColumn should match background
" highlight clear CursorLineNr    " Remove highlight color from current line number
highlight clear LineNr          " Current line number row will have same background color in relative mode

set rulerformat=%30(%=\:b%n%y%m%r%w\ %l,%c%V\ %P%) " A ruler on steroids
set showcmd                 " Show partial commands in status line
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
set wildmode=list:longest,full  " Command <Tab> completion, list matches, then longest common part, then all.
" set wildmode=longest:full,full  " Command <Tab> completion, list matches, then longest common part, then all.
set whichwrap=b,s,h,l,<,>,[,]   " Backspace and cursor keys wrap too
set scrolljump=6                " Lines to scroll when cursor leaves screen
set scrolloff=3                 " Minimum lines to keep above and below cursor
set foldenable                  " Auto fold code
set list
set listchars=tab:›\ ,trail:•,extends:#,nbsp:. " Highlight problematic whitespace

" Formatting {
set autoindent                  " Indent at the same level of the previous line
set shiftwidth=2                " Use indents of 2 spaces
set expandtab                   " Tabs are spaces, not tabs
set tabstop=2                   " An indentation every four columns
set softtabstop=4               " Let backspace delete indent
set nojoinspaces                " Prevents inserting two spaces after punctuation on a join (J)
set splitright                  " Puts new vsplit windows to the right of the current
set splitbelow                  " Puts new split windows to the bottom of the current
set comments=sl:/*,mb:*,elx:*/  " auto format comment blocks
"""""""""""""""""""
"for edit vimrc
"""""""""""""""""""""
nnoremap <leader>feR :source ~/.config/nvim/init.vim<CR>
nnoremap <leader>fed :e ~/.config/nvim/init.vim<CR>
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                             vim indent guides                              "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 1
let g:indent_guides_enable_on_vim_startup = 1

" Yank from the cursor to the end of the line, to be consistent with C and D.
nnoremap Y y$

" Code folding options
nnoremap <leader>f0 :set foldlevel=0<CR>
nnoremap <leader>f1 :set foldlevel=1<CR>
nnoremap <leader>f2 :set foldlevel=2<CR>
nnoremap <leader>f3 :set foldlevel=3<CR>
nnoremap <leader>f4 :set foldlevel=4<CR>
nnoremap <leader>f5 :set foldlevel=5<CR>
nnoremap <leader>f6 :set foldlevel=6<CR>
nnoremap <leader>f7 :set foldlevel=7<CR>
nnoremap <leader>f8 :set foldlevel=8<CR>
nnoremap <leader>f9 :set foldlevel=9<CR>

nnoremap <silent> <leader>/ :set invhlsearch<CR>

" Find merge conflict markers
map <leader>gm /\v^[<\|=>]{7}( .*\|$)<CR>

" Shortcuts
" Change Working Directory to that of the current file
cmap cwd lcd %:p:h

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
cnoremap kkk <C-R>=fnameescape(expand('%:h')).'/'<cr>

" Adjust viewports to the same size
nnoremap <Leader>w= <C-w>=
nnoremap <Leader>wd <C-w>c
nnoremap <Leader>wl <C-w>l
nnoremap <Leader>wh <C-w>h
nnoremap <Leader>wj <C-w>j
nnoremap <Leader>wk <C-w>k
nnoremap <Leader>wv <C-w>v
nnoremap <Leader>ws <C-w>s

" Easier horizontal scrolling
nnoremap zl zL
nnoremap zh zH
nnoremap <silent> <leader>q :qa<cr>
nnoremap <silent> <leader>Q :qa!<cr>

" omnicomplete
autocmd Filetype *
      \if &omnifunc == "" |
      \setlocal omnifunc=syntaxcomplete#Complete |
      \endif
set completeopt=menu,menuone,longest,preview,noselect " Searching includes can be slow
set fileformats=unix,mac,dos
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                  NerdTree                                  "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let NERDTreeShowBookmarks=1
let NERDTreeIgnore=['\.py[cd]$', '\~$', '\.swo$', '\.swp$', '^\.git$', '^\.hg$', '^\.svn$', '\.bzr$']
let NERDTreeChDirMode=0
let NERDTreeQuitOnOpen=1
let NERDTreeMouseMode=2
let NERDTreeShowHidden=1
let NERDTreeKeepTreeInNewTab=1
let g:nerdtree_tabs_open_on_gui_startup=0
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                              ctrlp fzf                              "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let $FZF_DEFAULT_OPTS .= ' --inline-info'
let g:fzf_files_options =
      \ '--preview "(highlight -O ansi {} || cat {}) 2> /dev/null | head -'.&lines.'"'
nnoremap <silent> <expr> <Leader>ff (expand('%') =~ 'NERD_tree' ? "\<c-w>\<c-w>" : '').":Files\<cr>"
nnoremap <silent> <Leader>sa :Ag <C-R><C-W><CR>
nnoremap <silent> <Leader>fr :CtrlPMRU<CR>
nnoremap <silent> <C-p> :CtrlPBuffer<CR>
nnoremap <silent> <leader>bb :CtrlPBuffer<CR>
let g:fzf_buffers_jump = 1
nnoremap <silent> <C-f> :Lines<CR>
nmap <leader>sb :Lines<CR>
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

" Let 'tl' toggle between this and the last accessed tab
let g:lasttab = 1
" nnoremap <Leader>tl :exe "tabn ".g:lasttab<CR>
" nnoremap <Leader><tab> :exe "tabn ".g:lasttab<CR>
au TabLeave * let g:lasttab = tabpagenr()

" Gita
cno gitfindcommit Glog --grep=
nnoremap <silent> <leader>gca :te git commit --all<CR>
nnoremap <silent> <leader>gpp :te git pull origin "$(git-branch-current 2> /dev/null)" && git push origin "$(git-branch-current 2> /dev/null)"<CR>
nnoremap <silent> <leader>gcff :te git commit --amend --reuse-message HEAD --all<CR>
nnoremap <silent> <leader>gws :te git status --short<CR>
" nnoremap <silent> <leader>gs :Gita status<CR>
" nnoremap <silent> <leader>gb :Gita blame<CR>
" nnoremap <silent> <leader>gs :Gstatus<CR>gg<C-n>
" nnoremap <silent> <leader>gd :Gvdiff<CR>
" nnoremap <silent> <leader>gc :Gcommit<CR>
" nnoremap <silent> <leader>gb :Gblame<CR>
" nnoremap <silent> <leader>gl :Glog<CR>
" nnoremap <silent> <leader>gp :Git push<CR>
" nnoremap <silent> <leader>gr :Gread<CR>
" nnoremap <silent> <leader>gw :Gwrite<CR>
" nnoremap <silent> <leader>ge :Gedit<CR>
" Mnemonic _i_nteractive
nnoremap <silent> <leader>gi :Git add -p %<CR>
nnoremap <silent> <leader>gg :SignifyToggle<CR>
" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete

source ~/.config/nvim/config/general.vim
source ~/.config/nvim/config/plugins/vimfiler.vim
source ~/.config/nvim/config/filetype.vim
source ~/.config/nvim/config/plugins/unite.vim
source ~/.config/nvim/config/plugins/ydenite.vim
source ~/.config/nvim/config/plugins/denite.vim
set nonumber
