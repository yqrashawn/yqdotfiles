call plug#begin('~/.vim/plugged')
Plug 'Shougo/vimproc'
Plug 'jnurmine/Zenburn'
Plug 'tpope/vim-commentary', {'on':['Commentary']}
Plug 'w0rp/ale'
Plug 'Valloric/YouCompleteMe'
Plug 'airblade/vim-rooter'
Plug 'Konfekt/FastFold'
Plug 'lambdalisue/vim-gita', {'on': ['Gita']}
Plug 'Shougo/unite.vim'
Plug 'Shougo/denite.nvim'
Plug 't9md/vim-choosewin'
Plug 'Shougo/vimfiler.vim'
Plug 'Shougo/junkfile.vim'
Plug 'rhysd/clever-f.vim'
Plug 'rafi/vim-blocks'
Plug 'itchyny/vim-gitbranch'
Plug 'itchyny/vim-parenmatch'
Plug 'osyo-manga/vim-anzu'
Plug 'airblade/vim-gitgutter'
Plug 'terryma/vim-multiple-cursors'
Plug 'terryma/vim-expand-region'
Plug 'kana/vim-operator-user'
Plug 'kana/vim-textobj-user'
Plug 'osyo-manga/vim-textobj-multiblock'
Plug 'rhysd/vim-operator-surround'
call plug#end()
" Plug
cmap ;pi PlugInstall<cr>
cmap ;pc PlugClean<cr>


" zenburn
colors zenburn

" commentary
nnoremap <C-x>; :Commentary<cr>
vnoremap <C-x>; :Commentary<cr>

" ale
let g:ale_linters = {
\   'javascript': ['eslint', 'jscs'],
\}
let g:ale_sign_column_always = 1
let g:ale_sign_error = '>>'
let g:ale_sign_warning = '--'
let g:ale_statusline_format = ['⨉ %d', '⚠ %d', '⬥ ok']
"set statusline+=%{ALEGetStatusLine()}
let g:ale_lint_on_save = 0
let g:ale_lint_on_text_changed = 0
let g:ale_lint_on_enter = 0
cmap ALE call: ALELint(20)

" youcompleteme
let g:ycm_key_list_select_completion=['<C-j>']
let g:ycm_key_list_previous_completion=['<C-k>']
"let g:ycm_key_invoke_completion='<C-x><C-o>'
let g:ycm_key_detailed_diagnostics = '<nop>'

" vim-rooter
let g:rooter_change_directory_for_non_project_files = 'current'
let g:rooter_silent_chdir = 1

" fastfold
let g:javascript_syntax_folding = 1
let g:fastfold_savehook = 0
let g:fastfold_fold_command_suffixes = ['x','X','a','A','o','O','c','C']
let g:fastfold_fold_movement_commands = [']z', '[z', 'zj', 'zk']

" Gita
nnoremap <silent> <leader>gd :Gita chaperone<cr>
nnoremap <silent> <leader>gs :Gita status<cr>
nnoremap <silent> <leader>gg :Gita grep<cr>
nnoremap <silent> <leader>gc :<C-u>Gita commit<CR>
nnoremap <silent> <leader>ga :<C-u>Gita commit --amend<CR>
nnoremap <silent> <leader>gp :<C-u>Gita push<CR>
cmap blame Gita blame
cmap Gdiff Gita diff-ls
autocmd MyAutoCmd FileType gita-status
	\ silent! nunmap <buffer> <C-L> |
	\ nmap <buffer> <C-R> <Plug>(gita-common-redraw) |
	\ nmap <buffer> cc    <Plug>(gita-commit-open) |
	\ nmap <buffer> cA    <Plug>(gita-commit-open-amend) |
	\ nmap <buffer> dg    <Plug>(gita-diff-right) |
	\ nmap <buffer> sg    <Plug>(gita-edit-right)

" Unite Dnite
source ~/.config/nvim/unite.vim
source ~/.config/nvim/denite.vim
source ~/.config/nvim/nite-mapping.vim

" chosewin
source ~/.config/nvim/choosewin.vim

" vimfiler
let g:vimfiler_data_directory = $HOME.'/vimfiler'
source ~/.config/nvim/vimfiler.vim

" junkfile
cmap ;junk JunkfileOpen<cr>

" anzu
nmap n n<Plug>(anzu-update-search-status)
nmap N N<Plug>(anzu-update-search-status)
nmap <silent> <Leader>cc :<C-u>call anzu#clear_search_status()<CR>
autocmd MyAutoCmd CursorHold * call anzu#clear_search_status()


" git-gutter
let g:gitgutter_map_keys = 0
let g:gitgutter_sh = $SHELL
nmap ]h         <Plug>GitGutterNextHunk
nmap [h         <Plug>GitGutterPrevHunk
nmap <Leader>hs <Plug>GitGutterStageHunk
nmap <Leader>hr <Plug>GitGutterUndoHunk

" vim-dsf

" vim-expand-region
nmap + <Plug>(expand_region_expand)

" textobj-surround
let g:textobj_multiblock_no_default_key_mappings = 1
vmap <silent>si <Plug>(operator-surround-append)
nmap <silent>saa <Plug>(operator-surround-append)<Plug>(textobj-multiblock-i)
nmap <silent>sdd <Plug>(operator-surround-delete)<Plug>(textobj-multiblock-a)
nmap <silent>srr <Plug>(operator-surround-replace)<Plug>(textobj-multiblock-a)

" textobj-multiblock
omap af <Plug>(textobj-multiblock-a)
omap if <Plug>(textobj-multiblock-i)
xmap af <Plug>(textobj-multiblock-a)
xmap if <Plug>(textobj-multiblock-i)
