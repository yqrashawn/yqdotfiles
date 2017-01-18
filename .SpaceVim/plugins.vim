call plug#begin('~/.vim/plugged')
Plug 'Shougo/vimproc'
Plug 'jnurmine/Zenburn'
Plug 'tpope/vim-commentary'
Plug 'ctrlpvim/ctrlp.vim'
"Plug 'sheerun/vim-polyglot'
Plug 'w0rp/ale'
Plug 'Valloric/YouCompleteMe'
Plug 'airblade/vim-rooter'
Plug 'Konfekt/FastFold'
Plug 'lambdalisue/vim-gita', {'on': ['Gita']}
Plug 'Shougo/unite.vim'
Plug 'Shougo/denite.nvim'
Plug 't9md/vim-choosewin'
Plug 'Shougo/vimfiler.vim'
call plug#end()

" zenburn
colors zenburn

" commentary
nnoremap <C-x>; :Commentary<cr>
vnoremap <C-x>; :Commentary<cr>

" CtrlP
" nnoremap <leader>fr :CtrlPMRUFiles<cr>
" nnoremap <leader>ff :CtrlPCurWD<cr>
" nnoremap <leader>bb :CtrlPBuffer<cr>
" nnoremap <leader>bq :CtrlPQuickfix<cr>
let g:ctrlp_working_path_mode = 'ra'

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
nnoremap <leader>gd :Gita chaperone<cr>
nnoremap <leader>gs :Gita status<cr>
nnoremap <leader>gg :Gita grep<cr>
cmap blame Gita blame
cmap Gdiff Gita diff-ls

" Unite Dnite
source ~/.config/nvim/unite.vim
source ~/.config/nvim/denite.vim
source ~/.config/nvim/nite-mapping.vim

" chosewin
source ~/.config/nvim/choosewin.vim

" vimfiler
let g:vimfiler_data_directory = $HOME.'/vimfiler'
source ~/.config/nvim/vimfiler.vim
