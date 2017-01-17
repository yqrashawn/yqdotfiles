call plug#begin('~/.vim/plugged')
Plug 'jnurmine/Zenburn'
Plug 'rafi/vim-tinycomment'
Plug 'ctrlpvim/ctrlp.vim'
"Plug 'sheerun/vim-polyglot'
Plug 'w0rp/ale'
call plug#end()

" zenburn
colors zenburn

" tinycomment
nnoremap <C-x>; :TinyCommentLines<cr>
vnoremap <C-x>; :<C-w>TinyCommentLines<cr>
let g:tinycomment_disable_keymaps=0

" CtrlP
nnoremap <leader>fr :CtrlPMixed<cr>
nnoremap <leader>bb :CtrlPBuffer<cr>
nnoremap <leader>bq :CtrlPQuickfix<cr>
let g:ctrlp_working_path_mode = 'ra'

" vim-javascript
"let g:javascript_plugin_jsdoc = 1


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
