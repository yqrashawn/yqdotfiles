let mapleader=' '

source ~/.config/nvim/filetype.vim
source ~/.config/nvim/func.vim
source ~/.config/nvim/plugins.vim
source ~/.config/nvim/default.vim
source ~/.config/nvim/ui.vim
"source ~/.config/nvim/large-file.vim
"" keymap

" gneral
nnoremap <leader>fs :w<cr>
nnoremap <leader>bd :bd<cr>
nnoremap <leader>fed :e ~/.config/nvim/init.vim<cr>:3<cr>
nnoremap <leader>feR :source ~/.config/nvim/init.vim<cr>
nnoremap <leader><tab> <C-^>
nnoremap <leader>q :confirm quit<cr>
nnoremap <C-space> /
nnoremap <C-k> za
nnoremap sp %
imap jk <esc>
imap kj <esc>
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap <C-b> <Left>
cnoremap <C-f> <Right>
xnoremap <C-r> :<C-u>call VSetSearch('/')<CR>:%s/\V<C-R>=@/<CR>//gc<Left><Left><Left>
nnoremap <leader>tn :<C-u>call ToggleNumber()<cr>
nnoremap <leader>tN :<C-u>call ToggleRelativeNumber()<cr>
nnoremap <leader>Tn :<C-u>call ToggleBG()<cr>
" nnoremap <c-l> :call GetPotionFold(32)<cr>


" edit
nnoremap Y y$
nnoremap <C-a> ^
nnoremap <C-e> $
nnoremap <tab> ==
nnoremap gy yyp
nnoremap gY yy:Commentary<cr>p
inoremap <C-b> <left>
inoremap <C-f> <right>
inoremap <C-a> <home>
inoremap <C-e> <end>
vnoremap <C-a> <home>
vnoremap <C-e> <end>
xnoremap < <gv
xnoremap > >gv|
nnoremap > >>_
nnoremap < <<_

" cmap
cmap ;h ~/
cmap ;1 .*
cmap <C-g> <C-u><esc>
cmap <C-a> <home>
cmap <C-e> <end>

" window
nnoremap s <nop>
nnoremap sv :vsplit<cr>
nnoremap ss :split<cr>
nnoremap sc <C-w><C-c>
nnoremap so <C-w><C-o>
nnoremap st :<c-u>tabnew<cr>

" tab
noremap <leader>1 1gt
noremap <leader>2 2gt
noremap <leader>3 3gt
noremap <leader>4 4gt
noremap <leader>5 5gt
noremap <leader>6 6gt
noremap <leader>7 7gt
noremap <leader>8 8gt
noremap <leader>9 9gt
noremap <leader>0 :tablast<cr>
