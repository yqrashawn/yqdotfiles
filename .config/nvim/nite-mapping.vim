" Unite
" -----
"
" Unite window mappings {{{
augroup unite_buffer_feature
    autocmd FileType unite call s:unite_settings()
augroup END
autocmd FileType unite call s:unite_settings()
function! s:unite_settings() abort "{{{
  silent! nunmap <buffer> <Space>
  silent! nunmap <buffer> <C-h>
  silent! nunmap <buffer> <C-k>
  silent! nunmap <buffer> <C-l>
  silent! nunmap <buffer> <C-r>

  imap <silent><buffer> <C-w>         <Plug>(unite_delete_backward_path)
  nmap <silent><buffer> <C-r> <Plug>(unite_redraw)
  imap <silent><buffer> <C-n> <Plug>(unite_select_next_line)
  imap <silent><buffer> <C-p> <Plug>(unite_select_previous_line)
  nmap <silent><buffer> '     <Plug>(unite_toggle_mark_current_candidate)
  nmap <silent><buffer> e     <Plug>(unite_do_default_action)
  nmap <silent><buffer><expr> ss unite#do_action('split')
  nmap <silent><buffer><expr> sv unite#do_action('vsplit')
  nmap <silent><buffer><expr> st unite#do_action('tabopen')
  nnoremap <silent><buffer> <Tab>  <C-w>w
  nmap <buffer> q             <Plug>(unite_exit)
  nmap <buffer> <C-g>         <Plug>(unite_exit)
  imap <buffer> <C-g>         <Plug>(unite_exit)
  imap <buffer> jj            <Plug>(unite_insert_leave)
  imap <buffer> <Tab>         <Plug>(unite_complete)
  nmap <buffer> <C-z>         <Plug>(unite_toggle_transpose_window)
  imap <buffer> <C-z>         <Plug>(unite_toggle_transpose_window)
  nmap <buffer> <C-w>         <Plug>(unite_delete_backward_path)
  nmap <buffer> <C-g>         <Plug>(unite_print_candidate)
  nmap <buffer> x             <Plug>(unite_quick_match_jump)

  let unite = unite#get_current_unite()
  if unite.profile_name ==# '^search'
    nnoremap <silent><buffer><expr> r unite#do_action('replace')
  else
    nnoremap <silent><buffer><expr> r unite#do_action('rename')
  endif
endfunction "}}}

" denite
" -----
nnoremap <silent> <leader>rl  :<C-u>Denite -resume<CR>
nnoremap <silent> <leader>ff  :<C-u>Denite file_rec<CR>
nnoremap <silent> <leader>bb  :<C-u>Denite buffer<CR>
nnoremap <silent> <leader>fq  :<C-u>Denite quickfix -buffer-name=list<CR>
nnoremap <silent> <leader>skf  :<C-u>Denite grep -buffer-name=grep<CR>
nnoremap <silent> <leader>sl  :<C-u>Denite line<CR>
nnoremap <silent> <leader>sc  :<C-u>DeniteCursorWord line<CR>
" nnoremap <silent> <leader>d  :<C-u>Denite directory_rec -default-action=cd<CR>
" nnoremap <silent> <leader>fr  :<C-u>Denite file_old<CR>
" nnoremap <silent> <leader>fr  :<C-u>Denite neomru/file<CR>
" nnoremap <silent> <leader>l  :<C-u>Denite location_list -buffer-name=list<CR>
" nnoremap <silent> <leader>j  :<C-u>Denite file_point<CR>
" nnoremap <silent> <leader>k  :<C-u>Denite mark -buffer-name=list<CR>
" nnoremap <silent> <leader>s  :<C-u>Denite session<CR>

" Open Unite with word under cursor or selection
" nnoremap <silent> <Leader>gf :DeniteCursorWord file_rec<CR>
" nnoremap <silent> <Leader>gg :DeniteCursorWord grep -buffer-name=grep<CR><CR>
" vnoremap <silent> <Leader>gg
" \ :<C-u>call VSetSearch('/')<CR>:execute 'Denite grep -buffer-name=grep -input='.@/<CR><CR>

" Unite
" -----
nnoremap <silent> <leader>?  :<C-u>Unite mapping -silent<CR>
nnoremap <silent> <leader>mt  :<C-u>Unite tab -select=`tabpagenr()-1`<CR>
nnoremap <silent> <leader><leader>  :<C-u>Unite -silent<CR>
nnoremap <silent> <leader>ss  :<C-u>Unite outline<CR>
nnoremap <silent> sl  :<C-u>Unite outline<CR>
nnoremap <silent> <leader>fr  :<C-u>Unite neomru/file<CR>
" nnoremap <silent> <leader>ff  :<C-u>Unite file/async<CR>
" nnoremap <silent><leader>fa :<C-u>Unite -buffer-name=files file_rec/async:!<cr>

" vim: set ts=2 sw=2 tw=80 noet :
