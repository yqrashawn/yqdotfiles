
" VimFiler
" --------

let g:vimfiler_as_default_explorer = 1
let g:vimfiler_restore_alternate_file = 0
"let g:vimfiler_preview_action = 'auto_preview'

let g:vimfiler_ignore_pattern =
	\ '^\%(\.git\|\.idea\|\.DS_Store\|\.vagrant\|\.stversions\|\.tmp'
	\ .'\|node_modules\|.*\.pyc\|.*\.egg-info\|__pycache__\)$'

if has('mac')
	let g:vimfiler_quick_look_command = '/usr/bin/qlmanage -p'
	let g:vimfiler_execute_file_list = {
		\ '_': '/Applications/Atom.app/Contents/MacOS/Atom'
		\ }
else
	let g:vimfiler_quick_look_command = 'gloobus-preview'
endif

call vimfiler#custom#profile('default', 'context', {
	\  'safe': 0,
	\  'explorer': 1,
	\  'winwidth': 25,
	\  'split': 1,
	\  'direction': 'topleft',
	\  'auto_expand': 1,
	\  'no_quit': 1,
	\  'force_hide': 1,
	\  'parent': 0,
	\  'toggle': 1,
	\ })

" keymap
nnoremap <silent> <C-p>        :<C-u>execute
  \ 'VimFiler -winwidth=25 -direction=topleft -buffer-name='.block#project()<CR>
nnoremap <silent> <leader>E        :<C-u>execute
  \ 'VimFiler -find -winwidth=25 -direction=topleft -buffer-name='.block#project()<CR>

autocmd MyAutoCmd FileType vimfiler call s:vimfiler_settings()

function! s:vimfiler_settings() abort "{{{
  setlocal nonumber norelativenumber

  silent! nunmap <buffer> <Space>
  silent! nunmap <buffer> <C-l>
  silent! nunmap <buffer> <C-j>
  silent! nunmap <buffer> gr
  silent! nunmap <buffer> gf
  silent! nunmap <buffer> -

  nnoremap <silent><buffer> gr  :<C-u>Denite grep:<C-R>=<SID>selected()<CR> -buffer-name=grep<CR>
  nnoremap <silent><buffer> gf  :<C-u>Denite file_rec:<C-R>=<SID>selected()<CR><CR>
  nnoremap <silent><buffer> gd  :<C-u>call <SID>change_vim_current_dir()<CR>
  nnoremap <silent><buffer><expr> sg  vimfiler#do_action('vsplit')
  nnoremap <silent><buffer><expr> sv  vimfiler#do_action('split')
  nnoremap <silent><buffer><expr> st  vimfiler#do_action('tabswitch')
  nmap <buffer> gx     <Plug>(vimfiler_execute_vimfiler_associated)
  nmap <buffer> '      <Plug>(vimfiler_toggle_mark_current_line)
  nmap <buffer> v      <Plug>(vimfiler_quick_look)
  nmap <buffer> p      <Plug>(vimfiler_preview_file)
  nmap <buffer> V      <Plug>(vimfiler_clear_mark_all_lines)
  nmap <buffer> i      <Plug>(vimfiler_switch_to_history_directory)
  nmap <buffer> <Tab>  <Plug>(vimfiler_switch_to_other_window)
  nmap <buffer> <C-r>  <Plug>(vimfiler_redraw_screen)
endfunction "}}}

" Returns selected items, or current cursor directory position
" Provide an argument to limit results with an integer.
function! s:selected(...) " {{{
  let marked = map(vimfiler#get_marked_files(b:vimfiler), 'v:val.action__path')
  if empty(marked)
    let file_dir = vimfiler#get_file_directory()
    call add(marked, empty(file_dir) ? '.' : file_dir)
  endif
  if a:0 > 0
    let marked = marked[: a:1]
  endif
  return join(marked, "\n")
endfunction "}}}

" Changes the directory for all buffers in a tab
function! s:change_vim_current_dir() "{{{
  let selected = s:selected(1)
  let b:vimfiler.current_dir = selected
  execute 'windo lcd '.fnameescape(selected)
  execute 'wincmd w'
  call vimfiler#force_redraw_screen()
  echo 'Changed local buffer working directory to `'.selected.'`'
endfunction "}}}

"}}}

" Plugin: vimfiler.vim {{{
" ---------------------------------------------------------
highlight vimfilerNonMark     ctermfg=132 guifg=#B05E87
highlight vimfilerLeaf        ctermfg=238 guifg=#444444
highlight vimfilerClosedFile  ctermfg=246 guifg=#949494
highlight link vimfilerOpenedFile  Normal
highlight link vimfilerNormalFile  Comment
highlight link vimfilerMarkedFile  Type
" }}}

" vim: set ts=2 sw=2 tw=80 noet :
