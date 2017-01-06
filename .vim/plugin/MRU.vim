" Author: Gergely Kontra <kgergely@mcl.hu>
" Version: 0.32
" Description:
"   Most recently used files appear in the file menu
"
" FEEDBACK PLEASE
"
" Installation: {{{
" - Drop it into your plugin directory
" - $MRU variable can contain the full filename, where the MRU files should
"   be written. (If not specified, assumed location on UNIX is
"   $HOME/.vimrecent and $VIM/_vimrecent on other systems.
" - MRU_num variable can contain the number of files to store (default is 4)
" - PATHSIZELIMIT variable can be used to limit the size of the path name
"   appearing in the menu
" - To define what to do, when you select a menu item, you can adjust
"   OPEN_FUNC variable. It must contain a globally available function, which
"   has one parameter: the file to be processed.
"   The default is 'SpWhenModified'. You can change it to
"   'SpWhenNamedOrModified', if you want (almost) always split windows, or,
"   you can write your own function.
"   }}}
"
" History: {{{
"    0.1:  * Initial release (not published)
"    0.2:  * You can access the files through your keyboard (1-9), when you are
"            in the file menu
"          * Bugfixes
"          * When you click on an item, the function named MRU_FUNCTION will be
"            called, or it will be opened in a window (in the current window,
"            when the file is not modified, or in a new window othervise)
"    0.21: * You can adjust PATHSIZELIMIT to limit the path size appeared in
"            the menu
"          * OPEN_FUNC is now really OPEN_FUNC :)
"          * Delete buffer, even when 'hidden' is set
"            Thanks to Roger Pilkey for the bug report
"    0.3:  * Use clientserver feature to synchronize the menu instances
"    0.31: * Shut up clientserver stuff
"    0.32: * Fixed filename escaping (Thx to Fritz Mehner for the patch)
" }}}
" TODO:
"    Are all valid filenames escaped?
"

if !exists('SpWhenModified') "integration with FavMenu {{{
  fu! SpWhenModified(f) "splits only when curr buf is modified
		let fesc = escape( a:f, " %" )
    if &mod
      exe 'sp '.fesc
    el
      exe 'e '.fesc
    en
  endf
  fu! SpWhenNamedOrModified(f) "splits, when curr buf has name, or is modified
    if bufname('%')!='' || &mod
      exe 'sp '.a:f
    el
      exe 'e '.a:f
    en
  endf
  fu! OpenFile()
    if exists('g:OPEN_FUNC')
      retu g:OPEN_FUNC
    el
      retu 'SpWhenModified'
    en
  endf
  fu! TruncPath(path)
    let p=a:path
    let pathlen=strlen(p)
    if exists('g:PATHSIZELIMIT') && pathlen>g:PATHSIZELIMIT
      let cut=match(p,'[/\\]',pathlen-g:PATHSIZELIMIT)
      if cut>0 && cut<pathlen
	let p='\.\.\.'.strpart(p,cut)
      en
    en
    retu p
  endf
end "}}}

fu! s:SendAll(what) "{{{
  exe 'cal '.a:what
  if has('clientserver')
    let servers=serverlist()
    let pos=0
    let re="[^\n]\\+"  "Thanx to Mark Hillebrand
    wh match(servers,re,pos) != -1
      let s=matchstr(servers,re,pos)
      let pos=pos+strlen(s)+1
      if v:servername!=s
	cal remote_expr(s,a:what)
      en
    endw
  en
endf "}}}

fu! MRUDestroy() " must be global :( {{{
  sv $MRU|set bh=delete
  " First cleanup old MRU's
  " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  " WARNING: Keep next 2 lines in sync with the :g below
  " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  g/^.\+$/sil! exe 'aun &File.'.
    \'[&'.line('.').']\ '.
    \escape(fnamemodify(getline('.'),':p:t'),' \.')
  " Figure out fullname
  q!
endf "}}}

fu! MRURefresh() " must be global :( {{{
  sv $MRU|set bh=delete
  " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  " WARNING: Keep next command in synx with the :g above
  " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  g/^.\+$/exe 'amenu 10.'.(line('.')+511).' &File.'.
    \'[&'.line('.').']\ '.
    \escape(fnamemodify(getline('.'),':p:t'),' \.').
    \'<Tab>'.
    \TruncPath(escape(fnamemodify(getline('.'),':p:h'),' \.')).
    \' :call <C-R>=OpenFile()<CR>("'.
    \escape(getline('.'),'\').'")<CR>'
  q!
endf "}}}

fu! s:MRUAdd(f) "{{{
  "let lz=&lz|se lz
  if a:f!='//' && !buflisted(a:f)  " if param not good...
    retu
  end

  sil! cal s:SendAll('MRUDestroy()')

  if a:f!='//' " add this file to the top (if real file)
    let fullname=fnamemodify(a:f,':p')
    let v=virtcol('.')|let l=line('.')
    norm H
    let hl=line('.')
    vs $MRU
    "let b=bufnr('%')|e! $MRU
    setl nobl bh=delete ma
    if search('^\V'.escape(fullname,'\').'\$','w')
      move 0
    el
      exe 'norm ggO'.fullname."\<Esc>"
    en
    let num=1+(exists('g:MRU_num') ? g:MRU_num : 4)
    sil! exe num.',$d _'
    let pm=&pm|let &pm=''
    wq
    " w|exe 'b' b " syntax lost!
    let &pm=pm
    exe 'norm' hl.'Gzt'.l.'G'.v.'|'
  en

  cal s:SendAll('MRURefresh()')
  "let &lz=lz
endf "}}}

if !exists('$MRU')
  if has('unix')
    let $MRU=$HOME.'/.vimrecent'
  el
    let $MRU=$VIM.'\_vimrecent'
  en
en

am 10.511 &File.-SepMRU- <Nop>
sil cal MRUDestroy()|sil cal MRURefresh()

aug MRU
  au!
  au BufWritePost * sil! call <SID>MRUAdd(@%)
aug END

" vim:ft=vim:fdm=marker:
