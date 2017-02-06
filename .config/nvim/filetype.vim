augroup MyAutoCmd " {{{

	" Automatically set read-only for files being edited elsewhere
	autocmd SwapExists * nested let v:swapchoice = 'o'

	" More eager than 'autoread'.
	autocmd WinEnter * checktime
"	autocmd WinEnter,FocusGained * checktime

	autocmd Syntax * if 5000 < line('$') | syntax sync minlines=200 | endif

	" Update filetype on save if empty
	autocmd BufWritePost * nested
		\ if &l:filetype ==# '' || exists('b:ftdetect')
		\ |   unlet! b:ftdetect
		\ |   filetype detect
		\ | endif

	" Reload Vim script automatically if setlocal autoread
	autocmd BufWritePost,FileWritePost *.vim nested
		\ if &l:autoread > 0 | source <afile> |
		\   echo 'source '.bufname('%') |
		\ endif

	" When editing a file, always jump to the last known cursor position.
	" Don't do it when the position is invalid or when inside an event handler
	autocmd BufReadPost *
		\ if &ft !~ '^git\c' && ! &diff && line("'\"") > 0 && line("'\"") <= line("$")
		\|   exe 'normal! g`"zvzz'
		\| endif

	" Disable paste and/or update diff when leaving insert mode
	autocmd InsertLeave *
			\ if &paste | setlocal nopaste mouse=a | echo 'nopaste' | endif |
			\ if &l:diff | diffupdate | endif

	autocmd FileType help
		\ setlocal iskeyword+=: | setlocal iskeyword+=# | setlocal iskeyword+=-

	autocmd FileType crontab setlocal nobackup nowritebackup

	autocmd FileType gitcommit setlocal spell

	autocmd FileType gitcommit,qfreplace setlocal nofoldenable

	" https://webpack.github.io/docs/webpack-dev-server.html#working-with-editors-ides-supporting-safe-write
	autocmd FileType html,css,jsx,javascript.jsx setlocal backupcopy=yes

	autocmd FileType javascript,jsx,javascript.jsx setlocal foldmethod=expr

	autocmd FileType zsh setlocal foldenable foldmethod=marker

	" Improved HTML include pattern
	autocmd FileType html
		\ setlocal includeexpr=substitute(v:fname,'^\\/','','') |
		\ setlocal path+=./;/

	autocmd FileType markdown
		\ setlocal spell expandtab autoindent
			\ formatoptions=tcroqn2 comments=n:>

	autocmd FileType apache setlocal path+=./;/

	autocmd FileType cam setlocal nonumber synmaxcol=10000

	autocmd FileType go highlight default link goErr WarningMsg |
				\ match goErr /\<err\>/

	" Open Quickfix window automatically
	autocmd QuickFixCmdPost [^l]* leftabove copen
		\ | wincmd p | redraw!
	autocmd QuickFixCmdPost l* leftabove lopen
		\ | wincmd p | redraw!

	" Fix window position of help/quickfix
	autocmd FileType help if &l:buftype ==# 'help'
		\ | wincmd L | endif
	autocmd FileType qf   if &l:buftype ==# 'quickfix'
		\ | wincmd J | endif

augroup END " }}}
