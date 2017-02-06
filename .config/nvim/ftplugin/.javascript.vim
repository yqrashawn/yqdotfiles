function! JavascriptFoldMethod(lnum)
	let l:line = getline(a:lnum)
	let l:i = 0
	let l:len = strlen(l:line)
	let l:ret = 0
	while i < len
		let l:char = strpart(l:line, i, 1)
		if l:char == '{' || l:char == '[' || l:char == '('
			let l:ret = l:ret + 1
		elseif l:char == '}' || l:char == ']' || l:char == ')'
			let l:ret = l:ret - 1
		endif
		let l:i = l:i + 1
	endwhile
	if l:ret > 0
		return printf('a%d', l:ret)
	elseif l:ret < 0
		return printf('s%d', -l:ret)
	else
		return '='
	endif
endfunction

setl foldmethod=expr
setl foldexpr=JavascriptFoldMethod(v:lnum)
setl foldlevel=0
