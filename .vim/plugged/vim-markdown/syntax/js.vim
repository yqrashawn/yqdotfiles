let b:current_syntax="js"
syn match identifier /[A-Z_a-z][0-9A-Z_a-z]*/
syn match operator '[!%&()*+,-.:;<=>?\[\\\]^{|}~/]'
syn match number '[0-9][0-9]*'
syn match string '\/\([^\/\\]\|\\.\)*\/'
" es6 11.6.2.1
syn keyword keyword break case class catch const continue debugger default delete do else export extends finally for function if import in instanceof let new return static super switch this throw try typeof var void while with yield
syn keyword jsStyleConstant undefined true false null
syn keyword jsStyleObject Array Boolean String Object Function Math RegExp Number JSON Map Set Date
syn keyword jsStyleProperties prototype split forEach parse
syn keyword domStyleObject window document XMLHttpRequest
syn keyword domStyleProperties innerHTML outerHTML textContent className style parentNode oninput onclick onchange onreadystatechange responseText createElement getElementById addEventListener removeEventListener removeAttribute encodeURIComponent stringify removeChild appendChild currentScript
syn keyword nodeStyleObject GLOBAL Buffer
syn keyword nodeStyleProperties require
syn region multiLineComment start=/\/\*/ end=/\*\//
syn region singleLineComment start=/\/\// end=/\n/
syn region string start=/'/ end=/'/ skip=/\\./
syn region string start=/"/ end=/"/ skip=/\\./
syn region string start=/`/ end=/`/ skip=/\\./ contains=escapedString
syn region escapedString start=/${/ end=/}/
hi def link escapedString statement
hi def link jsStyleConstant Constant
hi def link jsStyleObject Identifier
hi def link jsStyleProperties PreProc
hi def link domStyleObject Identifier
hi def link domStyleProperties PreProc
hi def link nodeStyleObject Identifier
hi def link nodeStyleProperties PreProc
hi def link singleLineComment Comment
hi def link multiLineComment Comment
