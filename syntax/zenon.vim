" Vim syntax file
" Language:	Zenon
" Current Maintainer:	Zenon dev (https://github.com/miki151/zenon)
" Last Change:	2019

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Read the C syntax to start with
if version < 600
  so <sfile>:p:h/c.vim
else
  runtime! syntax/c.vim
  unlet b:current_syntax
endif

" Zenon extensions, includes C++ keywords because they're also reserved
syn keyword cInclude            import
syn keyword cppStatement	new delete this friend using move requires
syn keyword cppAccess		public protected private
syn keyword cppModifier		inline virtual explicit export
syn keyword cppType		bool wchar_t string byte
syn keyword cppExceptions	throw try catch
syn keyword cppOperator		operator typeid
syn keyword cppOperator		and bitor or xor compl bitand and_eq or_eq xor_eq not not_eq
syn match cppCast		"\<\(const\|static\|dynamic\|reinterpret\)_cast\s*<"me=e-1
syn match cppCast		"\<\(const\|static\|dynamic\|reinterpret\)_cast\s*$"
syn keyword cppStorageClass	mutable
syn keyword cppStructure	class typename template namespace variant embed embed_returns mixin concept unchecked
syn keyword cppBoolean		true false
syn keyword cppConstant		__cplusplus null
syn match   cFormat		display "{.\{-}}" contained

syn keyword cppModifier	override final
syn keyword cppType		nullptr_t
syn keyword cppExceptions	noexcept
syn keyword cppStorageClass	constexpr decltype thread_local
syn keyword cppConstant	nullptr
syn keyword cppConstant	ATOMIC_FLAG_INIT ATOMIC_VAR_INIT
syn keyword cppConstant	ATOMIC_BOOL_LOCK_FREE ATOMIC_CHAR_LOCK_FREE
syn keyword cppConstant	ATOMIC_CHAR16_T_LOCK_FREE ATOMIC_CHAR32_T_LOCK_FREE
syn keyword cppConstant	ATOMIC_WCHAR_T_LOCK_FREE ATOMIC_SHORT_LOCK_FREE
syn keyword cppConstant	ATOMIC_INT_LOCK_FREE ATOMIC_LONG_LOCK_FREE
syn keyword cppConstant	ATOMIC_LLONG_LOCK_FREE ATOMIC_POINTER_LOCK_FREE


" The minimum and maximum operators in GNU C++
syn match cppMinMax "[<>]?"

" Default highlighting
if version >= 508 || !exists("did_cpp_syntax_inits")
  if version < 508
    let did_cpp_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink cppAccess		cppStatement
  HiLink cppCast		cppStatement
  HiLink cppExceptions		Exception
  HiLink cppOperator		Operator
  HiLink cppStatement		Statement
  HiLink cppModifier		Type
  HiLink cppType		Type
  HiLink cppStorageClass	StorageClass
  HiLink cppStructure		Structure
  HiLink cppBoolean		Boolean
  HiLink cppConstant		Constant
  HiLink cppRawStringDelimiter	Delimiter
  HiLink cppRawString		String
  delcommand HiLink
endif

let b:current_syntax = "cpp"

" vim: ts=8
