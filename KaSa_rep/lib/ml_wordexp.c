/* OCaml glue for calling the wordexp standard POSIX function
   (shell-like expansion of file-names, with pattern, tilde, etc.)
   Copyright (C) Antoine Mine' 2006
*/

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/alloc.h>

#include <wordexp.h>

CAMLprim value ml_wordexp(value arg)
{
  CAMLparam1(arg);
  CAMLlocal2(head,tail);
  wordexp_t p;
  int i;
  head = tail = Val_int(0);
  switch (wordexp(String_val(arg),&p,0)) {
  case 0: break;
  case WRDE_BADCHAR: 
    caml_invalid_argument("wordexp: | & ; < > ( ) { } characters are illegal in filenames");
    break;
  case WRDE_NOSPACE:
    caml_failwith("wordexp ran out of memory in filename expansion");
    break;
  case WRDE_SYNTAX:
    caml_invalid_argument("wordexp: syntax error in filename pattern");
    break;
  default:
    caml_failwith("wordexp: unknown error in filename expansion");
  }
  for (i=0;i<p.we_wordc;i++) {
    head = caml_alloc_tuple(2);
    Store_field(head,0,caml_copy_string(p.we_wordv[i]));
    Store_field(head,1,tail);
    tail = head;
  }
  wordfree(&p);
  CAMLreturn(head);
}
