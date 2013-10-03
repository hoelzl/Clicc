#if defined(__STDC__) || defined(__cplusplus)
# define P_(s) s
#else
# define P_(s) ()
#endif

/* ../src/runtime/c/array.c */
extern void rt_plain_vector_element_code P_((CL_FORM *base));
extern void rt_make_vector_t P_((CL_FORM *base));
extern void rt_make_vector_fixnum P_((CL_FORM *base));
extern void rt_make_vector_float P_((CL_FORM *base));
extern void rt_make_vector_char P_((CL_FORM *base));
extern void rt_make_vector_bit P_((CL_FORM *base));
extern void rt_pvref P_((CL_FORM *base));
extern void rt_set_pvref P_((CL_FORM *base));
extern void rt_sbvref P_((CL_FORM *base));
extern void rt_set_sbvref P_((CL_FORM *base));
extern void rt_bitop P_((CL_FORM *base));
extern void rt_shrink_smstr P_((CL_FORM *base));
/* ../src/runtime/c/catch.c */
extern void unwind_to P_((CL_FORM *saved_bind_top));
extern void rt_catch P_((CL_FORM *base));
extern void call_cont P_((CL_FORM *base));
extern void rt_throw P_((CL_FORM *base, int nargs));
extern void rt_unwind_protect P_((CL_FORM *base));
/* ../src/runtime/c/character.c */
extern void rt_standard_char_p P_((CL_FORM *base));
extern void rt_graphic_char_p P_((CL_FORM *base));
extern void rt_alpha_char_p P_((CL_FORM *base));
extern void rt_upper_case_p P_((CL_FORM *base));
extern void rt_lower_case_p P_((CL_FORM *base));
extern void rt_both_case_p P_((CL_FORM *base));
extern void rt_digit_char P_((CL_FORM *base));
extern void rt_char_code P_((CL_FORM *base));
extern void rt_code_char P_((CL_FORM *base));
extern void rt_char_upcase P_((CL_FORM *base));
extern void rt_char_downcase P_((CL_FORM *base));
/* ../src/runtime/c/clos.c */
extern void rt_make_instance P_((CL_FORM *base));
/* ../src/runtime/c/debug.c */
extern void inspect P_((CL_FORM *base, int level, int length, int in_list_p));
extern void stack_cont P_((CL_FORM *base, int offset, int nargs));
extern void dc_pure_symbol P_((CL_FORM *symbol));
extern void symbol_module_i P_((CL_FORM *sym_base, int i));
extern void show_alist P_((CL_FORM *base, int i));
/* ../src/runtime/c/environ.c */
extern void c_getenv_internal P_((CL_FORM *base));
extern void c_system_internal P_((CL_FORM *base));
extern void c_argc P_((CL_FORM *base));
extern void c_argv P_((CL_FORM *base));
/* ../src/runtime/c/file.c */
extern void c_fopen P_((CL_FORM *base));
extern void c_fclose P_((CL_FORM *base));
extern void c_ftell P_((CL_FORM *base));
extern void c_fseek P_((CL_FORM *base));
extern void c_file_length P_((CL_FORM *base));
extern void c_stdin P_((CL_FORM *base));
extern void c_stdout P_((CL_FORM *base));
extern void c_fgetc P_((CL_FORM *base));
extern void c_fputc P_((CL_FORM *base));
extern void c_ungetc P_((CL_FORM *base));
/* ../src/runtime/c/foreign.c */
extern void rt_make_c_char P_((CL_FORM *base));
extern void rt_make_c_short P_((CL_FORM *base));
extern void rt_make_c_int P_((CL_FORM *base));
extern void rt_make_c_long P_((CL_FORM *base));
extern void rt_make_c_unsigned_char P_((CL_FORM *base));
extern void rt_make_c_unsigned_short P_((CL_FORM *base));
extern void rt_make_c_unsigned_int P_((CL_FORM *base));
extern void rt_make_c_unsigned_long P_((CL_FORM *base));
extern void rt_make_c_float P_((CL_FORM *base));
extern void rt_make_c_double P_((CL_FORM *base));
extern void rt_make_c_long_double P_((CL_FORM *base));
extern void rt_cast_c_char P_((CL_FORM *base));
extern void rt_cast_c_short P_((CL_FORM *base));
extern void rt_cast_c_int P_((CL_FORM *base));
extern void rt_cast_c_long P_((CL_FORM *base));
extern void rt_cast_c_unsigned_char P_((CL_FORM *base));
extern void rt_cast_c_unsigned_short P_((CL_FORM *base));
extern void rt_cast_c_unsigned_int P_((CL_FORM *base));
extern void rt_cast_c_unsigned_long P_((CL_FORM *base));
extern void rt_cast_c_float P_((CL_FORM *base));
extern void rt_cast_c_double P_((CL_FORM *base));
extern void rt_cast_c_long_double P_((CL_FORM *base));
extern void rt_make_lisp_character P_((CL_FORM *base));
extern void rt_make_lisp_integer P_((CL_FORM *base));
extern void rt_make_lisp_float P_((CL_FORM *base));
extern void rt_internal_make_lisp_string P_((CL_FORM *base));
extern void rt_internal_make_c_string P_((CL_FORM *base));
extern void rt_internal_copy_c_string P_((CL_FORM *base));
extern void FFI_c_char_p P_((CL_FORM *base));
extern void FFI_c_short_p P_((CL_FORM *base));
extern void FFI_c_int_p P_((CL_FORM *base));
extern void FFI_c_long_p P_((CL_FORM *base));
extern void FFI_c_unsigned_char_p P_((CL_FORM *base));
extern void FFI_c_unsigned_short_p P_((CL_FORM *base));
extern void FFI_c_unsigned_int_p P_((CL_FORM *base));
extern void FFI_c_unsigned_long_p P_((CL_FORM *base));
extern void FFI_c_float_p P_((CL_FORM *base));
extern void FFI_c_double_p P_((CL_FORM *base));
extern void FFI_c_long_double_p P_((CL_FORM *base));
extern void FFI_c_string_p P_((CL_FORM *base));
extern void rt_internal_c_struct_p P_((CL_FORM *base));
extern void rt_internal_c_union_p P_((CL_FORM *base));
extern void rt_internal_c_handle_p P_((CL_FORM *base));
extern void rt_internal_c_array_p P_((CL_FORM *base));
extern void rt_internal_make_c_struct P_((CL_FORM *base));
extern void _make_c_struct_ptr P_((CL_FORM *base, CL_FORM *symbol, char *address));
extern void rt_internal_make_c_union P_((CL_FORM *base));
extern void _make_c_union_ptr P_((CL_FORM *base, CL_FORM *symbol, char *address));
extern void rt_internal_make_c_array P_((CL_FORM *base));
extern void _make_c_array_ptr P_((CL_FORM *base, CL_FORM *symbol, char *address));
extern void _make_c_handle P_((CL_FORM *base, CL_FORM *symbol, char *address));
extern void rt_internal_copy_c_struct P_((CL_FORM *base));
extern void rt_internal_copy_c_union P_((CL_FORM *base));
extern void rt_internal_copy_c_array P_((CL_FORM *base));
extern void rt_internal_get_struct_pointer P_((CL_FORM *base));
extern void rt_internal_get_union_pointer P_((CL_FORM *base));
extern void rt_internal_get_array_pointer P_((CL_FORM *base));
extern void c_struct_p P_((CL_FORM *base));
extern void c_union_p P_((CL_FORM *base));
extern void c_array_p P_((CL_FORM *base));
extern void c_handle_p P_((CL_FORM *base));
extern void rt_internal_get_symbol P_((CL_FORM *base));
extern void rt_internal_get_address P_((CL_FORM *base));
extern void FFI_free P_((CL_FORM *base));
/* ../src/runtime/c/fspecs.c */
extern void rt_calc_radix P_((CL_FORM *base));
extern void rt_calc_mant_dig P_((CL_FORM *base));
extern void rt_most_positive_fixnum P_((CL_FORM *base));
extern void rt_most_negative_fixnum P_((CL_FORM *base));
/* ../src/runtime/c/funcall.c */
extern void Ffuncall P_((CL_FORM *base, int nargs));
extern void Fapply P_((CL_FORM *base, int nargs));
extern void rest_apply P_((CL_FORM *base, int nargs, int len, CL_FORM *rest));
/* ../src/runtime/c/hash.c */
extern unsigned long hash P_((char *str));
extern void rt_sxhash_string P_((CL_FORM *base));
/* ../src/runtime/c/keysort.c */
extern void rt_init_keysort P_((CL_FORM *base));
extern void keysort P_((CL_FORM *first_arg, int nargs, int nkey, CL_FORM *keylist[], char suppl_flags[], int allow_other));
/* ../src/runtime/c/list.c */
extern void Flist P_((CL_FORM *base, int nargs));
extern void FlistX P_((CL_FORM *base, int nargs));
extern void Fappend P_((CL_FORM *base, int nargs));
/* ../src/runtime/c/main.c */
extern int main P_((int argc, char *argv[]));
/* ../src/runtime/c/number.c */
extern double get_float P_((CL_FORM *base));
extern double *make_float P_((CL_FORM *base, double num_float));
extern void Fzerop P_((CL_FORM *base));
extern void Fplusp P_((CL_FORM *base));
extern void Fminusp P_((CL_FORM *base));
extern void Foddp P_((CL_FORM *base));
extern void Fevenp P_((CL_FORM *base));
extern void Fnumeql P_((CL_FORM *base, int nargs));
extern void Fnumneql P_((CL_FORM *base, int nargs));
extern void Flt P_((CL_FORM *base, int nargs));
extern void Fltfix P_((CL_FORM *base));
extern void Fgt P_((CL_FORM *base, int nargs));
extern void Fle P_((CL_FORM *base, int nargs));
extern void Fge P_((CL_FORM *base, int nargs));
extern void Fplus P_((CL_FORM *base, int nargs));
extern void Fminus P_((CL_FORM *base, int nargs));
extern void Fmult P_((CL_FORM *base, int nargs));
extern void Fdiv P_((CL_FORM *base, int nargs));
extern void F1plus P_((CL_FORM *base));
extern void F1minus P_((CL_FORM *base));
extern void F1minusfix P_((CL_FORM *base));
extern void rt_expt P_((CL_FORM *base));
extern void rt_log P_((CL_FORM *base));
extern void Fsqrt P_((CL_FORM *base));
extern void Fsin P_((CL_FORM *base));
extern void Fcos P_((CL_FORM *base));
extern void Ftan P_((CL_FORM *base));
extern void Fasin P_((CL_FORM *base));
extern void Facos P_((CL_FORM *base));
extern void Fatan P_((CL_FORM *base));
extern void rt_float P_((CL_FORM *base));
extern void convert_to_int P_((CL_FORM *base, int ctype));
extern void rt_floor P_((CL_FORM *base));
extern void rt_ceiling P_((CL_FORM *base));
extern void rt_truncate P_((CL_FORM *base));
extern void rt_round P_((CL_FORM *base));
extern void Finteger_length P_((CL_FORM *base));
extern void Fdecode_float P_((CL_FORM *base));
/* ../src/runtime/c/obrep1.c */
extern void do_gc P_((CL_FORM *top));
extern void save_form P_((CL_FORM *form));
/* ../src/runtime/c/obrep2.c */
/* ../src/runtime/c/progv.c */
extern void rt_progv P_((CL_FORM *base));
/* ../src/runtime/c/string.c */
extern char *get_c_string P_((CL_FORM *lisp_string));
extern void make_string P_((CL_FORM *base, char *string));
/* ../src/runtime/c/structure.c */
extern void rt_new_struct P_((CL_FORM *base));
/* ../src/runtime/c/symbols.c */
extern void rt_make_symbol P_((CL_FORM *base));
extern void rt_setup_symbols_iterator P_((CL_FORM *base));
/* ../src/runtime/c/system.c */
extern void memallot P_((void));
extern CL_FORM *form_swap P_((CL_FORM *from, long num));
extern long *fx_swap P_((long *from, long num));
extern char *ch_swap P_((char *from, long num));
extern double *fl_swap P_((double *from, long num));
extern long *bits_swap P_((long *from, long num));
extern void gc P_((CL_FORM *base, int type));
extern void gc_symbols P_((CL_INIT *symbol));
extern long *fixnum_alloc P_((CL_FORM *base, long num));
extern char *char_alloc P_((CL_FORM *base, unsigned long rnum));
extern double *float_alloc P_((CL_FORM *base, long num));
extern CL_FORM *form_alloc P_((CL_FORM *base, long num));
extern long *bits_alloc P_((CL_FORM *base, long num));
extern void Labort P_((char *msg));
extern long FLAbort P_((char *msg));
extern void Lerror P_((CL_FORM *base, char *msg));
extern void warning P_((char *warning_str));
/* ../src/runtime/c/unix.c */
extern void unix_current_directory P_((CL_FORM *base));
extern void unix_file_mode P_((CL_FORM *base));
extern void unix_link_mode P_((CL_FORM *base));
extern void unix_readlink P_((CL_FORM *base));
/* ../src/runtime/c/values.c */
extern void Fvalues P_((CL_FORM *base, int nargs));
extern void Fvalues_list P_((CL_FORM *base));
extern void save_values P_((CL_FORM *base));

#undef P_
