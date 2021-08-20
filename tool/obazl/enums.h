/* module_type_e won't work if we put it in a .c file, maybe because we assign values? */
enum module_type_e {
    UNKNOWN_TYPE,
    COQ_MODULE = 1,
    OCAML_MODULE = 2,
    OCAML_SIG = 4,
    OCAML_ARCHIVE = 8,
    OCAML_CMT = 16,
    OCAML_SRC = 32,
    COQ_SRC   = 64,
    OCAML_SHARED = 128,
    C_HEADER = 256
};

/*
  file types:  opam, foo.opam, config, META, foo.py, CoqMakefile.in, .css, .sty, .js
*/


#define BYTE_TO_BINARY_PATTERN "%c%c%c%c%c%c%c%c %c%c%c%c%c%c%c%c"

#define BYTE_TO_BINARY(byte)  \
  (byte & 0x8000 ? '1' : '0'), \
  (byte & 0x4000 ? '1' : '0'), \
  (byte & 0x2000 ? '1' : '0'), \
  (byte & 0x1000 ? '1' : '0'), \
  (byte & 0x0800 ? '1' : '0'), \
  (byte & 0x0400 ? '1' : '0'), \
  (byte & 0x0200 ? '1' : '0'), \
  (byte & 0x0100 ? '1' : '0'), \
  (byte & 0x0080 ? '1' : '0'), \
  (byte & 0x0040 ? '1' : '0'), \
  (byte & 0x0020 ? '1' : '0'), \
  (byte & 0x0010 ? '1' : '0'), \
  (byte & 0x0008 ? '1' : '0'), \
  (byte & 0x0004 ? '1' : '0'), \
  (byte & 0x0002 ? '1' : '0'), \
  (byte & 0x0001 ? '1' : '0')

#define CHECK_BIT(var,pos) ((var) & (pos))

/* (1<<(pos))) */
