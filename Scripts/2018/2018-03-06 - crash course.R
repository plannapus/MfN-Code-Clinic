# Modified after StackOverflow QA: https://stackoverflow.com/questions/19226816/how-can-i-view-the-source-code-for-a-function

#####################S3 methods
t
# function (x) 
#   UseMethod("t")
# <bytecode: 0x10a520668>
#   <environment: namespace:base>

methods(t)
# [1] t.data.frame t.default    t.ts*       
#   see '?methods' for accessing help and source code

t.default
# function (x) 
#   .Internal(t.default(x))
# <bytecode: 0x10a520f10>
#   <environment: namespace:base>

t.ts
# Error: object 't.ts' not found

getAnywhere(t.ts)
# A single object matching ‘t.ts’ was found
# It was found in the following places
# registered S3 method for t from namespace stats
# namespace:stats
# with value
# 
# function (x) 
# {
#   cl <- oldClass(x)
#   other <- !(cl %in% c("ts", "mts"))
#   class(x) <- if (any(other)) 
#     cl[other]
#   attr(x, "tsp") <- NULL
#   t(x)
# }
# <bytecode: 0x10f727e30>
#   <environment: namespace:stats>

#####################S4 methods

library(Matrix)
chol2inv
# standardGeneric for "chol2inv" defined from package "base"
# 
# function (x, ...) 
#   standardGeneric("chol2inv")
# <bytecode: 0x1035fe8b0>
#   <environment: 0x1035f9008>
#   Methods may be defined for arguments: x
# Use  showMethods("chol2inv")  for currently available ones.

showMethods("chol2inv")
# Function: chol2inv (package base)
# x="ANY"
# x="CHMfactor"
# x="denseMatrix"
# x="diagonalMatrix"
# x="dtrMatrix"
# x="sparseMatrix"

getMethod("chol2inv","sparseMatrix")
# Method Definition:
#   
#   function (x, ...) 
#   {
#     chk.s(..., which.call = -2)
#     tcrossprod(solve(as(x, "triangularMatrix")))
#   }
# <environment: namespace:Matrix>
#   
#   Signatures:
#   x             
# target  "sparseMatrix"
# defined "sparseMatrix"

all.equal
# standardGeneric for "all.equal" defined from package "base"
# 
# function (target, current, ...) 
#   standardGeneric("all.equal")
# <environment: 0x1296b8640>
#   Methods may be defined for arguments: target, current
# Use  showMethods("all.equal")  for currently available ones.

showMethods(all.equal)
# Function: all.equal (package base)
# target="abIndex", current="abIndex"
# target="abIndex", current="numLike"
# target="ANY", current="ANY"
# target="ANY", current="Matrix"
# target="ANY", current="sparseMatrix"
# target="ANY", current="sparseVector"
# target="Matrix", current="ANY"
# target="Matrix", current="Matrix"
# target="numLike", current="abIndex"
# target="sparseMatrix", current="ANY"
# target="sparseMatrix", current="sparseMatrix"
# target="sparseMatrix", current="sparseVector"
# target="sparseVector", current="ANY"
# target="sparseVector", current="sparseMatrix"
# target="sparseVector", current="sparseVector"

getMethod("all.equal",list(target="sparseMatrix",current="ANY"))
# Method Definition:
#   
#   function (target, current, ...) 
#   {
#     .local <- function (target, current, check.attributes = TRUE, 
#                         ...) 
#     {
#       msg <- attr.all_Mat(target, current, check.attributes = check.attributes, 
#                           ...)
#       if (is.list(msg)) 
#         msg[[1]]
#       else .a.e.comb(msg, all.equal(as(target, "sparseVector"), 
#                                     current, check.attributes = check.attributes, ...))
#     }
#     .local(target, current, ...)
#   }
# <environment: namespace:Matrix>
#   
#   Signatures:
#   target         current
# target  "sparseMatrix" "ANY"  
# defined "sparseMatrix" "ANY"

############################Unexported functions
stats:::.makeNamesTs
# function (...) 
# {
#   l <- as.list(substitute(list(...)))[-1L]
#   nm <- names(l)
#   fixup <- if (is.null(nm)) 
#     seq_along(l)
#   else nm == ""
#   dep <- sapply(l[fixup], function(x) deparse(x)[1L])
#   if (is.null(nm)) 
#     return(dep)
#   if (any(fixup)) 
#     nm[fixup] <- dep
#   nm
# }
# <bytecode: 0x10e180d18>
#   <environment: namespace:stats>

############################Functions calling compiled code
t.default
# function (x) 
#   .Internal(t.default(x))
# <bytecode: 0x10a520f10>
#   <environment: namespace:base>

#####Here we need to access R source code.
#####There are plenty of copies around (including on CRAN)
#####but for convenience one can use a version of github such as
#####https://github.com/wch/r-source

#####Folder src/main contains a file called names.c that map Internal functions to their corresponding script
#####In this case line 723
{"t.default",	do_transpose,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
#####Then one need to search the C source code for function "do_transpose" which is in this case in file array.c
SEXP attribute_hidden do_transpose(SEXP call, SEXP op, SEXP args, SEXP rho)
{
  SEXP a, r, dims, dimnames, dimnamesnames = R_NilValue,
  ndimnamesnames, rnames, cnames;
  int ldim, ncol = 0, nrow = 0;
  R_xlen_t len = 0;
  
  checkArity(op, args);
  a = CAR(args);
  
  if (isVector(a)) {
    dims = getAttrib(a, R_DimSymbol);
    ldim = length(dims);
    rnames = R_NilValue;
    cnames = R_NilValue;
    switch(ldim) {
      case 0:
        len = nrow = LENGTH(a);
        ncol = 1;
        rnames = getAttrib(a, R_NamesSymbol);
        dimnames = rnames;/* for isNull() below*/
          break;
        case 1:
          len = nrow = LENGTH(a);
          ncol = 1;
          dimnames = getAttrib(a, R_DimNamesSymbol);
          if (dimnames != R_NilValue) {
            rnames = VECTOR_ELT(dimnames, 0);
            dimnamesnames = getAttrib(dimnames, R_NamesSymbol);
          }
          break;
          case 2:
            ncol = ncols(a);
            nrow = nrows(a);
            len = XLENGTH(a);
            dimnames = getAttrib(a, R_DimNamesSymbol);
            if (dimnames != R_NilValue) {
              rnames = VECTOR_ELT(dimnames, 0);
              cnames = VECTOR_ELT(dimnames, 1);
              dimnamesnames = getAttrib(dimnames, R_NamesSymbol);
            }
            break;
            default:
              goto not_matrix;
    }
  }
  else
    goto not_matrix;
  PROTECT(dimnamesnames);
  PROTECT(r = allocVector(TYPEOF(a), len));
  R_xlen_t i, j, l_1 = len-1;
  switch (TYPEOF(a)) {
    case LGLSXP:
      case INTSXP:
      // filling in columnwise, "accessing row-wise":
      for (i = 0, j = 0; i < len; i++, j += nrow) {
        if (j > l_1) j -= l_1;
        INTEGER(r)[i] = INTEGER(a)[j];
      }
    break;
    case REALSXP:
      for (i = 0, j = 0; i < len; i++, j += nrow) {
        if (j > l_1) j -= l_1;
        REAL(r)[i] = REAL(a)[j];
      }
    break;
    case CPLXSXP:
      for (i = 0, j = 0; i < len; i++, j += nrow) {
        if (j > l_1) j -= l_1;
        COMPLEX(r)[i] = COMPLEX(a)[j];
      }
    break;
    case STRSXP:
      for (i = 0, j = 0; i < len; i++, j += nrow) {
        if (j > l_1) j -= l_1;
        SET_STRING_ELT(r, i, STRING_ELT(a,j));
      }
    break;
    case VECSXP:
      for (i = 0, j = 0; i < len; i++, j += nrow) {
        if (j > l_1) j -= l_1;
        SET_VECTOR_ELT(r, i, VECTOR_ELT(a,j));
      }
    break;
    case RAWSXP:
      for (i = 0, j = 0; i < len; i++, j += nrow) {
        if (j > l_1) j -= l_1;
        RAW(r)[i] = RAW(a)[j];
      }
    break;
    default:
      UNPROTECT(2); /* r, dimnamesnames */
      goto not_matrix;
  }
  PROTECT(dims = allocVector(INTSXP, 2));
  INTEGER(dims)[0] = ncol;
  INTEGER(dims)[1] = nrow;
  setAttrib(r, R_DimSymbol, dims);
  UNPROTECT(1); /* dims */
    /* R <= 2.2.0: dropped list(NULL,NULL) dimnames :
    * if(rnames != R_NilValue || cnames != R_NilValue) */
    if(!isNull(dimnames)) {
      PROTECT(dimnames = allocVector(VECSXP, 2));
      SET_VECTOR_ELT(dimnames, 0, cnames);
      SET_VECTOR_ELT(dimnames, 1, rnames);
      if(!isNull(dimnamesnames)) {
        PROTECT(ndimnamesnames = allocVector(VECSXP, 2));
        SET_VECTOR_ELT(ndimnamesnames, 1, STRING_ELT(dimnamesnames, 0));
        SET_VECTOR_ELT(ndimnamesnames, 0,
                       (ldim == 2) ? STRING_ELT(dimnamesnames, 1):
                         R_BlankString);
        setAttrib(dimnames, R_NamesSymbol, ndimnamesnames);
        UNPROTECT(1); /* ndimnamesnames */
      }
      setAttrib(r, R_DimNamesSymbol, dimnames);
      UNPROTECT(1); /* dimnames */
    }
  copyMostAttrib(a, r);
  UNPROTECT(2); /* r, dimnamesnames */
    return r;
  not_matrix:
    error(_("argument is not a matrix"));
  return call;/* never used; just for -Wall */
}

######Functions called with .Primitive works the same way.
######Functions called with .C or .Fortran or.Call are the equivalent in non-base packages.


#######Alternatively package pryr has a function pryr::show_c_source that can help as well
pryr::show_c_source(.Internal(t.default(x)))

#######As a side note, operators are mode difficult to fetch:
%%
  # Error: unexpected SPECIAL in "%%"
`%%`
# function (e1, e2)  .Primitive("%%")
getAnywhere("%%")
# A single object matching ‘%%’ was found
# It was found in the following places
# package:base
# namespace:base
# with value
# 
# function (e1, e2)  .Primitive("%%")
