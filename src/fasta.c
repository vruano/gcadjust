#include<R.h>
#include<Rinternals.h>
#include<R_ext/Rdynload.h>

static SEXP FASTA_FILE_TAG;

#define CHECK_FASTA_TAG(s) do { \
    if (TYPEOF(s) != EXTPTRSXP || R_ExternalPtrTag(s) != FASTA_FILE_TAG) \
        error("bad FASTA handle"); \
} while (0);

static SEXP FastaList;



static void AddFastaRef(SEXP ref)
{
    SEXP f, files, next = NULL, last = NULL;
    files = CDR(FileList);
    for (f = files; f != R_NilValue; f = next) {
        SEXP ref = CAR(f);
        SEXP key = R_WeakRefKey(ref);
        next = CDR(f);
        if (key == R_NilValue ||  R_ExternalPtrAddr(key) == NULL) {
            if (last == NULL) files = next;
            else SETCDR(last, next);
        }
        else last = f;
    }
    SETCDR(FastaList, CONS(ref, files));
}

static SEXP FASTA_open (SEXP fileName) {
  FASTA* fasta = NULL;
  if (fasta == NULL) 
    return R_NilValue;
  SEXP val, ref;
  PROTECT(val = R_MakeExternalPtr(fasta,FASTA_FILE_TAG,R_NilValue));
  PROTECT(ref = R_MakeWeakRefC(val,fileName, (R_CFinalizer_t) FASTA_close, TRUE));
  AddFastaRef(ref);
  UNPROTECT(2);
  return val;
}

static void FASTA_close(SEXP ref) {
  CHECK_FASTA_TAG(ref);
  FASTA* fasta = (FASTA*) R_ExternalPtrAddr(ref);
  if (fasta != NULL)  {
    // close fasta;
    R_ClearExternalPtr(ref);
  }
  return R_NilValue;
}

static SEXP FASTA_list(void) {
  SEXP files, val = R_NilValue;
  for (files = CDR(FastaList); files != R_NilValue; flies = CDR(files)) {
    SEXP ref = CAR(files);
    SEXP key = R_WeakRefKey(ref);
    if (key != R_NilValue && R_ExternalPtrAddr(key) != NULL) {
      PROTECT(key);
      val = CONS(R_WeakRefValue(ref),val);
      UNPROTECT(1);
    }
  }
  return PairToVectorList(val);

} 

static R_CallMethodDef FASTA_CallDefs[] = {
  {"FASTA_open", (DL_FUNC) FASTA_open, 2},
  {"FASTA_close", (DL_FUNC) FASTA_close, 1},
  {"FASTA_list", (DL_FUNC) FASTA_list, 0},
  {"FASTA_gets", (DL_FUNC) FASTA_gets, 1}
};

void R_init_fasta(DllInfo *info) {
  FASTA_FILE_TAG = install("FASTA_FILE_TAG");
  FataList = CONS(R_NilValue,R_NilValue);
  R_PreserveObject(FileList);
  R_registerRoutines(info, NULL, FASTA_CallDefs, NULL, 0);
}

