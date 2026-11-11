#include <Rcpp.h>
#include <sys/stat.h>
#include <sndfile.hh>
#include <vector>

Rcpp::NumericVector read_n (SndfileHandle& file, int len) {
  std::vector<short int> v(len);
  sf_count_t c = file.read (&v[0], len);

  if (c != len) {
    if (file.error () != SF_ERR_NO_ERROR) Rcpp::Rcerr << "error: " << file.strError() << std::endl;
    return Rcpp::NumericVector(0);
  }

  Rcpp::NumericVector left(len);
  for (int i = 0; i < len; i++) left[i] = (uint16_t)v[i];

  return left;
}

// [[Rcpp::export]]
Rcpp::List cpp_read_sndfile (const char * fname, int from=1, int to=2147483645, bool metadata=true) {
  struct stat buffer;   
  if (::stat (fname, &buffer)) Rcpp::stop("missing file %s", fname);

  SndfileHandle file = SndfileHandle (fname) ;
  Rcpp::List r;

  if (metadata) {
    r["samples"] = file.frames ();
    r["sample_rate"] = file.samplerate ();
    r["bits"] = 16;
    r["date"] = file.getString (SF_STR_DATE);
  }

  if (to <= 0 || to >= from) {
    file.seek (from - 1, SEEK_SET);
    r["data"] = read_n(file, to - from + 1);
  } 


  return(r);
}
