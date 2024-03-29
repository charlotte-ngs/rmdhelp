% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mendeley_export_r6class.R
\docType{class}
\name{MendeleyExportToolR6}
\alias{MendeleyExportToolR6}
\title{R6 Class For A Mendeley Exporter Tool}
\usage{
MendeleyExportToolR6$new()
}
\value{
Object of \code{\link{R6Class}} to bridge the exported BibTeX references
        exported by Mendeley to be used in a RMarkdown document
}
\description{
Rmarkdown documents use BibTeX files to include literature references. BibTeX files
are ordinary text files conaining the literature reference records in a pre-defined
format. BibTeX files can either be created manually, but for larger documents the
manual creation and maintenance of BibTeX files becomes untractable. This can be
remedied by using a reference manager such as Mendeley. Literature references
can be exported automatically from Mendeley into BibTeX files. The exported BibTeX
records go either to a given directory or to a specified file. The main purpose of
this tool is to read the exported BibTeX file from Mendeley and collect them into
a local BibTeX file.
}
\section{Public Methods}{

\describe{
  \item{\code{new(ps_men_export_dir, ps_local_bib_dir)}}{Instantiation-Constructor method}
  \item{\code{add(ps_new_ref, pb_is_rstudio_available)}}{Add a new reference with the key ps_new_ref}
  \item{\code{set_mendeley_export_dir(ps_men_export_dir)}}{Setter for s_men_export_dir}
  \item{\code{get_mendeley_export_dir()}}{Getter for s_men_export_dir}
  \item{\code{set_local_bib_file(ps_local_bib_file)}}{Setter for s_local_bib_file}
  \item{\code{get_local_bib_file()}}{Getter for s_local_bib_file}
  \item{\code{set_this_rmd_file(ps_this_rmd_file)}}{Setter for s_this_rmd_file}
  \item{\code{get_this_rmd_file()}}{Getter for s_this_rmd_file}
}
}

\section{Private Methods}{

\describe{
  \item{\code{get_bib_from_mendeley_export(ps_new_ref)}}{Get BibTeX record based on citation key ps_new_ref}
  \item{\code{write_local_bib(pvec_bib_new_ref)}}{Adding pvec_bib_new_ref BibTeX record to the local bibliography file}
  \item{\code{convert_add_to_ref(ps_new_ref)}}{Convert the add() statement to the Rmarkdown reference}
  \item{\code{get_rmd_ref(ps_new_ref)}}{Get Rmarkdown reference from citation key ps_new_ref}
}
}

\examples{
\dontrun{
# instantiate a new MendeleyExportTool object
met <- rmdhelp::MendeleyExportToolR6$new()
# setting the current rmd-file
met$set_this_rmd_file(ps_this_rmd_file = ifelse(rstudioapi::isAvailable(),
                              rstudioapi::getActiveDocumentContext()$path,
                              whereami::thisfile()))
# check whether mendeley export dir exists
met$exists_mendeley_export_dir()
# add a new reference to the local BibTeX file
met$add("Phocas1998")
}

}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{s_men_export_dir}}{directory where mendeley exports its BibTeX records}

\item{\code{s_local_bib_file}}{name of local BibTeX file}

\item{\code{s_this_rmd_file}}{full path to current Rmarkdown source document}
}
\if{html}{\out{</div>}}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{s_men_export_dir}}{directory where mendeley exports its BibTeX records}

\item{\code{s_local_bib_file}}{name of local BibTeX file}

\item{\code{s_this_rmd_file}}{full path to current Rmarkdown source document}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-MendeleyExporter-new}{\code{MendeleyExportToolR6$new()}}
\item \href{#method-MendeleyExporter-set_mendeley_export_dir}{\code{MendeleyExportToolR6$set_mendeley_export_dir()}}
\item \href{#method-MendeleyExporter-get_mendeley_export_dir}{\code{MendeleyExportToolR6$get_mendeley_export_dir()}}
\item \href{#method-MendeleyExporter-set_local_bib_file}{\code{MendeleyExportToolR6$set_local_bib_file()}}
\item \href{#method-MendeleyExporter-get_local_bib_file}{\code{MendeleyExportToolR6$get_local_bib_file()}}
\item \href{#method-MendeleyExporter-set_this_rmd_file}{\code{MendeleyExportToolR6$set_this_rmd_file()}}
\item \href{#method-MendeleyExporter-get_this_rmd_file}{\code{MendeleyExportToolR6$get_this_rmd_file()}}
\item \href{#method-MendeleyExporter-add}{\code{MendeleyExportToolR6$add()}}
\item \href{#method-MendeleyExporter-clone}{\code{MendeleyExportToolR6$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MendeleyExporter-new"></a>}}
\if{latex}{\out{\hypertarget{method-MendeleyExporter-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MendeleyExportToolR6$new(
  ps_men_export_dir = getOption("rmdhelp.mendeley.export.dir"),
  ps_local_bib_file = "bibliography.bib",
  ps_this_rmd_file = ifelse(rstudioapi::isAvailable(),
    rstudioapi::getActiveDocumentContext()$path, whereami::thisfile())
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MendeleyExporter-set_mendeley_export_dir"></a>}}
\if{latex}{\out{\hypertarget{method-MendeleyExporter-set_mendeley_export_dir}{}}}
\subsection{Method \code{set_mendeley_export_dir()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MendeleyExportToolR6$set_mendeley_export_dir(ps_men_export_dir)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MendeleyExporter-get_mendeley_export_dir"></a>}}
\if{latex}{\out{\hypertarget{method-MendeleyExporter-get_mendeley_export_dir}{}}}
\subsection{Method \code{get_mendeley_export_dir()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MendeleyExportToolR6$get_mendeley_export_dir()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MendeleyExporter-set_local_bib_file"></a>}}
\if{latex}{\out{\hypertarget{method-MendeleyExporter-set_local_bib_file}{}}}
\subsection{Method \code{set_local_bib_file()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MendeleyExportToolR6$set_local_bib_file(ps_local_bib_file)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MendeleyExporter-get_local_bib_file"></a>}}
\if{latex}{\out{\hypertarget{method-MendeleyExporter-get_local_bib_file}{}}}
\subsection{Method \code{get_local_bib_file()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MendeleyExportToolR6$get_local_bib_file()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MendeleyExporter-set_this_rmd_file"></a>}}
\if{latex}{\out{\hypertarget{method-MendeleyExporter-set_this_rmd_file}{}}}
\subsection{Method \code{set_this_rmd_file()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MendeleyExportToolR6$set_this_rmd_file(ps_this_rmd_file)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MendeleyExporter-get_this_rmd_file"></a>}}
\if{latex}{\out{\hypertarget{method-MendeleyExporter-get_this_rmd_file}{}}}
\subsection{Method \code{get_this_rmd_file()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MendeleyExportToolR6$get_this_rmd_file()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MendeleyExporter-add"></a>}}
\if{latex}{\out{\hypertarget{method-MendeleyExporter-add}{}}}
\subsection{Method \code{add()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MendeleyExportToolR6$add(
  ps_new_ref,
  pb_is_rstudio_available = rstudioapi::isAvailable()
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MendeleyExporter-clone"></a>}}
\if{latex}{\out{\hypertarget{method-MendeleyExporter-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MendeleyExportToolR6$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
