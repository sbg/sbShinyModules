% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{file_picker_example_data}
\alias{file_picker_example_data}
\title{Example data frame to be used with in the file_pickers_demo_app.R}
\format{
## `file_picker_example_data`
A data frame with 8 rows and 24 columns:
\describe{
  \item{name}{File name.}
  \item{id}{File ID.}
  \item{extension}{File extension.}
  \item{size}{File size in bytes.}
  \item{sample_id}{A human readable identifier for a sample or specimen,
   which can contain some metadata information.}
  \item{tags}{Platform tags attached to the file.}
  \item{origin_task_id}{The id of a Platform task that generated a file.}
  \item{path}{Absolute path to a file.}
  \item{parent_id}{ID of a parent directory.}
  \item{experimental_strategy}{The method or protocol used to perform the
   laboratory analysis.}
  \item{library_id}{An identifier for the sequencing library preparation.}
  \item{platform}{The version (manufacturer, model, etc.) of the technology
   that was used for sequencing or assaying.}
  \item{platform_unit_id}{An identifier for lanes (Illumina), or for slides
   (SOLiD) in the case a library was split and ran over multiple lanes
    on the flow cell or slides. The platform unit ID refers to the lane ID
    or the slide ID.}
  \item{file_segment_number}{If the sequencing reads for a single library,
   sample and lane are divided into multiple (smaller) files, the File
   segment number is used to enumerate these. Otherwise, this field can be
   left blank.}
  \item{quality_scale}{For raw reads, this value denotes the sequencing
   technology and quality format.For BAM and SAM files, this value should
   always be ‘Sanger’.}
  \item{paired_end}{For paired-end sequencing, this value determines the end
   of the sequenced fragment. For single-end sequencing no value is needed.}
  \item{reference_genome}{The reference assembly (such as HG19 or GRCh37)
  to which the nucleotide sequence of a case can be aligned.}
  \item{investigation}{A value denoting the project or study that generated
   the data.}
  \item{species}{A group of organisms having some common characteristic or
   qualities, that differ from all other groups of organisms and are
   capable of breeding and producing a fertile offspring.}
  \item{batch_number}{Assigned distinctive identification code that
   signifies grouping.}
  \item{case_id}{A human-readable identifier, such as a number or a string
   for a subject that has taken part in the investigation or study.}
  \item{sample_type}{The type of material taken from a biological entity for
   testing, diagnosis, propagation, treatment, or research purposes. This
   includes tissues, body fluids, cells, organs, embryos, body excretory
   products, etc.}
  \item{md5_sum}{A 32-character hexadecimal number that is computed on a
   file.}
  \item{sbg_public_files_category}{SBG Platform public files category.}
  ...
}
}
\usage{
file_picker_example_data
}
\description{
Example data frame to be used with in the file_pickers_demo_app.R
}
\keyword{datasets}
