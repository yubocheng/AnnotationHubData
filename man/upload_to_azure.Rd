\name{upload_to_azure}
\alias{upload_to_azure}
\alias{Azure}
\alias{DataLake}
\alias{AZURE_SAS_URL}
\title{Upload a file to Microsoft Azure Data Lake}
\description{This function is for uploading a file resource to the
  Microsoft Azure Data Lake.}
\usage{upload_to_azure(file, sas)}
\arguments{
  \item{file}{
    The file or directory to upload.
  }
  \item{sas}{
    A SAS url for the designated destination on Microsoft Azure Data Lake.
  }
}
\details{
Uses the \href{https://docs.microsoft.com/en-us/azure/storage/common/storage-use-azcopy-v10}{azcopy Command Line Interface}
to copy a file to Microsoft Azure Data Lake. Assumes azcopy is properly installed
and that the \code{azcopy} program is in your PATH. The function
performs a recursive automatically so it can take a file or directory
for upload. The SAS URL is generated on Azure by someone who has
permission to the desired destination. Please be sure to use the SAS url
and not the SAS token. The sas url can be provided as an argument; if
the argument is not provided it will search for a system environment
variable `AZURE_SAS_URL`.
}
\value{
\code{TRUE} on success. If the command fails, the function
will exit with an error. 
}
\author{Lori Shepherd}

\examples{
\dontrun{
upload_to_azure("myfile.txt", "https://sasurl")
}
}
