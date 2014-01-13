\name{upload_to_S3}
\alias{upload_to_S3}
\alias{S3}
\alias{amazon}
\alias{AWS}
\title{
Upload a file to Amazon S3
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
upload_to_S3(file, remotename,
    bucket=getOption("ANNOTATION_HUB_BUCKET_NAME", "annotationhub"),
    profile, acl="public-read")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
The file to upload.
}
  \item{remotename}{
The name this file should have in S3, including any "keys"
that are part of the name. This should not start with 
a slash, but can contain forward slashes.
}
  \item{bucket}{
Name of the S3 bucket to copy to.
}
  \item{profile}{
Corresponds to a profile set in the config file for the AWS CLI
(see \href{http://docs.aws.amazon.com/cli/latest/userguide/cli-chap-getting-started.html#cli-multiple-profiles}{the documentation}).
If this argument is omitted,the default profile is used.
}
  \item{acl}{
Should be one of \code{private}, \code{public-read}, or \code{public-read-write}.
}
}
\details{
Uses the \href{https://aws.amazon.com/cli/}{AWS Command Line Interface}
to copy a file to Amazon S3. Assumes the CLI is properly configured
and that the \code{aws} program is in your PATH. The CLI should be
configured with the credentials of a user who has permission to
upload to the appropriate bucket. It's recommended to use
\href{https://aws.amazon.com/iam/}{IAM} to set up users
with limited permissions.

There is an \code{RAmazonS3} package but it seems to have issues
uploading files to S3.
}
\value{
\code{TRUE} on success. If the command fails, the function
will exit with an error. 
}
\author{Dan Tenenbaum}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
upload_to_S3("myfile.txt", "foo/bar/baz/yourfile.txt")
# If this is successful, the file should be accessible at 
# http://s3.amazonaws.com/annotationhub/foo/bar/baz/yourfile.txt
}
}