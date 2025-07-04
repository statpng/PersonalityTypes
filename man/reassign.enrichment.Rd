% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis_Enrichment.R
\name{reassign.enrichment}
\alias{reassign.enrichment}
\title{Reassign Members of Non-Significant Clusters}
\usage{
reassign.enrichment(Xnew, class, mod, wh.class)
}
\arguments{
\item{Xnew}{The numeric data matrix of observations to be reassigned. Should only
contain observations from non-significant clusters.}

\item{class}{A vector of original cluster assignments for all observations.}

\item{mod}{The fitted model object (e.g., from \code{mclust::Mclust}), which must contain
the cluster parameters, specifically \code{mod$parameters$mean}.}

\item{wh.class}{A vector containing the labels of the clusters considered "significant".}
}
\value{
A new vector of class assignments of the same length as the original \code{class}
vector, where members of non-significant clusters have been reassigned.
}
\description{
Reassigns data points from non-significant clusters to the nearest
significant cluster based on Euclidean distance.
}
\details{
This function is a post-processing step for clustering. After identifying a set of
"significant" or robust clusters (e.g., via enrichment analysis), this function
handles the observations that fall into non-significant clusters.

The reassignment is based on a nearest-centroid rule. For each data point in a
non-significant cluster, the function calculates the L2-norm (Euclidean distance)
to the centroids (mean vectors) of all \emph{significant} clusters. The point is then
reassigned to the cluster with the minimum distance. This method provides a principled
way to consolidate a clustering solution by merging less stable or sparsely populated
clusters into more robust, well-defined ones, thereby simplifying the final interpretation.

If all initial classes are already within the significant set (\code{wh.class}), the function
returns the original class assignments without change.
}
