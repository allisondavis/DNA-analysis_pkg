#' @title CG content function
#' @description This function allows you calculate the fraction of C and G content of a specified sequence.
#' @param sequence a single vector of individual character
#' @param alphabet all characters within the seqence to analyze
#' @keywords CG
#' @export
#' @examples
#' CG(FISHsequence, wordsize= 2, alphabet= c("a", "c", "t", "g"))
#'
# CG content
  #{seqinr} has a function to calculate the fraction of sequence that is GC, but not for CG. Here we create the equivalent function.
  #This function prints the fraction of CG content in the specified sequence.
CG <- function(sequence) {

  tot <- seqinr::count(sequence, wordsize = 2, alphabet = s2c("acgt"))
  cg <- tot[7]
  freq <- cg/sum(tot[1],tot[2],tot[3],tot[4],tot[5],tot[6],tot[7],tot[8],tot[9],tot[10],tot[11],tot[12],tot[13],tot[14],tot[15], tot[16])
  print(freq)

}

#' @title Sliding Widow Plot- GC or CG content
#' @description This function allows you create a sliding window plot of both GC and CG content within a specified sequence
#' @param inputseq a single vector of individual character
#' @param windowsize number of nucleotide sequences in a single step
#' @keywords swp
#' @export
#' @examples
#' swp.GC(100, FISHsequence)
#' swp.CG(100, FISHsequence)

# Sliding window plot
  #this function was created by Avril Coghlan, from Little Book of R
  #A sliding plot window creates a graph of DNA "word" content against the sequence postion.
    #by specifying a window size, the window moves section by section down the sequence calculating "word" content.

#GC
swp.GC <- function(windowsize, inputseq)
{
  starts <- seq(1, length(inputseq)-windowsize, by = windowsize)
  n <- length(starts)
  chunkGCs <- NULL
  for (i in 1:n) {
    chunk <- inputseq[starts[i]:(starts[i]+windowsize-1)]
    chunkGC <- seqinr::GC(chunk)
    chunkGCs[i] <- chunkGC
  }
  plot(starts,chunkGCs,type="b",xlab="Nucleotide start position",ylab="GC content")
}


#CG
swp.CG <- function(windowsize, inputseq)
{
  starts <- seq(1, length(inputseq)-windowsize, by = windowsize)
  n <- length(starts)
  chunkCGs <- NULL
  for (i in 1:n) {
    chunk <- inputseq[starts[i]:(starts[i]+windowsize-1)]
    chunkCG <- CG(chunk)
    chunkCGs[i] <- chunkCG
  }
  plot(starts,chunkCGs,type="b",xlab="Nucleotide start position",ylab="CG content")
}
