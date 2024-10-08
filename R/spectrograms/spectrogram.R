
#' Create Spectrograms
#'
#' Create and display spectrograms.
#'
#' This function is used to create and plot spectrograms. The user may specify
#' all analysis parameters, in addition to the colors used to display the
#' spectrogram.
#'
#' The function optionally returns a spectrogram object for which the plot()
#' function is defined.
#'
#' @export
#' @aliases spectrogram plot.spectrogram print.spectrogram
#' @param sound Either a numeric vector representing a sequence of samples
#' taken from a sound wave or a sound object created with the loadsound() or
#' makesound() functions.
#' @param fs The sampling frequency in Hz. If a sound object is passed this
#' does not need to be specified.
#' @param windowlength The desired analysis window length in milliseconds.
#' @param timestep If a negative value is given, -N, then N equally-spaced time
#' steps are calculated. If a positive number is given, this is the spacing
#' between adjacent analyses, in milliseconds.
#' @param padding The amount of zero padding for each window, measured in units
#' of window length. For example, if the window is 50 points, and padding = 10,
#' 500 zeros will be appended to each window.
#' @param preemphasisf Preemphasis of 6 dB per octave is added to frequencies
#' above the specified frequency. For no preemphasis, set to a frequency higher
#' than the sampling frequency.
#' @param maxfreq the maximum frequency to be displayed for the spectrogram up
#' to a maximum of fs/2. This is set to 5000 Hz by default.
#' @param colors If TRUE, a color spectrogram will be displayed. If FALSE,
#' greyscale is used. If a vector of colors is provided, these colors are used
#' to create the spectrogram.
#' @param nlevels The number of divisions to be used for the z-axis of the
#' spectrogram. By default it is set equal to the dynamic range, meaning that a
#' single color represents 1 dB on the z-axis.
#' @param dynamicrange Values greater than this many dB below the maximum will
#' be displayed in the same color.
#' @param maintitle A string indicating the spectrogram title if one is
#' desired.
#' @param show If FALSE, no spectrogram is plotted. This is useful if the user
#' would like to perform an action on an existing spectrogram plot without
#' having to redraw it.
#' @param window the window to be applied to the signal, applied by the
#' windowfunc function in this package.
#' @param windowparameter the parameter for the window to be applied to the
#' signal, if appropriate.
#' @param quality If TRUE, a contour plot is created, which results in a
#' high-quality image that may be slow to plot. If FALSE, a lower-quality image
#' is created that plots much faster.
#' @return If output is set to TRUE, an object of class 'spectrogram', a list
#' containing the elements:
#'
#' \item{spectrogram}{a matrix containing the magnitude at each bin center.
#' Frequencies differ across columns, while time varies between rows.}
#' \item{fs}{the sampling frequency of the sound from which the spectrogram was
#' made.} \item{windowlength}{the length of the analysis window used to create
#' the spectrogram.} \item{timestep}{the timestep (in milliseconds) used to
#' create the spectrogram.} \item{dynamicrange}{the dynamic range (in dB) of
#' the spectrogram.} \item{color}{the colors used to create the spectrogram.
#' This value corresponds to the 'color' parameter set when calling
#' spectrogram().} \item{maxfreq}{the maximum desired frequency when plotting.}
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' \dontrun{
#' data (sound)             ## use the example 'sound' object provided
#' # sound = loadsound()       ## or run this line to use you own sound
#'
#' spectrogram (sound)
#' # spectrogram (sound, quality = TRUE)
#' }


spectrogram = function (
    sound, fs = NA, windowlength = 5, timestep = -500, padding = 2,
    preemphasisf = 50, maxfreq = 5000, colors = TRUE, dynamicrange = 70,
    nlevels = dynamicrange, maintitle = "", show = TRUE, window = 'kaiser',
    windowparameter = 3, quality = FALSE){

  if (inherits(sound,"ts") & is.na(fs)) fs = frequency(sound)
  if (inherits(sound,"sound")){
    fs = sound$fs
    sound = sound$sound
  }

  n = ceiling((fs/1000) * windowlength)
  if (n%%2) n = n + 1

  if (timestep > 0) timestep = floor(timestep/1000 * fs)
  if (timestep <= 0) timestep = floor (length(sound) / -timestep)
  if (preemphasisf > 0) sound = preemphasis (sound, preemphasisf, fs)

  #sound = c(rep(0, floor(n / 2)), sound, rep(0, floor(n / 2)))
  spots = seq (floor(n / 2), length(sound)-n, timestep)

  padding = n*padding
  if ((n + padding)%%2) padding = padding + 1
  N = n + padding

  spect = sapply (spots,function(x){
    tmp = sound[x:(x+n-1)] * windowfunc(sound[x:(x+n-1)], window, windowparameter);
    tmp = c(tmp, rep(0, padding));
    tmp = tmp - mean(tmp);
    tmp = fft (tmp)[1:(N/2+1)];
    tmp = abs(tmp)^2;
    tmp = log(tmp, 10) * 10;
  })
  spect = t(spect)
  for (i in 1:nrow(spect)) spect[i,1] = min(spect[i,-1])

  hz = (0:(N/2)) * (fs/N)
  times = spots * (1000/fs)
  rownames(spect) = as.numeric (round(times, 2))
  colnames(spect) = as.numeric (round(hz, 2))

  if (colors == 'alternate') colors = c('black','red','orange','yellow','white')
  if (maxfreq > (fs/2)) maxfreq = fs/2
  spect = spect - max(spect)

  specobject = list(spectrogram = spect, fs = fs, windowlength = windowlength,
                    timestep = timestep, dynamicrange = dynamicrange, colors = colors, maxfreq=maxfreq)
  class(specobject) = "spectrogram"

  if (show == TRUE) plot(specobject, ylim = c(0, maxfreq), quality = quality)
  invisible (specobject)
}
