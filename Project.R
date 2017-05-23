{
 "cells": [
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "# XV. Kernel Methods\n",
    "Madis-Karli Koppel\n",
    "B03309\n",
    "\n",
    "## Exercise 1*\n",
    "\n",
    "Here we run not normalized p-spectrum on different strings and try to figure out why it behaves as it behaves. Length of kernel is 2."
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 68,
   "metadata": {
    "collapsed": true
   },
   "outputs": [],
   "source": [
    "library(kernlab)\n",
    "#\n",
    "# The function loads the \"reuters\" sample data, necessary for the XIV exercise session.\n",
    "# Note that it requires internet connection. \n",
    "# If you don't like it, download the reuters.txt file mentioned in line 19 manually,\n",
    "# put it in the working directory and change the read.table call to refer to the local copy.\n",
    "# Note that you can download the compressed version (reuters.txt.gz). In this case\n",
    "# you do not need to unpack the gz - for local files R handles unpacking automagically.\n",
    "#\n",
    "load.data = function() {\n",
    "  # The following is a slightly-processed version of the Reuters dataset. Namely:\n",
    "  #   All articles with no TOPIC annotations are dropped\n",
    "  #   The text of each article is converted to lowercase, whitespace is normalized to single-spaces.\n",
    "  #   Only the first term from the TOPIC annotation list is retained (some articles have several topics assigned).\n",
    "  #   The resulting dataset is a list of pairs (Topic, News content)\n",
    "  #\n",
    "  # This is a fun dataset to play with, so I suggest you play with it beyond what we do in the exercise session.\n",
    "  #\n",
    "  reuters = read.table(\"https://courses.cs.ut.ee/2014/ml/uploads/Main/reuters.txt\", header=T)\n",
    "  \n",
    "  # We leave only two topics here: Crude Oil and Grain-related news.\n",
    "  reuters = reuters[reuters$Topic == \"crude\" | reuters$Topic == \"grain\",]\n",
    "  \n",
    "  # In addition, to avoid waiting for too long, we limit our experiments to just 500 elements (300 train + 200 test).\n",
    "  set.seed(1)\n",
    "  reuters = reuters[sample(1:nrow(reuters), 500),]\n",
    "  \n",
    "  # Fixup the columns\n",
    "  reuters$Content = as.character(reuters$Content) # R originally loads this as factor.\n",
    "  reuters$Topic = factor(reuters$Topic) # Re-level the factor to have only two levels\n",
    "  reuters$y = 2*(reuters$Topic == \"grain\") - 1\n",
    "  reuters\n",
    "}\n",
    "\n",
    "\n",
    "\n",
    "circle.data = function() {\n",
    "  angles = runif(300)*2*pi\n",
    "  norms = c(runif(100), runif(100) + 2, runif(100) + 4)\n",
    "  x = sin(angles)*norms\n",
    "  y = cos(angles)*norms\n",
    "  c = factor(c(rep(1,100), rep(2,100), rep(3,100)))\n",
    "  data.frame(x=x, y=y, c=c)\n",
    "}"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 7,
   "metadata": {
    "collapsed": false
   },
   "outputs": [],
   "source": [
    "reuters <- load.data()\n",
    "reuters_train = reuters[1:300,]\n",
    "reuters_test = reuters[301:500,]"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 12,
   "metadata": {
    "collapsed": false
   },
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "[1] \"a and aa\"\n"
     ]
    },
    {
     "data": {
      "text/html": [
       "1"
      ],
      "text/latex": [
       "1"
      ],
      "text/markdown": [
       "1"
      ],
      "text/plain": [
       "[1] 1"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    },
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "[1] \"aa and aaa\"\n"
     ]
    },
    {
     "data": {
      "text/html": [
       "3"
      ],
      "text/latex": [
       "3"
      ],
      "text/markdown": [
       "3"
      ],
      "text/plain": [
       "[1] 3"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "k = stringdot(\"spectrum\", length=2, normalized=F)\n",
    "print(\"a and aa\")\n",
    "k(\"a\", \"aa\")\n",
    "print(\"aa and aaa\")\n",
    "k(\"aa\", \"aaa\")"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "Even though we have length 2 then we still get results 1 and 3, but we expect 0 and 2. We get 1 and 3 because this kernel also takes into account end of string symbol, this means that we are actually comparing strings a\\$ and aa\\$, and aa\\$ and aaa\\$.\n",
    "\n",
    "## Exercise 2\n",
    "\n",
    "Kernel matrixes. Here we are using normalized 5 - spectrum string kernel"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 24,
   "metadata": {
    "collapsed": false
   },
   "outputs": [],
   "source": [
    "k = stringdot(\"spectrum\", length=5, normalized=T)\n",
    "K <- kernelMatrix(k, reuters_train$Content)\n",
    "K_test <- kernelMatrix(k, reuters_train$Content, reuters_test$Content)\n",
    "print(\"test dimensions\")\n",
    "dim(K)\n",
    "dim(K_test)"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "Now we have to matrixes - 300 x 300 and 300 x 200. Kernel matrix is a matrix where element  i,j is k(i,j). In our second matrix it is k(reuters_train\\$Content[i], reuters_test\\$Content[j])\n",
    "\n",
    "## Exercise 3\n",
    "\n",
    "From lecture slides:\n",
    "\n",
    "kernel: w = sum(ai \\* xi)\n",
    "\n",
    "svm: w = sum(ai \\* xi \\* yi)\n",
    "\n",
    "These are not the same, SVM also has multiplication with yi.\n",
    "\n",
    "## Exercise 4\n",
    "\n",
    "Here I go back to spectrum with length 2 but this time I'll keep this normalized. Simply to get similar results that we did get in Panopto video. But length 5 gives the same result.\n",
    "\n",
    "1.\n",
    "First we create a classifier that sign for p-kernel value of grain topics  - p-kernel value of crude topics. From that we know that if the output is negative then p-kernel value was bigger for crude topics and so we should classify as crude. "
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 56,
   "metadata": {
    "collapsed": false
   },
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "[1] \"eat more corn\"\n"
     ]
    },
    {
     "data": {
      "text/html": [
       "1"
      ],
      "text/latex": [
       "1"
      ],
      "text/markdown": [
       "1"
      ],
      "text/plain": [
       "[1] 1"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    },
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "[1] \"petroleum\"\n"
     ]
    },
    {
     "data": {
      "text/html": [
       "-1"
      ],
      "text/latex": [
       "-1"
      ],
      "text/markdown": [
       "-1"
      ],
      "text/plain": [
       "[1] -1"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    },
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "[1] \"2. Evaluate on all training examples and compare its accuracy on training set\"\n"
     ]
    },
    {
     "data": {
      "text/html": [
       "0.733333333333333"
      ],
      "text/latex": [
       "0.733333333333333"
      ],
      "text/markdown": [
       "0.733333333333333"
      ],
      "text/plain": [
       "[1] 0.7333333"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    },
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "[1] \"3. Evaluate on all test examples and compute accuracy on test set\"\n"
     ]
    },
    {
     "data": {
      "text/html": [
       "0.745"
      ],
      "text/latex": [
       "0.745"
      ],
      "text/markdown": [
       "0.745"
      ],
      "text/plain": [
       "[1] 0.745"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "k = stringdot(\"spectrum\", length=2)\n",
    "f = function(x){\n",
    "    sign(k(reuters_train$Content[1], x) - k(reuters_train$Content[2], x))\n",
    "}\n",
    "print(\"eat more corn\")\n",
    "f(\"eat more corn\")\n",
    "print(\"petroleum\")\n",
    "f(\"petroleum\")\n",
    "print(\"2. Evaluate on all training examples and compare its accuracy on training set\")\n",
    "mean(reuters_train$y == sign(K[1,]- K[2,]))\n",
    "print(\"3. Evaluate on all test examples and compute accuracy on test set\")\n",
    "mean(reuters_test$y == sign(K_test[1,]- K_test[2,]))"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "We got accuracies of 0.73 and 0.745, atleast they are not random.\n",
    "\n",
    "4 . t(K) \\* alfa K + b, from kernel classifier function formula, we replace k(xi, x) with K. Also, we use this in next exercise.\n",
    "\n",
    "5 . t(Ktrain) \\* alfa + b\n",
    "\n",
    "\n",
    "## Exercise 5\n",
    "\n",
    "Kernel perceptron. I fixed the parts where "
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 66,
   "metadata": {
    "collapsed": false
   },
   "outputs": [
    {
     "data": {
      "text/html": [
       "0.96"
      ],
      "text/latex": [
       "0.96"
      ],
      "text/markdown": [
       "0.96"
      ],
      "text/plain": [
       "[1] 0.96"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "#\n",
    "# The Kernel Perceptron algorithm. Requires some fixing before use.\n",
    "#\n",
    "kernel_perceptron = function(K, y) {\n",
    "  alpha = rep(0, nrow(K))\n",
    "  b = 0\n",
    "  while (TRUE) {\n",
    "    # Make predictions for all points in the training sample\n",
    "    predictions = K %*% alpha + b # <----- FIXED\n",
    "    \n",
    "    # Find misclassified instances\n",
    "    misclassified = which(sign(predictions) != y)\n",
    "    \n",
    "    # If no misclassified, we're done!\n",
    "    if (length(misclassified) == 0) { break; }\n",
    "    \n",
    "    # If something is misclassified, pick the first element\n",
    "    i = misclassified[1]\n",
    "    \n",
    "    # Update parameters\n",
    "    alpha[i] = alpha[i] + y[i]   # <------- FIXED {Hint: in the usual perceptron this could be w := w + y[i]*x[i]}\n",
    "    b = b + y[i]                 # <------- FIXED {Hint: in the usual perceptron this could be b := b + y[i]}\n",
    "  }\n",
    "  \n",
    "  # Return result\n",
    "  result = list(alpha, b)\n",
    "  names(result) = c(\"alpha\", \"b\")\n",
    "  result\n",
    "}\n",
    "\n",
    "p = kernel_perceptron(K, reuters_train$y)\n",
    "mean(reuters_test$y == sign(t(K_test) %*% p$alpha + p$b))"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "Fixed predictions to use formulas from previous exercies, fixed parameter updates to use input y. Result was 0.96, more accurate than the classifiers from before.\n",
    "\n",
    "## Exercise 6\n",
    "\n",
    "Using ksvm function."
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 73,
   "metadata": {
    "collapsed": false
   },
   "outputs": [
    {
     "data": {
      "text/html": [
       "0.94"
      ],
      "text/latex": [
       "0.94"
      ],
      "text/markdown": [
       "0.94"
      ],
      "text/plain": [
       "[1] 0.94"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "m <- ksvm(K, reuters_train$y)\n",
    "mean(reuters_test$y == sign(t(K_test[m@alphaindex,]) %*% m@alpha + m@b))"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "Result we got was 0.94, also good result. We cannot say that perceptron is better based only on these examples as the difference was really small. We need more test to determine which one is better on given input.\n",
    "\n",
    "## Exercise 7\n",
    "\n",
    "By simply replacing X amd w we get sym \\* t(sym) \\* alfa = y where sym is the symbol for higher-dimensional data matrix. From lecture slide 75 we see that sym \\* t(sym) can be replaced with K so the final answer is :\n",
    "\n",
    "K \\* alfa = y\n",
    "\n",
    "If it is positive definite then we can invert it\n",
    "\n",
    "alfa = K^(-1) \\* y\n",
    "\n",
    "This means that we can fit the model to predict any training set perfectly. This is clearly overfitting (heard from panopto recording). \n",
    "\n",
    "## Exercise 8\n",
    "\n",
    "Kernel PCA\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 91,
   "metadata": {
    "collapsed": false
   },
   "outputs": [
    {
     "data": {
      "image/png": "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\ngACIBCDAfz5s4GuS5NPAAAAAAElFTkSuQmCC",
      "text/plain": [
       "plot without title"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "# center\n",
    "n <- nrow(K)\n",
    "one <- matrix(1, n,n)\n",
    "K_cent <- K - (one %*% K)/n - (K %*% one)/n + (one %*% K %*% one)/(n*n)\n",
    "\n",
    "# eigenvalue\n",
    "e = eigen(K_cent) # The largest eigenvector is in e$vectors[,1]\n",
    "\n",
    "#project training data onto largest eigenvalues\n",
    "plot(e$vectors[,1], e$vectors[,2], col=reuters_train$Topic)"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "We can see that the result is easily separable. This means that our PCA worked and produced a pretty good result.\n",
    "\n",
    "## Exercise 9\n",
    "\n",
    "Kernel PCA from kernlab"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 104,
   "metadata": {
    "collapsed": false
   },
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "Formal class 'kpca' [package \"kernlab\"] with 9 slots\n",
      "  ..@ rotated : num [1:300, 1:297] 2.21 -1.47 -3.97 -3.3 9.59 ...\n",
      "  ..@ pcv     : num [1:300, 1:297] 0.215 -0.143 -0.387 -0.321 0.933 ...\n",
      "  ..@ eig     : Named num [1:297] 0.0342 0.0247 0.0179 0.0138 0.0128 ...\n",
      "  .. ..- attr(*, \"names\")= chr [1:297] \"Comp.1\" \"Comp.2\" \"Comp.3\" \"Comp.4\" ...\n",
      "  ..@ kernelf : chr \" Kernel matrix used.\"\n",
      "  ..@ kpar    : list()\n",
      "  ..@ xmatrix :Formal class 'kernelMatrix' [package \"kernlab\"] with 1 slot\n",
      "  .. .. ..@ .Data: num [1:300, 1:300] 1 0.12 0.25 0.191 0.183 ...\n",
      "  ..@ kcall   : language .local(x = x)\n",
      "  ..@ terms   : NULL\n",
      "  ..@ n.action: NULL\n"
     ]
    },
    {
     "data": {
      "image/png": "iVBORw0KGgoAAAANSUhEUgAAA0gAAANICAMAAADKOT/pAAAAM1BMVEUAAABNTU1oaGh8fHyM\njIyampqnp6eysrK9vb3Hx8fQ0NDZ2dnh4eHp6enw8PD/AAD///89ODILAAAACXBIWXMAABJ0\nAAASdAHeZh94AAAgAElEQVR4nO2dC5uqKhRAaeo07/L//9oz+UQFBNkqwlrfvTWnVKxYbtig\nqgoAolFH7wBADiASgACIBCAAIgEIgEgAAiASgACIBCAAIgEIgEgAAiASgACIBCAAIgEIgEgA\nAiASgACIBCAAIgEIgEgAAiASgACIBCAAIgEIgEgAAiASgACIBCAAIgEIgEgAAiASgACIBCAA\nIgEIgEgAAiASgACIBCAAIgEIgEgAAiASgACIBCAAIgEIgEgAAiASgACIBCAAIgEIgEgAAiAS\ngACIBCAAIgEIgEgAAiASgACIBCAAIgEIgEgAAiASgACIBCAAIgEIgEgAAiASgACIBCAAIgEI\ngEgAAiASgACIBCAAIgEIgEgAAiASgACIBCAAIgEIgEgAAiASgACIBCAAIgEIgEgAAiASgACI\nBCAAIgEIgEgAAiASgACIBCAAIgEIgEgAAiASgACIBCAAIgEIgEgAAiASgACIBCAAIgEIgEgA\nAiASgACIBCAAIgEIgEgAAiASgACIBCAAIgEIgEgAAiASgACIBCAAIgEIgEgAAuwgkgI4GStq\nubw4BxQBIAkiAQiASAACIBKAAIgEIAAiAQiASAACIBKAAIgEIAAiAQiASAACIBKAAIgEIAAi\nAQiASAACIBKAAIgEIAAiAQiASCGsOqMYSgCR/KktQiUwgUj+KO0RYAQieaMmzwADiOQNIoEd\nRPIGkcAOIvlDHwmsIJI/ZO3ACiKFwDgSWEAkAAEQCUAARPKEVh24QCQvyDOAm11F+n6/1beS\nud2/typiI8h8g5sdRXq8abdlum5SxFYwFgsL7CjSXV0+f+q/fr8u6r5FEVuBSLDAjiJd1E//\n94+6bFHEViASLLCjSKO+urvjnlyFpY8EbohIXpC1Azf79pG+fuu/TtdHqhhHAjd7pr+vWtbu\n7bFJEQDHsO840r0eR7rc3s82jkREAjfMbPCCPhK4SUckpbNNEeshawdu0hFp5yKCYBwJFkAk\nHxAJFkAkHxAJFth1ZoN3Nyi5CksfCdzsKNLHmUVanbVLMHMCG7Bn0+7n4j55QqCI7bAL4VKF\ntHkp7NpH+nFPDJIoYnfcqtAkLIV9kw0f2rzVjYrYG6cqJCmKgaxdHG5VEKkYECkORIIaRIpj\nQRX6SKWASJG4VSFrVwqIFMmSKowjlQEiRYMqgEgAIiASgACItA7aczACkdaQ6Hm8cByItAay\n2jABkVbQKoRJ0INITsztN2b+wBREcmBrwSESTEEkB7bpPzTtYAoi2bEHHpINMAGR7LhEIv0N\nIxDJjqsrhEYwApEccDYR+IJIDugKgS+I5IQWHPiBSAACIBKAAIgEIAAiAQiASKsgCQFjEGkF\npMVhCiKtgIFamIJI4XAWBcxApHAQCWYgUjiIBDMQaQX0kWAKIq2ArB1MQaRVMI4EYxAJQABE\nAhAAkQAEQCQAARAJQABEAhAAkQAEQCQAARAJQABEAhAAkQAEQKS1MN0ONBBpHUwAhxGItA5O\nSYIRiLQKTpKFMYi0CkSCMYi0CkSCMYi0DvpIMAKR1kHWDkYg0loYRwINRAIQAJEABEAkAAEQ\nCUAARAIQAJEABECkFZD5himIFAxjsTAHkYJhdhDMQaRQmK8KBhApFEQCA4gUCiKBAUQKhj4S\nzEGkYMjawRxEWgHjSDAFkQAEQCQAARAJQABEAhAAkQAEQCQAARAJQABE8oXBI3CASH4wnQGc\nIJIfTLADJ4jkBVO+wQ0ieYFI4AaRvEAkcINIftBHAieI5AdZO3CCSL4wjgQOEAlAAEQCEACR\nAARApK2hb1UEiLQtZPsKAZG2hfGnQkCkTWFGRCkg0qYgUikg0qYgUikg0rbQRyoERFrDkNJe\nSm6TtSsERApnkMNHE8aRigCRwhmaazTcoAWRghkSCKQSoAORgkEkmINIwSASzEGkcOgjwQxE\nCicsawdFgEhr8B9HgkJAJAABEAlAAEQCEACRAARAJAABEAlAAEQCEACRAARAJAABEAlAAEQC\nEACRAARAJAABEAlAAEQCEACRAARAJAABEAlAAEQCEACRAARAJAABEAlAAEQCEACRAARAJAAB\nEAlAAEQCEACRAARAJAABEAlAAEQCEACRAARAJAABEAlAAEQCEACRAARAJAABEAlAAEQCEACR\nAARAJAABEAlAAEQCEACRAARAJAABEAlAAEQCEACRAARAJAABEAlAAEQCEACRAARAJAABEAlA\nAEQCEACRAARAJAABEAlAAEQCEACRAARAJAABEAlAAEQCEACRAARAJAABEAlAAEQCEACRAARA\nJAABEAlAAEQCEACRAARAJAABEAlAAEQCEACRAATYX6SPN6VuX5sWAbA3O4qk6hWvqua+SREA\nB7G3SHd1f1TV7119bFEEwEHsLdJFPV5/P9TbFkUAHMTeIiml/WPytsbKIgAOYm+R/nUiXbYo\nAuAgdhXp9v7xpT7//nzc3dkGRIKTsatIfbNNqctjiyIADmLPcaSfn4+P261OOdydHiESnA1m\nNgAIgEgAAkSIpOYcuFcAR4JIAAJEiTR9A5GgVBAJQACSDQACIBKAADEiPf4pdW1P0ZOdZ4pI\ncDIiRHpc6kzdrXkRkaBkIkSqT857fFyu9YuIBCUTIdKl+eP38vaLSFA4Aunvx/WKSFA4ESK9\nqW4K99sVkaBsIkT6UP/av37VFZGgaGLS3/feni/hyywgEpyMqAHZn1v31+8/RIKSYWYDgACI\nBCAAIgEIICWSUn0OTwBEgpMhJ1L1eTO9sQpEgpNB0w5AAEQCEACRAATgmg0AAiDSCeA+N+nD\nde2SZ3RfKUgUREoepT1CqpBsSB01eYYkQaTUQaRTECvS9/3616S73r+ldmheROEg0imIE+nz\nre8evX3J7RSVRoc+0hmIEen3qq4fP68LNzy+3//+/j1yr/JCT9yQtTsDESJ9qdENLH/vSiwo\nFV5rpuowjpQ+ESLdpveBfYidSFF4vaExdz7I2qUH6YUTgkipMbTjSv4WTkesSI/75e/xcp82\n8+Iotwq9LOpUKvdbOCGRIv1e2o7xRS5lV5VchepP3ohU7pdwRiJFuqp/dfr7ruTOM68KrkNt\nKGri0sH7AiFEijS057lApATahOBD9wNCiRTp0l5I/4FIIpBnOCuRIt3V9TXL7vuq7lJ7NC0i\nGXaJEqQZTkps1u7aTrW7Su3QvIg02GmiDvOBTkr0ONLn7aXRh9DuGItIgt1CBd2jU8KArB90\nXsAJIvmBSOBEpmn3T/JkpFkRKYBI4EQq2SA6HptidV3XR6LDUwrR6e/LKxh9XZRouiHB2rcm\nnUYKrhyiB2R/6ucf9SazP/MikiE8ujAoVA5MEdoO+lUFEd206yISk1ZnIFJBxCYb3us+0vcl\n+5kNK0Ckgohu2nHJYjtNriGTDwNOEGlD6q9E8nuBZGFmw6Y0EmXzccBKpEjv3R8Pkg0G6CUV\nQ2zTrk0yvJP+NoFIxRB/Yt/f4+dFqXfb4mvIpeIhUjHE9pFep8i+KfX2I7VD8yLODHMbSiE6\n2XB/ZaVEw9GsiBPDbLtSiM/a3dVFNhzNizgzJL/LQCD9fVWydxkzFAGQOBEibTQYu3KvAI4E\nkQAEYGYDgACIBCBAhEjcsQ+gI0Ik7iGbKs/n8+hdKI6Yph13NU+S2iJU2pm4PtLnW5+0e5O8\ntB0iRfDUHmEvYpMN3/fXle2ud9kxWURaz3PyDLtA1i43VolEryoWRMqNFSLRq4qHmQ3ZEdRH\nqkORQK+q+JCGSNkREF/Gi65XgZAW3bS7tde1ExuLnRdxfvY+lcI7PIxD0XN1WCFRKHel1fzv\nIbuWdE/ua2t+q8/6sEKikGt/70C6p5uPBVgfVhApWqThbhQXmf2ZF3F2Er4AytCmq4ZYtMIG\nRBJo2l1eQ7FfF64iZCNhkYYgpHWP1oekkj3ijn1b0ycaUvxMeiiKEomsXfSAbH0P2Vv295Bd\nycuipC9brCXqosIK40i7rJJgEZ5EZq5Vt4kks3ZjCCsxIJKL2Mx116rbX6NVEaL4sBJBtEhf\nt1ctucmdizQv4jhiM9dHJRoILrsjkmz4e+0ialIiIkV7cJhI2qNtETwTJVKkD3V9vET6UKJz\nhHIR6aDB2OUEHCFLmugB2ccWU2DyEemQ6UEeIi28H1YcRopMEcpXpPCAMk8rHHHt70WRJKci\nENxqIkV6ayPSj3oT26UqIZHCjhHJTE9dCjiiIolt6dTI9JG+LupDbJeqdEQKDCjJTE9dihKC\nIjHPriH6fKR2itBVaofmRZyGlGbVLfRb5MIIIjWIjCOp26fQ7hiLOAspibSAXMcGkRqY2SDH\niUQSTLXRR6pBJEGS6SPtCVm7GqkzZC+c2JdQ1m5f+uBW8oCSkEi/eY4jBSN+PaUTUXZoihDp\na3Q1rizHkYIpNCbVlN1ZiolIb7pHohf/Pm1N3KGXlGrI2yZ9d5rWolQfSZY0q8oyQ95uq+o+\nCnlJ1TJfkUJ2+kStRbJ2qzG4MjqRbxOVtJCXWC3zE8m600a/TtRalBLpW/TqJycQyeiKat9R\nzWzeDUrVnlOrZV77Y1nI7NeZBntjRbqXeu1vc2+otqt93OJjaCIlV8t8IqRtp81+JfcRHUSK\nNHgkeh2h9EWyzGLoo1H9Tlki+XR/LDs9f1m7S0ZSH9FK9Il9n9VV/f5eVWFZu5lIXUxWSlXb\niaQFwjPVsh5Pkbrgllrr1YFA1u79Lxr9yE7/Pp1IWo9paNdtkmwYCjpRLRvwa8N1SyWWT3Eh\nINLX61ykwvtIavyotfDkCx6S39VZatmAZafHfmnPx2X4A0uOFOn217T7VW/Vd3ki6Vm7kU/t\nOzt8hqTGkXwx57lHfiXQag0+SkWK9PWqL/UluXK8ipAbzRU1vLSbRZmh+5WCSKE7EJv+fn/9\n65+Svc/YOUTSmPeUIIbGqsM9CtkFZjaIoGaPsJr6HjPr78MpsguT52UQSQRjjwnW0XRPDu3+\n7S4SJ/Z1aJM7Vu49PauGBHpI+/eROLFPI0ok+lcdaYi0Y9aOE/umxPSR6F91JCFS01MLMCkm\nIpV8Yp+xHRYRVSKbhVmRyJSNsKAk1UeSJfXqZDVmdT8HkQYSmbIR5jNZuzXIt8PWiHTKiQ1e\npPDJAluY0SJ9Xsu70mpkVsG4WrCbiRy3s2Vnka4lXvs7QiRHo9Dyho1EehLZsq9IH+ryOqMv\n37tRGJmL5H2KcDsZz/6WL4nktjJm1z7Sm/qpn7O7P5K7Vk/aYc1tdD086MNO/AdEpK05JmuX\n14DsUjtr8r4aTp/w2G5/7mzUVAZE2p69xpEqPSJlNUVoueevS9DPsVvabdVvWMVPZaCPlBT0\nkRzFe+5GgEj6qX8hRRgga5cUZO0cxQ/tVucOBYmk2sSEwAhsCqMt0BI/jpThHfvGlXy5Debf\nR2rze0pJiAQJwcwGe/nK8A/Twioka9ddhPXUIhELZ0SKdJM9xdxUxBHoQWihxncXaQgaR9Lz\nDueD3pkBqfS3LMdXr/lpejaRXG/aFq/awHTOE5DC84UFRLDo9PdDbFcsRRyNW6TQFpq+/Mpx\npKNrZfAIVhERLFKkx+0qeiKSoYjDccac4K6OWNb7MJ/CRQpb/JxEN+3yvxuFsw0WLpLMOOwG\nR3lfM20i2dYvYw4GInng+nThEUZwZpBg3Qww01i2fX1EklslwSKk2Dln0NbGrsbKVc4AM43O\n2NdHJLlVEixCjl0vo7WVSGG1fd6Kc61PH0lslQSLOCvjWnmQSGHrk7UTWyXBIrxI8JqNT/06\npNItuzVbHF+n25JvyF0jRHKxpvvjrd56R5u7Qkof5VeaOWTj162fDYhkp0/IBdhR+aknkKIQ\nPsqvNLP3p4D2m/MbRyQr/RCRtdLPBPPOhac40W6NmXqLLvP228IgOCJZ6UKRrdLPBfMenT31\n1G+dMlLbNU+3SohkZXL3vdk+zQXznkiXuEj+saUckbpuICKFMzZluk+dX/rFG/rnhT7QZiJJ\nNK+CejvF5Biew63Wje8jkpWFiKSaZSr99uVDesK4ymRl8Y8Z1uF3z43zFcldZD79JkRazUIf\nSXXLaO91gWgx4mwzsShEAasBoa01hytZZfKaz/G0fTGIZGUpa6dJpLXuRldSdXWT5Md6gxSw\nSifY7cmq3dcN39FHCmZhHElr1hmafaNLBe1DiAL2ZeVEyi0TMYyEG0AkO4vtL0dGb5Om2wIy\nIsnFkedk+tD5YRzJWMhyRV9cxH+MaQ9CToWYPOtvSfVsug3lI5KDYkWy9nyCqr9lK8c07YIU\ncEknlGtrbsKaS7JhgXJFMhYUHknMN5OdPMcRUK8DhlI3z6g9nx6XlsgmP16qSJaqbtMrcId8\nRfLZ8GY1fus63GrkKiWj/DgiLb666mwKw8bnC3lsWP7Mo93wyH2c9rPNQaTmaZzGVvNFA0Xy\nkW95w3VPvR8JPBndIcC+54H58aSbgaWKNKrFsxvpNVY55Fre/HKrTbVlOJZ7apMlg4pPge6S\nYUIiJd4MLFckLWYMj9pfRrlWbNy+0OJiz8n/J6MZR1ps2cVOxUiDYkXSh1OH2DNU7WFaw4p9\n8usiLS6mze5KtgI5WI4hMgNfSVCwSH1ho9gzb9CF95Gm7lmaN92GrZtuRBr6SadjqVcTfFVK\nREquCK2wPhwZ9mAUpby3OHq21pbpXPE5XXc91doTj/dnQ6Q17CnSEIpMe6BHqZBNDs9Pa9ZN\nTeaKz0m8g70r9JFWsF0RcycsMSe4QTc/Vfb1+NKoecdo0mIZOUejMBI/qJQlktWYebY6sEE3\nXnz411/35u/p+fe3o20HXiR9UClMJNPWbXEhqEE33Ui7cj8KZBQpvNG4H0nW2iR3qqEokcyZ\n7Li44D4l9q9d144CJVsDjCTZjkpypzoQqYqJC0sXaahFWp4DnRxJ9uyT3KkORJottKZFZ93y\ns884+G/0eJLMNSe5Uz1FieSRJgtr5w3+2Lb8rHMNJ/MozTqb5E71FCbSoiZhWW9NJOuWlXra\ny0w02ZBknU1yp3rKEmmx5no1/oYt6Yvbt2x9J930d4rdEfvIdgqUJZLnxUw8dmB0FdbVqYqo\ntbckvQRZffWHhC8BUZJIhgAwNcsqknlB5YwpS6m6gPC3P6nlGdsJvGntlEZRItUP+kweg1n9\no3NBVXWXCTIEuXZKkOWgPlSGpEVKjLQ7SFVRInUyDFXf0LTqB4ZG6swWnFxg37SFtpsxWUbX\nC5H8QaRVbCdSpY36mCuyPlXBuqBqJufZL8WlWlemKbtRLz7dPlJyINIqthJJ6c+uiGAORUMU\n6gLSfNXpms9RAeP6kG7WLj1SzCPq5C3SuKYrLSjppQSINLT8bE270Zqq/enVNHfbd5PQyJPU\nb8SUs0jTA/7QGhs12UyFTRzrc3Rask4pZYom/ZpNKHoOaz6fJ2ihJEzaN2LKWqTZtsYXZLAl\nr4eh1t6KLhD156UvOdh4o5oft119uBgQHsmSwLeasUimlpumjq1xpr0+jmZK72a5pgR1D896\nol2lueV/7Dy8qXImUojzhYmkqWOLKd3rpiHY/v8WS8HDGyPflPegYgJNlTNRnEjf77e6/t3u\n31sVYdiIeWPTd6fTfeaRqn1QbXjyuJZqu13VZbt9f+i0Z5WlR2EiPd7UwHWTIkxbsYWN8UJD\nTtu2C117UFkbhfb9GA/ALjDcYRGTPCmrj3RXl8+f+q/fr4u6b1HEZCuuYRpTgtsp0jR1Z0k1\nGMdon03mwWenu9PSEcnBtHmcQFN4R5Eu6qf/+0ddtihith1H2Jga1D1b41jv5UikURFjdftb\nsjzVkHhYpL9eylQk8g8tJm0O/3J2FGk8OGpKOg+sLCJ4dwwdI0ccM1zOeLxwuzWtcdYGlqGj\nZN1qxxCORmskcNBNhQQacnPyjkhu5o00NXndtmL/aEpQtGeX9z+3uys887YTZnKMFag9hx+1\nZUghtTBn3z7S12/91059pACceYn50sZGXi9S/Tz83AsizUo2Jsnja082Ia14kaqr1nZ7e2xS\nxFpCp4+ab0JWPz2b567KLohkSFwYa7yASJHrN6snYCIiVd/3ehzpcnvfYxwpjFUdM1PTsL1i\n0CCSswobM4CG6hpdeySqXyJBrfQ+UlpFyDBpmL1kfKquhdc9uuqfK5U+Irb2iIgUvYWg0mzK\nJuLzmGJF8ohAXotU4yZhn5576j+3o0Xk2zuLrT0CIu3apnJ+3hRamBMKFcmjT+TZbZrb1lcB\nr5/bv3cWWXviw8m+Iu1XlAilirRcjGER335UWJXf6eS++AbRniKlmVFwUKZIHj2T+SKnPzE8\nukG0Y5RAJBFsRUgdvNeJtLRK/faZTVtix15+uEgH95vOJJJcSFgjknmdyVUh0g1aMvVsv9oa\nGP0Oz+SdSiS54lf0kUwizc6jXd7sXoyr/OH1LJjAPT48N3EikbyHXHwKCM/aGUUavyK5h3FM\nq+F29Wy7GBWy5eO7VIWKtGYcaR5uTBMbpsscw0SczepZKpEOkQKKOLqaji6cokx7JLCHIgf4\nfiPT+iUv0jabDQaRQorYuAeyHKM6fzqjLAmJ9XsocoB/3fuk3Yh2MsfoWYrj62/H4UafSqT4\nnJjrbmDeGx90mSUkIvdQZoZ2/19/fuGKDXuFxoREOrqNeSaRokdpDPV88epchq0Mz/MNmmYM\nBaaeIutlJ85Tdyi0nnkun45IjCPtWcRMFo+LB9m20j4vqR1UgwVFau9vN2wtrJ75RrDDW1TJ\nUJJIc1mmjbRwkZYIqmlyIunirNia96qj48R06Koov4oWSW+kBZSsBbbFgDR5XkCqj1Q/7iGS\nniEcxd7D+yx7g0jVJG/gk7xrH5ZzC6EiCWXtKu3WF/OtLceKNQ6a8oOIJLxKIkXMkmzac3f5\nVK+822gcSVAk2XEks5Zesq7I8hnXLMekskTSZKlmN29Rw9CQX/mW5mB7YfDmH8cemU1aeu1R\neGiMFensXaqiRNJCjvnmLWGJBOPCqmsdNptNrq/gW8VDK3acSMl9TcEUJlIfcrrIY5xPFyVS\ns9nhZY8auefReLNGV1Qf6fxdqtJE6ptx5oKCRDK1A9Xkfw88jsaBprkW306kiKxdBl0qRNLe\nm/aaputOkxCmmRJVuEjao3mBsHbPwuLbHfzXjyMh0kYcIZKl1zR9f/ri9IUqVKTlSiTbTEqx\nO4JIG7FfH2n2hnUcyTOfN+sjLbFYiVYN6jrFTEyjij7SVmwpkmW8aKl35Nt7mmTtltlfpARJ\nMUyGUZxIw2hqWL5ueN+8/nj7xjctkWDpaFyCSEmGySAKFMld5KJIs4jmeW6H9aC7eDQuLpV8\nRooRKeDuYc73p32shTlF00nY5pi0lPyu/Ns9z+f5m0lnpBCRBK71PXp/yPo5/dPqdFSLy7vd\n0xaIRrtTikg+m10KWsM400woy6a1KBQi0noRaNYdRRkihU1Y8NiSn0i6PP4iRTTNTppoyIFC\nRDKMG63dVLvB9h/eIrUNLlMVl7vt8mqRaAzGUoJIw7COhEiTrJ3L0KFeP1uNmvo6tCGf89xA\nTFRZuS7piXiKEEn7T2Jz44tEunIUbXBpHWqlGdYw5CJisxKrVqVnFU8BIvWxw1DdV1/fy28c\nqUui1bW0E2aIYU3faVKNI9N7VXhs2axnVVKDsRCRbBpV9mjitdHF5Z7PZ59raP4felWdWcaQ\ntDpvF155NxKprAZjMSKZNro+AxGUBrSI1PaO5iLtXQG3EmmLjSZLASJZfVmXFNcu7RAgUtU3\n79pV+it0DxGrW3z3AdVNqnxhqfgiRLK04AJF0pJ/C+OwU4ae0rNfp+8fTQLQEQ2iTcpEpA1W\nOboIcz4gyIaRQZaZQbZIMp640yTs2qzdcxqAjmkQbRADEWmDVRIsYijGr7DxsqYzaV3H9XFF\n/Vv7aXq9yqr60UeSXyXBIupi/LN206lBWpTr/vCpONZbgFWV+/UTQtZOfpUEi2gL8k1+z0Wq\ntFdeDx4K6FXLol1GIjGOJL9KgkV4MdSEcX9K27/+BR+R9EfL8dqzQbSqjmoXvDesXVK1lweR\n7Izquj6sq4WxyfBq5XJgUovNFdfv0tyrJjB0KxnXLqshJg8i2XnW3nQimWcaaZFqMZY040nD\nHV5ti3lcmnWpKOdKxrXLSg3Ig0hWnq07fetu2p961XhdJP2Ybr18/VOry+t2a+UtxPqVjGtn\n1Tc7AkSyZhyebWvuaV6y9UbvNFluujWsMki0tsKObQ1aU9/CbG1EigSRrDnwWd0aL9m2hYyr\n21JyQ7NutUjtdswFLK5ZxYlEPsIKIk3ycNr0n04V45JDzTPcyHzyrL2zyoHZllfJGN1HIh/h\noHiRJnltLb6oUeNtuqTrEL78XlRA8qvS80kTsVm7Y/IRJ4mCiDR6HgUdNUonSIkUdVgf2meL\nGhmKiRtHOqQbdZooiEj6sz08zd50Hp+dx+6oQ6xvVNggehwj0v5FrqN4kUyhaDI31bSk+1i5\n3XHUc8tbVPojRDpPMhGR9LhTBwtlm4A3TdA5b4xnmbYQf9Ke1wY2qYAHRAdEiuOYcaS6Q2S7\nTMpoyXU8n9q1hLZlG5H2768gUhzHzGxootErn73NDjxHI0nbYk5x7xINRaGPFMUhItW/VnfO\n3QZ70Cee9zDJED1OkwDTOc1OI1JPJ9BEpJDmnPOI3Q2FPvc5wlpOvj1BnRzDOFIE6YgUcum7\nhaNnL9IxVUOouxG084dKsG/hiDTQGDQOSJMJRMvrOyrqs0k3HFS/REQKamkd2izbu/A8RVqX\nXau/9nEImj47V588m7ZfR6Pn84gGloxIlk2Yp0rEl7eevQvPUaT1VyLuxpFmOyIiUjeO9Nyp\nlzQtXHuM2oQhG1gZjv6Hpq53LzxLkQS2MdkRIZGGDtIRIsU3dmwimV5EpA1W2bWIkMrvubHl\nTQ0npbt/umNrV/Q40uTZ9SoibbDKrkXEi6Q17haaiaObJGnnKbgyDsPj+TCP806enQvvBX2k\n6LJPbAoAAA0kSURBVCJiRZpO+nZp1D50cWu4wLdVpdOMMBoxn50xeXYuvBdk7eKLCO4jdRfZ\nMk0E9ynIdKaSIyadVqMXplEw6ydmHEl4lX2LCMzaaRaF3bFlNGjbv3Bo12Bz1p5dmz05ihQ4\njjSJQ4jkwhJ8zh1lJchTpPCylKWdtrymvu7konP5Va+8jxIxINJUpBV9pK51qDdxnlk2eDxm\nb+T1gb1BpLlI3l2s6a3NW4Vagc6d6DazINJmx470/USkaR+pCulijZbsa9loElDqNSAM99Fh\no2PHGWI7Ik2zdusZq5OpSK46vdVHPkNsR6RqOo60muNF2uVyEM4zrkbPUiVus1lZEEkQk0Gx\ns9sC1Di8BYRIW6+SYBFbMK7JErOtgzYh3gIKDnDbtMEQaTUnFWla9YRmW3tuRLq+rTgQbBQT\n6SOt5bQiyRKmRpd8FxMpoOxhpW2S39UWfoqStUjR6YON8K1t4SIJXhEiqQYV40jrEClCIKG9\nCUvH10H/kLrcD149w+q+tYomJVL65CyS3KaWCThkuptMI/29W1etnM/uL+99sUuNSEHkK1Ls\n+X2TrTlDW0gjfqGCjvT33m7fPwrs2LgWP0MXPx0QyWtbhlaiHoOMdc4SpNwiTffaL9JNtxmW\nMLcsf4Yufjogkv+29C2NapmpPlqrYZhIfoyGroIDknWF9Lv46ZCvSIJ9JEPtHtVYo0izV5bf\nMRflwSBSWBShHyRGziKJZe3mtXtcAw310VFF3ZV9nf7DfoTdgIl+kBQZi2TPEISeJrEkkqE+\nLvQ+XMnvqjlHcO2UgvNcnjsrshbJsvFRpHJJNbvcVs9UpFl9jGg0qfGptp48n1ap/dbbhXx7\nXSWKpD86m3+Ok2an1VX2bkQRK6fc7ck5/pUn0qid5uyR6EtOA9dinYipNDEyJC2S9pgbZYvk\nzpG5311spaxvxmQqUsK7Fg8i2cvzTkVLt/yjaly6h31Eil4lqSK05tyCKqp2ZGlnNmj5x8iQ\nbkcEkaJXSaoIPXMwTm/PlnzVR7XYhNMehYiTIdnUWLrBMp5yRBrdq6XXSDUPFp7a5eoM22mW\nmTzLkKwMMaQbLOMpRSRjntuUjtMxOGJLhOd6oBUmy+NDTTEiGba7mEwwiTRbA5GgKkYkozPa\ni+Yj5dwR03ZybvmDL4hUVfYZOTNHjCJ1a+fbboFFShapb6dZY8qfGa+Zb0vb8bjjJWROISKZ\n5wJ1M63bfxokqOePNrmFJt7Y5xTRwiuaYkQyz04dpbdNInWPXbyxznJNPOdAs3NjShHJlee2\nOzC05IZ4021nUjWTFolm5+aUI5IDa6tsSEd0HaF+lWnVTFsk7RE2AZEqxwFby+s1iwwiaY+2\nV5IhackzAZFqbF2IPq/XajKtknpICrtawp4g0vYgkpMut5CUSF6lzC+7h0hbgkgLNLmFadPO\nJNL0hY3wShxMFkq42ZkLiOTFPNmgPY7+3Ly2ekkxWYis3eYgkh/T6ntY1s6rnPlCifbe8gGR\n/DBccOuYcaSVIsHGIJIvS8f0nfohiJQmiCTFXv2QNX0k2BxEkmOv5Hc1CGsrkuzC3iCSHKOh\nmw1r8XB1YocuZBf2BZGk0Gv1TgGBBlw6IJIUT8vj1iWKlUMMiwGRQvG4o+VOSTPRYuhVxYFI\nYXjd0fKUIsltqkgQKQxrfTtAJMnKz8hTJIgUhKO+7d9HkmyOIVIkiBSES6T9s3aCCQJEigSR\ngjDXt2d399YTTxKljxQHIoVhqG955Lvy+BTHgUhhGOpbLsfy0wXRpECkUKb1jd4FVIgUiOGo\njUhQIVIQxn4EIkGFSEGYe0O59JEgBkTyxxJ7yHcBIoVgbcSR7wJE8ufZXd7u6B2B9EAkf6bX\n/wboQSR/nvSGwAYiedPFo1RFIlYeCSJ5k/aAUSLRslibSxHJfr8+bxIXSXs8bifSsPkIyhDJ\neufXEYlcS3UVaVgu9g2dL7AVIpLHVpePpsHH2x2rQxIiiexEd5epk6lUhEhq8mzE52gaZMau\n1SEXkbQvDZHiOUAk+Zq4b0swhXanhEj1/yl8mEAQqUVcpJ1jRBLNoWgBulB0vpBUhEg+faSz\ni5REBz3aZkSS5YisnXRzIoley+5E2tz9COeb0liGSD7jSOJtoxM29I+nCUUpNFMDKUUkH4Tb\nRmesDofTTQw+3feGSBtyvuqQACf90hAJQABEAhAAkaactGkBx4JIY8gQHMtpD2OINEZ8xh0E\ncOLDGCKN8BhFtd+z75w1ICVOPPSGSCN8RDIvcOKDaTKceTIIIo1Y/iltS5z4YJoMiCTNniKN\nJw8t+mD5sc9cB5LhzF9i6SJNp7MuttAQaUNOHNaLF2lW3rorNyCSBCfuaBYuktdJ6GMsP/aJ\nD6YpcdrUJyKFF2j8sU98MAUBEEmswNMeTEGAwkXyulAXwCLFi+R16UiABUoXSeRixgCIBCAA\nIgEIgEgAAiASgACIBCAAIgEIgEgAAiASgACIBCAAIgEIgEgAAiASgACIBCAAIgEIgEgAAiAS\ngACIBCAAIgEIkKhIACdjRS2XF2ctCe2KBJl9nNw+j/THSejrSWhXJMjs4+T2eRDpLGT2cXL7\nPIh0FjL7OLl9HkQ6C5l9nNw+DyKdhcw+Tm6fB5HOQmYfJ7fPg0hnIbOPk9vnQaSzkNnHye3z\nINJZyOzj5PZ5EOksZPZxcvs8iHQWMvs4uX2ejEUCOC+IBCAAIgEIgEgAAiASgACIBCAAIgEI\ngEgAAiASgACIBCAAIgEIgEgAAiASgACIBCAAIgEIgEgAAiQk0urrlyfI/aIu98fReyFGTj9N\n9dF9DtEfKZ0v5yejX+taf5K3o3dDipx+mteHaf6Q/ZHS+XJ+1O3oXZDiW11+qp+L+j56R4TI\n6Kd5/SxNnRf+kdIR6UO9H70LUtzV19/jZzYfKKOf5kNdW5GEf6SURPo4ehekuKnfKqfjeEY/\njbpXrUjCP1I6It3U17+/zt/RuyFB+1Nl0qnI6qf5mf46Uj9SOr/1renQXo/eDwHyEymbn6bK\nXySlPqvqcc+hFZGbSBn9NFX+IjU8ckga5yZSQxY/TZWvSJMhihxq3yVLkXL5PO3HEP6Rjv9u\nMhSpSQj9ZpO1a8nhp6mqcdZO7EdK57u5qNdsjSxq33s9RPGlsshzVVn9NFUvkvCPlI5I99dH\nejTDZCcnt5kNGf00VS9StjMbHpe6jZfFUfwtp3RxXj/N0EKV/ZHSEenvkHdRb3lkWB/1xOKj\n90KOjH6aQSTZHykhkQDOCyIBCIBIAAIgEoAAiAQgACIBCIBIAAIgEoAAiAQgACIBCIBIAAIg\nEoAAiAQgACIBCIBIAAIgEoAAiAQgACIBCIBIAAIgEoAAiAQgACIBCIBIAAIgEoAAiAQgACIB\nCIBIAAIgEoAAiAQgACIBCIBIAAIgEoAAiAQgACIlw+wOrZZbtrZ3nPu6Ld5n/PbVrjBe7kMZ\nX4YY+CpT4W36U8xeaGmq/+/rTuMLJjzUb7PCaLkfhUjy8FWmwqxW26p58/r17lik436db+jn\n0v8bkeTgq0yFMJE+XwFp0YSH+pwu9aGuiLQBfJXHoNTjTf31cqqPt+Z+4aptaX3dVHOv7e6F\n1xKX9o7i94u6N9X/TYs1d/X++uve36P7b7Fr3airrm/VxJduA9X0DYiCr/IYlPoT5q/eX2tf\nrr037/Xz651OpFu3RLvw7fXqt/potlK9PKpXfx8vdnlFrL/48z3x5adCpC3gqzyGvzr/qumf\n6vLz6rV8drVavf78rP9uXvh6Lfi4qq9hYfWS56dqF6k9+vur39Dna41/qo5OP42S07Knf0A0\nfJXHoF6h4hVuXhnqryam6O92D39LvIR7vJqBt3qdr9fr1/rVpkHXNvKaDXWLPdSlqte7ItIu\n8FUeQ5+C7p+6Wv379X7VRFIdpoXVK3PwbdmQ+YUKkTaBr/IYrCJdO238RFIX9WbeUGV+oUKk\nTeCrPAabSP/U28fX70gk0zr9n98/Sk9xI9JR8FUeQ1uHb0PXRvNGF6lZQlv4e9xHqt7r3pBq\n+0//Xu/RR9ofvspjaOvwKGv3WzVC/HR9pN9uierjpdqXnrUbekZv6n3I2n3VQ66PvwXqrN03\nWbud4Ks8hq4O9+NIfz68gsi97RF9dy+0S1xeUtVDSv+acaT3fit/jbtHk3ZQ9RCvPo70ro0j\nzf1BJDn4Ko+hr8Mfl2ZmQ/X9Vnvz58n1u27rtS+8Zjaof81EhXfjzIb3v6X//rq1G6ptvM1n\nNiDSlvBVnpOvZmJ3j9mJ37pX1b03WwSR5OCrPCnX++ifZidGs78//03fRiQ5+CpPym+Tt+sw\nOjE+H+k2eZfzkSThqzwrX6MAY3Tin/kM2X4VRJKDrxJAAEQCEACRAARAJAABEAlAAEQCEACR\nAARAJAABEAlAAEQCEACRAARAJAABEAlAAEQCEACRAARAJAABEAlAAEQCEACRAARAJAABEAlA\nAEQCEACRAARAJAABEAlAAEQCEACRAAT4D9aMUIjPsnPYAAAAAElFTkSuQmCC",
      "text/plain": [
       "plot without title"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "kpc <- kpca(K)\n",
    "plot(rotated(kpc), col=reuters_train$Topic)"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "We get a similar result but it has a different scale. Also, this result is upside down when compared with the result from exercise 8.\n",
    "\n",
    "## Exercise 10\n",
    "\n",
    "KPCA discerning non-linear components."
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 115,
   "metadata": {
    "collapsed": false
   },
   "outputs": [
    {
     "data": {
      "image/png": "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\nEAlAgP8BgaXZTnxQWiYAAAAASUVORK5CYII=",
      "text/plain": [
       "plot without title"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    },
    {
     "data": {
      "image/png": "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\nZGIAAAAASUVORK5CYII=",
      "text/plain": [
       "plot without title"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "data = circle.data()\n",
    "plot(data$x, data$y, col=data$c)\n",
    "\n",
    "#kpca\n",
    "kpc <- kpca(~., data)\n",
    "plot(rotated(kpc), col=data$c)"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "Once again, kpca did separate the data. This time, as the data was easily separable, we have even better separation than in previous exercises. This time the data is even linarly separable - can be separated using simple line function.\n",
    "\n",
    "## Exercise 11\n",
    "\n",
    "K-means kernel version.\n",
    "\n",
    "Steps of standard k-means algorithm are:\n",
    "\n",
    "1. set k cluster centers\n",
    "2. assign each point to its closest center\n",
    "3. recalculate new cluster centers based on the points assigned to it\n",
    "4. goto step 2 until nothing changes\n",
    "\n",
    "However, I'm not fully sure how to take this to kernel space, I'm not also fully sure how to do distance calculations in kernel space. This means I'll only show results from kkmeans."
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 199,
   "metadata": {
    "collapsed": false
   },
   "outputs": [
    {
     "data": {
      "text/html": [
       "0.776666666666667"
      ],
      "text/latex": [
       "0.776666666666667"
      ],
      "text/markdown": [
       "0.776666666666667"
      ],
      "text/plain": [
       "[1] 0.7766667"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    },
    {
     "data": {
      "image/png": "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\nYII=",
      "text/plain": [
       "plot without title"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "set.seed(777)\n",
    "kk <- kkmeans(K,2)\n",
    "\n",
    "plot(e$vectors[,1], e$vectors[,2], col=kk@.Data)\n",
    "pred <- ifelse(kk@.Data == 2, 1, -1)\n",
    "mean(pred == reuters_train$y)"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "Ofcourse here alot depends on where initial cluster centers are. The best result I got was accuracy of 0.77, about the same as we had with perceptron in exercise 4. We can also see that k-means did separate the data in about the same place as PCA did. "
   ]
  }
 ],
 "metadata": {
  "anaconda-cloud": {},
  "kernelspec": {
   "display_name": "R",
   "language": "R",
   "name": "ir"
  },
  "language_info": {
   "codemirror_mode": "r",
   "file_extension": ".r",
   "mimetype": "text/x-r-source",
   "name": "R",
   "pygments_lexer": "r",
   "version": "3.3.2"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 2
}
