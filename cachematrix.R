## This source file contains code for Programming Assignment 2 for the
## R Programming course on Coursera. The objective of this assignment
## is to wrap a matrix in such a way that we can cache the inverse
## of this matrix. We follow the instructions in the README.md,
## using a function makeCacheMatrix() to create a wrapped matrix and
## a function cacheSolve() that computes and caches the inverse.

## makeCacheMatrix wraps a matrix, allowing us to cache its inverse
## (which is computationally expensive to calculate). You can change
## the value of the underlying matrix, in which case we will invalidate
## the cache.
makeCacheMatrix <- function(underlying_matrix = matrix()) {
        cached_inverse_matrix <- NULL
        set <- function(new_matrix) {
                underlying_matrix <<- new_matrix
                cached_inverse_matrix <<- NULL
        }
        get <- function() underlying_matrix
        setInverse <- function(new_inverse_matrix) {
                cached_inverse_matrix <<- new_inverse_matrix
        }
        getInverse <- function() cached_inverse_matrix

        # This is the set of functions that we export
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This is the function that returns the inverse of a wrapped matrix.
##
## It caches the result. (It also checks the cache, itself, first.)
## (FYI, I really don't like having the "..." in here, because I want
## to make sure I have the true inverse in the cache. But it's in the
## assignment, so caller beware.
cacheSolve <- function(wrapped_matrix, ...) {
        inverse_matrix <- wrapped_matrix$getInverse()
        if (!is.null(inverse_matrix)) {
                message("Retrieved cached inverse")
                return(inverse_matrix)
        }
        raw_matrix <- wrapped_matrix$get()

        # This "..." passes in the variable arguments we didn't use:
        inverse_matrix <- solve(raw_matrix, ...)
        wrapped_matrix$setInverse(inverse_matrix)
        inverse_matrix    # Remember, this returns the value
}

## A test function to make sure things are working as we expect.
## Returns TRUE iff all tests pass.
testMatrixCaching <- function() {
        identity3 <- matrix(c(1, 0, 0,
                              0, 1, 0,
                              0, 0, 1),
                            nrow = 3)
        wrapped_i3 <- makeCacheMatrix(identity3)
        if (!is.null(wrapped_i3$getInverse())) {
                message("wrapped_i3 has a non-empty cache!")
                return(F)
        }
        inverse_i3 <- cacheSolve(wrapped_i3)
        if(!identical(identity3, inverse_i3)) {
                message("miscalculated the inverse of I3")
                return(F)
        }
        cached_inverse <- wrapped_i3$getInverse()
        if(!identical(cached_inverse, inverse_i3)) {
                message("we\'re not caching the inverse")
                return(F)
        }

        wedgy_matrix <- matrix(c(1, 1, 1,
                                 0, 1, 1,
                                 0, 0, 1),
                               nrow = 3)
        expected_inverse <- matrix(c(1, -1,  0,
                                    0,  1, -1,
                                    0,  0,  1),
                                nrow = 3)

        wrapped_wedge <- makeCacheMatrix(wedgy_matrix)
        if (!is.null(wrapped_wedge$getInverse())) {
                message("wrapped_wedge has a non-empty cache!")
                return(F)
        }
        inverse_wedge <- cacheSolve(wrapped_wedge)
        if(!identical(expected_inverse, inverse_wedge)) {
                message("miscalculated the inverse of wedgy_matrix!")
                return(F)
        }
        cached_inverse <- wrapped_wedge$getInverse()
        if(!identical(cached_inverse, inverse_wedge)) {
                message("we\'re not caching the inverse")
                return(F)
        }
        T   # Yay, everything passed!
}

