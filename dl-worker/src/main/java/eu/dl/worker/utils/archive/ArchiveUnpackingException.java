package eu.dl.worker.utils.archive;

import eu.dl.core.UnrecoverableException;

/**
 * This exception is thrown when archive unpacking process fails.
 */
class ArchiveUnpackingException extends UnrecoverableException {

    /**
     * 
     */
    private static final long serialVersionUID = -1991752768696142433L;

    /**
     * Initialize exception with a reason for failure.
     *
     * @param message
     *         reason for exception
     */
    ArchiveUnpackingException(final String message) {
        super(message);
    }

    /**
     * Initialize exception with a reason for failure.
     *
     * @param message
     *         reason for exception
     * @param throwable
     *         root of the cause
     */
    ArchiveUnpackingException(final String message, final Throwable throwable) {
        super(message, throwable);
    }
}
