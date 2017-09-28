package eu.dl.worker.clean;

import eu.dl.core.UnrecoverableException;

/**
 * Exception used to indicate an error when cleaning document.
 * 
 * @author skajrajdr
 */
public class UncleanableException extends UnrecoverableException {
    private static final long serialVersionUID = 3836307518441916418L;

    /**
     * Initialize exception with a reason for failure.
     * 
     * @param message
     *            reason for exception
     */
    public UncleanableException(final String message) {
        super(message);
    }

    /**
     * Initialize exception with a reason for failure.
     * 
     * @param message
     *            reason for exception
     * @param throwable
     *            root of the cause
     */
    public UncleanableException(final String message, final Throwable throwable) {
        super(message, throwable);
    }
}
