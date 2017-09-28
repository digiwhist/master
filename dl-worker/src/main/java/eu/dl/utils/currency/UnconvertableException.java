package eu.dl.utils.currency;

import eu.dl.core.UnrecoverableException;

/**
 * Exception used to indicate an error when converting currencies.
 * 
 * @author skajrajdr
 */
public class UnconvertableException extends UnrecoverableException {
    private static final long serialVersionUID = 3836343258441916418L;

    /**
     * Initialize exception with a reason for failure.
     * 
     * @param message
     *            reason for exception
     */
    public UnconvertableException(final String message) {
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
    public UnconvertableException(final String message, final Throwable throwable) {
        super(message, throwable);
    }
}
