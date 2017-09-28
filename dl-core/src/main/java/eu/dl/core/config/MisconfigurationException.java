package eu.dl.core.config;

/**
 * This exception(or its subclasses) should be thrown in case the worker failed
 * and message shouldnt be resend, as its understood as unrecoverable.
 *
 * @author Kuba Krafka
 */
public class MisconfigurationException extends RuntimeException {

    private static final long serialVersionUID = 8995212758791087970L;

    /**
     * Initialize exception with a reason for failure.
     *
     * @param message
     *            reason for exception
     */
    public MisconfigurationException(final String message) {
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
    public MisconfigurationException(final String message, final Throwable throwable) {
        super(message, throwable);
    }
}
